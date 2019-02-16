# NHL Player Utilization by Team Over Time
# 
# This R script will create an HTML table showing the top players on an NHL team
# for each season.  For purposes of this script, "top players" is defined by
# the percentage of available minutes a player spent on the ice.
# Data is pulled from the NHL's stats REST API.



# Set variables that control how the script will operate

teamId <- 18;	# 18 = Nashville Predators on nhl.com/stats  (call "getLeagueHistorySummary()" to look up the ID for another team)
colors <- c("253,187,48", "0,45,98");   # Hard-coding the colors to use in the final output (this is blue and gold for the Preds)


source("nhl-r.R")
library(reshape2);

### Build a list of seasons for the specified team
seasons <- getTeamSeasonSummaries(teamId)

teamFullName <- seasons$teamFullName[nrow(seasons)];

if (any(is.na(seasons$otLosses)) == TRUE) {
  seasons[which(is.na(seasons$otLosses)), ]$otLosses <- 0;
}

# Get a season-by-season summary for every player that's ever played for the team
skaters <- getTeamPlayersSummaryBySeason(teamId, "skaters");
goalies <- getTeamPlayersSummaryBySeason(teamId, "goalies");


### Build a list of all of the teams in the league
teams <- getLeagueHistorySummary();


### Get all of the games played by the specified team
games <- getGameSummariesForTeam(teamId);


### Get all player stats in each game
skaterStats <- getPlayerGameDetails(teamId, "skaters");
goalieStats <- getPlayerGameDetails(teamId, "goalies");


### Rename the "timeOnIcePerGame" column for skater stats so that it matches the column name for goalie stats
columnNames <- names(skaterStats);
columnNames[columnNames == "timeOnIcePerGame"] <- "timeOnIce";
names(skaterStats) <- columnNames;



### TOI per game
### (Figure out how long each game lasted.  We don't have that statistic directly, but we can try to approximate
###  it by totaling the TOI for the goalies.  There's only one goalie on the ice at a time, so if we add up the total
###  amount of time played by goalies, it should be the length of the game.  Amount will be a little short because it
###  won't account for time when goalie is pulled.)

timeOnIce <- goalieStats %>%
                group_by(gameId) %>%
                summarize(timeOnIce = sum(timeOnIce));

games <- merge(games, timeOnIce);
games[games$timeOnIce < 3600, ]$timeOnIce <- 3600;




### TOI per season
### (Add up the total length of all games in the season to get the total playing time for an entire season)
seasons <- merge(seasons, games %>%
                   group_by(seasonId) %>%
                   summarize(timeOnIce = sum(timeOnIce)));



### Define a function that computes each player's TOI rank for their position group for each season

computeSeasonToiRank <- function (stats, players) {
  # Sum up each player's TOI for each season
  seasonToi <- stats %>%
                  group_by(seasonId, playerId) %>%
                  summarize(playerName = max(playerName), timeOnIce = sum(timeOnIce)) %>%
                  arrange(seasonId, -timeOnIce);

  # Look up the total time available for each season and figure out what percentage of that time each player was on the ice  
  seasonToi <- merge(seasonToi, seasons[, c("seasonId", "timeOnIce")], by = "seasonId");
  seasonToi$toiPercentage <- seasonToi$timeOnIce.x / seasonToi$timeOnIce.y;
  
  # Look up each player's position code.  If a player is not a Defenseman or Goalie, mark him as a Forward.
  seasonToi <- merge(seasonToi, players %>% group_by(playerId) %>% summarize(playerPositionCode = max(playerPositionCode)));
  if (any(!players$playerPositionCode %in% c("D", "G")) == TRUE) {
    seasonToi[!(seasonToi$playerPositionCode %in% c("D", "G")), ]$playerPositionCode <- "F";
  }
  
  seasonToi <- seasonToi[order(seasonToi$seasonId, -seasonToi$toiPercentage), ];
  seasonToi$seasonRank <- NA;

  seasonToi <- seasonToi %>%
                  arrange(seasonId, playerPositionCode, desc(toiPercentage)) %>%
                  group_by(seasonId, playerPositionCode) %>%
                  mutate(seasonRank = rank(desc(toiPercentage), ties.method = "first"))
}

skaterSeasonToi <- computeSeasonToiRank(skaterStats, skaters);
goalieSeasonToi <- computeSeasonToiRank(goalieStats, goalies);



### Build table with unique players

columns <- c("playerId", "playerName", "playerFirstName", "playerLastName", "playerPositionCode", "playerHeight", "playerWeight", "playerShootsCatches", "playerBirthDate", "playerNationality", "playerBirthCity", "playerBirthStateProvince", "playerBirthCountry", "playerDraftOverallPickNo", "playerDraftRoundNo", "playerDraftYear", "playerTeamsPlayedFor", "seasonId");

players <- merge(goalies, goalies %>%
                   filter(gameTypeId == regularSeasonGameTypeId) %>%
                   group_by(playerId, gameTypeId) %>%
                   summarize(seasonId = max(seasonId))
                 )[ , columns];

players <- rbind(players,
                 merge(skaters, skaters %>%
                         filter(gameTypeId == regularSeasonGameTypeId) %>%
                         group_by(playerId, gameTypeId) %>%
                         summarize(seasonId = max(seasonId))
                 )[ , columns]
            );

names(players) <- c("playerId", "name", "firstName", "lastName", "positionCode", "height", "weight", "shootsCatches", "birthDate", "nationality", "birthCity", "birthStateProvince", "birthCountry", "draftOverallPickNo", "draftRoundNo", "draftYear", "teamsPlayedFor", "latestSeasonId");


# NHL API no longer provides the players' sweater numbers as part of its bio data.  So we have to use another API
# to get that information.

roster <- lapply(seasons$seasonId, function (x) {
              url <- paste0("https://statsapi.web.nhl.com/api/v1/teams/", teamId, "/roster?season=", x);
              roster <- fromJSON(url)$roster;
              roster <- flatten(roster);
              roster$seasonId <- x;
              roster
           })

roster <- do.call(rbind, roster);

roster <- merge(roster, roster %>%
                          group_by(person.id) %>%
                          summarize(seasonId = max(seasonId)));

# Merge the sweater number data into our "players" data frame
players <- merge(players, roster %>% select(playerId = person.id, sweaterNumber = jerseyNumber), all.x = TRUE);

if (any(is.na(players$sweaterNumber)) == TRUE) {
  players[which(is.na(players$sweaterNumber)), ]$sweaterNumber <- "??";
}




### Build final output table


seasonToi <- rbind(goalieSeasonToi, skaterSeasonToi);
keyPlayerIds <- numeric(0);
keyPlayerIds <- c(keyPlayerIds, unique(seasonToi[seasonToi$playerPositionCode == "G" & seasonToi$seasonRank <= 2, ]$playerId));
keyPlayerIds <- c(keyPlayerIds, unique(seasonToi[seasonToi$playerPositionCode == "D" & seasonToi$seasonRank <= 4, ]$playerId));
keyPlayerIds <- c(keyPlayerIds, unique(seasonToi[seasonToi$playerPositionCode == "F" & seasonToi$seasonRank <= 6, ]$playerId));
seasonToi <- seasonToi[seasonToi$playerId %in% keyPlayerIds, ];

output <- dcast(seasonToi[ , c("seasonId", "playerId", "toiPercentage")], playerId ~ seasonId, value.var = "toiPercentage");
output <- merge(output, players[ , c("playerId", "name", "positionCode", "sweaterNumber", "latestSeasonId")]);

output$group <- "";
output$groupOrder <- 0;
output[output$positionCode == "G", ]$group <- "Goalies";
output[output$positionCode == "G", ]$groupOrder <- 1;
output[output$positionCode == "D", ]$group <- "Defensemen";
output[output$positionCode == "D", ]$groupOrder <- 2;
output[output$groupOrder == 0, ]$group <- "Forwards";
output[output$groupOrder == 0, ]$groupOrder <- 3;

output$lastSweaterCode <- 1;
output[output$latestSeasonId >= 20112012, ]$lastSweaterCode <- 2;

x <- paste("-`", sort(seasons$seasonId), "`", sep = "", collapse = ", ");
x <- paste("with(output, order(groupOrder, ", x, "))", sep = "");
x <- eval(parse(text = x));
output <- output[x, ];

seasonRecords <- paste(seasons$wins, "-", seasons$losses, "-", seasons$ties, "-", seasons$otLosses, sep="");
seasonPoints <- paste(seasons$points, "points");
seasonSummaries <- paste(seasonRecords, seasonPoints, sep="<br />");


### Build HTML

html <- "<!DOCTYPE html>";
html <- c(html, "<html>");
html <- c(html, "<head>");
html <- c(html, paste("   <title>", teamFullName, " History</title>", sep = ""));
html <- c(html, "   <style type=\"text/css\">");
html <- c(html, "      table#teamHistoryTable {border-collapse: collapse; font-family: \"Segoe UI\"; border: 2pt solid #e0e0e0;}");
html <- c(html, "      table#teamHistoryTable th, table#teamHistoryTable td {border-left: 1pt solid #e0e0e0;}");
html <- c(html, "      table#teamHistoryTable thead tr th {min-width: 54pt; text-align: center; font-size: 1.1em;}");
html <- c(html, "      table#teamHistoryTable tbody tr.summaries td {text-align: center; font-size: 0.75em;}");
html <- c(html, "      table#teamHistoryTable tbody tr td.group-header {font-weight: bold; transform: rotate(-90deg); border-left: 2pt;}");
html <- c(html, "      table#teamHistoryTable tbody tr td.sweater-number {min-width: 36pt; text-align: center;}");
html <- c(html, "      table#teamHistoryTable tbody tr td.player-name    {min-width: 144pt; text-align: left; white-space: nowrap;}");
html <- c(html, "      table#teamHistoryTable tbody tr td {text-align: right; padding-right: 4pt;}");
html <- c(html, "   </style>");
html <- c(html, "</head>");
html <- c(html, "<body>");
html <- c(html, "<table id=\"teamHistoryTable\">");
html <- c(html, "<thead><tr>");
html <- c(html, "   <th colspan=\"3\"></th>");
html <- c(html, paste("<th>", paste(substr(seasons$startYear, 3, 4), "/", substr(seasons$startYear + 1, 3, 4), sep = ""), "</th>", sep = "", collapse = ""));
html <- c(html, "</tr></thead>");
html <- c(html, "<tbody>");

html <- c(html, "   <tr class=\"summaries\">");
html <- c(html, "      <td colspan=\"3\"></td>");
html <- c(html, paste("<td>", seasonSummaries, "</td>", sep = "", collapse = ""));
html <- c(html, "   </tr>");

groupCounter <- 1;
groupMax <- -1;

for (i in 1:nrow(output)) {
  isNewGroup <- (groupCounter > groupMax);
  topBorderStyle <- "";
  
  if (isNewGroup == TRUE) {
    topBorderStyle <- " border-top: 3pt solid #888888;";
  }
  
  html <- c(html, paste("   <tr style=\"background-color: rgba(", colors[(i %% 2) + 1], ", 0.1);", topBorderStyle, "\">", sep = ""));
  
  if (isNewGroup == TRUE) {
    groupCounter <- 1;
    groupMax <- nrow(output[output$group == output[i, "group"], ]);
    html <- c(html, paste("      <td rowspan=\"", groupMax, "\" class=\"group-header\">", output[i, "group"], "</td>", sep = ""));
  }
  
  html <- c(html, paste("      <td class=\"sweater-number\">", output[i, "sweaterNumber"], "</td>", sep = ""));
  html <- c(html, paste("      <td class=\"player-name\">", output[i, "name"], "</td>", sep = ""));
  
  fontStyle <- "";
  if (i %% 2 == 1) {
    fontStyle <- "color: #f0f0f0; ";
  }
  
  for (j in 1:nrow(seasons)) {
    toi <- output[i, j + 1];
    
    if (is.na(toi) == TRUE) {
      html <- c(html, "      <td></td>");
    }
    else {
      percentage <- round(toi, 2) * 2;
      if (percentage > 1) {
        percentage <- 1;
      }
      else if (percentage < 0.12) {
        percentage <- 0.12;
      }
      
      html <- c(html, paste("      <td style=\"", fontStyle, "background-color: rgba(", colors[(i %% 2) + 1], ", ", percentage, ")\">", sprintf("%1.1f", toi * 100), "%</td>", sep = ""));
    }
  }
  
  html <- c(html, "   </tr>");
  groupCounter <- groupCounter + 1;
}

html <- c(html, "</tbody>");
html <- c(html, "</table>");
html <- c(html, "</body></html>");

writeLines(html, paste0("output\\", gsub(" ", "", teamFullName), "History.html"));
