# Longest Gap in Scoring for a Team
# 
# Sometimes players will go a really long time between scoring goals for a team



source("nhl-r.R")


teamId <- 18;	# 18 = Nashville Predators on nhl.com/stats  (call "getLeagueHistorySummary()" to look up the ID for another team)

### Get all player stats in each game
skaterStats <- getPlayerGameDetails(teamId, "skaters");
goalieStats <- getPlayerGameDetails(teamId, "goalies");

columnNames <- names(skaterStats)
columnNames <- columnNames[columnNames %in% names(goalieStats)]

# Combine skaters and goalies into a common dataset
playerStats <- rbind(
  skaterStats %>% select_(.dots = columnNames),
  goalieStats %>% select_(.dots = columnNames)
)

playerStats$playerBirthDate <- as.Date(playerStats$playerBirthDate);
playerStats$gameDate <- as.Date(playerStats$gameDate, tz = "America/Chicago");

# Add a column that tracks the number of games the player has played for the specified team
playerStats <- playerStats %>%
                  group_by(playerId) %>%
                  mutate(teamGameNo = dense_rank(gameDate))


# Add a flag to identify players who are currently on the team
currentSeasonId <- computeSeasonIdForDate();
currentPlayerIds <- unique(playerStats[playerStats$seasonId == currentSeasonId, ]$playerId);
playerStats$isCurrentPlayer <- playerStats$playerId %in% currentPlayerIds;


# For each player, add a column that shows when each player scored his previous goal for the specified team
goals <- playerStats %>%
            select(playerId, playerName, playerBirthDate, gameDate, gameId, teamAbbrev, opponentTeamAbbrev, goals, teamGameNo, isCurrentPlayer) %>%
            filter(goals > 0) %>%
            group_by(playerId) %>%
            mutate(lastGoal = lag(gameDate, order_by = gameDate)) %>%
            arrange(playerId, gameDate)

# Quick sanity check
sum(goals$goals)
sum(getTeamSeasonSummaries(teamId)$goalsFor)


# Show the longest scoring droughts for the specified team
goals %>%
  mutate(yearsSince = computeYearsAndDaysNumber(lastGoal, gameDate)) %>%
  arrange(desc(yearsSince)) %>%
  select(playerId, playerName, gameDate, teamAbbrev, opponentTeamAbbrev, goals, teamGameNo, lastGoal, yearsSince)


# Check all of the current players.  If they scored today, where would that put them on the list?
goals %>%
  filter(isCurrentPlayer) %>%
  group_by(playerId) %>% 
  summarize(playerName = max(playerName), lastGoal = max(gameDate)) %>%
  mutate(daysSince  = Sys.Date() - lastGoal) %>%
  mutate(yearsSince = computeYearsAndDaysNumber(lastGoal, Sys.Date())) %>%
  arrange(desc(daysSince))