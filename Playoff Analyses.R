source("nhl-r.R")

teamId <- 18;


#------------------------------------------------------------------------------


# Get all playoff games for a team

x <- getGameSummariesForTeam(teamId, gameTypeId = playoffsGameTypeId)
  
x %>%
  select(gameDate, team = teamAbbrev, opp = opponentTeamAbbrev, gameLocationCode, goalsFor, goalsAgainst, shotsFor, shotsAgainst, ppGoalsFor, ppGoalsAgainst) %>%
  arrange(-ppGoalsAgainst)


#------------------------------------------------------------------------------


# Show the game-by-game playoff performance for an individual player

playerId <- 8471228 # A. Rad
x <- getPlayerHistory(playerId, gameTypeId = playoffsGameTypeId)

x %>%
  arrange(gameId) %>%
  select(playerName, seasonId, gameTypeId, gameId, teamAbbrev, opponentTeamAbbrev, gameDate, toi = timeOnIcePerGame, shifts = shiftsPerGame, shots, assists, goals)


x %>% group_by(seasonId, gameTypeId, teamAbbrev) %>% summarize(goals = sum(goals), games = n()) %>% arrange(seasonId, gameTypeId)

x %>% group_by(teamAbbrev) %>% summarize(goals = sum(goals), games = n(), first = min(seasonId), last = max(seasonId)) %>% arrange(last)


#------------------------------------------------------------------------------


# Franchise leaders in playoffs

skaterStats <- getPlayerGameDetails(teamId, "skaters", gameTypeId = playoffsGameTypeId);

skaterStats$hatTricks <- ifelse(skaterStats$goals >= 3, 1, 0)

skaterStats %>%
  group_by(playerId) %>%
  summarize(playerName = max(playerName), goals = sum(goals), assists = sum(assists), gamesPlayed = sum(gamesPlayed), otGoals = sum(otGoals), toi = sum(timeOnIcePerGame), hatTricks = sum(hatTricks), ppGoals = sum(ppGoals)) %>%
  arrange(-gamesPlayed)


#------------------------------------------------------------------------------


# Players to score in their first-ever playoff game
skaters <- getTeamPlayersSummaryBySeason(teamId, "skaters");
playerIds <- unique(skaters$playerId);
df <- getPlayerHistory(playerIds, gameTypeId = playoffsGameTypeId)

df <- df %>%
        group_by(playerId) %>%
        arrange(playerId, gameId) %>%
        mutate(gameNumber = row_number()) %>%
        select(playerId, playerName, gameId, gameDate, teamAbbrev, opp = opponentTeamAbbrev, goals, assists, toi = timeOnIcePerGame, gameNumber)

df %>%
  filter(gameNumber == 1 & goals > 0) %>%
  arrange(gameDate)


#------------------------------------------------------------------------------


# Play-off performance of trade deadline acquisitions

skaters <- getTeamPlayersSummaryBySeason(teamId, "skaters");
candidates <- skaters %>% filter(endsWith(playerTeamsPlayedFor, ", NSH")) %>% select(playerId, playerName, seasonId, gamesPlayed) %>% arrange(seasonId)

playerIds <- unique(candidates$playerId);
df <- getPlayerHistory(playerIds)

gameGroups <- df %>%
                group_by(playerId, seasonId, teamAbbrev, gameTypeId) %>%
                summarize(gamesPlayed = sum(gamesPlayed), goals = sum(goals), assists = sum(assists), firstGame = min(gameDate), lastGame = max(gameDate))

candidates <- merge(candidates, gameGroups %>% filter(gameTypeId == 2 & teamAbbrev != "NSH"), by = c("playerId", "seasonId"), all.x = TRUE) %>%
  select(playerId, seasonId, playerName, oldTeam = teamAbbrev, oldTeamGames = gamesPlayed.y, oldTeamGoals = goals, lastGame) %>%
  arrange(seasonId, lastGame)

candidates <- merge(candidates, gameGroups %>% filter(gameTypeId == 2 & teamAbbrev == "NSH"), by = c("playerId", "seasonId"), all.x = TRUE) %>%
  select(playerId, seasonId, playerName, oldTeam, oldTeamGames, oldTeamGoals, lastGame = lastGame.x, newTeamGames = gamesPlayed, newTeamGoals = goals, firstGame) %>%
  arrange(seasonId, lastGame)

teamPoGames <- getGameSummariesForTeam(teamId, gameTypeId = playoffsGameTypeId) %>%
                group_by(seasonId) %>%
                summarize(teamPoGames = sum(gamesPlayed))

candidates <- merge(candidates, teamPoGames, all.x = TRUE)

candidates <- merge(candidates, gameGroups %>% filter(gameTypeId == 3 & teamAbbrev == "NSH"), by = c("playerId", "seasonId"), all.x = TRUE) %>%
  select(playerId, seasonId, playerName, oldTeam, oldTeamGames, oldTeamGoals, lastGame = lastGame.x, newTeamGames, newTeamGoals, firstGame = firstGame.x, teamPoGames, poGames = gamesPlayed, poGOals = goals) %>%
  arrange(seasonId, lastGame)

View(candidates)
