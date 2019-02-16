# Youngest and Oldest to Play and Score for a Team
# 
# Who is the youngest player to play for a team?
# Who is the oldest player to play for a team?
# Who is the youngest player to score a goal for a team?
# Who is the oldest player to score a goal for a team?



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


firstGame <- playerStats %>%
              group_by(playerId) %>%
              summarize(gameId = min(gameId))

firstGoal <- playerStats %>%
              filter(goals > 0) %>%
              group_by(playerId) %>%
              summarize(gameId = min(gameId))

lastGame <- playerStats %>%
              group_by(playerId) %>%
              summarize(gameId = max(gameId))

lastGoal <- playerStats %>%
              filter(goals > 0) %>%
              group_by(playerId) %>%
              summarize(gameId = max(gameId))

firstGame <- merge(firstGame, playerStats) %>%
              select(gameId, gameDate, playerId, playerName, playerBirthDate, playerPositionCode, opponentTeamAbbrev, goals) %>%
              mutate(ageDays = gameDate - playerBirthDate) %>%
              mutate(ageYears = computeYearsAndDaysNumber(playerBirthDate, gameDate))

head( firstGame %>% arrange(ageDays) , 10 )

firstGoal <- merge(firstGoal, playerStats) %>%
              select(gameId, gameDate, playerId, playerName, playerBirthDate, playerPositionCode, opponentTeamAbbrev, goals) %>%
              mutate(ageDays = gameDate - playerBirthDate) %>%
              mutate(ageYears = computeYearsAndDaysNumber(playerBirthDate, gameDate))

head( firstGoal %>% arrange(ageDays) , 10 )


lastGame <- merge(lastGame, playerStats) %>%
              select(gameId, gameDate, playerId, playerName, playerBirthDate, playerPositionCode, opponentTeamAbbrev, goals) %>%
              mutate(ageDays = gameDate - playerBirthDate) %>%
              mutate(ageYears = computeYearsAndDaysNumber(playerBirthDate, gameDate))

head( lastGame %>% arrange(desc(ageDays)) , 10 )

lastGoal <- merge(lastGoal, playerStats) %>%
              select(gameId, gameDate, playerId, playerName, playerBirthDate, playerPositionCode, opponentTeamAbbrev, goals) %>%
              mutate(ageDays = gameDate - playerBirthDate) %>%
              mutate(ageYears = computeYearsAndDaysNumber(playerBirthDate, gameDate))

head( lastGoal %>% arrange(desc(ageDays)) , 10 )



