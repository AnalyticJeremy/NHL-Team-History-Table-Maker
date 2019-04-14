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

skaterStats$wins <- NA

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

firstWin <- playerStats %>%
              filter(wins > 0) %>%
              group_by(playerId) %>%
              summarize(gameId = min(gameId))

lastGame <- playerStats %>%
              group_by(playerId) %>%
              summarize(gameId = max(gameId))

lastGoal <- playerStats %>%
              filter(goals > 0) %>%
              group_by(playerId) %>%
              summarize(gameId = max(gameId))

lastWin <- playerStats %>%
              filter(wins > 0) %>%
              group_by(playerId) %>%
              summarize(gameId = max(gameId))


## ALL PLAYERS


# Youngest players to play a game
firstGame <- merge(firstGame, playerStats) %>%
              select(gameId, gameDate, playerId, playerName, playerBirthDate, playerPositionCode, opponentTeamAbbrev, goals) %>%
              mutate(ageDays = gameDate - playerBirthDate) %>%
              mutate(ageYears = computeYearsAndDaysNumber(playerBirthDate, gameDate))
head( firstGame %>% arrange(ageDays) , 10 )


# Youngest players to score a goal
firstGoal <- merge(firstGoal, playerStats) %>%
              select(gameId, gameDate, playerId, playerName, playerBirthDate, playerPositionCode, opponentTeamAbbrev, goals) %>%
              mutate(ageDays = gameDate - playerBirthDate) %>%
              mutate(ageYears = computeYearsAndDaysNumber(playerBirthDate, gameDate))
head( firstGoal %>% arrange(ageDays) , 10 )


# Oldest players to play a game
lastGame <- merge(lastGame, playerStats) %>%
              select(gameId, gameDate, playerId, playerName, playerBirthDate, playerPositionCode, opponentTeamAbbrev, goals) %>%
              mutate(ageDays = gameDate - playerBirthDate) %>%
              mutate(ageYears = computeYearsAndDaysNumber(playerBirthDate, gameDate))
head( lastGame %>% arrange(desc(ageDays)) , 10 )


# Oldest players to score a goal
lastGoal <- merge(lastGoal, playerStats) %>%
              select(gameId, gameDate, playerId, playerName, playerBirthDate, playerPositionCode, opponentTeamAbbrev, goals) %>%
              mutate(ageDays = gameDate - playerBirthDate) %>%
              mutate(ageYears = computeYearsAndDaysNumber(playerBirthDate, gameDate))
head( lastGoal %>% arrange(desc(ageDays)) , 10 )



### GOALIES ONLY

# Youngest goalies to play a game
head( firstGame %>% filter(playerPositionCode == "G") %>% arrange(ageDays) , 10 )


# Youngest goalies to win a game
firstWin <- merge(firstWin, playerStats) %>%
              select(gameId, gameDate, playerId, playerName, playerBirthDate, playerPositionCode, opponentTeamAbbrev, wins) %>%
              mutate(ageDays = gameDate - playerBirthDate) %>%
              mutate(ageYears = computeYearsAndDaysNumber(playerBirthDate, gameDate))
head( firstWin %>% arrange(ageDays) , 10 )


# Oldest goalies to play a game
head( lastGame %>% filter(playerPositionCode == "G") %>% arrange(desc(ageDays)) , 10 )


# Oldest goalies to win a game
lastWin <- merge(lastWin, playerStats) %>%
            select(gameId, gameDate, playerId, playerName, playerBirthDate, playerPositionCode, opponentTeamAbbrev, wins) %>%
            mutate(ageDays = gameDate - playerBirthDate) %>%
            mutate(ageYears = computeYearsAndDaysNumber(playerBirthDate, gameDate))
head( lastWin %>% arrange(desc(ageDays)) , 10 )

