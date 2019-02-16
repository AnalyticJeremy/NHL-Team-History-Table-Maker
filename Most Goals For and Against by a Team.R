# Most Goals For and Against in a Game
# 
# For a given team, what's the most goals they've every scored in a game?
# For a given team, what's the most goals they've given up in a game?



source("nhl-r.R")


teamId <- 18;	# 18 = Nashville Predators on nhl.com/stats  (call "getLeagueHistorySummary()" to look up the ID for another team)

### Get all of the games played by the specified team
games <- getGameSummariesForTeam(teamId);

# Show games with most goals for
games %>%
  select(gameId, gameDate, teamAbbrev, opponentTeamAbbrev, gameLocationCode, goalsFor, goalsAgainst, shotsFor, shotsAgainst) %>%
  mutate(rank = dense_rank(desc(goalsFor))) %>%
  arrange(desc(goalsFor), gameDate) %>%
  filter(rank <= 4)

games %>%
  group_by(goalsFor) %>%
  summarize(numberOfGames = n()) %>%
  arrange(desc(goalsFor))

games %>%
  group_by(goalsFor, gameLocationCode) %>%
  summarize(numberOfGames = n()) %>%
  arrange(desc(goalsFor, gameLocationCode))



# Show games with most goals against
games %>%
  select(gameId, gameDate, teamAbbrev, opponentTeamAbbrev, gameLocationCode, goalsFor, goalsAgainst, shotsFor, shotsAgainst) %>%
  mutate(rank = dense_rank(desc(goalsAgainst))) %>%
  arrange(desc(goalsAgainst), gameDate) %>%
  filter(rank <= 4)

games %>%
  group_by(goalsAgainst) %>%
  summarize(numberOfGames = n()) %>%
  arrange(desc(goalsAgainst))