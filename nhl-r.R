# nhl-r
# Common Functions
#
# This file contains a set of helper functions for retrieving data from the NHL API

library(jsonlite);
library(dplyr);

apiUrlBase <- "http://www.nhl.com/stats/rest/";
firstSeasonId <- "19171918";
playerTypes <- c("skaters", "goalies");


### Build a dataframe of game types
### (this is static data that doesn't change)

regularSeasonGameTypeId <- 2;
playoffsGameTypeId <- 3;

gameTypes <- data.frame(
  gameTypeId = c(regularSeasonGameTypeId, playoffsGameTypeId),
  name = c("Regular Season", "Playoffs"),
  stringsAsFactors = FALSE
);


#------------------------------------------------------------------------------


buildSeasonIdForYear <- function(startYear) {
  paste0(startYear, startYear + 1)
}


#------------------------------------------------------------------------------


buildSeasonIdFromGameId <- function(gameId) {
  startYear <- as.integer(substr(gameId, 1, 4))
  paste0(startYear, startYear + 1)
}


#------------------------------------------------------------------------------


computeSeasonIdForDate <- function(date = NA) {
  if (is.na(date)) {
    date = Sys.Date();
  } else {
    date = as.Date(date);
  }
  
  month <- suppressWarnings(as.integer(format(date, "%m")));
  year  <- suppressWarnings(as.integer(format(date, "%Y")));
  
  # For any dates before August, assume we're looking at the season that started in the previous year.
  # (e.g.  May, 2018 would be looking at the the 2017-2018 season)
  if (month < 8) {
    year <- year - 1;
  }
  
  buildSeasonIdForYear(year);
}


#------------------------------------------------------------------------------


computeStartYearForSeason <- function(seasonId) {
  as.integer(substr(seasonId, 1, 4))
}


#------------------------------------------------------------------------------


# This function encodes a "x years and y days" description of a timespan
# into a single integer.
computeYearsAndDaysNumber <- function(d1, d2) {
  d1 <- as.Date(d1);
  d2 <- as.Date(d2);
  
  # We need to make sure that for each pair of dates, the day in d1 is EARLIER than its mate in d2
  diffs <- as.integer(d2 - d1);
  diffs <- ifelse(diffs > 0, 0, diffs)
  d1 <- d1 + diffs
  d2 <- d2 - diffs
  
  year1 <- as.integer(format(d1, "%Y"));
  year2 <- as.integer(format(d2, "%Y"));
  years <- year2 - year1;
  
  dateInYear <- function(d, y) {
    output <- paste0(y, format(d, "-%m-%d"));
    output <- as.Date(ifelse(is.na(d), NA, output));
  }
  
  d1y <- dateInYear(d1, year2);
  diffs <- as.integer(d2 - d1y);
  years <- ifelse(diffs < 0, years - 1, years)
  d1y <- ifelse(diffs < 0, dateInYear(d1, year2 - 1), d1y);
  
  days <- as.integer(d2 - d1y);
  
  #sprintf("%s years and %s days", years, days)
  (years * 1000000) + days;
}


#------------------------------------------------------------------------------


playerTypeToSummaryReportName <- function(playerType) {
  paste0(substr(playerType, 1, nchar(playerType) - 1), "summary")
}


#------------------------------------------------------------------------------


paramsListToString <- function(params, collapse = "&") {
  childLists <- params[sapply(params, is.list)]
  params <- params[!sapply(params, is.list)]
  
  if (length(childLists) > 0) {
    params <- append(params, lapply(childLists, paramsListToString, collapse = "+and+") )
  }
  
  params <- expand.grid(params, stringsAsFactors = FALSE)
  
  queries <- c()
  keys <- names(params)
  for (i in 1:nrow(params)) {
    values <- as.character(params[i, ])
    query <- paste(keys, values, sep="=", collapse=collapse)
    queries <- queries <- c(queries, query)
  }
  
  queries
}


#------------------------------------------------------------------------------


buildUrls <- function(apiMethod, params) {
  queryStrings <- paramsListToString(params)
  urls <- sprintf("%s%s?%s", apiUrlBase, apiMethod, queryStrings)
  urls <- unique(urls)
  return(urls)
}


#------------------------------------------------------------------------------


getGameTypeIdFromUrl <- function(url) {
  pattern <- ".*gameTypeId=([0-9]+).*";
  
  output <- ifelse(grepl(pattern, url), gsub(pattern, "\\1", url), NA);
  as.integer(output);
}


#------------------------------------------------------------------------------


getDataFromNhlApi <- function(apiMethod, params) {
  urls <- buildUrls(apiMethod, params);
  
  # Retrieve data for all of the URLs
  jsonList <- lapply(urls, function(url) {
                jsonData <- fromJSON(url)
                
                if (jsonData$total <= 0) {
                  return(NULL)
                } else {
                  jsonData <- jsonData$data;
                  
                  # If there is no "seasonId" column, add it
                  if ("gameId" %in% names(jsonData) & !"seasonId" %in% names(jsonData)) {
                    jsonData$seasonId <- buildSeasonIdFromGameId(jsonData$gameId); 
                  }
                  
                  if ("seasonId" %in% names(jsonData)) {
                    jsonData$startYear <- as.integer(substr(jsonData$seasonId, 1, 4));
                  }
                  
                  jsonData$gameTypeId <- getGameTypeIdFromUrl(url);
                  
                  if ("gameDate" %in% names(jsonData)) {
                    # Convert ISO-8601 date string to an R date object and convert the timezone
                    jsonData$gameDate <- as.POSIXct( strptime(jsonData$gameDate, "%Y-%m-%dT%H:%M:%SZ", tz="UTC") )
                    attr(jsonData$gameDate, "tzone") <- "America/Chicago"
                  }
                  
                  return(jsonData);
                }
              });
  
  # Remove any NULL entries from the list
  jsonList <- jsonList[!sapply(jsonList, is.null)]
  
  return(bind_rows(jsonList));
}


#------------------------------------------------------------------------------


getTeamSeasonSummaries <- function(teamId, gameTypeId = NA, startSeasonId = NA, endSeasonId = NA) {
  if (is.na(startSeasonId)) {
    startSeasonId = firstSeasonId;
  }
  
  if (is.na(endSeasonId)) {
    endSeasonId = computeSeasonIdForDate();
  }
  
  if (is.na(gameTypeId)) {
    gameTypeId = gameTypes$gameTypeId
  }
  
  apiMethod <- "team"
  
  params <- list(
    isAggregate = "false",
    reportType = "basic",
    isGame = "false",
    reportName = "teamsummary",
    cayenneExp = list(
      "seasonId%3E" = startSeasonId,
      "seasonId%3C" = endSeasonId,
      "teamId" = teamId,
      "gameTypeId" = gameTypeId
    )
  )
  
  getDataFromNhlApi(apiMethod, params);
}


#------------------------------------------------------------------------------


getLeagueHistorySummary <- function() {
  apiMethod <- "team"
  
  params <- list(
    isAggregate = "false",
    reportType = "basic",
    isGame = "false",
    reportName = "teamsummary"
  )
  
  data <- getDataFromNhlApi(apiMethod, params);
  
  latestSeason <- data %>% group_by(teamId) %>% summarize(seasonId = max(seasonId));
  latestSeason <- merge(latestSeason, data, by = c("teamId", "seasonId")) %>% select(teamId, teamAbbrev, teamFullName);
  
  summary <- data %>%
    group_by(teamId) %>%
    summarize(gamesPlayed = sum(gamesPlayed),
              goalsAgainst = sum(goalsAgainst),
              goalsFor = sum(goalsFor),
              wins = sum(wins),
              losses = sum(losses),
              ties = sum(ties, na.rm = TRUE),
              otLosses = sum(otLosses, na.rm = TRUE),
              points = sum(points),
              shootoutGamesWon = sum(shootoutGamesWon),
              seasons = n_distinct(seasonId),
              firstSeason = min(seasonId),
              latestSeason = max(seasonId),
              names = n_distinct(teamFullName));
  
  merge(latestSeason, summary, by = "teamId");
}


#------------------------------------------------------------------------------


getLeagueSeasonSummary <- function(seasonId = NA, gameTypeId = NA) {
  if (is.na(seasonId)) {
    seasonId = computeSeasonIdForDate();
  }
  
  if (is.na(gameTypeId)) {
    gameTypeId = gameTypes$gameTypeId
  }
  
  apiMethod <- "team"
  
  params <- list(
    isAggregate = "false",
    reportType = "basic",
    isGame = "false",
    reportName = "teamsummary",
    cayenneExp = list(
      "seasonId" = seasonId,
      "gameTypeId" = gameTypeId
    )
  )
  
  getDataFromNhlApi(apiMethod, params);
}


#------------------------------------------------------------------------------


getTeamPlayersSummaryBySeason <- function(teamId, playerType = "skaters", startSeasonId = NA, endSeasonId = NA, gameTypeId = NA) {
  if (is.na(startSeasonId)) {
    startSeasonId = firstSeasonId;
  }
  
  if (is.na(endSeasonId)) {
    endSeasonId = computeSeasonIdForDate();
  }
  
  if (is.na(gameTypeId)) {
    gameTypeId = gameTypes$gameTypeId
  }
  
  apiMethod <- playerType
  
  params <- list(
    isAggregate = "false",
    reportType = "basic",
    isGame = "false",
    reportName = playerTypeToSummaryReportName(playerType),
    cayenneExp = list(
      "seasonId%3E" = startSeasonId,
      "seasonId%3C" = endSeasonId,
      "teamId" = teamId,
      "gameTypeId" = gameTypeId
    )
  )
  
  getDataFromNhlApi(apiMethod, params);
}


#------------------------------------------------------------------------------


getGameSummariesForTeam <- function(teamId, startSeasonId = NA, endSeasonId = NA, gameTypeId = NA) {
  if (is.na(startSeasonId)) {
    startSeasonId = firstSeasonId;
  }
  
  if (is.na(endSeasonId)) {
    endSeasonId = computeSeasonIdForDate();
  }
  
  if (is.na(gameTypeId)) {
    gameTypeId = gameTypes$gameTypeId
  }
  
  apiMethod <- "team"
  
  params <- list(
    isAggregate = "false",
    reportType = "basic",
    isGame = "true",
    reportName = "teamsummary",
    cayenneExp = list(
      "seasonId%3E" = startSeasonId,
      "seasonId%3C" = endSeasonId,
      "teamId" = teamId,
      "gameTypeId" = gameTypeId
    )
  )
  
  getDataFromNhlApi(apiMethod, params);
}


#------------------------------------------------------------------------------


getPlayerGameDetails <- function(teamId, playerType = "skaters", startSeasonId = NA, endSeasonId = NA, gameTypeId = NA) {
  if (is.na(startSeasonId)) {
    startSeasonId = firstSeasonId;
  }
  
  if (is.na(endSeasonId)) {
    endSeasonId = computeSeasonIdForDate();
  }
  
  if (is.na(gameTypeId)) {
    gameTypeId = gameTypes$gameTypeId
  }
  
  apiMethod <- playerType
  
  params <- list(
    isAggregate = "false",
    reportType = "basic",
    isGame = "true",
    reportName = playerTypeToSummaryReportName(playerType),
    cayenneExp = list(
      "seasonId%3E" = startSeasonId,
      "seasonId%3C" = endSeasonId,
      "teamId" = teamId,
      "gameTypeId" = gameTypeId
    )
  )
  
  getDataFromNhlApi(apiMethod, params);
}


#------------------------------------------------------------------------------


getPlayerHistory <- function(playerId, playerType = "skaters", gameTypeId = NA) {
  if (is.na(gameTypeId)) {
    gameTypeId = gameTypes$gameTypeId
  }
  
  apiMethod <- playerType
  
  params <- list(
    isAggregate = "false",
    reportType = "basic",
    isGame = "true",
    reportName = playerTypeToSummaryReportName(playerType),
    cayenneExp = list(
      "playerId" = playerId,
      "gameTypeId" = gameTypeId
    )
  )
  
  getDataFromNhlApi(apiMethod, params);
}


#------------------------------------------------------------------------------
