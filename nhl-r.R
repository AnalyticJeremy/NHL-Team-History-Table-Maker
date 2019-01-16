# nhl-r
# Common Functions
#
# This file contains a set of helper functions for retrieving data from the NHL API

library(jsonlite);
library(dplyr);

urlBase <- "http://www.nhl.com/stats/rest/";
firstSeasonId <- "19171918";
playerTypes <- c("skaters", "goalies");


#------------------------------------------------------------------------------


### Build a dataframe of game types
### (this is static data that doesn't change)

regularSeasonGameTypeId <- 2;
playoffsGameTypeId <- 3;

buildGameTypes <- function() {
  data.frame(
    gameTypeId = c(regularSeasonGameTypeId, playoffsGameTypeId),
    name = c("Regular Season", "Playoffs"),
    stringsAsFactors = FALSE
  );
}


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


getDataFromNhlApi <- function(url, gameTypeId = NA) {
  output <- data.frame();
  jsonData <- fromJSON(url);
  
  if (jsonData$total > 0) {
    jsonData <- jsonData$data;
    
    if ("seasonId" %in% names(jsonData)) {
      jsonData$startYear <- as.integer(substr(jsonData$seasonId, 1, 4));
    }
    
    if (!is.na(gameTypeId)) {
      jsonData$gameTypeId <- gameTypeId;
    }

    output <- rbind(output, jsonData);
  }

  return(output);
}


#------------------------------------------------------------------------------


getTeamSeasonSummaries <- function(teamId, gameTypeId = NA, startSeasonId = NA, endSeasonId = NA) {
  if (is.na(startSeasonId)) {
    startSeasonId = firstSeasonId;
  }
  
  if (is.na(endSeasonId)) {
    endSeasonId = computeSeasonIdForDate();
  }
  
  gameTypeFilter <- "";
  if (!is.na(gameTypeId)) {
    gameTypeFilter <- paste0("+and+gameTypeId=", gameTypeId);
  }
  
  requestUrl <- sprintf("%steam?isAggregate=false&reportType=basic&isGame=false&reportName=teamsummary&cayenneExp=seasonId%%3E=%s+and+seasonId%%3C=%s+and+teamId=%s%s", urlBase, startSeasonId, endSeasonId, teamId, gameTypeFilter);
  
  getDataFromNhlApi(requestUrl, gameTypeId);
}


#------------------------------------------------------------------------------


getLeagueHistorySummary <- function() {
  requestUrl <- sprintf("%steam?isAggregate=false&reportType=basic&isGame=false&reportName=teamsummary", urlBase);
  
  data <- getDataFromNhlApi(requestUrl);
  
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
  
  gameTypeFilter <- "";
  if (!is.na(gameTypeId)) {
    gameTypeFilter <- paste0("+and+gameTypeId=", gameTypeId);
  }
  
  requestUrl <- sprintf("%steam?isAggregate=false&reportType=basic&isGame=false&reportName=teamsummary&cayenneExp=seasonId=%s%s", urlBase, seasonId, gameTypeFilter);
  
  getDataFromNhlApi(requestUrl, gameTypeId);
}


#------------------------------------------------------------------------------


getTeamPlayersSummaryBySeason <- function(teamId, playerType = "skaters", startSeasonId = NA, endSeasonId = NA) {
  if (is.na(startSeasonId)) {
    startSeasonId = firstSeasonId;
  }
  
  if (is.na(endSeasonId)) {
    endSeasonId = computeSeasonIdForDate();
  }
  
  output <- lapply(gameTypes$gameTypeId, function(gameTypeId) {
    requestUrl <- sprintf("%s%s?isAggregate=false&reportType=basic&isGame=false&reportName=skatersummary&cayenneExp=seasonId%%3E=%s+and+seasonId%%3C=%s+and+teamId=%s+and+gameTypeId=%s", urlBase, playerType, startSeasonId, endSeasonId, teamId, gameTypeId);
    getDataFromNhlApi(requestUrl, gameTypeId);
  });
  
  output <- bind_rows(output);
  return(output);
}


#------------------------------------------------------------------------------


getGameSummariesForTeam <- function(teamId, startSeasonId = NA, endSeasonId = NA) {
  if (is.na(startSeasonId)) {
    startSeasonId = firstSeasonId;
  }
  
  if (is.na(endSeasonId)) {
    endSeasonId = computeSeasonIdForDate();
  }
  
  output <- lapply(gameTypes$gameTypeId, function(gameTypeId) {
    requestUrl <- sprintf("%steam?isAggregate=false&reportType=basic&isGame=true&reportName=teamsummary&cayenneExp=seasonId%%3E=%s+and+seasonId%%3C=%s+and+teamId=%s+and+gameTypeId=%s", urlBase, startSeasonId, endSeasonId, teamId, gameTypeId);
    getDataFromNhlApi(requestUrl, gameTypeId);
  });
  
  output <- bind_rows(output);
  
  output$seasonId <- buildSeasonIdFromGameId(output$gameId)
  
  return(output);
}


#------------------------------------------------------------------------------


getPlayerGameDetails <- function(teamId, playerType = "skaters", startSeasonId = NA, endSeasonId = NA) {
  if (is.na(startSeasonId)) {
    startSeasonId = firstSeasonId;
  }
  
  if (is.na(endSeasonId)) {
    endSeasonId = computeSeasonIdForDate();
  }
  
  reportName <- paste0(substr(playerType, 1, nchar(playerType) - 1), "summary");
  
  output <- lapply(gameTypes$gameTypeId, function(gameTypeId) {
    requestUrl <- sprintf("%s%s?isAggregate=false&reportType=basic&isGame=true&reportName=%s&cayenneExp=seasonId%%3E=%s+and+seasonId%%3C=%s+and+gameTypeId=%s+and+teamId=%s", urlBase, playerType, reportName, startSeasonId, endSeasonId, gameTypeId, teamId);
    getDataFromNhlApi(requestUrl, gameTypeId);
  });
  
  output <- bind_rows(output);
  
  output$seasonId <- buildSeasonIdFromGameId(output$gameId)
  
  return(output);
}


#------------------------------------------------------------------------------


