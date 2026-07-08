library(httr2)
library(jsonlite)
library(readr)
library(dplyr)

season <- Sys.getenv("NBA_SEASON", unset = "2025-26")
season_type <- Sys.getenv("NBA_SEASON_TYPE", unset = "Regular Season")

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

nba_stats_url <- "https://stats.nba.com/stats/leaguedashplayerstats"

request <- request(nba_stats_url) |>
  req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 Chrome/126 Safari/537.36") |>
  req_headers(
    Accept = "application/json, text/plain, */*",
    Origin = "https://www.nba.com",
    Referer = "https://www.nba.com/stats/players/traditional"
  ) |>
  req_url_query(
    College = "",
    Conference = "",
    Country = "",
    DateFrom = "",
    DateTo = "",
    Division = "",
    DraftPick = "",
    DraftYear = "",
    GameScope = "",
    GameSegment = "",
    Height = "",
    LastNGames = 0,
    LeagueID = "00",
    Location = "",
    MeasureType = "Base",
    Month = 0,
    OpponentTeamID = 0,
    Outcome = "",
    PORound = 0,
    PaceAdjust = "N",
    PerMode = "Totals",
    Period = 0,
    PlayerExperience = "",
    PlayerPosition = "",
    PlusMinus = "N",
    Rank = "N",
    Season = season,
    SeasonSegment = "",
    SeasonType = season_type,
    ShotClockRange = "",
    StarterBench = "",
    TeamID = 0,
    TwoWay = 0,
    VsConference = "",
    VsDivision = "",
    Weight = ""
  ) |>
  req_timeout(30)

response <- tryCatch(
  req_perform(request),
  error = function(e) {
    stop(
      "NBA stats API request failed. The endpoint can be slow or temporarily unavailable.\n",
      "Try again later, or save a compatible season CSV under data/raw/ and run the downstream scripts with NBA_SEASON_FILE.\n",
      "Original error: ", conditionMessage(e),
      call. = FALSE
    )
  }
)
payload <- resp_body_json(response, simplifyVector = FALSE)

result <- payload$resultSets[[1]]
headers <- unlist(result$headers)
rows <- result$rowSet

if (length(rows) == 0) {
  stop("NBA API returned no rows. Check the season value or try again later.")
}

raw_api <- as_tibble(do.call(rbind, lapply(rows, as.data.frame.list)))
names(raw_api) <- headers

standardized <- raw_api |>
  transmute(
    Rk = row_number(),
    Player = PLAYER_NAME,
    Age = AGE,
    Team = TEAM_ABBREVIATION,
    Pos = NA_character_,
    G = GP,
    GS = NA_real_,
    MP = MIN,
    FG = FGM,
    FGA = FGA,
    `FG%` = FG_PCT,
    `3P` = FG3M,
    `3PA` = FG3A,
    `3P%` = FG3_PCT,
    `2P` = FGM - FG3M,
    `2PA` = FGA - FG3A,
    `2P%` = if_else((FGA - FG3A) > 0, (FGM - FG3M) / (FGA - FG3A), NA_real_),
    `eFG%` = NA_real_,
    FT = FTM,
    FTA = FTA,
    `FT%` = FT_PCT,
    ORB = OREB,
    DRB = DREB,
    TRB = REB,
    AST = AST,
    STL = STL,
    BLK = BLK,
    TOV = TOV,
    PF = PF,
    PTS = PTS,
    `Trp-Dbl` = TD3,
    Awards = NA_character_,
    `Player-additional` = PLAYER_ID
  )

safe_season <- gsub("-", "_", season)
raw_path <- file.path("data/raw", paste0("nbastats_", safe_season, "_api_raw.csv"))
standardized_path <- file.path("data/raw", paste0("nbastats_", safe_season, ".csv"))

write_csv(raw_api, raw_path)
write_csv(standardized, standardized_path)

cat("Saved raw NBA API response to:", raw_path, "\n")
cat("Saved standardized season file to:", standardized_path, "\n")
cat("Rows:", nrow(standardized), "\n")
