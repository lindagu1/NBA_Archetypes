library(rvest)
library(readr)
library(dplyr)

season <- Sys.getenv("NBA_SEASON", unset = "2025-26")
safe_season <- gsub("-", "_", season)

season_end_year <- as.integer(substr(season, 6, 7)) + 2000
url <- paste0("https://www.basketball-reference.com/leagues/NBA_", season_end_year, "_per_game.html")

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

page <- read_html(url)
tables <- html_table(page, fill = TRUE)

if (length(tables) == 0) {
  stop("No tables found on Basketball Reference page: ", url)
}

stats <- tables[[1]] |>
  filter(Player != "Player") |>
  mutate(across(where(is.character), ~na_if(.x, ""))) |>
  mutate(across(
    c(Rk, Age, G, GS, MP, FG, FGA, `FG%`, `3P`, `3PA`, `3P%`, `2P`, `2PA`, `2P%`, `eFG%`, FT, FTA, `FT%`, ORB, DRB, TRB, AST, STL, BLK, TOV, PF, PTS),
    as.numeric
  ))

counting_stats <- c("MP", "FG", "FGA", "3P", "3PA", "2P", "2PA", "FT", "FTA", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS")

stats <- stats |>
  mutate(across(all_of(counting_stats), ~.x * G))

output_path <- file.path("data/raw", paste0("nbastats_", safe_season, ".csv"))
write_csv(stats, output_path)

cat("Saved Basketball Reference season file to:", output_path, "\n")
cat("Source:", url, "\n")
cat("Rows:", nrow(stats), "\n")
