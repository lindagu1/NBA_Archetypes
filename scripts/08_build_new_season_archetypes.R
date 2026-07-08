library(tidyverse)

season <- Sys.getenv("NBA_SEASON", unset = "2025-26")
safe_season <- gsub("-", "_", season)

input_path <- Sys.getenv(
  "NBA_SEASON_FILE",
  unset = file.path("data/raw", paste0("nbastats_", safe_season, ".csv"))
)

if (!file.exists(input_path)) {
  stop(
    "Could not find new-season stats file: ", input_path, "\n",
    "Run scripts/07_fetch_nba_stats_api.R first, or set NBA_SEASON_FILE to a saved CSV."
  )
}

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

baseline_raw <- readr::read_csv("data/raw/nbastats.csv", show_col_types = FALSE)
new_raw <- readr::read_csv(input_path, show_col_types = FALSE)
baseline_clusters <- readr::read_csv("output/tables/player_clusters_named.csv", show_col_types = FALSE)
baseline_km <- readRDS("output/tables/kmeans_model.rds")

clean_feature_matrix <- function(df) {
  player_col <- intersect(names(df), c("Player"))[1]
  games_col <- intersect(names(df), c("G", "GP"))[1]
  mp_col <- intersect(names(df), c("MP", "MIN"))[1]
  team_col <- intersect(names(df), c("Team", "TEAM_ABBREVIATION"))[1]
  pos_col <- intersect(names(df), c("Pos", "POSITION"))[1]
  age_col <- intersect(names(df), c("Age", "AGE"))[1]

  stopifnot(!is.na(player_col), !is.na(games_col), !is.na(mp_col))

  df2 <- df |>
    mutate(.player = .data[[player_col]]) |>
    group_by(.player) |>
    arrange(desc(.data[[mp_col]]), .by_group = TRUE) |>
    slice(1) |>
    ungroup() |>
    select(-.player)

  df2 <- df2 |>
    mutate(
      .games = as.numeric(.data[[games_col]]),
      .minutes = as.numeric(.data[[mp_col]]),
      .minutes_per_game = if (max(.minutes, na.rm = TRUE) <= 60) .minutes else .minutes / .games
    ) |>
    filter(.games >= 41, .minutes_per_game >= 24)

  meta_cols <- c(player_col, team_col, pos_col, age_col, games_col, mp_col)
  meta_cols <- meta_cols[!is.na(meta_cols) & meta_cols %in% names(df2)]

  player_index <- df2 |>
    select(all_of(meta_cols)) |>
    rename(
      Player = all_of(player_col),
      Team = any_of(team_col),
      Pos = any_of(pos_col),
      Age = any_of(age_col),
      G = any_of(games_col),
      MP = any_of(mp_col)
    )

  drop_numeric <- intersect(
    names(df2),
    c("Rk", "Age", "AGE", "G", "GP", "GS", "MP", "MIN", "FG%", "3P%", "2P%", "eFG%", "FT%", "PTS", ".games", ".minutes", ".minutes_per_game")
  )

  X <- df2 |>
    select(where(is.numeric)) |>
    select(-any_of(drop_numeric))

  kept_rows <- complete.cases(X)
  X <- X[kept_rows, , drop = FALSE]
  player_index <- player_index[kept_rows, , drop = FALSE]

  list(player_index = player_index, X = X)
}

baseline <- clean_feature_matrix(baseline_raw)
new_season <- clean_feature_matrix(new_raw)

feature_names <- colnames(readRDS("output/tables/nba_features_scaled.rds"))
missing_features <- setdiff(feature_names, colnames(new_season$X))

if ("Trp-Dbl" %in% missing_features) {
  new_season$X[["Trp-Dbl"]] <- 0
  missing_features <- setdiff(feature_names, colnames(new_season$X))
}

if (length(missing_features) > 0) {
  stop("New-season file is missing required features: ", paste(missing_features, collapse = ", "))
}

baseline_X <- baseline$X[, feature_names, drop = FALSE]
new_X <- new_season$X[, feature_names, drop = FALSE]

baseline_means <- sapply(baseline_X, mean, na.rm = TRUE)
baseline_sds <- sapply(baseline_X, sd, na.rm = TRUE)

new_scaled <- sweep(new_X, 2, baseline_means, "-")
new_scaled <- sweep(new_scaled, 2, baseline_sds, "/")
new_scaled <- as.data.frame(new_scaled)
rownames(new_scaled) <- new_season$player_index$Player

assign_to_centroids <- function(X, centers) {
  assignments <- apply(X, 1, function(row) {
    distances <- rowSums((centers - matrix(row, nrow = nrow(centers), ncol = ncol(centers), byrow = TRUE))^2)
    which.min(distances)
  })
  as.integer(assignments)
}

new_clusters <- tibble(
  Player = rownames(new_scaled),
  Cluster = assign_to_centroids(as.matrix(new_scaled), baseline_km$centers)
) |>
  left_join(distinct(baseline_clusters, Cluster, Archetype), by = "Cluster") |>
  left_join(new_season$player_index, by = "Player") |>
  relocate(Player, Team, Pos, Age, G, MP, Cluster, Archetype)

new_profile <- new_scaled |>
  as_tibble(rownames = "Player") |>
  left_join(select(new_clusters, Player, Cluster), by = "Player") |>
  group_by(Cluster) |>
  summarise(across(where(is.numeric), mean), n_players = n(), .groups = "drop") |>
  arrange(Cluster)

write_csv(new_clusters, file.path("output/tables", paste0("player_clusters_named_", safe_season, ".csv")))
write_csv(new_profile, file.path("output/tables", paste0("cluster_profiles_", safe_season, ".csv")))
saveRDS(new_scaled, file.path("output/tables", paste0("nba_features_scaled_", safe_season, ".rds")))

cat("Assigned", nrow(new_clusters), "players to original archetype centroids for", season, "\n")
cat("Saved:", file.path("output/tables", paste0("player_clusters_named_", safe_season, ".csv")), "\n")
