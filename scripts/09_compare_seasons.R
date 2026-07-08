library(tidyverse)

season <- Sys.getenv("NBA_SEASON", unset = "2025-26")
safe_season <- gsub("-", "_", season)

new_clusters_path <- file.path("output/tables", paste0("player_clusters_named_", safe_season, ".csv"))
new_scaled_path <- file.path("output/tables", paste0("nba_features_scaled_", safe_season, ".rds"))
new_raw_path <- Sys.getenv(
  "NBA_SEASON_FILE",
  unset = file.path("data/raw", paste0("nbastats_", safe_season, ".csv"))
)

if (!file.exists(new_clusters_path) || !file.exists(new_scaled_path)) {
  stop("Run scripts/08_build_new_season_archetypes.R before comparing seasons.")
}

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

baseline_clusters <- readr::read_csv("output/tables/player_clusters_named.csv", show_col_types = FALSE) |>
  rename(
    Previous_Cluster = Cluster,
    Previous_Season_Archetype = Archetype
  )

new_clusters <- readr::read_csv(new_clusters_path, show_col_types = FALSE) |>
  rename(
    New_Cluster = Cluster,
    New_Season_Archetype = Archetype
  )

baseline_scaled <- readRDS("output/tables/nba_features_scaled.rds") |>
  as.data.frame() |>
  rownames_to_column("Player")

new_scaled <- readRDS(new_scaled_path) |>
  as.data.frame() |>
  rownames_to_column("Player")

feature_names <- setdiff(colnames(baseline_scaled), "Player")

describe_top_changes <- function(player) {
  before <- baseline_scaled |> filter(Player == player)
  after <- new_scaled |> filter(Player == player)

  if (nrow(before) == 0 || nrow(after) == 0) {
    return(NA_character_)
  }

  deltas <- map_dbl(feature_names, \(feature) after[[feature]][1] - before[[feature]][1])
  names(deltas) <- feature_names

  top <- sort(abs(deltas), decreasing = TRUE)[1:min(5, length(deltas))]
  paste(
    map_chr(names(top), \(feature) {
      delta <- deltas[[feature]]
      direction <- ifelse(delta >= 0, "up", "down")
      paste0(feature, " ", direction, " ", sprintf("%.2f", abs(delta)), " z")
    }),
    collapse = "; "
  )
}

make_note <- function(changed, key_changes) {
  if (isTRUE(changed)) {
    paste("Archetype changed; biggest standardized shifts:", key_changes)
  } else {
    paste("Archetype stayed the same; biggest standardized shifts:", key_changes)
  }
}

comparison <- baseline_clusters |>
  select(Player, Previous_Season_Archetype, Previous_Cluster) |>
  inner_join(
    new_clusters |> select(Player, Team, Pos, Age, G, MP, New_Season_Archetype, New_Cluster),
    by = "Player"
  ) |>
  mutate(
    `Changed Archetype?` = Previous_Season_Archetype != New_Season_Archetype,
    `Key Statistical Changes` = map_chr(Player, describe_top_changes),
    `Notes / Interpretation` = map2_chr(`Changed Archetype?`, `Key Statistical Changes`, make_note)
  ) |>
  select(
    Player,
    Previous_Season_Archetype,
    New_Season_Archetype,
    `Changed Archetype?`,
    Previous_Cluster,
    New_Cluster,
    `Key Statistical Changes`,
    `Notes / Interpretation`
  ) |>
  arrange(desc(`Changed Archetype?`), Player)

new_players <- new_clusters |>
  anti_join(baseline_clusters, by = "Player") |>
  mutate(
    `Key Stats Driving Assignment` = map_chr(Player, \(player) {
      row <- new_scaled |> filter(Player == player)
      if (nrow(row) == 0) return(NA_character_)
      values <- unlist(row[feature_names])
      top <- sort(abs(values), decreasing = TRUE)[1:min(5, length(values))]
      paste(
        map_chr(names(top), \(feature) {
          value <- values[[feature]]
          direction <- ifelse(value >= 0, "above", "below")
          paste0(feature, " ", direction, " baseline by ", sprintf("%.2f", abs(value)), " z")
        }),
        collapse = "; "
      )
    })
  ) |>
  select(
    Player,
    Team,
    Pos,
    `New Season Archetype` = New_Season_Archetype,
    Cluster = New_Cluster,
    `Key Stats Driving Assignment`
  ) |>
  arrange(`New Season Archetype`, Player)

write_csv(comparison, file.path("output/tables", paste0("archetype_changes_", safe_season, ".csv")))
write_csv(new_players, file.path("output/tables", paste0("new_players_", safe_season, ".csv")))

cat("Returning-player comparison saved to:", file.path("output/tables", paste0("archetype_changes_", safe_season, ".csv")), "\n")
cat("New-player table saved to:", file.path("output/tables", paste0("new_players_", safe_season, ".csv")), "\n")
cat("Returning players:", nrow(comparison), "\n")
cat("Changed archetypes:", sum(comparison$`Changed Archetype?`), "\n")
cat("New players:", nrow(new_players), "\n")
