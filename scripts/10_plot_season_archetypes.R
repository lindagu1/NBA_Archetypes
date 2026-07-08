library(tidyverse)
library(ggrepel)

season <- Sys.getenv("NBA_SEASON", unset = "2025-26")
safe_season <- gsub("-", "_", season)

new_scaled_path <- file.path("output/tables", paste0("nba_features_scaled_", safe_season, ".rds"))
new_clusters_path <- file.path("output/tables", paste0("player_clusters_named_", safe_season, ".csv"))

if (!file.exists(new_scaled_path) || !file.exists(new_clusters_path)) {
  stop("Run scripts/08_build_new_season_archetypes.R before plotting the new season.")
}

dir.create("output/figure", recursive = TRUE, showWarnings = FALSE)

baseline_scaled <- readRDS("output/tables/nba_features_scaled.rds")
baseline_clusters <- readr::read_csv("output/tables/player_clusters_named.csv", show_col_types = FALSE)
new_scaled <- readRDS(new_scaled_path)
new_clusters <- readr::read_csv(new_clusters_path, show_col_types = FALSE)

pca <- prcomp(baseline_scaled, scale. = FALSE)

project_scores <- function(X) {
  as.matrix(X) %*% pca$rotation[, 1:2, drop = FALSE]
}

baseline_scores <- as.data.frame(project_scores(baseline_scaled)) |>
  setNames(c("PC1", "PC2")) |>
  mutate(Player = rownames(baseline_scaled)) |>
  left_join(select(baseline_clusters, Player, Cluster, Archetype), by = "Player")

new_scores <- as.data.frame(project_scores(new_scaled)) |>
  setNames(c("PC1", "PC2")) |>
  mutate(Player = rownames(new_scaled)) |>
  left_join(select(new_clusters, Player, Cluster, Archetype), by = "Player")

get_representatives <- function(scores) {
  scores |>
    group_by(Cluster) |>
    slice_min(PC1^2 + PC2^2, n = 1, with_ties = FALSE) |>
    ungroup() |>
    pull(Player)
}

plot_clusters <- function(scores, title, subtitle) {
  reps <- get_representatives(scores)
  scores <- scores |>
    mutate(label = if_else(Player %in% reps, Player, ""))
  label_scores <- scores |>
    filter(label != "")

  ggplot(scores, aes(PC1, PC2)) +
    geom_point(aes(color = factor(Cluster)), size = 2, alpha = 0.82) +
    geom_text_repel(
      data = label_scores,
      aes(label = label, color = factor(Cluster)),
      size = 3.5,
      fontface = "bold",
      show.legend = FALSE,
      max.overlaps = 50,
      seed = 42,
      box.padding = 0.9,
      point.padding = 0.5,
      min.segment.length = 0,
      bg.color = "white",
      bg.r = 0.15
    ) +
    guides(color = guide_legend(override.aes = list(label = "", size = 4, alpha = 1))) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11)
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Principal Component 1",
      y = "Principal Component 2",
      color = "Cluster"
    )
}

baseline_plot <- plot_clusters(
  baseline_scores,
  "NBA Player Archetypes, 2024-25",
  "Original k-means clustering visualized in baseline PCA space"
)

new_plot <- plot_clusters(
  new_scores,
  paste0("NBA Player Archetypes, ", season),
  "New season assigned to original archetype centroids and projected into baseline PCA space"
)

baseline_path <- "output/figure/nba_player_clusters_2024_25.png"
new_path <- file.path("output/figure", paste0("nba_player_clusters_", safe_season, ".png"))

ggsave(baseline_path, plot = baseline_plot, width = 8, height = 6, dpi = 300)
ggsave(new_path, plot = new_plot, width = 8, height = 6, dpi = 300)

cat("Saved baseline plot to:", baseline_path, "\n")
cat("Saved new-season plot to:", new_path, "\n")
