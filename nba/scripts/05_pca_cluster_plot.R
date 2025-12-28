library(tidyverse)
library(factoextra)
library(ggrepel)

# Load data
X  <- readRDS("output/tables/nba_features_scaled.rds")
km <- readRDS("output/tables/kmeans_model.rds")

# PCA
pca <- prcomp(X, scale. = FALSE)

pca_df <- as.data.frame(pca$x[, 1:2])
pca_df$Player  <- rownames(X)
pca_df$Cluster <- factor(km$cluster)

# Representative players (closest to cluster center)
get_representatives <- function() {
  reps <- c()
  for (i in unique(km$cluster)) {
    idx <- which(km$cluster == i)
    center <- km$centers[i, ]
    dists <- apply(X[idx, ], 1, function(x) sum((x - center)^2))
    reps <- c(reps, rownames(X)[idx[which.min(dists)]])
  }
  reps
}

rep_players <- get_representatives()

pca_df$label <- ifelse(
  pca_df$Player %in% rep_players,
  pca_df$Player,
  ""
)

# Only draw ellipses for clusters with enough points
cluster_sizes <- pca_df %>% count(Cluster, name = "n")
ellipse_ok <- cluster_sizes %>% filter(n >= 3) %>% pull(Cluster)

small_clusters <- cluster_sizes %>% filter(n < 3) %>% pull(Cluster)

# Plot
p <- ggplot(pca_df, aes(PC1, PC2, color = Cluster)) +
  geom_point(size = 2, alpha = 0.8) +
  stat_ellipse(
    data = pca_df %>% filter(Cluster %in% ellipse_ok),
    level = 0.8,
    linewidth = 1,
    type = "t"
  ) +
  geom_point(
    data = pca_df %>% filter(Cluster %in% small_clusters),
    size = 15,
    shape = 21,
    stroke = 1,
    fill = NA
  ) + 
  geom_text_repel(
    aes(label = label),
    size = 3.5,
    fontface = "bold",
    max.overlaps = 50,
    force = 5,
    force_pull = 0.5,
    box.padding = 1.0,
    point.padding = 0.6,
    min.segment.length = 0,
    seed = 42,
    bg.color = "white",
    bg.r = 0.15 
  ) +
  theme_minimal() +
  theme(
  legend.position = "none",
  plot.title = element_text(hjust = 0.5, family = "Arial", face = "bold", size = 14),
  plot.subtitle = element_text(hjust = 0.5, family = "Arial", size = 11),
  text = element_text(family = "Arial") 
)+
  labs(
    title = "NBA Player Archetypes by K-Means Clustering",
    subtitle = "PCA Projection of Standardized Player Statistics",
    x = "Principal Component 1",
    y = "Principal Component 2"
  )

# Save figure
dir.create("output/figure", recursive = TRUE, showWarnings = FALSE)

suppressWarnings(ggsave(
  "output/figure/nba_player_clusters.png",
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)
)