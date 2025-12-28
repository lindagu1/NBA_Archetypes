library(tidyverse)

X <- readRDS("output/tables/nba_features_scaled.rds")
clusters <- readr::read_csv("output/tables/player_clusters.csv", show_col_types = FALSE)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

df <- as_tibble(X) %>%
  mutate(Player = rownames(X)) %>%
  left_join(clusters, by = "Player")

stopifnot(nrow(df) == nrow(X))
stopifnot(all(!is.na(df$Cluster)))

#cluster profiles
cluster_profiles <- df %>%
  group_by(Cluster) %>%
  summarise(
    across(where(is.numeric), mean),
    n_players = n(),
    .groups = "drop"
  ) %>%
  arrange(Cluster)

write_csv(cluster_profiles, "output/tables/cluster_profiles.csv")

#cluster signatures, what is the most distinctive feature of that group
z_thresh <- 0.75

cluster_signatures <- cluster_profiles %>%
  select(-n_players) %>%
  pivot_longer(cols = -Cluster, names_to = "stat", values_to = "z") %>%
  mutate(abs_z = abs(z)) %>%
  arrange(Cluster, desc(abs_z)) %>%
  filter(abs_z >= z_thresh)

write_csv(cluster_signatures, "output/tables/cluster_signatures.csv")

# Top 5 stats per cluster (by |z|)
top_features_per_cluster <- cluster_profiles %>%
  select(-n_players) %>%
  pivot_longer(cols = -Cluster, names_to = "stat", values_to = "z") %>%
  mutate(abs_z = abs(z)) %>%
  group_by(Cluster) %>%
  slice_max(abs_z, n = 5, with_ties = FALSE) %>%
  arrange(Cluster, desc(abs_z)) %>%
  ungroup()

write_csv(top_features_per_cluster, "output/tables/top_features_per_cluster.csv")

# archetype names

cluster_names <- tribble(
  ~Cluster, ~Archetype,
  1, "Defensive Rim Protectors",
  2, "Perimeter Shooters and Secondary Guards",
  3, "Interior Bigs",
  4, "Low-Usage Role Players",
  5, "Superstar Offensive Hubs",
  6, "Versatile Two-Way Forwards",
  7, "Primary Shot Creators"
)
write_csv(cluster_names, "output/tables/cluster_names.csv")

# player with archetype labels
clusters_named <- clusters %>%
  left_join(cluster_names, by = "Cluster") %>%
  arrange(Cluster, Player)

write_csv(clusters_named, "output/tables/player_clusters_named.csv")