library(tidyverse)
library(factoextra)

X <- readRDS("output/tables/nba_features_scaled.rds")

set.seed(20)

p1 <- fviz_nbclust(X, kmeans, method = "wss", k.max = 12) + ggtitle("WSS/Elbow Method")
#measures how tight the clusters are, the elbow is when adding more clusters doesnt improve by that much
p2 <- fviz_nbclust(X, kmeans, method = "silhouette", k.max = 12) + ggtitle("Silhouette Method")
#measures how well each player fits their assigned cluster compared to other clusters, closer to its own + further from others
#high value means player archetypes are distinct, low means cluster overlaps
p3 <- fviz_nbclust(X, kmeans, method = "gap_stat", k.max = 12, nboot = 50) + ggtitle("Gap Statistic")
#compares your clustering to random data with the same shape
dir.create("output/figure", recursive = TRUE, showWarnings = FALSE)
ggsave("output/figure/k_elbow_wss.png",
    plot = p1,
    width = 7, height = 5, dpi = 300)

ggsave("output/figure/k_silhouette.png",
    plot = p2,
    width = 7, height = 5, dpi = 300)
 
ggsave("output/figure/k_gap_statistic.png",
    plot = p3,
    width = 7, height = 5, dpi = 300)

list.files("output/figure")