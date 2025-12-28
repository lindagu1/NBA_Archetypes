#elbow suggests k=7
#silhouette suggests k=5
#gap suggest min k=3

#we choose k = 7 to be conservative but not overfitting

library(tidyverse)

X <- readRDS ("output/tables/nba_features_scaled.rds")

set.seed(20)
k <- 7
km <- kmeans(X, centers = k, nstart = 50)

saveRDS(km, "output/tables/kmeans_model.rds")

clusters <- tibble( 
    Player = rownames(X),
    Cluster = as.integer(km$cluster)
)

write_csv(clusters, "output/tables/player_clusters.csv")

print(clusters %>% count(Cluster))

# cluster_assignment <- tibble(
#     player = rownames(X),
#     cluster = as.integer(km$cluster)
# )

# cluster_assignment %>%
#     filter(cluster==1) %>% 
#     select(player)
