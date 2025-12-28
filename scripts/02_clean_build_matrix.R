library(tidyverse)
df <- readr::read_csv("data/raw/nbastats.csv", show_col_types = FALSE)

player_col <- intersect(names(df), c("Player"))[1]
games_col <- intersect(names(df), c("G"))[1]
mp_col <- intersect(names(df), c("MP"))[1]
team_col <- intersect(names(df), c("Team"))[1]
pos_col <- intersect(names(df), c("Pos"))[1]
age_col <- intersect(names(df), c("Age"))[1]

stopifnot(!is.na(player_col), !is.na(games_col), !is.na(mp_col))

df2 <- df %>%
    mutate(.player=.data[[player_col]])%>%
    group_by(.player) %>%
    arrange(desc(.data[[mp_col]]), .by_group = TRUE) %>%
    slice(1) %>%
    ungroup() %>%
    select(-.player)

df2 <- df2 %>%
    filter(.data[[games_col]] >=41,
    .data[[mp_col]]/.data[[games_col]] >= 24)

meta_cols <- c(player_col, team_col, pos_col, age_col, games_col, mp_col)
meta_cols <- meta_cols[!is.na(meta_cols) & meta_cols %in% names(df2)]

player_index <- df2 %>%
    select(all_of(meta_cols))

drop_numeric <- intersect(names(df2), c("Rk", "Age", "G", "GS", "MP", "GP", "FG%", "3P%", "2P%", "eFG%", "FT%", "PTS"))
X <- df2 %>%
    select(where(is.numeric)) %>%
    select(-any_of(drop_numeric)) %>%
    drop_na()

kept_rows <- complete.cases(df2 %>% select(where(is.numeric)) %>% select(-any_of(drop_numeric)))
player_index <- player_index[kept_rows, , drop=FALSE]

X_scaled <- as.data.frame(scale(X))

rownames(X_scaled) <- player_index[[player_col]]

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

saveRDS(X_scaled, "output/tables/nba_features_scaled.rds")
readr::write_csv(player_index, "output/tables/player_index.csv")

cat("Saved cleaned feature matrix:\n")
cat("output/tables/nba_features_scaled.rds\n")
cat("Saved player index:\n")
cat("output/tables/player_index.csv\n\n")
cat("Matrix shape:", nrow(X_scaled), "players x", ncol(X_scaled), "features\n")
