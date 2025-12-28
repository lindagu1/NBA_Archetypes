library(tidyverse)
df <- readr :: read_csv("data/raw/nbastats.csv", show_col_types = FALSE)
glimpse(df)
cat("Rows:", nrow(df), "Cols:", ncol(df), "\n")
head(df,3)

