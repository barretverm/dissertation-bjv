#############################################################################~
# PREPARE DATA FOR LIWC ANALYSIS     
# - Match deBERTa based on response IDs
#############################################################################~

library(tidyverse)

processed_main_df <- readRDS("data/processed/main_scored.rds")

pred_files <- list.files(
  "/home/bear/dissertation-bjv/outputs/deberta-v3-small",
  pattern = "predictions_.*\\.csv",
  recursive = TRUE,
  full.names = TRUE
)

# check that all files have identical ResponseIds
id_lists <- map(pred_files, ~ read_csv(.x) |> pull(ResponseId) |> sort())
all_equal <- map_lgl(id_lists[-1], ~ identical(.x, id_lists[[1]]))
all(all_equal)

deberta_key <- map_dfr(pred_files, ~ read_csv(.x, show_col_types = FALSE) |>
                         distinct(ResponseId, classification, dimension))

# verify the key structure
deberta_key |> count(classification, dimension)

# check within classification×dimension, all three text conditions have same IDs
map_dfr(pred_files, ~ read_csv(.x, show_col_types = FALSE) |>
          distinct(ResponseId, classification, dimension, model)) |>
  count(ResponseId, classification, dimension) |>
  filter(n != 3)

# match the data from 
df_long_matched <- df_long_final |>
  semi_join(deberta_key, by = c("ResponseId", "classification", "dimension"))

# word counts from final models
df_long_matched |>
  filter(SJTs_all != "") |>
  mutate(
    words_SJTs_all = str_count(SJTs_all, "\\S+"),
    words_SJT_AM   = str_count(na_if(SJT_AM, ""), "\\S+"),
    words_SJT_CM   = str_count(na_if(SJT_CM, ""), "\\S+")
  ) |>
  summarize(
    SJTs_all_min    = min(words_SJTs_all,    na.rm = TRUE),
    SJTs_all_max    = max(words_SJTs_all,    na.rm = TRUE),
    SJTs_all_median = median(words_SJTs_all, na.rm = TRUE),
    SJT_AM_min      = min(words_SJT_AM,      na.rm = TRUE),
    SJT_AM_max      = max(words_SJT_AM,      na.rm = TRUE),
    SJT_AM_median   = median(words_SJT_AM,   na.rm = TRUE),
    SJT_CM_min      = min(words_SJT_CM,      na.rm = TRUE),
    SJT_CM_max      = max(words_SJT_CM,      na.rm = TRUE),
    SJT_CM_median   = median(words_SJT_CM,   na.rm = TRUE)
  ) |>
  print(width = Inf)

# export to csv for LIWC
write_csv(df_long_matched, "data/processed/main_processed_for_LIWC.csv")
