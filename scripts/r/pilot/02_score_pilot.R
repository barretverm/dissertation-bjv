#############################################################################~
# SCORE PILOT DATA                                                          
#############################################################################~

library(dplyr)
library(tidyr)
source("R/02_score_pilot_utilities.R")

in_path  <- "data/processed/pilot_clean.rds"
out_path <- "data/processed/pilot_scored.rds"

df <- readRDS(in_path)

# DIVIDE RESPONDENTS BY CONDITION -----------------------------------------
# filtering by attention check isolates job types and excludes failed checks

df <- df |>
  filter(cont_attn_chk == 1) |>
  mutate(
    condition = case_when(
      AMJ1_attn_chk == 1 ~ "AMJ1",
      AMJ2_attn_chk == 1 ~ "AMJ2",
      CMJ1_attn_chk == 1 ~ "CMJ1",
      CMJ2_attn_chk == 1 ~ "CMJ2",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(condition))

# REMOVE ATTENTION CHECK ITEMS --------------------------------------------

df <- df |> select(-contains("attn_chk"))

# REVERSE CODE ------------------------------------------------------------

df_r_coded <- df |>
  mutate(across(
    (starts_with("cont") |
     starts_with("AMJ1") |
     starts_with("AMJ2") |
     starts_with("CMJ1") |
     starts_with("CMJ2")) &
     ends_with("R"),
    ~ 6 - .x)
  )

# SEPARATE CONDITIONS -----------------------------------------------------

AMJ1 <- df_r_coded |>
  filter(condition == "AMJ1") |>
  select(starts_with("AMJ1"), starts_with("cont"))

AMJ2 <- df_r_coded |>
  filter(condition == "AMJ2") |>
  select(starts_with("AMJ2"), starts_with("cont"))

CMJ1 <- df_r_coded |>
  filter(condition == "CMJ1") |>
  select(starts_with("CMJ1"), starts_with("cont"))

CMJ2 <- df_r_coded |>
  filter(condition == "CMJ2") |>
  select(starts_with("CMJ2"), starts_with("cont"))

# MEAN SCORE EACH CONDITION -----------------------------------------------

pilot_scores <- bind_rows(
  mutate(score_pilot_condition(AMJ1, "AMJ1"), job_type = "AMJ1", .before = 1),
  mutate(score_pilot_condition(AMJ2, "AMJ2"), job_type = "AMJ2", .before = 1),
  mutate(score_pilot_condition(CMJ1, "CMJ1"), job_type = "CMJ1", .before = 1),
  mutate(score_pilot_condition(CMJ2, "CMJ2"), job_type = "CMJ2", .before = 1)
)

# EXPORT ------------------------------------------------------------------

saveRDS(pilot_scores, out_path)
