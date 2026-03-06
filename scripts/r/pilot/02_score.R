#############################################################################>
# SCORE PILOT DATA                                                          
#############################################################################>

library(dplyr)
library(tidyr)

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
# mean score into separate tibbles

AMJ1_scores <- tibble(
  # control
  cont_AM_mean = rowMeans(select(AMJ1, starts_with("cont_AM")),  na.rm = T),
  cont_CM_mean = rowMeans(select(AMJ1, starts_with("cont_CM")),  na.rm = T),
  cont_grand_mean = rowMeans(select(AMJ1, starts_with("cont_")), na.rm = T),
  # manipulation
  exp_AM_mean = rowMeans(select(AMJ1, starts_with("AMJ1-AM")),  na.rm = T),
  exp_CM_mean = rowMeans(select(AMJ1, starts_with("AMJ1-CM")),  na.rm = T),
  exp_grand_mean = rowMeans(select(AMJ1, starts_with("AMJ1-")), na.rm = T)
)

AMJ2_scores <- tibble(
  # control
  cont_AM_mean = rowMeans(select(AMJ2, starts_with("cont_AM")),  na.rm = T),
  cont_CM_mean = rowMeans(select(AMJ2, starts_with("cont_CM")),  na.rm = T),
  cont_grand_mean = rowMeans(select(AMJ2, starts_with("cont_")), na.rm = T),
  # manipulation
  exp_AM_mean = rowMeans(select(AMJ2, starts_with("AMJ2-AM")),  na.rm = T),
  exp_CM_mean = rowMeans(select(AMJ2, starts_with("AMJ2-CM")),  na.rm = T),
  exp_grand_mean = rowMeans(select(AMJ2, starts_with("AMJ2-")), na.rm = T)
)

CMJ1_scores <- tibble(
  # control
  cont_AM_mean = rowMeans(select(CMJ1, starts_with("cont_AM")),  na.rm = T),
  cont_CM_mean = rowMeans(select(CMJ1, starts_with("cont_CM")),  na.rm = T),
  cont_grand_mean = rowMeans(select(CMJ1, starts_with("cont_")), na.rm = T),
  # manipulation
  exp_AM_mean = rowMeans(select(CMJ1, starts_with("CMJ1-AM")),  na.rm = T),
  exp_CM_mean = rowMeans(select(CMJ1, starts_with("CMJ1-CM")),  na.rm = T),
  exp_grand_mean = rowMeans(select(CMJ1, starts_with("CMJ1-")), na.rm = T)
)

CMJ2_scores <- tibble(
  # control
  cont_AM_mean = rowMeans(select(CMJ2, starts_with("cont_AM")),  na.rm = T),
  cont_CM_mean = rowMeans(select(CMJ2, starts_with("cont_CM")),  na.rm = T),
  cont_grand_mean = rowMeans(select(CMJ2, starts_with("cont_")), na.rm = T),
  # manipulation
  exp_AM_mean = rowMeans(select(CMJ2, starts_with("CMJ2-AM")),  na.rm = T),
  exp_CM_mean = rowMeans(select(CMJ2, starts_with("CMJ2-CM")),  na.rm = T),
  exp_grand_mean = rowMeans(select(CMJ2, starts_with("CMJ2-")), na.rm = T)
)

# COMBINE INTO SINGLE TIBBLE ----------------------------------------------

pilot_scores <- bind_rows(
  mutate(AMJ1_scores, job_type = "AMJ1", .before = 1),
  mutate(AMJ2_scores, job_type = "AMJ2", .before = 1),
  mutate(CMJ1_scores, job_type = "CMJ1", .before = 1),
  mutate(CMJ2_scores, job_type = "CMJ2", .before = 1)
)

# EXPORT ------------------------------------------------------------------

saveRDS(pilot_scores, out_path)