#############################################################################>
# PROCESS MEASURES        
# - Score BIMI
# - Concatenate SJTs
#############################################################################>

library(dplyr)
library(tidyr)
source("R/02_score_main_utilities.R")

in_path  <- "data/processed/main_clean.rds"
out_path <- "data/processed/main_scored.rds"

df <- readRDS(in_path)
df_questions <- df$questions # pull questions from list

# SCORE BIMI --------------------------------------------------------------

## REVERSE CODE ----

df_r_coded <- df_questions |>
  mutate(across(
      (starts_with("AMJ1_BIMI") |
       starts_with("AMJ2_BIMI") |
       starts_with("CMJ1_BIMI") |
       starts_with("CMJ2_BIMI")) &
      ends_with("R"),
    ~ 8 - .x)
  )

## MEAN SCORE BIMI SCALES ----

df_main_scored <- df_r_coded |>
  rowwise() |>
  mutate(
    AMJ1_SCORED_A = mean(c_across(starts_with("AMJ1_BIMI_A")), na.rm = TRUE),
    AMJ1_SCORED_C = mean(c_across(starts_with("AMJ1_BIMI_C")), na.rm = TRUE),
    AMJ2_SCORED_A = mean(c_across(starts_with("AMJ2_BIMI_A")), na.rm = TRUE),
    AMJ2_SCORED_C = mean(c_across(starts_with("CMJ2_BIMI_C")), na.rm = TRUE),
    CMJ1_SCORED_A = mean(c_across(starts_with("CMJ1_BIMI_A")), na.rm = TRUE),
    CMJ1_SCORED_C = mean(c_across(starts_with("CMJ1_BIMI_C")), na.rm = TRUE),
    CMJ2_SCORED_A = mean(c_across(starts_with("CMJ2_BIMI_A")), na.rm = TRUE),
    CMJ2_SCORED_C = mean(c_across(starts_with("CMJ2_BIMI_C")), na.rm = TRUE),
  ) |>
  ungroup()

# CONCATENATE SJTs --------------------------------------------------------

df_main_all <- df_main_scored |>
  rowwise() |>
  mutate(
    SJT_AMJ1_all = paste(na.omit(c_across(
      matches("^AMJ1.*_(agree|cons|neuro|extra)$"))), collapse = " "),
    SJT_AMJ2_all = paste(na.omit(c_across(
      matches("^AMJ2.*_(agree|cons|neuro|extra)$"))), collapse = " "),
    SJT_CMJ1_all = paste(na.omit(c_across(
      matches("^CMJ1.*_(agree|cons|neuro|extra)$"))), collapse = " "),
    SJT_CMJ2_all = paste(na.omit(c_across(
      matches("^CMJ2.*_(agree|cons|neuro|extra)$"))), collapse = " "),
    SJT_CMJ1_CM  = paste(na.omit(c_across(
      matches("^AMJ1.*_(agree|cons|neuro)$"))), collapse = " "),
    SJT_CMJ2_CM  = paste(na.omit(c_across(
      matches("^AMJ2.*_(agree|cons|neuro)$"))), collapse = " "),
    SJT_AMJ1_CM  = paste(na.omit(c_across(
      matches("^CMJ1.*_(agree|cons|neuro)$"))), collapse = " "),
    SJT_AMJ2_CM  = paste(na.omit(c_across(
      matches("^CMJ2.*_(agree|cons|neuro)$"))), collapse = " "),
    SJT_AMJ1_AM  = paste(na.omit(c_across(
      matches("^AMJ1.*_extra$"))), collapse = " "),
    SJT_AMJ2_AM  = paste(na.omit(c_across(
      matches("^AMJ2.*_extra$"))), collapse = " "),
    SJT_CMJ1_AM  = paste(na.omit(c_across(
      matches("^CMJ1.*_extra$"))), collapse = " "),
    SJT_CMJ2_AM  = paste(na.omit(c_across(
      matches("^CMJ2.*_extra$"))), collapse = " "),
  ) |>
  ungroup()

# SHRINK DATASET TO RELEVANT VARIABLES ------------------------------------

df_main_all <- df_main_all |>
  select(
    ResponseId,
    age,
    gender,
    race,
    hrs_work,
    education,
    industry,
    job_title,
    AgenticJob,
    CommunalJob,
    matches("^SJT_.*_all$"),
    matches("^SJT_.*_CM$"),
    matches("^SJT_.*_AM$"),
    contains("_SCORED_")
  )

# EXPORT ------------------------------------------------------------------

saveRDS(pilot_scores, out_path)