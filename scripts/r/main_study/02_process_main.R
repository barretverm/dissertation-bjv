#############################################################################~
# PROCESS MEASURES        
# - Score BIMI
# - Concatenate SJTs
# - Convert to long format
# - Prep data for validation checks and ML analysis
#############################################################################~

library(dplyr)
library(tidyr)

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
    AMJ2_SCORED_C = mean(c_across(starts_with("AMJ2_BIMI_C")), na.rm = TRUE),
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
   # all
    SJT_AMJ1_all = paste(na.omit(c_across(
      matches("^AMJ1.*_(agree|cons|neuro|extra)$"))), collapse = " "),
    SJT_AMJ2_all = paste(na.omit(c_across(
      matches("^AMJ2.*_(agree|cons|neuro|extra)$"))), collapse = " "),
    SJT_CMJ1_all = paste(na.omit(c_across(
      matches("^CMJ1.*_(agree|cons|neuro|extra)$"))), collapse = " "),
    SJT_CMJ2_all = paste(na.omit(c_across(
      matches("^CMJ2.*_(agree|cons|neuro|extra)$"))), collapse = " "),
   # communal (agree, cons, & neuro)
    SJT_AMJ1_CM  = paste(na.omit(c_across(
     matches("^AMJ1.*_(agree|cons|neuro)$"))), collapse = " "),
    SJT_AMJ2_CM  = paste(na.omit(c_across(
     matches("^AMJ2.*_(agree|cons|neuro)$"))), collapse = " "),
    SJT_CMJ1_CM  = paste(na.omit(c_across(
      matches("^CMJ1.*_(agree|cons|neuro)$"))), collapse = " "),
    SJT_CMJ2_CM  = paste(na.omit(c_across(
      matches("^CMJ2.*_(agree|cons|neuro)$"))), collapse = " "),
   # agenctic (extraversion) 
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

# CONVERT TO LONG FORMAT --------------------------------------------------

df_long_final <- df_main_all |>
  # step 1: one row per participant per job type
  pivot_longer(
    cols      = c(AgenticJob, CommunalJob),
    names_to  = "classification",
    values_to = "job_type"
  ) |>
  mutate(
    classification = case_when(
      classification == "AgenticJob"  ~ "agentic",
      classification == "CommunalJob" ~ "communal"
    ),
    # step 2: pull correct BIMI scores for this job type
    BIMI_A = case_when(
      job_type == "AMJ1" ~ AMJ1_SCORED_A,
      job_type == "AMJ2" ~ AMJ2_SCORED_A,
      job_type == "CMJ1" ~ CMJ1_SCORED_A,
      job_type == "CMJ2" ~ CMJ2_SCORED_A
    ),
    BIMI_C = case_when(
      job_type == "AMJ1" ~ AMJ1_SCORED_C,
      job_type == "AMJ2" ~ AMJ2_SCORED_C,
      job_type == "CMJ1" ~ CMJ1_SCORED_C,
      job_type == "CMJ2" ~ CMJ2_SCORED_C
    ),
    # step 3: pull correct SJT text for this job type
    SJTs_all = case_when(
      job_type == "AMJ1" ~ SJT_AMJ1_all,
      job_type == "AMJ2" ~ SJT_AMJ2_all,
      job_type == "CMJ1" ~ SJT_CMJ1_all,
      job_type == "CMJ2" ~ SJT_CMJ2_all
    ),
    SJT_CM = case_when(
      job_type == "AMJ1" ~ SJT_AMJ1_CM,
      job_type == "AMJ2" ~ SJT_AMJ2_CM,
      job_type == "CMJ1" ~ SJT_CMJ1_CM,
      job_type == "CMJ2" ~ SJT_CMJ2_CM
    ),
    SJT_AM = case_when(
      job_type == "AMJ1" ~ SJT_AMJ1_AM,
      job_type == "AMJ2" ~ SJT_AMJ2_AM,
      job_type == "CMJ1" ~ SJT_CMJ1_AM,
      job_type == "CMJ2" ~ SJT_CMJ2_AM
    )
  ) |>
  # step 4: one row per participant per job type per BIMI dimension
  pivot_longer(
    cols         = c(BIMI_A, BIMI_C),
    names_to     = "dimension",
    names_pattern = "BIMI_(A|C)",
    values_to    = "BIMI_mean"
  ) |>
  mutate(
    dimension      = case_when(dimension == "A" ~ "AM", dimension == "C" ~ "CM"),
    classification = factor(classification),
    dimension      = factor(dimension),
    job_type       = factor(job_type),
    ResponseId     = factor(ResponseId)
  ) |>
  # step 5: drop wide columns, keep only long-format variables
  select(
    ResponseId, 
    age, 
    gender, 
    race, 
    hrs_work, 
    education, 
    industry, 
    job_title,
    SJTs_all, 
    SJT_CM, 
    SJT_AM,
    classification, 
    job_type, 
    dimension, 
    BIMI_mean
  )

# EXPORT ------------------------------------------------------------------

saveRDS(df_long_final, out_path)