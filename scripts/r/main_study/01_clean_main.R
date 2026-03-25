#############################################################################>
# CLEAN MAIN STUDY DATA                                                           
#############################################################################>

library(dplyr)
library(readr)

raw_path <- "data/raw/CRC_main_study.csv"
out_path <- "data/processed/main_clean.rds"

# READ IN DATA ------------------------------------------------------------

df <- read_csv(raw_path, col_names = T, na = "")

# REMOVE QUALTRICS ARTIFACTS ----------------------------------------------
# qualtrics exports two metadata rows at the top

df <- df[-c(1:2),]

# CONVERT VARIABLES TO NUMERIC WHERE APPROPRIATE --------------------------

df <- df |>
  mutate(across(
    c("Duration (in seconds)", Q_RecaptchaScore,
      starts_with("attn_chk"), contains("_t_"),
      starts_with("AMJ1_BIMI"), starts_with("AMJ2_BIMI"),
      starts_with("CMJ1_BIMI"), starts_with("CMJ2_BIMI"),
      age, text_medium, gender, race, employment_status, 
      hrs_work, education, industry, eng_proficiency, text_medium),
    readr::parse_number))

# EXCLUSIONS --------------------------------------------------------------

df_clean <- df |>
  filter(!is.na(Q_RecaptchaScore)) |>
  filter(Q_RecaptchaScore > 0.5) |> # remove Q-RecaptchaScore < .5 (see README)
  #select(!starts_with("AI_warning")) |>
  filter(is.na(Q_TerminateFlag) | Q_TerminateFlag != "PoorQuality")

# SEPARATE TIME AND QUESTION VARIABLES ------------------------------------

df_time       <- df_clean |> select(contains("_t_"))
df_questions  <- df_clean |> select(!contains("_t_"))

# combine
df_list <- list(
  time = df_time,
  questions = df_questions
)

# EXPORT ------------------------------------------------------------------

saveRDS(df_list, out_path)
