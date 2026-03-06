#############################################################################>
# CLEAN PILOT DATA                                                            
#############################################################################>

library(readr)
library(dplyr)
library(tidyr)

raw_path <- "data/raw/SONA_pilot_test_data.csv"
out_path <- "data/processed/pilot_clean.rds"

# READ IN DATA ------------------------------------------------------------

df <- read_csv(raw_path, col_names = T, na = "")

# REMOVE QUALTRICS ARTIFACTS ----------------------------------------------
# qualtrics exports two metadata rows at the top

df <- df[-c(1:2),]

# CONVERT MEASURES TO NUMERIC ---------------------------------------------

df <- df |>
  mutate(across(
    c(Q_RecaptchaScore,
      starts_with("AMJ1"), starts_with("AMJ2"),
      starts_with("CMJ1"), starts_with("CMJ2"),
      starts_with("cont"), age, 
      alter, text_medium,
      gender, race,
      employment_status, hrs_work,
      education,industry),
    readr::parse_number))

# EXCLUSIONS --------------------------------------------------------------
# remove Q_RecaptchaScore < .5 (see README)

sum(is.na(df$Q_RecaptchaScore)) # count cases

df_clean <- df |>
  filter(!is.na(Q_RecaptchaScore)) |>
  filter(Q_RecaptchaScore > 0.5)

# EXPORT ------------------------------------------------------------------

saveRDS(df_clean, out_path)
