#############################################################################~
# DESCRIPTIVE STATISTICS OF FINAL ANALYSES
#############################################################################~

library(dplyr)

post_path <- "data/processed/descriptive_data.rds" # data post analysis
pre_path  <- "data/raw/CRC_main_study.csv" # raw data

df_post <- readRDS(post_path)
df_pre  <- read_csv(pre_path)

# SELECT AND FILTER DATA --------------------------------------------------

# select sample from agentic and communal job types (different sample sizes)
agentic_demo_sample   <- df_post |> filter(classification == "agentic")
communal_demo_sample  <- df_post |> filter(classification == "communal")

# select only demo variables of interest
demographics <- df_pre[,c(
  "ResponseId", "age", "gender", "race", "education")]

# match respondents based on response id
agentic_demos  <- inner_join(agentic_demo_sample, demographics, by = "ResponseId")
communal_demos <- inner_join(communal_demo_sample, demographics, by = "ResponseId")

# RECODE DEMOGRAPHIC VALUES -----------------------------------------------

# agentic
agentic_demos <- agentic_demos |>
  mutate(
    gender_label = recode(gender,
                          `1` = "Male",
                          `2` = "Female",
                          `3` = "Non-binary / third gender",
                          `4` = "Prefer not to say"),
    race_label = recode(race,
                        `1` = "American Indian or Alaska Native",
                        `2` = "Asian",
                        `3` = "Black or African American",
                        `4` = "Hispanic or Latino",
                        `5` = "Middle Eastern or North African",
                        `6` = "Native Hawaiian or Other Pacific Islander",
                        `7` = "White",
                        `8` = "Another race or ethnicity",
                        `9` = "Prefer not to say"),
    race_label = if_else(grepl(",", race_label), "Multiracial", race_label),
    education_label = recode(education,
                             `1` = "Less than high school",
                             `2` = "High school diploma or GED",
                             `3` = "Some college, no degree",
                             `4` = "Associate degree",
                             `5` = "Bachelor's degree",
                             `6` = "Graduate or professional degree",
                             `7` = "Other",
                             `8` = "Prefer not to say")
  )

# communal
communal_demos <- communal_demos |>
  mutate(
    gender_label = recode(gender,
                          `1` = "Male",
                          `2` = "Female",
                          `3` = "Non-binary / third gender",
                          `4` = "Prefer not to say"),
    race_label = recode(race,
                        `1` = "American Indian or Alaska Native",
                        `2` = "Asian",
                        `3` = "Black or African American",
                        `4` = "Hispanic or Latino",
                        `5` = "Middle Eastern or North African",
                        `6` = "Native Hawaiian or Other Pacific Islander",
                        `7` = "White",
                        `8` = "Another race or ethnicity",
                        `9` = "Prefer not to say"),
    race_label = if_else(grepl(",", race_label), "Multiracial", race_label),
    education_label = recode(education,
                             `1` = "Less than high school",
                             `2` = "High school diploma or GED",
                             `3` = "Some college, no degree",
                             `4` = "Associate degree",
                             `5` = "Bachelor's degree",
                             `6` = "Graduate or professional degree",
                             `7` = "Other",
                             `8` = "Prefer not to say")
  )

# BASIC DEMOGRAPHICS ------------------------------------------------------

# using agentic only because this condition had the full sample size
# age
agentic_demos <- agentic_demos |>
  mutate(age = as.numeric(age))

agentic_demos |>
  summarize(
    n        = n(),
    median_age = round(median(age, na.rm = T), 2),
    sd_age   = round(sd(age, na.rm = T), 2),
    min_age  = min(age, na.rm = T),
    max_age  = max(age, na.rm = T)
  )

# gender
agentic_demos |>
  count(gender_label) |>
  mutate(prop = round(n / sum(n) * 100, 1))

# race
agentic_demos |>
  count(race_label) |>
  mutate(prop = round(n / sum(n) * 100, 1))

# education
agentic_demos |>
  count(education_label) |>
  mutate(prop = round(n / sum(n) * 100, 1))

# EXAMINE VARIANCE ABNORMALITIES IN COMMUNAL CLASSIFICATION ---------------
# see 03_discriminant_validity.R for more info

# create subgroups
communal_CM_low <- communal_demos |>
  filter(dimension == "CM", predicted_BIMI < 2) |>
  mutate(outlier_group = "low")

communal_CM_normal <- communal_demos |>
  filter(dimension == "CM", predicted_BIMI >= 2) |>
  mutate(outlier_group = "normal")

combined <- bind_rows(communal_CM_low, communal_CM_normal)
t.test(predicted_BIMI ~ outlier_group, data = combined)

# gender (narrowed to male/female because this is how the literature has processed it)
combined_gender <- combined |>
  filter(gender_label %in% c("Male", "Female", "Non-binary / third gender"))

chisq.test(table(combined_gender$outlier_group, combined_gender$gender_label))

combined_gender |>
  count(outlier_group, gender_label) |>
  group_by(outlier_group) |>
  mutate(prop = round(n / sum(n), 3))

# age
combined <- combined |> mutate(age = as.numeric(age))
t.test(age ~ outlier_group, data = combined)

# race
# collapsing racial groups with very low report rates (e.g., Hawaiian = 1)
combined <- combined |>
  mutate(race_collapsed = case_when(
    race_label == "White"                          ~ "White",
    race_label == "Black or African American"      ~ "Black or African American",
    race_label == "Asian"                          ~ "Asian",
    race_label == "Multiracial"                    ~ "Multiracial",
    race_label == "Hispanic or Latino"             ~ "Hispanic or Latino",
    T                                              ~ "Other"
  ))

fisher.test(table(combined$outlier_group, combined$race_collapsed),
            simulate.p.value = T, B = 10000)

# inspect proportions
combined |> 
  count(outlier_group, race_collapsed) |> 
  group_by(outlier_group) |> 
  mutate(prop = round(n / sum(n), 3))

# education
combined <- combined |>
  mutate(education_collapsed = case_when(
    education_label == "Bachelor's degree"              ~ "Bachelor's degree",
    education_label == "Graduate or professional degree" ~ "Graduate or professional degree",
    education_label == "Some college, no degree"        ~ "Some college, no degree",
    education_label == "Associate degree"               ~ "Associate degree",
    education_label == "High school diploma or GED"     ~ "High school diploma or GED",
    T                                                   ~ "Other"
  ))

fisher.test(table(combined$outlier_group, combined$education_collapsed),
            simulate.p.value = T, B = 10000)

combined |>
  count(outlier_group, education_collapsed) |>
  group_by(outlier_group) |>
  mutate(prop = round(n / sum(n), 3))

