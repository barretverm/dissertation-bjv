#############################################################################>
# VALIDATION STUDY
# - Evaluate discriminant validity of main study data
#############################################################################>
library(dplyr)
library(readr)
library(rstatix)
library(ggplot2)
library(psych)
source("R/ttest_utilities.R")

# IMPORT DATA ----

agentic_AM_path <- "outputs/deberta-v3-small/agentic_AM"
agentic_CM_path <- "outputs/deberta-v3-small/agentic_CM"
communal_AM_path <- "outputs/deberta-v3-small/communal_AM"
communal_CM_path <- "outputs/deberta-v3-small/communal_CM"

# SJTs - all
agentic_AM_SJT_all   <- read_csv(
  file.path(agentic_AM_path, 'predictions_agentic_AM_SJT_all_v1_kfold.csv'))
agentic_CM_SJT_all   <- read_csv(
  file.path(agentic_CM_path, 'predictions_agentic_CM_SJT_all_v1_kfold.csv'))
communal_AM_SJT_all  <- read_csv(
  file.path(communal_AM_path, 'predictions_communal_AM_SJT_all_v1_kfold.csv'))
communal_CM_SJT_all  <- read_csv(
  file.path(communal_CM_path, 'predictions_communal_CM_SJT_all_v1_kfold.csv'))

# SJTs - AM
agentic_AM_SJT_AM   <- read_csv(
  file.path(agentic_AM_path, 'predictions_agentic_AM_SJT_AM_v1_kfold.csv'))
agentic_CM_SJT_AM   <- read_csv(
  file.path(agentic_CM_path, 'predictions_agentic_CM_SJT_AM_v1_kfold.csv'))
communal_AM_SJT_AM  <- read_csv(
  file.path(communal_AM_path, 'predictions_communal_AM_SJT_AM_v1_kfold.csv'))
communal_CM_SJT_AM  <- read_csv(
  file.path(communal_CM_path, 'predictions_communal_CM_SJT_AM_v1_kfold.csv'))

# SJTS - CM
agentic_AM_SJT_CM   <- read_csv(
  file.path(agentic_AM_path, 'predictions_agentic_AM_SJT_CM_v1_kfold.csv'))
agentic_CM_SJT_CM   <- read_csv(
  file.path(agentic_CM_path, 'predictions_agentic_CM_SJT_CM_v1_kfold.csv'))
communal_AM_SJT_CM  <- read_csv(
  file.path(communal_AM_path, 'predictions_communal_AM_SJT_CM_v1_kfold.csv'))
communal_CM_SJT_CM  <- read_csv(
  file.path(communal_CM_path, 'predictions_communal_CM_SJT_CM_v1_kfold.csv'))

# ASSUMPTION CHECKS ----

# run normality tests across all datasets
normality_results <- bind_rows(
  test_normality(agentic_AM_SJT_all,  "agentic_AM_SJT_all"),
  test_normality(agentic_CM_SJT_all,  "agentic_CM_SJT_all"),
  test_normality(communal_AM_SJT_all, "communal_AM_SJT_all"),
  test_normality(communal_CM_SJT_all, "communal_CM_SJT_all"),
  test_normality(agentic_AM_SJT_AM,   "agentic_AM_SJT_AM"),
  test_normality(agentic_CM_SJT_AM,   "agentic_CM_SJT_AM"),
  test_normality(communal_AM_SJT_AM,  "communal_AM_SJT_AM"),
  test_normality(communal_CM_SJT_AM,  "communal_CM_SJT_AM"),
  test_normality(agentic_AM_SJT_CM,   "agentic_AM_SJT_CM"),
  test_normality(agentic_CM_SJT_CM,   "agentic_CM_SJT_CM"),
  test_normality(communal_AM_SJT_CM,  "communal_AM_SJT_CM"),
  test_normality(communal_CM_SJT_CM,  "communal_CM_SJT_CM")
)

normality_results

plot_qq(agentic_AM_SJT_all,  "agentic_AM_SJT_all")
plot_qq(agentic_CM_SJT_all,  "agentic_CM_SJT_all")
plot_qq(communal_AM_SJT_all, "communal_AM_SJT_all")
plot_qq(communal_CM_SJT_all, "communal_CM_SJT_all")
plot_qq(agentic_AM_SJT_AM,   "agentic_AM_SJT_AM")
plot_qq(agentic_CM_SJT_AM,   "agentic_CM_SJT_AM")
plot_qq(communal_AM_SJT_AM,  "communal_AM_SJT_AM")
plot_qq(communal_CM_SJT_AM,  "communal_CM_SJT_AM")
plot_qq(agentic_AM_SJT_CM,   "agentic_AM_SJT_CM")
plot_qq(agentic_CM_SJT_CM,   "agentic_CM_SJT_CM")
plot_qq(communal_AM_SJT_CM,  "communal_AM_SJT_CM")
plot_qq(communal_CM_SJT_CM,  "communal_CM_SJT_CM")

# homogeneity of variance
# SJTs - all
levene_results_SJT_all <- bind_rows(
  run_levene(agentic_AM_SJT_all,  agentic_CM_SJT_all,  "agentic AM vs agentic CM"),
  run_levene(communal_AM_SJT_all, communal_CM_SJT_all, "communal AM vs communal CM")
)

# SJTs - AM
levene_results_SJT_AM <- bind_rows(
  run_levene(agentic_AM_SJT_AM,  agentic_CM_SJT_AM,  "agentic AM vs agentic CM"),
  run_levene(communal_AM_SJT_AM, communal_CM_SJT_AM, "communal AM vs communal CM")
)

# SJTs - CM
levene_results_SJT_CM <- bind_rows(
  run_levene(agentic_AM_SJT_CM,  agentic_CM_SJT_CM,  "agentic AM vs agentic CM"),
  run_levene(communal_AM_SJT_CM, communal_CM_SJT_CM, "communal AM vs communal CM")
)

levene_results_SJT_all
levene_results_SJT_AM
levene_results_SJT_CM


# T-TESTS ----

# SJTs - all
ttest_results_SJT_all <- bind_rows(
  between_ttest(
    x     = agentic_AM_SJT_all$actual_BIMI,
    y     = agentic_CM_SJT_all$actual_BIMI,
    label = "actual: agentic AM vs agentic CM"
  ),
  between_ttest(
    x     = agentic_AM_SJT_all$predicted_BIMI,
    y     = agentic_CM_SJT_all$predicted_BIMI,
    label = "predicted: agentic AM vs agentic CM"
  ),
  between_ttest(
    x     = communal_AM_SJT_all$actual_BIMI,
    y     = communal_CM_SJT_all$actual_BIMI,
    label = "actual: communal AM vs communal CM"
  ),
  between_ttest(
    x     = communal_AM_SJT_all$predicted_BIMI,
    y     = communal_CM_SJT_all$predicted_BIMI,
    label = "predicted: communal AM vs communal CM"
  )
)
ttest_results_SJT_all

# SJTs - AM
ttest_results_SJT_AM <- bind_rows(
  between_ttest(
    x     = agentic_AM_SJT_AM$actual_BIMI,
    y     = agentic_CM_SJT_AM$actual_BIMI,
    label = "actual: agentic AM vs agentic CM"
  ),
  between_ttest(
    x     = agentic_AM_SJT_AM$predicted_BIMI,
    y     = agentic_CM_SJT_AM$predicted_BIMI,
    label = "predicted: agentic AM vs agentic CM"
  ),
  between_ttest(
    x     = communal_AM_SJT_AM$actual_BIMI,
    y     = communal_CM_SJT_AM$actual_BIMI,
    label = "actual: communal AM vs communal CM"
  ),
  between_ttest(
    x     = communal_AM_SJT_AM$predicted_BIMI,
    y     = communal_CM_SJT_AM$predicted_BIMI,
    label = "predicted: communal AM vs communal CM"
  )
)
ttest_results_SJT_AM

# SJTs - CM
ttest_results_SJT_CM <- bind_rows(
  between_ttest(
    x     = agentic_AM_SJT_CM$actual_BIMI,
    y     = agentic_CM_SJT_CM$actual_BIMI,
    label = "actual: agentic AM vs agentic CM"
  ),
  between_ttest(
    x     = agentic_AM_SJT_CM$predicted_BIMI,
    y     = agentic_CM_SJT_CM$predicted_BIMI,
    label = "predicted: agentic AM vs agentic CM"
  ),
  between_ttest(
    x     = communal_AM_SJT_CM$actual_BIMI,
    y     = communal_CM_SJT_CM$actual_BIMI,
    label = "actual: communal AM vs communal CM"
  ),
  between_ttest(
    x     = communal_AM_SJT_CM$predicted_BIMI,
    y     = communal_CM_SJT_CM$predicted_BIMI,
    label = "predicted: communal AM vs communal CM"
  )
)
ttest_results_SJT_CM

# ANOVA ----

# SJTS - all
ANOVA_SJT_all <- bind_rows(
  agentic_AM_SJT_all,
  agentic_CM_SJT_all,
  communal_AM_SJT_all,
  communal_CM_SJT_all
)

anova_predicted_SJT_all <- aov(
  predicted_BIMI ~ classification * dimension, data = ANOVA_SJT_all)
anova_actual_SJT_all    <- aov(
  actual_BIMI    ~ classification * dimension, data = ANOVA_SJT_all)

summary(anova_predicted_SJT_all)
summary(anova_actual_SJT_all)

# plot
plot_interaction(
  ANOVA_SJT_all, "predicted_BIMI", "Predicted BIMI: Classification x Dimension (SJT_all)")
plot_interaction(
  ANOVA_SJT_all, "actual_BIMI",    "Actual BIMI: Classification x Dimension (SJT_all)")

# SJTS - AM
ANOVA_SJT_AM <- bind_rows(
  agentic_AM_SJT_AM,
  agentic_CM_SJT_AM,
  communal_AM_SJT_AM,
  communal_CM_SJT_AM
)

anova_predicted_SJT_AM <- aov(
  predicted_BIMI ~ classification * dimension, data = ANOVA_SJT_AM)
anova_actual_SJT_AM    <- aov(
  actual_BIMI    ~ classification * dimension, data = ANOVA_SJT_AM)

summary(anova_predicted_SJT_AM)
summary(anova_actual_SJT_AM)

# plot
plot_interaction(
  ANOVA_SJT_AM, "predicted_BIMI", "Predicted BIMI: Classification x Dimension (SJT_AM)")
plot_interaction(
  ANOVA_SJT_AM, "actual_BIMI",    "Actual BIMI: Classification x Dimension (SJT_AM)")

# SJTS - CM
ANOVA_SJT_CM <- bind_rows(
  agentic_AM_SJT_CM,
  agentic_CM_SJT_CM,
  communal_AM_SJT_CM,
  communal_CM_SJT_CM
)

anova_predicted_SJT_CM <- aov(
  predicted_BIMI ~ classification * dimension, data = ANOVA_SJT_CM)
anova_actual_SJT_CM    <- aov(
  actual_BIMI    ~ classification * dimension, data = ANOVA_SJT_CM)

summary(anova_predicted_SJT_CM)
summary(anova_actual_SJT_CM)

# plot
plot_interaction(
  ANOVA_SJT_CM, "predicted_BIMI", "Predicted BIMI: Classification x Dimension (SJT_CM)")
plot_interaction(
  ANOVA_SJT_CM, "actual_BIMI",    "Actual BIMI: Classification x Dimension (SJT_CM)")
