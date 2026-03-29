#############################################################################~
# VALIDATION STUDY
# - Evaluate discriminant validity of main study data
#############################################################################~

library(dplyr)
library(readr)
library(rstatix)
library(ggplot2)
library(welchADF)
library(effectsize)
library(patchwork)
library(emmeans)
library(tidyr)
source("R/03_discriminant_validity_utilities.R")

# IMPORT DATA -------------------------------------------------------------

agentic_AM_path <- "outputs/deberta-v3-small/agentic_AM"
agentic_CM_path <- "outputs/deberta-v3-small/agentic_CM"
communal_AM_path <- "outputs/deberta-v3-small/communal_AM"
communal_CM_path <- "outputs/deberta-v3-small/communal_CM"

out_path <- "data/processed/descriptive_data.rds"

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

# ASSUMPTION CHECKS -------------------------------------------------------

# normality
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

# stack plots for export
p1 <- plot_variance_hist(agentic_AM_SJT_all,  agentic_CM_SJT_all,  
                        "AM Subscale", "CM Subscale",
                        title = "Agentic Job Types"
)
p2 <- plot_variance_hist(communal_AM_SJT_all, communal_CM_SJT_all, 
                        "AM Subscale", "CM Subscale",
                        title = "Communal Job Types"
)

(p1 / p2)

p1 <- plot_variance_box(agentic_AM_SJT_all,  agentic_CM_SJT_all,  
                        "AM Subscale", "CM Subscale",
                        title = "Agentic Job Types"
)
p2 <- plot_variance_box(communal_AM_SJT_all, communal_CM_SJT_all, 
                        "AM Subscale", "CM Subscale",
                        title = "Communal Job Types"
)

(p1 / p2) & theme(legend.position = "none")

# REGULAR FACTORIAL ANOVA -------------------------------------------------

ANOVA_SJT_all <- bind_rows(agentic_AM_SJT_all, agentic_CM_SJT_all, 
                           communal_AM_SJT_all, communal_CM_SJT_all)
ANOVA_SJT_AM  <- bind_rows(agentic_AM_SJT_AM,  agentic_CM_SJT_AM,  
                           communal_AM_SJT_AM,  communal_CM_SJT_AM)
ANOVA_SJT_CM  <- bind_rows(agentic_AM_SJT_CM,  agentic_CM_SJT_CM,  
                           communal_AM_SJT_CM,  communal_CM_SJT_CM)
## SJTS - actual ----

run_anova(ANOVA_SJT_all, "actual_BIMI")      # actual scores
run_anova(ANOVA_SJT_all, "predicted_BIMI")   # SJTs predicted scores - all
run_anova(ANOVA_SJT_AM,  "predicted_BIMI")   # SJTs predicted scores - AM
run_anova(ANOVA_SJT_CM,  "predicted_BIMI")   # SJTs predicted scores - CM

# INTERACTION PLOTS -------------------------------------------------------

# actual - only need a single plot since the values are the same across SJT type
plot_interaction(
  ANOVA_SJT_all, "actual_BIMI", "Actual BIMI Scores: Job Type x Dimension")

# predicted
p1 <- plot_interaction(
  ANOVA_SJT_all, "predicted_BIMI", "All SJTs Concatenated")
p2 <- plot_interaction(
  ANOVA_SJT_AM, "predicted_BIMI", "Agentic SJTs")
p3 <- plot_interaction(
  ANOVA_SJT_CM, "predicted_BIMI", "Communal SJTs")

# combine side-by-side
(p1 | p2 | p3 ) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Predicted BIMI: Job Type × Dimension",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    )
  ) &
  theme(legend.position = "bottom") &
  labs(color = "Job Type")

# EXPORT DATA FOR DESCRIPTIVES --------------------------------------------
# i will export one of the SJT ANOVA dataframes and match respondents by ID

saveRDS(ANOVA_SJT_all, out_path)

