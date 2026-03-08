#############################################################################~
# ANALYZE MAIN DATA
#############################################################################~

library(dplyr)
library(effectsize)
library(tidyr)
library(ggplot2)
source("R/03_analyze_pilot_utilities.R")

in_path <- "data/processed/pilot_scored.rds"

df <- readRDS(in_path)

# FILTER BY CONDITION ------------------------------------------------------

AMJ1 <- df |> filter(job_type == "AMJ1")
AMJ2 <- df |> filter(job_type == "AMJ2")
CMJ1 <- df |> filter(job_type == "CMJ1")
CMJ2 <- df |> filter(job_type == "CMJ2")
AMJ1_AMJ2 <- bind_rows(AMJ1, AMJ2)
CMJ1_CMJ2 <- bind_rows(CMJ1, CMJ2)

# T-TESTS -----------------------------------------------------------------

results <- bind_rows(
  # AMJ1
  paired_ttest(AMJ1$exp_grand_mean, AMJ1$cont_grand_mean, "grand mean")      
  |> mutate(group = "AMJ1"),
  paired_ttest(AMJ1$exp_AM_mean,    AMJ1$cont_AM_mean,    "cont -> AM")      
  |> mutate(group = "AMJ1"),
  paired_ttest(AMJ1$exp_CM_mean,    AMJ1$cont_CM_mean,    "cont -> CM")      
  |> mutate(group = "AMJ1"),
  paired_ttest(AMJ1$cont_AM_mean,   AMJ1$cont_CM_mean,    "contAM -> contCM")
  |> mutate(group = "AMJ1"),
  # AMJ2
  paired_ttest(AMJ2$exp_grand_mean, AMJ2$cont_grand_mean, "grand mean")      
  |> mutate(group = "AMJ2"),
  paired_ttest(AMJ2$exp_AM_mean,    AMJ2$cont_AM_mean,    "cont -> AM")      
  |> mutate(group = "AMJ2"),
  paired_ttest(AMJ2$exp_CM_mean,    AMJ2$cont_CM_mean,    "cont -> CM")      
  |> mutate(group = "AMJ2"),
  paired_ttest(AMJ2$cont_AM_mean,   AMJ2$cont_CM_mean,    "contAM -> contCM")
  |> mutate(group = "AMJ2"),
  # CMJ1
  paired_ttest(CMJ1$exp_grand_mean, CMJ1$cont_grand_mean, "grand mean")      
  |> mutate(group = "CMJ1"),
  paired_ttest(CMJ1$exp_AM_mean,    CMJ1$cont_AM_mean,    "cont -> AM")      
  |> mutate(group = "CMJ1"),
  paired_ttest(CMJ1$exp_CM_mean,    CMJ1$cont_CM_mean,    "cont -> CM")      
  |> mutate(group = "CMJ1"),
  paired_ttest(CMJ1$cont_AM_mean,   CMJ1$cont_CM_mean,    "contAM -> contCM")
  |> mutate(group = "CMJ1"),
  # CMJ2
  paired_ttest(CMJ2$exp_grand_mean, CMJ2$cont_grand_mean, "grand mean")      
  |> mutate(group = "CMJ2"),
  paired_ttest(CMJ2$exp_AM_mean,    CMJ2$cont_AM_mean,    "cont -> AM")      
  |> mutate(group = "CMJ2"),
  paired_ttest(CMJ2$exp_CM_mean,    CMJ2$cont_CM_mean,    "cont -> CM")      
  |> mutate(group = "CMJ2"),
  paired_ttest(CMJ2$cont_AM_mean,   CMJ2$cont_CM_mean,    "contAM -> contCM")
  |> mutate(group = "CMJ2"),
  # CMJ1 + CMJ2 combined
  paired_ttest(CMJ1_CMJ2$exp_grand_mean, CMJ1_CMJ2$cont_grand_mean, "grand mean")      
  |> mutate(group = "CMJ_combined"),
  paired_ttest(CMJ1_CMJ2$exp_AM_mean,    CMJ1_CMJ2$cont_AM_mean,    "cont -> AM")      
  |> mutate(group = "CMJ_combined"),
  paired_ttest(CMJ1_CMJ2$exp_CM_mean,    CMJ1_CMJ2$cont_CM_mean,    "cont -> CM")      
  |> mutate(group = "CMJ_combined"),
  paired_ttest(CMJ1_CMJ2$cont_AM_mean,   CMJ1_CMJ2$cont_CM_mean,    "contAM -> contCM")
  |> mutate(group = "CMJ_combined"),
  
  # between: agentic vs communal controls
  between_ttest(CMJ1_CMJ2$cont_grand_mean, AMJ1_AMJ2$cont_grand_mean, "CM <-> AM") 
  |> mutate(group = "between")
) |>
  relocate(group, .before = test) |>
  mutate(sig = case_when(
    p < .001 ~ "***",
    p < .01  ~ "**",
    p < .05  ~ "*",
    T        ~ ""
  ),
  p = round(p, 3)
)

# MIXED ANOVA -------------------------------------------------------------

df_anova <- df |>
  mutate(participant = 1:n()) |>
  select(participant, job_type, exp_AM_mean, exp_CM_mean, cont_AM_mean, cont_CM_mean) |>
  mutate(
    classification = case_when(
      job_type %in% c("AMJ1", "AMJ2") ~ "Agentic Job",
      job_type %in% c("CMJ1", "CMJ2") ~ "Communal Job"
    )
  )

df_long <- df_anova |>
  pivot_longer(
    cols = c(exp_AM_mean, exp_CM_mean, cont_AM_mean, cont_CM_mean),
    names_to = c("source", "dimension"),
    names_pattern = "(exp|cont)_(AM|CM)_mean",
    values_to = "score"
  ) |>
  mutate(
    classification = factor(classification),
    dimension      = factor(dimension),
    source         = factor(source),
    participant    = factor(participant)
  )

mixed_mod <- aov(
  score ~ classification * dimension * source +
    Error(participant / (dimension * source)),
  data = df_long
)
summary(mixed_mod)

# PLOT --------------------------------------------------------------------

ggplot(df_long,
       aes(dimension, score,
           color = classification,
           group = interaction(classification, source),
           linetype = source)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point") +
  scale_y_continuous(limits = c(1, 5)) +
  scale_linetype_manual(values = c(exp = "solid", cont = "dashed"),
                        labels = c(exp = "Experimental", cont = "Control")) +
  labs(y = "Mean Rating", x = "Dimension", 
       color = "Job Type", linetype = "Source")