#############################################################################>
# ANALYZE PILOT DATA                                                        
#############################################################################>

library(dplyr)
library(effectsize)
library(tidyr)
library(ggplot2)

in_path  <- "data/processed/pilot_scored.rds"

df <- readRDS(in_path)

# T-TESTS -----------------------------------------------------------------

AMJ1 <- df |> filter(job_type == "AMJ1")
AMJ2 <- df |> filter(job_type == "AMJ2")
CMJ1 <- df |> filter(job_type == "CMJ1")
CMJ2 <- df |> filter(job_type == "CMJ2")
AMJ1_AMJ2 <- bind_rows(AMJ1, AMJ2)
CMJ1_CMJ2 <- bind_rows(CMJ1, CMJ2)

## AMJ1 ----
paired_ttest(AMJ1$exp_grand_mean, 
            AMJ1$cont_grand_mean, 
            "grand mean"
            )

paired_ttest(AMJ1$exp_AM_mean, 
            AMJ1$cont_AM_mean, 
            "cont -> AM"
            )

paired_ttest(AMJ1$exp_CM_mean, 
            AMJ1$cont_CM_mean, 
            "cont -> CM"
            )

paired_ttest(AMJ1$cont_AM_mean,       
             AMJ1$cont_CM_mean,
             "contAM -> contCM"
             )

## AMJ2 ----
paired_ttest(AMJ2$exp_grand_mean, 
             AMJ2$cont_grand_mean, 
             "grand mean"
)

paired_ttest(AMJ2$exp_AM_mean, 
             AMJ2$cont_AM_mean, 
             "cont -> AM"
)

paired_ttest(AMJ2$exp_CM_mean, 
             AMJ2$cont_CM_mean, 
             "cont -> CM"
)

paired_ttest(AMJ2$cont_AM_mean,       
             AMJ2$cont_CM_mean,
             "contAM -> contCM"
)

## CMJ1 ----
paired_ttest(CMJ1$exp_grand_mean, 
             CMJ1$cont_grand_mean, 
             "grand mean"
)

paired_ttest(CMJ1$exp_AM_mean, 
             CMJ1$cont_AM_mean, 
             "cont -> AM"
)

paired_ttest(CMJ1$exp_CM_mean, 
             CMJ1$cont_CM_mean, 
             "cont -> CM"
)

paired_ttest(CMJ1$cont_AM_mean,       
             CMJ1$cont_CM_mean,
             "contAM -> contCM"
)

## CMJ2 ----
paired_ttest(CMJ2$exp_grand_mean, 
             CMJ2$cont_grand_mean, 
             "grand mean"
)

paired_ttest(CMJ2$exp_AM_mean, 
             CMJ2$cont_AM_mean, 
             "cont -> AM"
)

paired_ttest(CMJ2$exp_CM_mean, 
             CMJ2$cont_CM_mean, 
             "cont -> CM"
)

paired_ttest(CMJ2$cont_AM_mean,       
             CMJ2$cont_CM_mean,
             "contAM -> contCM"
)

## CMJ1 + CMJ2 ----
paired_ttest(CMJ1_CMJ2$exp_grand_mean, 
             CMJ1_CMJ2$cont_grand_mean, 
             "grand mean"
)

paired_ttest(CMJ1_CMJ2$exp_AM_mean, 
             CMJ1_CMJ2$cont_AM_mean, 
             "cont -> AM"
)

paired_ttest(CMJ1_CMJ2$exp_CM_mean, 
             CMJ1_CMJ2$cont_CM_mean, 
             "cont -> CM"
)

paired_ttest(CMJ1_CMJ2$cont_AM_mean,       
             CMJ1_CMJ2$cont_CM_mean,
             "contAM -> contCM"
)

## agentic vs communal controls ----
between_ttest(CMJ1_CMJ2$cont_grand_mean, 
              AMJ1_AMJ2$cont_grand_mean, 
              "CM <-> AM")

# MIXED ANOVA -------------------------------------------------------------
# looking for interaction effect 

df <- df |> mutate(participant = 1:n())
df <- df[,c("participant", "job_type", 
            "exp_AM_mean", "exp_CM_mean",
            "cont_AM_mean", "cont_CM_mean")]

# create job type factor
df <- df |>
  mutate(
      classification = case_when(
      job_type %in% c("AMJ1", "AMJ2") ~ "Agentic Job",
      job_type %in% c("CMJ1", "CMJ2") ~ "Communal Job"
    )
  )

# convert to long format
df_long <- df |>
  pivot_longer(
    cols = c(exp_AM_mean, exp_CM_mean, cont_AM_mean, cont_CM_mean),
    names_to = c("source", "dimension"),
    names_pattern = "(exp|cont)_(AM|CM)_mean",
    values_to = "score"
  )

# convert to factors
df_long <- df_long |>
  mutate(
    classification = factor(classification),
    dimension = factor(dimension),
    participant = factor(participant)
  )

# run mixed anova
mixed_mod <- aov(
  score ~ classification * dimension +
    Error(participant/dimension),
  data = df_long
)
summary(mixed_mod)

# plot
ggplot(df_long,
       aes(dimension, score,
           color = classification,
           group = classification)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point") +
  scale_y_continuous(limits = c(1, 5)) +
  labs(y = "Mean Rating")