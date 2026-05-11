source("R/ttest_utilities.R")

# T-TESTS -----------------------------------------------------------------

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

# WELCH'S FACTORIAL ANOVA -------------------------------------------------

## SJTS - all ----
ANOVA_SJT_all <- bind_rows(
  agentic_AM_SJT_all,
  agentic_CM_SJT_all,
  communal_AM_SJT_all,
  communal_CM_SJT_all
)

summary(welchADF.test(
  actual_BIMI ~ classification * dimension, 
  data = ANOVA_SJT_all)
)

summary(welchADF.test(
  predicted_BIMI ~ classification * dimension, 
  data = ANOVA_SJT_all)
)

# calculate effect sizes
agentic_all  <- ANOVA_SJT_all %>% filter(dimension == "AM")
communal_all <- ANOVA_SJT_all %>% filter(dimension == "CM")

hedges_g(actual_BIMI    ~ classification, data = agentic_all)
hedges_g(actual_BIMI    ~ classification, data = communal_all)

hedges_g(predicted_BIMI ~ classification, data = agentic_all)
hedges_g(predicted_BIMI ~ classification, data = communal_all)

## SJTS - AM ----
ANOVA_SJT_AM <- bind_rows(
  agentic_AM_SJT_AM,
  agentic_CM_SJT_AM,
  communal_AM_SJT_AM,
  communal_CM_SJT_AM
)

summary(welchADF.test(
  predicted_BIMI ~ classification * dimension, 
  data = ANOVA_SJT_AM)
)

# calculate effect sizes
agentic_AM  <- ANOVA_SJT_AM %>% filter(dimension == "AM")
communal_AM <- ANOVA_SJT_AM %>% filter(dimension == "CM")

hedges_g(predicted_BIMI ~ classification, data = agentic_AM)
hedges_g(predicted_BIMI ~ classification, data = communal_AM)

## SJTS - CM ----
ANOVA_SJT_CM <- bind_rows(
  agentic_AM_SJT_CM,
  agentic_CM_SJT_CM,
  communal_AM_SJT_CM,
  communal_CM_SJT_CM
)

summary(welchADF.test(
  predicted_BIMI ~ classification * dimension, 
  data = ANOVA_SJT_CM)
)

# calculate effect size
agentic_CM  <- ANOVA_SJT_CM %>% filter(dimension == "AM")
communal_CM <- ANOVA_SJT_CM %>% filter(dimension == "CM")

hedges_g(predicted_BIMI ~ classification, data = agentic_CM)
hedges_g(predicted_BIMI ~ classification, data = communal_CM)

# OUTLIERS ----------------------------------------------------------------

# SJTs - all
ANOVA_SJT_all |>
  group_by(classification, dimension) |>
  identify_outliers(actual_BIMI)

ANOVA_SJT_all |>
  group_by(classification, dimension) |>
  identify_outliers(predicted_BIMI)

# SJTs - AM
ANOVA_SJT_AM |>
  group_by(classification, dimension) |>
  identify_outliers(actual_BIMI)

ANOVA_SJT_AM |>
  group_by(classification, dimension) |>
  identify_outliers(predicted_BIMI)

# SJTs - CM
ANOVA_SJT_CM |>
  group_by(classification, dimension) |>
  identify_outliers(actual_BIMI)

ANOVA_SJT_CM |>
  group_by(classification, dimension) |>
  identify_outliers(predicted_BIMI)


# identify the threshold visually - looks like below ~0.5 from the plot
communal_CM_outliers <- ANOVA_SJT_CM |>
  filter(dimension == "CM", predicted_BIMI < 0.5)

# how many are there?
nrow(communal_CM_outliers)

# which folds are they in?
communal_CM_outliers |>
  count(fold)

# are they all from one classification group?
communal_CM_outliers |>
  count(classification)

# look at the raw rows
communal_CM_outliers |>
  select(ResponseId, classification, dimension, predicted_BIMI, actual_BIMI, fold) |>
  arrange(predicted_BIMI)


# probe variance further - histograms
plot_variance_hist(agentic_AM_SJT_CM,  agentic_CM_SJT_CM,  
                   "AM Subscale", "CM Subscale", 
                   title = "Agentic Job Types"
)
plot_variance_hist(communal_AM_SJT_CM, communal_CM_SJT_CM, 
                   "AM Subscale", "CM Subscale",
                   title = "Communal Job Types"
)


# stack plots for export
p1 <- plot_variance_hist(agentic_AM_SJT_CM,  agentic_CM_SJT_CM,  
                         "AM Subscale", "CM Subscale",
                         title = "Agentic Job Types"
)
p2 <- plot_variance_hist(communal_AM_SJT_CM, communal_CM_SJT_CM, 
                         "AM Subscale", "CM Subscale",
                         title = "Communal Job Types"
)

(p1 / p2)

# stack plots for export
p1 <- plot_variance_hist(agentic_AM_SJT_AM,  agentic_CM_SJT_AM,  
                         "AM Subscale", "CM Subscale",
                         title = "Agentic Job Types"
)
p2 <- plot_variance_hist(communal_AM_SJT_AM, communal_CM_SJT_AM, 
                         "AM Subscale", "CM Subscale",
                         title = "Communal Job Types"
)

(p1 / p2)

