#############################################################################>
# Helper functions for discriminant validity evidence (hypothesis 2)
#############################################################################>

# test normality for a single dataframe
test_normality <- function(df, name) {
  tibble(
    dataset   = name,
    variable  = c("predicted_BIMI", "actual_BIMI"),
    statistic = c(shapiro.test(df$predicted_BIMI)$statistic,
                  shapiro.test(df$actual_BIMI)$statistic),
    p_value   = c(shapiro.test(df$predicted_BIMI)$p.value,
                  shapiro.test(df$actual_BIMI)$p.value)
  )
}

# qq plots
plot_qq <- function(df, name) {
  df |>
    select(predicted_BIMI, actual_BIMI) |>
    tidyr::pivot_longer(everything(), names_to = "variable", values_to = "value") |>
    ggplot(aes(sample = value)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    facet_wrap(~variable) +
    labs(title = name) +
    theme_minimal()
}

# run Levene's test for a pair of dataframes
run_levene <- function(df1, df2, label) {
  combined <- bind_rows(
    df1 |> mutate(condition = "group1"),
    df2 |> mutate(condition = "group2")
  )
  bind_rows(
    combined |> levene_test(predicted_BIMI ~ condition) |> mutate(test = paste0("predicted_BIMI: ", label)),
    combined |> levene_test(actual_BIMI ~ condition)    |> mutate(test = paste0("actual_BIMI: ",    label))
  )
}

# ANOVA/interaction plots - compute means and SEs for plotting
plot_interaction <- function(df, dv, title) {
  df |>
    group_by(classification, dimension) |>
    summarise(
      mean  = mean(.data[[dv]], na.rm = TRUE),
      se    = sd(.data[[dv]], na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) |>
    ggplot(aes(x = dimension, y = mean, color = classification, group = classification)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
    scale_y_continuous(limits = c(1, 7)) +
    labs(
      title = title,
      x     = "Dimension",
      y     = "Mean BIMI Score",
      color = "Classification"
    ) +
    theme_minimal()
}
