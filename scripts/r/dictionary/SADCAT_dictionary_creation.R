#############################################################################~
# DICTIONARY CREATION
#############################################################################~

library(readr)
library(tidyverse)
library(psych)

# IMPORT DICTIONARY RATINGS -----------------------------------------------
AIM_ratings <- read_csv('data/processed/AIM_ratings.csv', 
                        col_names = c("word", "rater1", "rater2", "rater3"), 
                        skip = 1) |> na.omit()
CIM_ratings <- read_csv('data/processed/CIM_ratings.csv',
                        col_names = c("word", "rater1", "rater2", "rater3"), 
                        skip = 1) |> na.omit()
IM_ratings <- read_csv('data/processed/IM_ratings.csv',
                       col_names = c("word", "rater1", "rater2", "rater3"), 
                       skip = 1) |> na.omit()

word_ratings <- list(AIM_ratings, CIM_ratings, IM_ratings)
construct_names <- c("AIM", "CIM", "IM")
names(word_ratings) <- construct_names


# RATER BIAS DIAGNOSTICS --------------------------------------------------

for (nm in construct_names) {
  df <- word_ratings[[nm]]
  cat(sprintf("Construct: %s\n", nm))
  cat(sprintf("  rater1 — M = %.2f, SD = %.2f, range = %d-%d\n",
              mean(df$rater1), sd(df$rater1), min(df$rater1), max(df$rater1)))
  cat(sprintf("  rater2 — M = %.2f, SD = %.2f, range = %d-%d\n",
              mean(df$rater2), sd(df$rater2), min(df$rater2), max(df$rater2)))
  cat(sprintf("  rater3 — M = %.2f, SD = %.2f, range = %d-%d\n\n",
              mean(df$rater3), sd(df$rater3), min(df$rater3), max(df$rater3)))
}

# ICC: 3-RATER & TWO RATER ------------------------------------------------

for (nm in construct_names) {
  df <- word_ratings[[nm]]
  
  # 3-rater: two-way mixed, absolute agreement, average measures
  icc_3 <- psych::ICC(df[, c("rater1", "rater2", "rater3")])
  icc_3_val <- icc_3$results %>% filter(type == "ICC3k") %>% pull(ICC)
  
  # 2-rater: rater1 & rater3 only (valid raters)
  icc_2 <- psych::ICC(df[, c("rater1", "rater3")])
  icc_2_val <- icc_2$results %>% filter(type == "ICC3k") %>% pull(ICC)
  
  cat(sprintf("Construct: %s\n", nm))
  cat(sprintf("  3-rater ICC3k (incl. biased rater2):    %.3f\n", icc_3_val))
  cat(sprintf("  2-rater ICC3k (rater1 + rater3 only):   %.3f\n\n", icc_2_val))
}

# DISTRIBUTION OF RATER 2 AND RATER 3 MEANS -------------------------------

dist_data <- map(construct_names, function(nm) {
  df <- word_ratings[[nm]] %>%
    select(word, rater1, rater3) %>%
    mutate(mean_r13 = (rater1 + rater3) / 2)
  
  cat(sprintf("Construct: %s\n", nm))
  cat("  Quantiles of mean rating:\n")
  print(quantile(df$mean_r13, probs = c(0, .25, .50, .75, 1)))
  cat(sprintf("  Retained at mean >= 2.5:        %d / 530\n", sum(df$mean_r13 >= 2.5)))
  cat(sprintf("  Retained at mean >= 3.0:        %d / 530\n", sum(df$mean_r13 >= 3.0)))
  cat(sprintf("  Retained at mean >= 3.5:        %d / 530\n", sum(df$mean_r13 >= 3.5)))
  cat(sprintf("  Retained at both raters >= 3:   %d / 530\n",
              sum(df$rater1 >= 3 & df$rater3 >= 3)))
  cat(sprintf("  Retained at both raters >= 4:   %d / 530\n\n",
              sum(df$rater1 >= 4 & df$rater3 >= 4)))
  
  df %>% mutate(construct = nm)
}) %>% bind_rows()

# HISTOGRAM ---------------------------------------------------------------

for (nm in construct_names) {
  df <- word_ratings[[nm]] %>%
    select(word, rater1, rater3) %>%
    mutate(mean_r13 = (rater1 + rater3) / 2) %>%
    filter(rater1 >= 3 & rater3 >= 3) %>%
    arrange(desc(mean_r13))
  
  cat(sprintf("\n=== %s retained words (both >= 3), n = %d ===\n", nm, nrow(df)))
  print(df, n = Inf)
}
