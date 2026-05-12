#############################################################################~
# DICTIONARY CREATION - RELIABILITY ESTIMATES
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

# REMOVE ADJUDICATED WORDS ------------------------------------------------

aim_remove <- c("adapt", "apex", "assure", "certain", "consult", 
                "definitely", "exceedingly", "highly", "pledge", "promise")

cim_remove <- c("acknowledge", "balance", "constructive", "devote",
                "embrace", "equalize", "facilitate", "inspire", "talk",
                "willingness") 

im_remove <- c("complete", "diagnose", "distinctly", "enact", "enforce",
               "experience", "impartial", "improve", "insis", "investigate",
               "maintain", "patience", "patient", "perseverant", "persistent",
               "pivitol", "positively", "praise", "prioritize", "progress",
               "prudent", "push", "recognize", "remedy", "respectful", 
               "safeguard", "total", "uplift", "utilize", "utterly") 

aim_final <- word_ratings[["AIM"]] %>%
  select(word, rater1, rater3) %>%
  mutate(mean_r13 = (rater1 + rater3) / 2) %>%
  filter(rater1 >= 3 & rater3 >= 3) %>%
  filter(!word %in% aim_remove) %>%
  arrange(desc(mean_r13))

cim_final <- word_ratings[["CIM"]] %>%
  select(word, rater1, rater3) %>%
  mutate(mean_r13 = (rater1 + rater3) / 2) %>%
  filter(rater1 >= 3 & rater3 >= 3) %>%
  filter(!word %in% cim_remove) %>%
  arrange(desc(mean_r13))

im_final <- word_ratings[["IM"]] %>%
  select(word, rater1, rater3) %>%
  mutate(mean_r13 = (rater1 + rater3) / 2) %>%
  filter(rater1 >= 3 & rater3 >= 3) %>%
  filter(!word %in% im_remove) %>%
  arrange(desc(mean_r13))

# INSPECT FINAL LISTS -----------------------------------------------------

cat(sprintf("\n=== AIM final words, n = %d ===\n", nrow(aim_final)))
print(aim_final, n = Inf)

cat(sprintf("\n=== CIM final words, n = %d ===\n", nrow(cim_final)))
print(cim_final, n = Inf)

cat(sprintf("\n=== IM final words, n = %d ===\n", nrow(im_final)))
print(im_final, n = Inf)

# REMOVE DUPLICATES -------------------------------------------------------
# i'm retaining words that appear on two + lists with the highest ratings

duplicate_assignments <- tribble(
  ~word,           ~keep_in,
  "excel",         "AIM",
  "exemplary",     "IM",
  "meticulous",    "IM",
  "immediately",   "AIM",
  "efficiently",   "AIM",
  "exceed",        "AIM",
  "productive",    "AIM",
  "productivity",  "AIM",
  "professional",  "AIM",
  "organize",      "AIM",
  "proactive",     "AIM",
  "proactively",   "AIM",
  "unsurpassed",   "AIM",
  # "insist",        "AIM",
  "inspire",       "AIM",
  "innovative",    "AIM",
  "influence",     "AIM",
  "initiative",    "AIM",
  "expert",        "AIM",
  "exemplify",     "AIM",
  "pride",         "AIM",
  "honest",        "CIM",
  "genuine",       "CIM",
  "faithful",      "CIM",
  "integrity",     "IM",  
  "accountability","AIM",
  "accountable",   "AIM",
  "diligent",      "AIM",
  "ensure",        "AIM"
)

# apply assignments
aim_final <- aim_final %>%
  filter(!word %in% filter(duplicate_assignments, keep_in != "AIM")$word)

cim_final <- cim_final %>%
  filter(!word %in% filter(duplicate_assignments, keep_in != "CIM")$word)

im_final <- sim_final %>%
  filter(!word %in% filter(duplicate_assignments, keep_in != "IM")$word)

# counts
cat(sprintf("AIM final: %d words\n", nrow(aim_final)))
cat(sprintf("CIM final: %d words\n", nrow(cim_final)))
cat(sprintf("IM final:  %d words\n", nrow(im_final)))

# VERIFY NO DUPLICATES ----------------------------------------------------

all_words <- c(aim_final$word, cim_final$word, im_final$word)
dupes_remaining <- all_words[duplicated(all_words)]
length(dupes_remaining)

# EXPORT ------------------------------------------------------------------

write_csv(aim_final, "data/processed/AIM_final.csv")
write_csv(cim_final, "data/processed/CIM_final.csv")
write_csv(im_final,  "data/processed/IM_final.csv")
