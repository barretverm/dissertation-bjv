# This script takes the initial generated word lists and refines them for raters

library(dplyr)
library(stringr)
library(readr)
library(textstem)

source('R/dictionary_lexicon_preprocessing.R')

# IMPORT DATA -------------------------------------------------------------

df <- read_csv(
  "data/raw/raw_combined_wordlists.csv",
  col_names = T,
  na = ""
)

# PREPROCESS --------------------------------------------------------------
# collapse the three forms of IM into one vector; separate single words vs phrases

words <- c(df$AIM, df$CIM, df$IM) 
words <- words[!is.na(words)]

prepped <- separate_words_phrases(words) 
glimpse(prepped)

# LEMMATIZE ---------------------------------------------------------------
# lemmatize single-word terms only (phrases exported separately)

single_lemmas <- prepped$single_words %>% 
  mutate(
    lemma = textstem::lemmatize_words(term),
    changed = lemma != term
  )
head(single_lemmas)

# counts
single_lemmas %>% count(changed)   
single_lemmas %>% filter(changed) %>% head(30)

# STRUCTURE THE RATING SHEET ----------------------------------------------
# create lemma-level units with examples

rating_units <- single_lemmas %>%
  group_by(lemma) %>%
  summarize(
    n_forms = n_distinct(term),
    examples = paste(head(sort(unique(term)), 6), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(lemma) %>%
  filter(!str_detect(lemma, "^\\d+$"))  # remove pure numeric lemmas (e.g., "0")

head(rating_units)

# export ------------------------------------------------------------------

write_csv(rating_units, "data/processed/rating_lemmas.csv")     # single words
write_csv(prepped$phrases, "data/processed/rating_phrases.csv") # phrases