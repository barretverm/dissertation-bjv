#############################################################################~
# DICTIONARY CREATION - SADCAT
#############################################################################~

library(wordnet)
library(SADCAT)
library(tidyverse)
Sys.setenv(WNHOME = "WordNet-3.0")
setDict("WordNet-3.0/dict")

# IMPORT WORD LISTS WITH SENSES -------------------------------------------

AIM <- read_csv('data/processed/AIM_final_senses.csv')
CIM <- read_csv('data/processed/CIM_final_senses.csv')
IM  <- read_csv('data/processed/IM_final_senses.csv')

# make sure column names are exactly: term, PoS, sense
# and strings are not factors
aim_input <- AIM %>% mutate(across(everything(), as.character)) %>%
  mutate(sense = as.numeric(sense))
cim_input <- CIM %>% mutate(across(everything(), as.character)) %>%
  mutate(sense = as.numeric(sense))
im_input  <- IM  %>% mutate(across(everything(), as.character)) %>%
  mutate(sense = as.numeric(sense))

# some words were not in WordNet, so I denoted with "999" - separating for now
# and recombining later

im_input <- IM %>% filter(PoS != 999)
excluded_im <- IM %>% filter(PoS == 999)

# EXPAND VIA WORDNET ------------------------------------------------------

aim_expanded <- SADCAT::Full_Expand(datax = aim_input, antonym = F, syns = F)
cim_expanded <- SADCAT::Full_Expand(datax = cim_input, antonym = F, syns = F)
im_expanded  <- SADCAT::Full_Expand(datax = im_input,  antonym = F, syns = F)

# append "999" cases
im_expanded <- c(im_expanded, excluded_im$term)

# COMBINE DICTIONARIES AND EXPORT -----------------------------------------

dictionaries <- list(
  aim_expanded,
  cim_expanded,
  im_expanded
) 

# for R
saveRDS(dictionaries, "data/dictionaries/dictionaries.RDS")

# for export to colab
write.csv(data.frame(word = dictionaries[[1]]), 
          "data/dictionaries/AIM_dictionary.csv", row.names = F)
write.csv(data.frame(word = dictionaries[[2]]), 
          "data/dictionaries/CIM_dictionary.csv", row.names = F)
write.csv(data.frame(word = dictionaries[[3]]), 
          "data/dictionaries/IM_dictionary.csv",  row.names = F)
