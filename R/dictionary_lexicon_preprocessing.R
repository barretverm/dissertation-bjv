#############################################################################~
# Lexicon preprocessing utilities                                             
# - Normalize terms                                                           
# - Flag multiword phrases                                                    
# - Separate single words from phrases                                        
#############################################################################~

# preprocess, normalize, remove duplicates
prep_lexicon <- function(x) {
  tibble::tibble(raw_term = x) %>%
    dplyr::mutate(
      raw_term = stringr::str_squish(raw_term),
      raw_term = stringr::str_to_lower(raw_term),
      
      # normalize common separators inside terms:
      # convert hyphens to spaces
      norm = stringr::str_replace_all(raw_term, "-", " "),
      norm = stringr::str_squish(norm),
      
      # classify: phrase if it has 2+ "word" tokens
      n_tokens = stringr::str_count(norm, "\\S+") ,
      is_phrase = n_tokens >= 2
    ) %>%
    dplyr::distinct(norm, .keep_all = T) %>%   # de-dupe by normalized form
    dplyr::select(raw_term, norm, n_tokens, is_phrase)
}

# wrapper function to separate single and multiword phrases
separate_words_phrases <- function(x) {
  lex <- prep_lexicon(x)
  
  single_words <- lex %>% 
    dplyr::filter(!is_phrase) %>% 
    dplyr::transmute(term = norm)
  
  phrases <- lex %>% 
    dplyr::filter(is_phrase) %>% 
    dplyr::transmute(phrase = norm)
  
  list(
    lexicon = lex,
    single_words = single_words,
    phrases = phrases
  )
}