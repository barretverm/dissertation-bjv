#############################################################################~
# Scale scoring utilities                                                     
# - Filter respondents by condition via attention checks                      
# - Reverse code flagged items                                                
# - Mean score agentic, communal, and grand composites per condition          
# - Combine into long format with one row per respondent per condition        
#############################################################################~

score_pilot_condition <- function(dat, prefix) {
  tibble::tibble(
    cont_AM_mean    = rowMeans(dplyr::select(dat, dplyr::starts_with("cont_AM")),                na.rm = T),
    cont_CM_mean    = rowMeans(dplyr::select(dat, dplyr::starts_with("cont_CM")),                na.rm = T),
    cont_grand_mean = rowMeans(dplyr::select(dat, dplyr::starts_with("cont_")),                  na.rm = T),
    exp_AM_mean     = rowMeans(dplyr::select(dat, dplyr::starts_with(paste0(prefix, "-AM_"))),   na.rm = T),
    exp_CM_mean     = rowMeans(dplyr::select(dat, dplyr::starts_with(paste0(prefix, "-CM_"))),   na.rm = T),
    exp_grand_mean  = rowMeans(dplyr::select(dat, dplyr::starts_with(paste0(prefix, "-"))),      na.rm = T)
  )
}