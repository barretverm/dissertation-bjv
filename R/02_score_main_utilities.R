score_main_condition <- function(dat, prefix) {
  tibble(
    AM_mean    = rowMeans(dplyr::select(dat, dplyr::starts_with(paste0(prefix, "_BIMI_A"))), na.rm = T),
    CM_mean    = rowMeans(dplyr::select(dat, dplyr::starts_with(paste0(prefix, "_BIMI_C"))), na.rm = T),
    grand_mean = rowMeans(dplyr::select(dat, dplyr::starts_with(paste0(prefix, "_BIMI_"))), na.rm = T)
  )
}