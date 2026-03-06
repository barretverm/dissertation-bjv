# paired sample t-test
paired_ttest <- function(x, y, label = NULL) {
  
  t_res <- t.test(x, y, paired = TRUE)
  d_res <- effectsize::cohens_d(x, y, paired = TRUE)
  
  data.frame(
    test = label,
    mean_x = round(mean(x, na.rm = TRUE), 2),
    mean_y = round(mean(y, na.rm = TRUE), 2),
    mean_diff = round(mean(x - y, na.rm = TRUE), 2),
    t = round(unname(t_res$statistic), 2),
    df = unname(t_res$parameter),
    p = t_res$p.value,
    CI_low = round(t_res$conf.int[1], 2),
    CI_high = round(t_res$conf.int[2], 2),
    cohens_d = round(d_res$Cohens_d, 2)
  )
}

# independent
between_ttest <- function(x, y, label = NULL) {
  
  t_res <- t.test(x, y, paired = FALSE)
  d_res <- effectsize::cohens_d(x, y, paired = FALSE)
  
  data.frame(
    test = label,
    mean_x = round(mean(x, na.rm = TRUE), 2),
    mean_y = round(mean(y, na.rm = TRUE), 2),
    mean_diff = round(mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE), 2),
    t = round(unname(t_res$statistic), 2),
    df = round(unname(t_res$parameter), 2),
    p = t_res$p.value,
    CI_low = round(t_res$conf.int[1], 2),
    CI_high = round(t_res$conf.int[2], 2),
    cohens_d = round(d_res$Cohens_d, 2)
  )
}