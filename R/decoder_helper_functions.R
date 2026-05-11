#############################################################################~
# Helper functions for decoder discriminant validity evidence (RQ1)
#############################################################################~

# Load decoder model prediction data

# Character: "gpt4o", "mistral", or "llama"
# Return named list of 12 dataframes (4 conditions × 3 SJT types)
load_decoder_data <- function(model) {
  
  # Model-specific file suffixes
  suffixes <- list(
    gpt4o   = "gpt-4o_v1.0.csv",
    mistral = "mistralai_Mistral-7B-Instruct-v0.3_v1.0.csv",
    llama   = "meta-llama_Meta-Llama-3.1-8B-Instruct_v2.0.csv"
  )
  
  if (!model %in% names(suffixes)) {
    stop("model must be one of: gpt4o, mistral, llama")
  }
  
  suffix    <- suffixes[[model]]
  base_path <- file.path("outputs/decoder_models", model)
  
  # Helper to build filename
  build_path <- function(classification, dimension, sjt_type) {
    prompt <- if (classification == "agentic") "promptAM" else "promptCM"
    filename <- paste0("decoder_", classification, "_", dimension, "_", 
                       sjt_type, "_", prompt, "_", suffix)
    file.path(base_path, paste0(classification, "_", dimension), filename)
  }
  
  # Load all 12 combinations
  list(
    # SJTs_all
    agentic_AM_SJT_all  = read_csv(build_path("agentic",  "AM", "SJTs_all"), show_col_types = F),
    agentic_CM_SJT_all  = read_csv(build_path("agentic",  "CM", "SJTs_all"), show_col_types = F),
    communal_AM_SJT_all = read_csv(build_path("communal", "AM", "SJTs_all"), show_col_types = F),
    communal_CM_SJT_all = read_csv(build_path("communal", "CM", "SJTs_all"), show_col_types = F),
    # SJT_AM
    agentic_AM_SJT_AM   = read_csv(build_path("agentic",  "AM", "SJT_AM"), show_col_types = F),
    agentic_CM_SJT_AM   = read_csv(build_path("agentic",  "CM", "SJT_AM"), show_col_types = F),
    communal_AM_SJT_AM  = read_csv(build_path("communal", "AM", "SJT_AM"), show_col_types = F),
    communal_CM_SJT_AM  = read_csv(build_path("communal", "CM", "SJT_AM"), show_col_types = F),
    # SJT_CM
    agentic_AM_SJT_CM   = read_csv(build_path("agentic",  "AM", "SJT_CM"), show_col_types = F),
    agentic_CM_SJT_CM   = read_csv(build_path("agentic",  "CM", "SJT_CM"), show_col_types = F),
    communal_AM_SJT_CM  = read_csv(build_path("communal", "AM", "SJT_CM"), show_col_types = F),
    communal_CM_SJT_CM  = read_csv(build_path("communal", "CM", "SJT_CM"), show_col_types = F)
  )
}

# Wrapper for all analyses (found in 03_discriminant validity)
#' Run discriminant validity analyses for a decoder model
#'
#' @param data List returned by load_decoder_data()
#' @param model_label Character label for plot titles (e.g., "GPT-4o")
#' @return List containing ANOVA dataframes, ANOVA results, and plots
run_discriminant_analysis <- function(data, model_label) {
  
  # Combine into ANOVA-ready dataframes
  ANOVA_SJT_all <- bind_rows(data$agentic_AM_SJT_all, data$agentic_CM_SJT_all,
                             data$communal_AM_SJT_all, data$communal_CM_SJT_all)
  ANOVA_SJT_AM  <- bind_rows(data$agentic_AM_SJT_AM,  data$agentic_CM_SJT_AM,
                             data$communal_AM_SJT_AM,  data$communal_CM_SJT_AM)
  ANOVA_SJT_CM  <- bind_rows(data$agentic_AM_SJT_CM,  data$agentic_CM_SJT_CM,
                             data$communal_AM_SJT_CM,  data$communal_CM_SJT_CM)
  
  # Run ANOVAs
  cat("\n===", model_label, "===\n")
  
  cat("\n--- Actual BIMI ---\n")
  anova_actual <- run_anova(ANOVA_SJT_all, "actual_BIMI")
  
  cat("\n--- Predicted BIMI: All SJTs ---\n")
  anova_all <- run_anova(ANOVA_SJT_all, "predicted_BIMI")
  
  cat("\n--- Predicted BIMI: Agentic SJTs ---\n")
  anova_AM <- run_anova(ANOVA_SJT_AM, "predicted_BIMI")
  
  cat("\n--- Predicted BIMI: Communal SJTs ---\n")
  anova_CM <- run_anova(ANOVA_SJT_CM, "predicted_BIMI")
  
  # Variance plots (histograms)
  hist_agentic <- plot_variance_hist(
    data$agentic_AM_SJT_all, data$agentic_CM_SJT_all,
    "AM Subscale", "CM Subscale", title = "Agentic Job Types"
  )
  hist_communal <- plot_variance_hist(
    data$communal_AM_SJT_all, data$communal_CM_SJT_all,
    "AM Subscale", "CM Subscale", title = "Communal Job Types"
  )
  hist_combined <- (hist_agentic / hist_communal)
  
  # Variance plots (box plots)
  box_agentic <- plot_variance_box(
    data$agentic_AM_SJT_all, data$agentic_CM_SJT_all,
    "AM Subscale", "CM Subscale", title = "Agentic Job Types"
  )
  box_communal <- plot_variance_box(
    data$communal_AM_SJT_all, data$communal_CM_SJT_all,
    "AM Subscale", "CM Subscale", title = "Communal Job Types"
  )
  box_combined <- (box_agentic / box_communal) & theme(legend.position = "none")
  
  # Interaction plots
  int_actual <- plot_interaction(
    ANOVA_SJT_all, "actual_BIMI", 
    paste0("Actual BIMI Scores: Job Type × Dimension (", model_label, ")")
  )
  
  int_all <- plot_interaction(ANOVA_SJT_all, "predicted_BIMI", "All SJTs Concatenated")
  int_AM  <- plot_interaction(ANOVA_SJT_AM,  "predicted_BIMI", "Agentic SJTs")
  int_CM  <- plot_interaction(ANOVA_SJT_CM,  "predicted_BIMI", "Communal SJTs")
  
  int_predicted <- (int_all | int_AM | int_CM) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title = paste0("Predicted BIMI: Job Type × Dimension (", model_label, ")"),
      theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
    ) &
    theme(legend.position = "bottom") &
    labs(color = "Job Type")
  
  # Return everything
  list(
    data = list(
      SJT_all = ANOVA_SJT_all,
      SJT_AM  = ANOVA_SJT_AM,
      SJT_CM  = ANOVA_SJT_CM
    ),
    anova = list(
      actual  = anova_actual,
      SJT_all = anova_all,
      SJT_AM  = anova_AM,
      SJT_CM  = anova_CM
    ),
    plots = list(
      histograms      = hist_combined,
      boxplots        = box_combined,
      int_actual      = int_actual,
      int_predicted   = int_predicted
    )
  )
}
