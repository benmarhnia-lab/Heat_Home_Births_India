#' Run Full Analysis for Exposure Variables
#'
#' @description
#' This function performs a comprehensive analysis of exposure variables using either GEE (Generalized Estimating Equations)
#' or GLMER (Generalized Linear Mixed Effects Regression) models. It includes IPTW (Inverse Probability of Treatment Weighting)
#' calculation, Love plots generation, and model fitting with confidence intervals.
#'
#' @param data A data frame containing the analysis data
#' @param exposure_vars Character vector of exposure variable names to analyze
#' @param covariates Character vector of covariate names to include in the analysis
#' @param dv_var Character string specifying the dependent variable name
#' @param cluster_var Character string specifying the clustering variable name
#' @param trim_quantiles Numeric vector of length 2 specifying the quantiles for trimming weights (default: c(0.01, 0.99))
#' @param corstr Character string specifying the correlation structure for GEE models (default: "ar1")
#' @param output_dir Character string specifying the directory for saving results
#' @param model_type Character string specifying the type of model to use ("gee" or "glmer")
#'
#' @return A list containing:
#' \itemize{
#'   \item models: List of fitted models for each exposure variable
#'   \item results: Data frame containing model results with columns:
#'     \itemize{
#'       \item exposure: Exposure variable name
#'       \item estimate: Odds ratio estimate
#'       \item conf_low: Lower confidence interval
#'       \item conf_high: Upper confidence interval
#'       \item converged: Logical indicating if model converged
#'       \item n_rows: Number of observations used
#'       \item exposure_type: Type of exposure (Wet Bulb or Dry Bulb)
#'       \item threshold: Threshold value
#'       \item duration: Duration of exposure
#'       \item threshold_type: Type of threshold (Percentile or Absolute)
#'     }
#' }
#'
#' @details
#' The function performs the following steps for each exposure variable:
#' \enumerate{
#'   \item Validates the exposure variable has sufficient unique values
#'   \item Calculates IPTW weights
#'   \item Generates Love plots for covariate balance
#'   \item Fits the specified model type (GEE or GLMER)
#'   \item Extracts coefficients and confidence intervals
#'   \item Saves results and models
#' }
#'
#' Output files generated:
#' \itemize{
#'   \item love_plots/: Directory containing Love plots for each exposure
#'   \item all_models.rds: RDS file containing all fitted models
#'   \item model_results.xlsx: Excel file with detailed results
#' }
#'
#' @export 
#' @author: Arnab K. Dey, Anna Dimitrova
#' @date: May 2025

# Function to run the full analysis
run_full_analysis <- function(data, 
                            exposure_vars,
                            covariates,
                            dv_var,
                            cluster_var,
                            trim_quantiles = c(0.01, 0.99),
                            corstr = "ar1",
                            output_dir,
                            model_type = "gee") {
  
  # Validate model_type
  if (!model_type %in% c("gee", "glmer")) {
    stop("model_type must be either 'gee' or 'glmer'")
  }
  
  # Create output directories if they don't exist
  dir.create(file.path(output_dir, "love_plots"), recursive = TRUE, showWarnings = FALSE)
  
  # Initialize lists to store results
  all_models <- list()
  
  # Initialize results dataframe with specified columns
  results <- data.frame(
    exposure = character(),
    estimate = numeric(),
    conf_low = numeric(),
    conf_high = numeric(),
    converged = logical(),
    n_rows = numeric(),
    exposure_type = character(),
    threshold = character(),
    duration = character(),
    threshold_type = character(),
    stringsAsFactors = FALSE
  )
  
  # Setup progress tracking
  total_exposures <- length(exposure_vars)
  processed_combinations <- 0
  successful_models <- 0
  skipped_models <- 0
  
  cat("\n===============================================================\n")
  cat("Starting model fitting for", total_exposures, "exposure variables\n")
  cat("Using", toupper(model_type), "models\n")
  cat("===============================================================\n")
  
  # Start overall timer
  tic("Total execution time")
  
  # Loop over all exposure variables
  for (exp_idx in 1:length(exposure_vars)) {
    exposure_var <- exposure_vars[exp_idx]
    processed_combinations <- processed_combinations + 1
    
    # Calculate overall progress percentage
    progress_pct <- round(processed_combinations / total_exposures * 100, 1)
    
    cat("\n[", format(Sys.time(), "%H:%M:%S"), "] Processing exposure", exp_idx, "of", total_exposures, 
        ":", exposure_var, "(Overall progress:", progress_pct, "%)\n")
    
    # Check if exposure_var has at least two unique values
    if (length(unique(data[[exposure_var]])) < 2) {
      warning(paste("Skipping", exposure_var, "because it has only one unique value."))
      skipped_models <- skipped_models + 1
      next
    }
    
    # Calculate IPTW weights
    cat("  - Calculating IPTW weights...\n")
    data_weighted <- function_iptw_weights(data, exposure_var, covariates, trim_quantiles)
    
    if (is.null(data_weighted)) {
      warning(paste("Failed to calculate weights for", exposure_var))
      skipped_models <- skipped_models + 1
      next
    }
    
    # Create Love plot
    cat("  - Creating Love plot...\n")
    love_plot_path <- file.path(output_dir, "love_plots", 
                               paste0("love-", exposure_var, ".png"))
    function_love_plots(data_weighted, exposure_var, covariates, love_plot_path)
    
    # Fit model based on model_type
    cat("  - Fitting", toupper(model_type), "model...\n")
    model_results <- if (model_type == "gee") {
      function_gee_model(data_weighted, exposure_var, dv_var, cluster_var, corstr)
    } else {
      function_glmer_model(data_weighted, exposure_var, dv_var, cluster_var)
    }
    
    if (is.null(model_results)) {
      warning(paste("Failed to fit", toupper(model_type), "model for", exposure_var))
      skipped_models <- skipped_models + 1
      next
    }
    
    # Store model
    model_name <- paste(model_type, "model", exposure_var, sep = "-")
    all_models[[model_name]] <- model_results$model
    
    # Extract coefficients and calculate confidence intervals
    beta <- model_results$exposure_coef["beta"]
    ci_lower <- model_results$exposure_coef["ci_lower"]
    ci_upper <- model_results$exposure_coef["ci_upper"]
    
    # Create new row for results
    new_row <- data.frame(
      exposure = exposure_var,
      estimate = exp(beta),
      conf_low = exp(ci_lower),
      conf_high = exp(ci_upper),
      converged = !is.null(model_results$model) && 
                 if(model_type == "gee") {
                   !is.null(model_results$model$geese$error) && model_results$model$geese$error == 0
                 } else {
                   TRUE  # For GLMER, we assume convergence if model exists
                 },
      n_rows = nrow(data_weighted),
      exposure_type = ifelse(grepl("wb", exposure_var), "Wet Bulb", "Dry Bulb"),
      threshold = str_extract(exposure_var, "(?<=_)\\d+"),
      duration = case_when(
        grepl("hotday", exposure_var) ~ "1-day",
        grepl("_2d", exposure_var) ~ "2-day",
        grepl("_3d", exposure_var) ~ "3-day",
        grepl("_4d", exposure_var) ~ "4-day",
        grepl("_5d", exposure_var) ~ "5-day",
        TRUE ~ NA_character_
      ),
      threshold_type = case_when(
        grepl("80|825|85|875|90|925|95|97", exposure_var) ~ "Percentile",
        grepl("28|29|30|31|32", exposure_var) ~ "Absolute",
        TRUE ~ NA_character_
      ),
      stringsAsFactors = FALSE
    )
    
    results <- rbind(results, new_row)
    successful_models <- successful_models + 1
    
    # Print model results
    cat("  - Model results for", exposure_var, ":", 
        sprintf("OR=%.3f (95%% CI: %.3f-%.3f)", 
                exp(beta), 
                exp(ci_lower), 
                exp(ci_upper)), "\n")
  }
  
  # End total timer
  toc()
  
  # Save results
  cat("\nSaving results...\n")
  
  # Save all models
  saveRDS(all_models, file.path(output_dir, "all_models.rds"))
  
  # Save results to Excel
  write.xlsx(results, file.path(output_dir, "model_results.xlsx"))
  
  # Print final summary
  cat("\n===============================================================\n")
  cat("Final Summary\n")
  cat("===============================================================\n")
  cat("Total exposure variables:", total_exposures, "\n")
  cat("Successful models:", successful_models, "\n")
  cat("Skipped models:", skipped_models, "\n")
  cat("Model type used:", toupper(model_type), "\n")
  cat("\nResults saved to:", output_dir, "\n")
  
  # add a beep to signal completion
  try(beepr::beep(sound = 'ping'), silent = TRUE)
  
  return(list(
    models = all_models,
    results = results
  ))
}
