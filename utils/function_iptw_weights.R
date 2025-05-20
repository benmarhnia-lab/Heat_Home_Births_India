#' Calculate Inverse Probability of Treatment Weights (IPTW)
#'
#' This function calculates stabilized Inverse Probability of Treatment Weights (IPTW)
#' using propensity scores derived from a logistic regression model. The weights are
#' trimmed to handle extreme values and stabilized to improve efficiency.
#'
#' @param data A data frame containing the exposure variable and covariates
#' @param exposure_var Character string specifying the name of the binary exposure variable
#' @param covariates Character vector specifying the names of covariates to include in the propensity score model
#' @param trim_quantiles Numeric vector of length 2 specifying the quantiles for trimming propensity scores (default: c(0.01, 0.99))
#'
#' @return A data frame with the following additional columns:
#'   \item{ps_score}{Raw propensity scores}
#'   \item{ps_score_trimmed}{Trimmed propensity scores}
#'   \item{stabilized_weight}{Stabilized IPTW weights}
#'
#' @details The function performs the following steps:
#'   1. Fits a logistic regression model to estimate propensity scores
#'   2. Trims extreme propensity scores using specified quantiles
#'   3. Calculates stabilized weights using the formula: 
#'      - For exposed: mean(exposure)/ps_score
#'      - For unexposed: (1-mean(exposure))/(1-ps_score)
#'
#' @export
#' @author Arnab K. Dey, Anna Dimitrova
#' @date May 2025

function_iptw_weights <- function(data, exposure_var, covariates, trim_quantiles = c(0.01, 0.99)) {
  # Fit logistic regression model for propensity scores
  ps_formula <- as.formula(paste(exposure_var, "~", 
                                paste(covariates, collapse = " + ")))
  
  ps_model <- tryCatch({
    glm(ps_formula, data = data, family = binomial(link = "logit"))
  }, error = function(e) {
    warning(paste("Error in propensity score model:", conditionMessage(e)))
    return(NULL)
  })
  
  if (is.null(ps_model)) {
    return(NULL)
  }
  
  # Calculate propensity scores
  data$ps_score <- predict(ps_model, type = "response")
  
  # Trim propensity scores using user-specified quantiles
  ps_quantiles <- quantile(data$ps_score, trim_quantiles, na.rm = TRUE)
  data$ps_score_trimmed <- ifelse(
    data$ps_score < ps_quantiles[1], 
    ps_quantiles[1], 
    ifelse(data$ps_score > ps_quantiles[2], 
           ps_quantiles[2], data$ps_score))
  
  # Calculate stabilized weights
  exposure_mean <- mean(data[[exposure_var]], na.rm = TRUE)
  data$stabilized_weight <- ifelse(data[[exposure_var]] == 1, 
                                  exposure_mean/data$ps_score_trimmed, 
                                  (1-exposure_mean)/(1-data$ps_score_trimmed))
  
  return(data)
} 
