#' Fit Generalized Estimating Equations (GEE) Model
#'
#' This function fits a GEE model for binary outcomes with seasonal controls and stabilized weights.
#' It handles clustered data and provides coefficient estimates with confidence intervals.
#'
#' @param data A data frame containing the variables for analysis
#' @param exposure_var Character string specifying the name of the exposure variable
#' @param dv_var Character string specifying the name of the dependent variable
#' @param cluster_var Character string specifying the name of the clustering variable
#' @param corstr Character string specifying the correlation structure. Default is "ar1" (AR-1).
#'              Other options include "independence", "exchangeable", "unstructured", etc.
#'
#' @return A list containing:
#'   \item{model}{The fitted GEE model object}
#'   \item{summary}{Summary statistics of the fitted model}
#'   \item{exposure_coef}{A named vector containing:
#'     \itemize{
#'       \item beta: Coefficient estimate for the exposure variable
#'       \item se: Standard error of the coefficient
#'       \item ci_lower: Lower bound of 95% confidence interval
#'       \item ci_upper: Upper bound of 95% confidence interval
#'     }
#'   }
#'
#'
#' @importFrom geepack geeglm
#' @importFrom stats as.formula binomial coef
#'
# Function to fit GEE models
function_gee_model <- function(data, exposure_var, dv_var, cluster_var, corstr = "ar1") {
  # Create formula for GEE model
  gee_formula <- as.formula(paste(dv_var, "~", exposure_var,
                                 "+ month_sin1 + month_cos1 + month_sin2 + month_cos2"))
  
  # Fit GEE model
  gee_model <- tryCatch({
    geeglm(
      gee_formula,
      data = data,
      id = data[[cluster_var]], 
      weights = data$stabilized_weight,
      family = binomial(link = "logit"),
      corstr = corstr
    )
  }, error = function(e) {
    warning(paste("Error in GEE model:", conditionMessage(e)))
    return(NULL)
  })
  
  if (is.null(gee_model)) {
    return(NULL)
  }
  
  # Get model summary
  model_summary <- summary(gee_model)
  
  # Extract coefficient and standard error
  beta <- coef(model_summary)[exposure_var, "Estimate"]
  se <- coef(model_summary)[exposure_var, "Std.err"]
  
  # Calculate confidence intervals
  ci_lower <- beta - (1.96 * se)
  ci_upper <- beta + (1.96 * se)
  
  # Create exposure coefficient vector with proper names
  exposure_coef <- c(
    beta = beta,
    se = se,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
  
  # Return both the model and its summary
  return(list(
    model = gee_model,
    summary = model_summary,
    exposure_coef = exposure_coef
  ))
} 
