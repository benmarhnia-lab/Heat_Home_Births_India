#' Fit Generalized Estimating Equations (GEE) Model with Effect Modification
#'
#' This function fits a GEE model with effect modification between an exposure variable
#' and a modifier variable, while controlling for seasonal patterns using Fourier terms.
#'
#' @param data A data frame containing the variables for the model
#' @param exposure_var Character string specifying the name of the exposure variable
#' @param modifier_var Character string specifying the name of the effect modifier variable
#' @param dv_var Character string specifying the name of the dependent variable
#' @param cluster_var Character string specifying the name of the clustering variable
#' @param corstr Character string specifying the correlation structure. Default is "ar1" (AR-1).
#'              Other options include "independence", "exchangeable", "unstructured", etc.
#'
#' @return A geeglm object containing the fitted GEE model, or NULL if the model fitting fails
#'
#' @importFrom geepack geeglm
#' @importFrom stats as.formula binomial
#' @author Arnab K. Dey, Anna Dimitrova
#' @date May 2025

function_EM_model_gee <- function(data, exposure_var, modifier_var, dv_var, cluster_var, corstr = "ar1") {
  # Create formula for GEE model with interaction
  gee_formula <- as.formula(paste(dv_var, "~", exposure_var, "*", modifier_var,
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
  
  # Return just the model object
  return(gee_model)
}
