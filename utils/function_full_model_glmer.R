#' Fit GLMER Models with Random Effects
#'
#' This function fits a Generalized Linear Mixed Effects Regression (GLMER) model
#' with random effects for cluster-level variation. The model includes seasonal
#' controls (monthly sine and cosine terms) and uses stabilized weights.
#'
#' @param data A data frame containing the variables for the model
#' @param exposure_var Character string specifying the name of the exposure variable
#' @param dv_var Character string specifying the name of the dependent variable
#' @param cluster_var Character string specifying the name of the clustering variable
#'
#' @return A list containing:
#'   \item{model}{The fitted GLMER model object}
#'   \item{summary}{Summary statistics of the fitted model}
#'   \item{exposure_coef}{A named vector containing the exposure coefficient estimates:
#'     \describe{
#'       \item{beta}{Point estimate}
#'       \item{se}{Standard error}
#'       \item{ci_lower}{Lower bound of 95\% confidence interval}
#'       \item{ci_upper}{Upper bound of 95\% confidence interval}
#'     }
#'   }
#'
#' @details The function fits a binomial GLMER model with logit link function.
#' The model includes seasonal controls using sine and cosine terms for monthly
#' variation. Random effects are specified at the cluster level. The model uses
#' stabilized weights and the 'bobyqa' optimizer for fitting.
#'
#' @importFrom lme4 glmer glmerControl
#' @importFrom stats binomial coef

# Function to fit GLMER models with random effects
function_glmer_model <- function(data, exposure_var, dv_var, cluster_var) {
  # Create formula for GLMER model with random effects
  glmer_formula <- as.formula(paste(dv_var, "~", exposure_var,
                                   "+ month_sin1 + month_cos1 + month_sin2 + month_cos2",
                                   "+ (1|", cluster_var, ")"))
  
  # Fit GLMER model
  glmer_model <- tryCatch({
    lme4::glmer(
      glmer_formula,
      data = data,
      weights = data$stabilized_weight,
      family = binomial(link = "logit"),
      control = lme4::glmerControl(optimizer = "bobyqa")
    )
  }, error = function(e) {
    warning(paste("Error in GLMER model:", conditionMessage(e)))
    return(NULL)
  })
  
  if (is.null(glmer_model)) {
    return(NULL)
  }
  
  # Get model summary
  model_summary <- summary(glmer_model)
  
  # Extract coefficient and standard error
  beta <- coef(model_summary)[exposure_var, "Estimate"]
  se <- coef(model_summary)[exposure_var, "Std. Error"]
  
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
    model = glmer_model,
    summary = model_summary,
    exposure_coef = exposure_coef
  ))
} 
