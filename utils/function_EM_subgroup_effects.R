#' Calculate Subgroup Effects from Interaction Models
#'
#' @description
#' Extracts subgroup-specific odds ratios, interaction p-values, and RERI (Relative
#' Excess Risk due to Interaction) from regression models containing interaction terms.
#' Uses the msm package for delta method calculations of confidence intervals.
#'
#' @param model A fitted regression model object (typically glm with family=binomial for logistic regression).
#'              The model must include interaction terms between exposure and modifier variables.
#' @param exposure Character string specifying the name of the exposure variable in the model.
#' @param modifier Character string specifying the name of the effect modifier variable in the model.
#'
#' @return A list containing three data frames:
#' \describe{
#'   \item{odds_ratios}{Data frame with exposure odds ratios for each level of the modifier variable}
#'   \item{interaction_p_values}{Data frame with p-values for each interaction term}
#'   \item{reri}{Data frame with RERI estimates and confidence intervals for each non-reference level}
#' }
#'
#' @details
#' This function analyzes interaction effects in regression models by:
#' 1. Extracting odds ratios for the exposure within each level of the modifier
#' 2. Calculating p-values for each interaction term
#' 3. Computing RERI (Relative Excess Risk due to Interaction) as a measure of additive interaction
#'    using the msm package's deltamethod function for accurate confidence intervals
#'
#' The function assumes:
#' - The model is properly specified with interaction terms in the format "exposure:modifierlevel"
#' - For categorical modifiers, the reference level is correctly set in the model
#' - The model's vcov matrix is available and contains all necessary terms
#'
#' @note
#' RERI is calculated as RR11 - RR10 - RR01 + 1, where:
#' - RR11 is the joint effect of exposure and modifier
#' - RR10 is the effect of exposure alone (at reference level of modifier)
#' - RR01 is the effect of modifier alone (without exposure)
#'
#' RERI > 0 indicates positive interaction on the additive scale.
#'
#' @importFrom msm deltamethod
#'
#' @author Arnab K. Dey, Anna Dimitrova
#' @date May 2025

calculate_subgroup_effects <- function(model, exposure, modifier) {
  # Load required package
  if (!requireNamespace("msm", quietly = TRUE)) {
    stop("Package 'msm' is needed for this function to work. Please install it.")
  }
  # Step-0: Preprocessing
  ## Get model coefficients and variance-covariance matrix
  coefs <- coef(model)
  vcov_matrix <- vcov(model)

  ## Identify interaction terms
  interaction_terms <- grep(paste0("^", exposure, ":", modifier), names(coefs), value = TRUE)
  modifier_levels <- gsub(paste0(exposure, ":", modifier), "", interaction_terms)

  ## Get all levels from the data
  all_levels <- levels(model$data[[modifier]])
  reference_level <- all_levels[!all_levels %in% modifier_levels][1]

  ## Main effect of exposure
  beta_exposure <- coefs[exposure]
  se_exposure <- sqrt(vcov_matrix[exposure, exposure])

  ## Initialize results dataframe
  levels_to_process <- all_levels
  n_levels <- length(levels_to_process)

  # Step-1: Extract odds ratios and confidence intervals
  ## Initialize results data frame
  results <- data.frame(
    ModifierLevel = levels_to_process,
    OR = numeric(n_levels),
    OR_LowerCI = numeric(n_levels),
    OR_UpperCI = numeric(n_levels),
    stringsAsFactors = FALSE
  )

  ## Find reference level index
  ref_idx <- which(results$ModifierLevel == reference_level)

  ## Calculate OR for reference level
  results[ref_idx, "OR"] <- exp(beta_exposure)
  results[ref_idx, "OR_LowerCI"] <- exp(beta_exposure - 1.96 * se_exposure)
  results[ref_idx, "OR_UpperCI"] <- exp(beta_exposure + 1.96 * se_exposure)

  ## Calculate OR for each non-reference level
  for (i in 1:n_levels) {
    if (i != ref_idx) {
      level <- levels_to_process[i]
      interaction_term <- paste0(exposure, ":", modifier, level)
      
      if (interaction_term %in% names(coefs)) {
        beta_interaction <- coefs[interaction_term]
        se_interaction <- sqrt(vcov_matrix[interaction_term, interaction_term])
        cov_exp_int <- vcov_matrix[exposure, interaction_term]
        
        # Combined effect
        effect <- beta_exposure + beta_interaction
        se_effect <- sqrt(se_exposure^2 + se_interaction^2 + 2*cov_exp_int)
        
        # Calculate OR and CI
        results[i, "OR"] <- exp(effect)
        results[i, "OR_LowerCI"] <- exp(effect - 1.96 * se_effect)
        results[i, "OR_UpperCI"] <- exp(effect + 1.96 * se_effect)
      }
    }
  }

  # Step-2: Calculate interaction p-values
  ## Initialize interaction p-values data frame
  interaction_tests <- data.frame(
    ModifierLevel = modifier_levels,
    Interaction_P = numeric(length(modifier_levels)),
    stringsAsFactors = FALSE
  )
  ## Find reference level index
  for (i in 1:length(interaction_terms)) {
    term <- interaction_terms[i]
    if (term %in% names(coefs)) {
      coef_val <- coefs[term]
      se_val <- sqrt(vcov_matrix[term, term])
      z_val <- coef_val / se_val
      interaction_tests$Interaction_P[i] <- 2 * pnorm(-abs(z_val))
    }
  }

  # Step-3: Calculate RERI
  ## Initialize RERI results data frame
  reri_results <- data.frame(
    ModifierLevel = modifier_levels,
    RERI = numeric(length(modifier_levels)),
    RERI_LowerCI = numeric(length(modifier_levels)),
    RERI_UpperCI = numeric(length(modifier_levels)),
    stringsAsFactors = FALSE
  )

  ## Get reference level values
  RR10 <- results[ref_idx, "OR"] # Exposure effect in reference group

  ## calculate RERI for each modifier level
  for (i in 1:length(modifier_levels)) {
  level <- modifier_levels[i]
  modifier_term <- paste0(modifier, level)
  interaction_term <- paste0(exposure, ":", modifier, level)
  
  # Get level index in results
  level_idx <- which(results$ModifierLevel == level)
      
  # Get RR01 (modifier effect alone)
  if (modifier_term %in% names(coefs)) {
    beta_modifier <- coefs[modifier_term]
    RR01 <- exp(beta_modifier)
  } else {
    RR01 <- 1
    beta_modifier <- 0
  }
      
  # Calculate RERI using msm package's deltamethod function
  if (interaction_term %in% names(coefs)) {
    beta_interaction <- coefs[interaction_term]
    
    # Create parameter vector
    param_vector <- c(beta_exposure, beta_modifier, beta_interaction)
    
    # Define RERI formula as an expression
    reri_formula <- expression(exp(x1 + x2 + x3) - exp(x1) - exp(x2) + 1)
    
    # Extract relevant portion of variance-covariance matrix
    if (modifier_term %in% names(coefs)) {
      # Both modifier main effect and interaction are in model
      param_names <- c(exposure, modifier_term, interaction_term)
    } else {
      # Only interaction term is in model (no modifier main effect)
      param_names <- c(exposure, interaction_term)
      param_vector <- c(beta_exposure, beta_interaction)
      reri_formula <- expression(exp(x1 + x3) - exp(x1) - 1 + 1) # Simplified since exp(0) = 1
    }
    
    # Get the subset of the variance-covariance matrix
    vcov_subset <- vcov_matrix[param_names, param_names]
    
    # Calculate RERI
    RR11 <- exp(beta_exposure + beta_modifier + beta_interaction)
    RERI <- RR11 - RR10 - RR01 + 1
    
    # Use deltamethod to calculate standard error
    RERI_SE <- deltamethod(reri_formula, param_vector, vcov_subset)
    
  } else {
    # No interaction term in model
    RERI <- NA
    RERI_SE <- NA
  }
  
  # Confidence intervals
  reri_results$RERI[i] <- RERI
  if (!is.na(RERI_SE)) {
    reri_results$RERI_LowerCI[i] <- RERI - 1.96 * RERI_SE
    reri_results$RERI_UpperCI[i] <- RERI + 1.96 * RERI_SE
  } else {
    reri_results$RERI_LowerCI[i] <- NA
    reri_results$RERI_UpperCI[i] <- NA
  }
  }

  # Return results as a list
  return(list(
    odds_ratios = results,
    interaction_p_values = interaction_tests,
    reri = reri_results
  ))
}

