# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script runs the effect modification models for the paper. 
#               It generates the main results for a 4-day heatwave at 85th percentile.
# @author: Arnab K. Dey, Anna Dimitrova
# @date: May 2025

# Load Libraries ----
rm(list = ls())
pacman::p_load(tidyverse, data.table, openxlsx, lme4, here, tictoc)
pacman::p_load(MatchIt, cobalt, sandwich, survey, geepack)

# Set paths ----
source(here("paths.R"))

# Source functions ----
source(here("utils", "function_iptw_weights.R"))
source(here("utils", "function_EM_model_gee.R"))
source(here("utils", "function_EM_subgroup_effects.R"))
source(here("utils", "function_EM_extract_results_excel.R"))

# Read datasets ----
df_paper_final <- readRDS(here(path_processed, "df_dhs_temperature_merged.rds"))

# Set constants ----
## Exposure variables
varlist_exposure_all <- c("hw_wb_85_4d", "hw_db_85_4d")

## Modifier variables
modifier_vars <- c("rural", "hh_access_issue_distance", "lt_mean_wbgt_tert_psu", 
                   "state_janani_bi", "state_home_birth_bi", "mat_edu_level_bi", 
                   "hh_wealth_poorest2", "hh_religion_bi", "hh_caste_bi")

## Covariates for IPTW
covariates_all <- c("mat_edu_level", "mat_age_grp_at_birth", 
                    "mat_parity_fac", "mat_birth_order", 
                    "mat_media_exp_any",
                    "hh_caste_club",
                    "hh_religion_club", "hh_access_issue_distance", 
                    "hh_wealth_quintile_ru_og", "rural", "state_name_fac")

# Prepare data ----
### Convert all modifier variables to factors
df_paper_final <- df_paper_final |>
  mutate(across(all_of(modifier_vars), as.factor))

### Relevel modifier variables
df_paper_final$state_home_birth_bi <- relevel(df_paper_final$state_home_birth_bi, ref = "Medium_or_High_HB")
df_paper_final$hh_wealth_poorest2 <- relevel(df_paper_final$hh_wealth_poorest2, ref = "richer3")
df_paper_final$mat_edu_level_bi <- relevel(df_paper_final$mat_edu_level_bi, ref = "primary or higher")
df_paper_final$hh_access_issue_distance <- relevel(df_paper_final$hh_access_issue_distance, ref = "not-a-big-prob")
df_paper_final$hh_caste_bi <- relevel(df_paper_final$hh_caste_bi, ref = "general")

### Convert all covariates to factors
df_paper_final <- df_paper_final |>
  mutate(across(all_of(covariates_all), as.factor))

# Run models in a loop ----
# Create empty list to store model results
model_results_list <- list()

# Loop through each exposure variable
for (exposure_var in varlist_exposure_all) {
  # Calculate IPTW weights
  df_paper_final <- function_iptw_weights(
    data = df_paper_final,
    exposure_var = exposure_var,
    covariates = covariates_all,
    trim_quantiles = c(0.01, 0.99)
  )
  
  # Loop through each modifier variable
  for (modifier_var in modifier_vars) {
    # Generate model name
    model_name <- paste0(exposure_var, "_X_", modifier_var)
    
    # Fit GEE model with effect modification
    model_results <- function_EM_model_gee(
      data = df_paper_final,
      exposure_var = exposure_var,
      modifier_var = modifier_var,
      dv_var = "dv_home_del_num",
      cluster_var = "psu_fac"
    )
    
    # Store the model in the list if successful
    if (!is.null(model_results)) {
      model_results_list[[model_name]] <- model_results
      cat("Completed model:", model_name, "\n")
    } else {
      cat("Error in model:", model_name, "\n")
    }
  }
}

# Save outputs ----
path_out <- here(path_outputs, "models", "effect_modification", "main_models")
dir.create(path_out, showWarnings = FALSE, recursive = TRUE)

## Save model results list
model_results_list |> saveRDS(here(path_out, "gee_interaction_models.rds"))

## Extract and save results
process_models(model_results_list, path_output = path_out)
