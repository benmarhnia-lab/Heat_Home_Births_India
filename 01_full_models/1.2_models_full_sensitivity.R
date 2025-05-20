# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script runs the full models for sensitivity analysis
# @author: Arnab K. Dey, Anna Dimitrova
# @date: May 2025

# Load Libraries ----
rm(list = ls())
pacman::p_load(tidyverse, here, tictoc, data.table, lme4, MatchIt, cobalt, sandwich, survey, geepack, openxlsx)

# Set paths ----
source(here("paths.R"))

# Source utility functions ----
source(here("01_src", "02_models", "utils", "function_iptw_weights.R"))
source(here("01_src", "02_models", "utils", "function_love_plots.R"))
source(here("01_src", "02_models", "utils", "function_full_analysis.R"))

# Read data ----
df_paper_final <- readRDS(here(path_processed, "df_dhs_temperature_merged.rds"))

# Check ICC at different levels ----
model_state <- glmer(dv_home_del_num ~ hotday_wb_80 + (1 | state_name_fac), data = df_paper_final, family = binomial(link = "logit"))
performance::icc(model_state)

# Set constants ----
## Create datasets for sensitivity analysis ----
### deliveries only at own home ----
df_sensitivity_own_home <- df_paper_final |>
  filter(m15 != "other home" & m15 != "parents' home")

### Considering only one child per household ----
set.seed(12345)
df_sensitivity_one_child <- df_paper_final |>
  group_by(caseid) |>
  sample_n(1) |>
  ungroup()

## Define exposure variables ----
varlist_exposure_all <- c(
  # # Wet bulb temperature (percentile)
  "hotday_wb_80", "hw_wb_80_2d", "hw_wb_80_3d", "hw_wb_80_4d", "hw_wb_80_5d",
  "hotday_wb_825", "hw_wb_825_2d", "hw_wb_825_3d", "hw_wb_825_4d", "hw_wb_825_5d",
  "hotday_wb_85", "hw_wb_85_2d", "hw_wb_85_3d", "hw_wb_85_4d", "hw_wb_85_5d",
  "hotday_wb_875", "hw_wb_875_2d", "hw_wb_875_3d", "hw_wb_875_4d", "hw_wb_875_5d",
  "hotday_wb_90", "hw_wb_90_2d", "hw_wb_90_3d", "hw_wb_90_4d", "hw_wb_90_5d",
  "hotday_wb_925", "hw_wb_925_2d", "hw_wb_925_3d", "hw_wb_925_4d", "hw_wb_925_5d",
  "hotday_wb_95", "hw_wb_95_2d", "hw_wb_95_3d", "hw_wb_95_4d", "hw_wb_95_5d",
  ## Dry bulb temperature (percentile)
  "hotday_db_80", "hw_db_80_2d", "hw_db_80_3d", "hw_db_80_4d", "hw_db_80_5d",
  "hotday_db_825", "hw_db_825_2d", "hw_db_825_3d", "hw_db_825_4d", "hw_db_825_5d",
  "hotday_db_85", "hw_db_85_2d", "hw_db_85_3d", "hw_db_85_4d", "hw_db_85_5d",
  "hotday_db_875", "hw_db_875_2d", "hw_db_875_3d", "hw_db_875_4d", "hw_db_875_5d",
  "hotday_db_90", "hw_db_90_2d", "hw_db_90_3d", "hw_db_90_4d", "hw_db_90_5d",
  "hotday_db_925", "hw_db_925_2d", "hw_db_925_3d", "hw_db_925_4d", "hw_db_925_5d",
  "hotday_db_95", "hw_db_95_2d", "hw_db_95_3d", "hw_db_95_4d", "hw_db_95_5d")

## Define covariates for direct matching ----
covariates_all <- c("mat_edu_level", "mat_age_grp_at_birth", 
                "mat_parity_fac", "mat_birth_order", 
                "mat_media_exp_any",
                "hh_caste_club",
                "hh_religion_club", "hh_access_issue_distance", 
                "hh_wealth_quintile_ru_og", "rural", "state_name_fac")

## Convert all covariates to factors
df_paper_final <- df_paper_final |>
  mutate(across(all_of(covariates_all), as.factor))

# Run sensitivity analysis ----
## Deliveries only at own home ----
run_full_analysis(
  data = df_sensitivity_own_home,
  exposure_vars = varlist_exposure_all,
  covariates = covariates_all,
  dv_var = "dv_home_del_num",
  cluster_var = "psu_fac",
  trim_quantiles = c(0.01, 0.99),
  corstr = "ar1",
  model_type = "gee",
  output_dir = here(path_outputs, "models", "full_models", "sensitivity_models", "own_home")
) 

## Considering only one child per household ----
run_full_analysis(
  data = df_sensitivity_one_child,
  exposure_vars = varlist_exposure_all,
  covariates = covariates_all,
  dv_var = "dv_home_del_num",
  cluster_var = "psu_fac",
  trim_quantiles = c(0.01, 0.99),
  corstr = "ar1",
  model_type = "gee",
  output_dir = here(path_outputs, "models", "full_models", "sensitivity_models", "one_child")
)

## Full model but with exchangeable correlation structure ----
run_full_analysis(
  data = df_paper_final,
  exposure_vars = varlist_exposure_all,
  covariates = covariates_all,
  dv_var = "dv_home_del_num",
  cluster_var = "psu_fac",
  corstr = "exchangeable",
  model_type = "gee",
  output_dir = here(path_outputs, "models", "full_models", "sensitivity_models", "exchangeable")
)

## Full model but with more relaxed trimming ----
run_full_analysis(
  data = df_paper_final,
  exposure_vars = varlist_exposure_all,
  covariates = covariates_all,
  dv_var = "dv_home_del_num",
  cluster_var = "psu_fac",
  trim_quantiles = c(0.05, 0.95),
  corstr = "ar1",
  model_type = "gee",
  output_dir = here(path_outputs, "models", "full_models", "sensitivity_models", "relaxed_trimming_95")
)

