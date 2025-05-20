#' Create and Save Love Plots for Covariate Balance Assessment
#'
#' @description
#' This function generates Love plots to visualize the balance of covariates before and after
#' applying stabilized weights. Love plots are commonly used in propensity score analysis
#' to assess the effectiveness of weighting in reducing covariate imbalance between
#' treatment and control groups.
#'
#' @param data A data.table or data.frame containing the analysis data
#' @param exposure_var Character string specifying the name of the exposure/treatment variable
#' @param covariates Character vector containing the names of covariates to assess balance for
#' @param output_path Character string specifying the file path where the plot should be saved
#'
#' @return A ggplot object containing the Love plot
#'
#' @details
#' The function creates a Love plot that shows standardized mean differences for each covariate
#' before and after weighting. The plot includes:
#' \itemize{
#'   \item Red dots (shape 16) for unweighted standardized differences
#'   \item Blue triangles (shape 17) for weighted standardized differences
#'   \item A vertical line at 0.1 to indicate the conventional threshold for balance
#'   \item Alphabetically ordered covariates with clear labels
#'   \item A title indicating the exposure variable being analyzed
#' }
#'
#' The plot is automatically saved as a high-resolution (300 DPI) PNG file at the specified
#' output path, with dimensions of 20x16 inches.
#'
#' @import data.table
#' @import ggplot2
#' @importFrom cobalt love.plot
#'
#' @export
#' @author Arnab K. Dey, Anna Dimitrova
#' @date May 2025

# Function to create and save Love plots
function_love_plots <- function(data, exposure_var, covariates, output_path) {
  # Define variable labels
  var_labels <- c(
    "hh_access_issue_distance_not-a-big-prob" = "Distance not a big problem for healthcare access",
    "hh_caste_club_OBC" = "Caste: Other Backward Classes",
    "hh_caste_club_Other" = "Caste: Other",
    "hh_caste_club_SC" = "Caste: Scheduled Caste",
    "hh_caste_club_ST" = "Caste: Scheduled Tribe",
    "hh_religion_club_christian" = "Religion: Christian",
    "hh_religion_club_hindu" = "Religion: Hindu",
    "hh_religion_club_muslim" = "Religion: Muslim",
    "hh_religion_club_other" = "Religion: Other",
    "hh_wealth_quintile_ru_og_middle" = "Household wealth: Middle quintile",
    "hh_wealth_quintile_ru_og_poorer" = "Household wealth: Poorer quintile",
    "hh_wealth_quintile_ru_og_poorest" = "Household wealth: Poorest quintile",
    "hh_wealth_quintile_ru_og_richer" = "Household wealth: Richer quintile",
    "hh_wealth_quintile_ru_og_richest" = "Household wealth: Richest quintile",
    "mat_age_grp_at_birth_15-19" = "Mother's age at birth: 15-19 years",
    "mat_age_grp_at_birth_20-24" = "Mother's age at birth: 20-24 years",
    "mat_age_grp_at_birth_25-29" = "Mother's age at birth: 25-29 years",
    "mat_age_grp_at_birth_30-49" = "Mother's age at birth: 30-49 years",
    "mat_birth_order_Four or more" = "Birth order: Fourth or higher",
    "mat_birth_order_One" = "Birth order: First",
    "mat_birth_order_Three" = "Birth order: Third",
    "mat_birth_order_Two" = "Birth order: Second",
    "mat_edu_level_higher" = "Mother's education: Higher",
    "mat_edu_level_no education" = "Mother's education: No education",
    "mat_edu_level_primary" = "Mother's education: Primary",
    "mat_edu_level_secondary" = "Mother's education: Secondary",
    "mat_media_exp_any_yes" = "Mother exposed to media: Yes",
    "mat_parity_fac_1 child" = "Number of children: 1 child",
    "mat_parity_fac_2 children" = "Number of children: 2 children",
    "mat_parity_fac_3 children" = "Number of children: 3 children",
    "mat_parity_fac_4+ children" = "Number of children: 4 or more children",
    "rural_rural" = "Rural residence"
  )

  # Define exposure labels
  exposure_labels <- c(
    "hotday_db_80" = "DBT >= 80th percentile, 1-day",
    "hotday_wb_80" = "WBGT >= 80th percentile, 1-day",
    "hw_db_80_2d" = "DBT >= 80th percentile, 2-days",
    "hw_wb_80_2d" = "WBGT >= 80th percentile, 2-days",
    "hw_db_80_3d" = "DBT >= 80th percentile, 3-days",
    "hw_wb_80_3d" = "WBGT >= 80th percentile, 3-days",
    "hw_db_80_4d" = "DBT >= 80th percentile, 4-days",
    "hw_wb_80_4d" = "WBGT >= 80th percentile, 4-days",
    "hw_db_80_5d" = "DBT >= 80th percentile, 5-days",
    "hw_wb_80_5d" = "WBGT >= 80th percentile, 5-days",
    "hotday_db_825" = "DBT >= 82.5th percentile, 1-day",
    "hotday_wb_825" = "WBGT >= 82.5th percentile, 1-day",
    "hw_db_825_2d" = "DBT >= 82.5th percentile, 2-days",
    "hw_wb_825_2d" = "WBGT >= 82.5th percentile, 2-days",
    "hw_db_825_3d" = "DBT >= 82.5th percentile, 3-days",
    "hw_wb_825_3d" = "WBGT >= 82.5th percentile, 3-days",
    "hw_db_825_4d" = "DBT >= 82.5th percentile, 4-days",
    "hw_wb_825_4d" = "WBGT >= 82.5th percentile, 4-days",
    "hw_db_825_5d" = "DBT >= 82.5th percentile, 5-days",
    "hw_wb_825_5d" = "WBGT >= 82.5th percentile, 5-days",
    "hotday_db_85" = "DBT >= 85th percentile, 1-day",
    "hotday_wb_85" = "WBGT >= 85th percentile, 1-day",
    "hw_db_85_2d" = "DBT >= 85th percentile, 2-days",
    "hw_wb_85_2d" = "WBGT >= 85th percentile, 2-days",
    "hw_db_85_3d" = "DBT >= 85th percentile, 3-days",
    "hw_wb_85_3d" = "WBGT >= 85th percentile, 3-days",
    "hw_db_85_4d" = "DBT >= 85th percentile, 4-days",
    "hw_wb_85_4d" = "WBGT >= 85th percentile, 4-days",
    "hw_db_85_5d" = "DBT >= 85th percentile, 5-days",
    "hw_wb_85_5d" = "WBGT >= 85th percentile, 5-days",
    "hotday_db_875" = "DBT >= 87.5th percentile, 1-day",
    "hotday_wb_875" = "WBGT >= 87.5th percentile, 1-day",
    "hw_db_875_2d" = "DBT >= 87.5th percentile, 2-days",
    "hw_wb_875_2d" = "WBGT >= 87.5th percentile, 2-days",
    "hw_db_875_3d" = "DBT >= 87.5th percentile, 3-days",
    "hw_wb_875_3d" = "WBGT >= 87.5th percentile, 3-days",
    "hw_db_875_4d" = "DBT >= 87.5th percentile, 4-days",
    "hw_wb_875_4d" = "WBGT >= 87.5th percentile, 4-days",
    "hw_db_875_5d" = "DBT >= 87.5th percentile, 5-days",
    "hw_wb_875_5d" = "WBGT >= 87.5th percentile, 5-days",
    "hotday_db_90" = "DBT >= 90th percentile, 1-day",
    "hotday_wb_90" = "WBGT >= 90th percentile, 1-day",
    "hw_db_90_2d" = "DBT >= 90th percentile, 2-days",
    "hw_wb_90_2d" = "WBGT >= 90th percentile, 2-days",
    "hw_db_90_3d" = "DBT >= 90th percentile, 3-days",
    "hw_wb_90_3d" = "WBGT >= 90th percentile, 3-days",
    "hw_db_90_4d" = "DBT >= 90th percentile, 4-days",
    "hw_wb_90_4d" = "WBGT >= 90th percentile, 4-days",
    "hw_db_90_5d" = "DBT >= 90th percentile, 5-days",
    "hw_wb_90_5d" = "WBGT >= 90th percentile, 5-days",
    "hotday_db_925" = "DBT >= 92.5th percentile, 1-day",
    "hotday_wb_925" = "WBGT >= 92.5th percentile, 1-day",
    "hw_db_925_2d" = "DBT >= 92.5th percentile, 2-days",
    "hw_wb_925_2d" = "WBGT >= 92.5th percentile, 2-days",
    "hw_db_925_3d" = "DBT >= 92.5th percentile, 3-days",
    "hw_wb_925_3d" = "WBGT >= 92.5th percentile, 3-days",
    "hw_db_925_4d" = "DBT >= 92.5th percentile, 4-days",
    "hw_wb_925_4d" = "WBGT >= 92.5th percentile, 4-days",
    "hw_db_925_5d" = "DBT >= 92.5th percentile, 5-days",
    "hw_wb_925_5d" = "WBGT >= 92.5th percentile, 5-days",
    "hotday_db_95" = "DBT >= 95th percentile, 1-day",
    "hotday_wb_95" = "WBGT >= 95th percentile, 1-day",
    "hw_db_95_2d" = "DBT >= 95th percentile, 2-days",
    "hw_wb_95_2d" = "WBGT >= 95th percentile, 2-days",
    "hw_db_95_3d" = "DBT >= 95th percentile, 3-days",
    "hw_wb_95_3d" = "WBGT >= 95th percentile, 3-days",
    "hw_db_95_4d" = "DBT >= 95th percentile, 4-days",
    "hw_wb_95_4d" = "WBGT >= 95th percentile, 4-days",
    "hw_db_95_5d" = "DBT >= 95th percentile, 5-days",
    "hw_wb_95_5d" = "WBGT >= 95th percentile, 5-days"
  )

  # Create the Love plot
  setDT(data)
  love_plot <- love.plot(
    data |> select(all_of(covariates)), 
    treat = data[[exposure_var]],
    weights = data$stabilized_weight,
    binary = "std",
    threshold = 0.1,
    abs = FALSE,
    var.order = "alphabetical",
    var.names = var_labels,
    colours = c("#E41A1C", "#377EB8"),  # Colors for unweighted and weighted
    shapes = c(16, 17),  # Shapes for unweighted and weighted
    size = 3,
    position = "center",
    title = paste0("Covariate Balance Before and After Weighting\n", 
                  "Exposure Variable: ",
                  exposure_labels[exposure_var]),
    sample.names = c("Unweighted", "Weighted"),  # Names for the legend
    line = TRUE
  )

  # Customize the plot
  love_plot <- love_plot +
    theme(
      legend.position = "bottom",  # Move legend to the bottom
      legend.title = element_blank(),  # Remove legend title
      legend.text = element_text(size = 12),  # Adjust legend text size
      legend.key.size = unit(1.5, "lines")  # Adjust legend key size
    )
  
  # Save the plot
  ggsave(
    output_path, 
    plot = love_plot, 
    width = 20, 
    height = 16, 
    units = "in",
    dpi = 300,
    bg = "white"
  )
  
  return(love_plot)
} 
