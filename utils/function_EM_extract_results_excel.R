#' Process Interaction Models and Export Results to Excel
#'
#' @description
#' This function processes a list of interaction models, extracting subgroup effects,
#' interaction p-values, and RERI (Relative Excess Risk due to Interaction) measures.
#' Results are organized by modifiers and exposures and exported to three separate
#' Excel workbooks.
#'
#' @param model_list A named list of fitted models. Model names should follow the pattern
#'                  "exposure_X_modifier" to enable automatic extraction of variables.
#' @param path_output Character string specifying the directory path where the Excel
#'                   workbooks should be saved.
#'
#' @return A list containing three workbook objects:
#' \describe{
#'   \item{odds_ratios_wb}{Workbook with odds ratios and confidence intervals by modifier and exposure}
#'   \item{interaction_p_wb}{Workbook with interaction p-values by modifier and exposure}
#'   \item{reri_wb}{Workbook with RERI values and confidence intervals by modifier and exposure}
#' }
#'
#' @details
#' The function extracts exposure and modifier information from model names,
#' processes each combination, and organizes results into separate worksheets by modifier.
#' Each worksheet contains results for all exposures that interact with that modifier.
#'
#' This function depends on the `calculate_subgroup_effects` function which
#' extracts odds ratios, interaction p-values, and RERI from the models.
#'
#' @note
#' Excel workbooks are saved with the following names:
#' - subgroup_odds_ratios.xlsx
#' - subgroup_interaction_pvalues.xlsx
#' - subgroup_reri.xlsx
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeDataTable setColWidths createStyle addStyle saveWorkbook
#' @importFrom here here
#' @importFrom msm deltamethod
#'
#' @author Arnab K. Dey, Anna Dimitrova
#' @date May 2025

library(openxlsx)
library(here)
library(msm) 

process_models <- function(model_list, path_output) {
  # Extract all unique modifiers and exposures from model names
  all_model_names <- names(model_list)

  # Parse model names to extract exposures and modifiers
  model_info <- lapply(all_model_names, function(name) {
    # Extract parts based on the pattern described
    parts <- strsplit(name, "_X_")[[1]]
    if (length(parts) < 2) {
      return(NULL) # Skip if pattern doesn't match
    }
    exposure <- parts[1]
    modifier_part <- parts[2]

    # Use the modifier part directly without additional parsing
    modifier <- modifier_part

    return(list(
      model_name = name,
      exposure = exposure,
      modifier = modifier
    ))
  })

  # Remove NULL entries
  model_info <- model_info[!sapply(model_info, is.null)]

  # Extract unique modifiers and exposures
  unique_modifiers <- unique(sapply(model_info, function(x) x$modifier))
  unique_exposures <- unique(sapply(model_info, function(x) x$exposure))

  # Create three workbooks
  wb_odds_ratios <- createWorkbook()
  wb_interaction_p <- createWorkbook()
  wb_reri <- createWorkbook()

  # Process each modifier
  for (modifier in unique_modifiers) {
    # Create a clean sheet name (remove special characters that Excel doesn't like)
    sheet_name <- gsub("[\\/:*?\"<>|]", "_", modifier)

    # Create sheets for each modifier in each workbook
    addWorksheet(wb_odds_ratios, sheet_name)
    addWorksheet(wb_interaction_p, sheet_name)
    addWorksheet(wb_reri, sheet_name)

    # Create data frames to store all results for this modifier
    odds_results <- data.frame(
      Exposure = character(),
      ModifierLevel = character(),
      OR = numeric(),
      OR_LowerCI = numeric(),
      OR_UpperCI = numeric(),
      stringsAsFactors = FALSE
    )

    int_p_results <- data.frame(
      Exposure = character(),
      ModifierLevel = character(),
      Interaction_P = numeric(),
      stringsAsFactors = FALSE
    )

    reri_results <- data.frame(
      Exposure = character(),
      ModifierLevel = character(),
      RERI = numeric(),
      RERI_LowerCI = numeric(),
      RERI_UpperCI = numeric(),
      stringsAsFactors = FALSE
    )

    # Process each exposure for this modifier
    for (exposure in unique_exposures) {
      # Find the model with this exposure and modifier
      matching_model_info <- NULL
      for (info in model_info) {
        if (info$exposure == exposure && info$modifier == modifier) {
          matching_model_info <- info
          break
        }
      }

      if (!is.null(matching_model_info)) {
        model_name <- matching_model_info$model_name
        model <- model_list[[model_name]]

        # Extract the actual exposure and modifier variable names from the model
        exposure_var <- exposure
        modifier_var <- modifier

        # Calculate subgroup effects
        effects <- calculate_subgroup_effects(model, exposure_var, modifier_var)

        # Process odds ratios
        odds_data <- effects$odds_ratios
        odds_data$Exposure <- exposure
        odds_df <- data.frame(
          Exposure = odds_data$Exposure,
          ModifierLevel = odds_data$ModifierLevel,
          OR = odds_data$OR,
          OR_LowerCI = odds_data$OR_LowerCI,
          OR_UpperCI = odds_data$OR_UpperCI,
          stringsAsFactors = FALSE
        )
        odds_results <- rbind(odds_results, odds_df)

        # Add blank row
        odds_results <- rbind(odds_results, data.frame(
          Exposure = "",
          ModifierLevel = "",
          OR = NA,
          OR_LowerCI = NA,
          OR_UpperCI = NA,
          stringsAsFactors = FALSE
        ))

        # Process interaction p-values
        p_data <- effects$interaction_p_values
        if (nrow(p_data) > 0) {
          p_data$Exposure <- exposure
          p_df <- data.frame(
            Exposure = p_data$Exposure,
            ModifierLevel = p_data$ModifierLevel,
            Interaction_P = p_data$Interaction_P,
            stringsAsFactors = FALSE
          )
          int_p_results <- rbind(int_p_results, p_df)

          # Add blank row
          int_p_results <- rbind(int_p_results, data.frame(
            Exposure = "",
            ModifierLevel = "",
            Interaction_P = NA,
            stringsAsFactors = FALSE
          ))
        }

        # Process RERI
        reri_data <- effects$reri
        if (nrow(reri_data) > 0) {
          reri_data$Exposure <- exposure
          reri_df <- data.frame(
            Exposure = reri_data$Exposure,
            ModifierLevel = reri_data$ModifierLevel,
            RERI = reri_data$RERI,
            RERI_LowerCI = reri_data$RERI_LowerCI,
            RERI_UpperCI = reri_data$RERI_UpperCI,
            stringsAsFactors = FALSE
          )
          reri_results <- rbind(reri_results, reri_df)

          # Add blank row
          reri_results <- rbind(reri_results, data.frame(
            Exposure = "",
            ModifierLevel = "",
            RERI = NA,
            RERI_LowerCI = NA,
            RERI_UpperCI = NA,
            stringsAsFactors = FALSE
          ))
        }
      }
    }

    # Write all results to their respective sheets
    if (nrow(odds_results) > 0) {
      writeDataTable(wb_odds_ratios,
        sheet = sheet_name,
        x = odds_results,
        startRow = 1, startCol = 1,
        tableStyle = "TableStyleMedium2"
      )
    }

    if (nrow(int_p_results) > 0) {
      writeDataTable(wb_interaction_p,
        sheet = sheet_name,
        x = int_p_results,
        startRow = 1, startCol = 1,
        tableStyle = "TableStyleMedium2"
      )
    }

    if (nrow(reri_results) > 0) {
      writeDataTable(wb_reri,
        sheet = sheet_name,
        x = reri_results,
        startRow = 1, startCol = 1,
        tableStyle = "TableStyleMedium2"
      )
    }

    # Format numbers for better readability
    if (nrow(odds_results) > 0) {
      setColWidths(wb_odds_ratios, sheet = sheet_name, cols = 1:5, widths = c(30, 20, 15, 15, 15))

      # Format numbers using style
      for (i in 2:(nrow(odds_results) + 1)) {
        for (j in 3:5) {
          style <- createStyle(numFmt = "0.00")
          addStyle(wb_odds_ratios, sheet = sheet_name, style = style, rows = i, cols = j)
        }
      }
    }

    if (nrow(int_p_results) > 0) {
      setColWidths(wb_interaction_p, sheet = sheet_name, cols = 1:3, widths = c(30, 20, 15))

      # Format p-values
      for (i in 2:(nrow(int_p_results) + 1)) {
        style <- createStyle(numFmt = "0.0000")
        addStyle(wb_interaction_p, sheet = sheet_name, style = style, rows = i, cols = 3)
      }
    }

    if (nrow(reri_results) > 0) {
      setColWidths(wb_reri, sheet = sheet_name, cols = 1:5, widths = c(30, 20, 15, 15, 15))

      # Format numbers
      for (i in 2:(nrow(reri_results) + 1)) {
        for (j in 3:5) {
          style <- createStyle(numFmt = "0.00")
          addStyle(wb_reri, sheet = sheet_name, style = style, rows = i, cols = j)
        }
      }
    }
  }

  # Save the workbooks
  saveWorkbook(wb_odds_ratios, here(path_output, "subgroup_odds_ratios.xlsx"), overwrite = TRUE)
  saveWorkbook(wb_interaction_p, here(path_output, "subgroup_interaction_pvalues.xlsx"), overwrite = TRUE)
  saveWorkbook(wb_reri, here(path_output, "subgroup_reri.xlsx"), overwrite = TRUE)

  return(list(
    odds_ratios_wb = wb_odds_ratios,
    interaction_p_wb = wb_interaction_p,
    reri_wb = wb_reri
  ))
}

