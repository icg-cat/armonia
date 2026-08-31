#' Write the dictionary workbook to disk
#'
#' @description Internal helper for \code{dict_init()}. Builds the full
#' workbook, the Instructions sheet, the styled \code{Variable_Map_Wide} sheet,
#' the \code{Factor_Levels} sheet (with embedded \code{XLOOKUP} formulas
#' resolving \code{target_name}), the \code{Change_Log} sheet, and the
#' \code{Original_Metadata} snapshot, then saves it to \code{save_path}.
#'
#' @param wide_map The wide variable map built by \code{dict_init()}.
#' @param factor_map The candidate factor rows built by \code{mine_factors()}
#'   (possibly zero rows).
#' @param match_by Character string. The variable-alignment strategy used by
#'   \code{dict_init()} (\code{"position"} or \code{"name"}), used for the
#'   Instructions text and the initial \code{Change_Log} entry.
#' @param save_path Character string. File path for the output \code{.xlsx}
#'   dictionary.
#'
#' @return Invisibly returns \code{TRUE} on success. The primary output is the
#'   \code{.xlsx} file written to \code{save_path}.
#'
#' @keywords internal
write_dictionary_workbook <- function(wide_map, factor_map, match_by, save_path) {
  wb <- openxlsx::createWorkbook()
  header_style <- openxlsx::createStyle(textDecoration = "bold", border = "Bottom")

  # Instructions
  instr_text <- c(
    "SHEET: Variable_Map_Wide",
    "On this sheet users can modify the variables to harmonize. You need to review that equivalent variables in data versions are set in the same row and are given the same 'target_name'. 'target_name's can be modified as needed. The order can be changed as well. Variables unwanted in the harmonized data can be dropped (rows removed). A 'version 0' of the initial metadata is saved in the sheet 'Original:Metadata' for traceback purposes. ",
    if(match_by == "position") {
    "CAUTION: Variables aligned by COLUMN POSITION. Verify that 'orig_w1' and 'orig_w2' actually correspond."
  } else {
    "Variables aligned by NAME. Merging different languages may require manual row moves."
  },
  "",
  "SHEET: Factor_Levels",
    "CAUTION: Column C (target_name) is calculated automatically from target_name in 'Variable_Map_Wide'. Requires Excel 2021/365.",
  "LEGACY FIX: If showing #NAME?, replace formula with VLOOKUP, i.e. (european standard):",
  "=IFERROR(INDEX('Variable_Map_Wide'!$A:$A; MATCH(B2; 'Variable_Map_Wide'!$C:$C; 0)); INDEX('Variable_Map_Wide'!$A:$A; MATCH(B2; 'Variable_Map_Wide'!$D:$D; 0)))",
  "",
    "",
  "SHEET: Original_Metadata",
    "This sheet is only meant to leave a registry of the original structure of the data. Not for harmonization purposes. "
  )
  # --- Sheet: Instructions ---
  openxlsx::addWorksheet(wb, "Instructions")
  openxlsx::writeData(wb, "Instructions", x = instr_text, startCol = 1, startRow = 1)

  # --- Sheet: Variable_Map_Wide ---
  openxlsx::addWorksheet(wb, "Variable_Map_Wide")
  openxlsx::writeData(wb, "Variable_Map_Wide", wide_map)
  openxlsx::addStyle(wb, "Variable_Map_Wide", header_style, rows = 1, cols = 1:ncol(wide_map))
  openxlsx::freezePane(wb, "Variable_Map_Wide", firstCol = TRUE)

  # --- Sheet: Factor Levels ---
  openxlsx::addWorksheet(wb, "Factor_Levels")

  if(nrow(factor_map) == 0){
    openxlsx::writeData(wb, "Factor_Levels", "No factors found")
  } else {
    openxlsx::writeDataTable(wb, "Factor_Levels",
                               x = factor_map,
                               tableName = "Table_Factors",
                               tableStyle = "TableStyleMedium2")

  # Embed FORMULAS with =XLOOKUP
  n_meta_rows <- nrow(wide_map) + 1

  # Create function arguments
  lookup_array_refC <- sprintf("'%s'!$C$2:$C$%d", "Variable_Map_Wide", n_meta_rows)
  lookup_array_refD <- sprintf("'%s'!$D$2:$D$%d", "Variable_Map_Wide", n_meta_rows)
  return_array_ref <- sprintf("'%s'!$A$2:$A$%d", "Variable_Map_Wide", n_meta_rows)
  n_data_rows <- nrow(factor_map)
  data_row_indices <- 2:(n_data_rows + 1)

  formula_vec <- paste0(
    "_xlfn.IFNA(",
    "_xlfn.XLOOKUP(",
    "B", data_row_indices, ", ",
    lookup_array_refC, ", ",
    return_array_ref,
    "), ",
    "_xlfn.XLOOKUP(",
    "B", data_row_indices, ", ",
    lookup_array_refD, ", ",
    return_array_ref,
    "))"
  )
  # Write formula into table
    openxlsx::writeFormula(
      wb,
      sheet = "Factor_Levels",
      x = formula_vec,
      startCol = 3,
      startRow = 2)
  }

  # --- Sheet: Change Log ---
  openxlsx::addWorksheet(wb, "Change_Log")
  openxlsx::writeData(wb, "Change_Log", data.frame(timestamp = as.character(Sys.time()), action = "INIT", match_strategy = match_by))

  # --- Sheet: Original_Metadata ---
  openxlsx::addWorksheet(wb, "Original_Metadata")
  openxlsx::writeData(wb, "Original_Metadata", wide_map)

  # --- SSave Workbook ---
  openxlsx::saveWorkbook(wb, save_path, overwrite = TRUE)
  cli::cli_alert_success("Dictionary initialized at {.file {save_path}}")

  return(invisible(TRUE))
}
