#' Write detected dictionary gaps to a timestamped copy
#'
#' @description Internal helper for \code{dict_check_compat(update = TRUE)}.
#' Appends the variables and factor levels missing from the dictionary into a
#' new, timestamped copy of the workbook (\code{yymmdd_<original>_copy.xlsx}),
#' leaving the original file untouched. If neither \code{missing_vars} nor
#' \code{missing_levels} has any rows, no file is written.
#'
#' @param dict_path Character string. Path to the original \code{.xlsx}
#'   dictionary file.
#' @param wave_col Character. Name of the wave identifier column in
#'   \code{Variable_Map_Wide} where new variables are appended.
#' @param wide_map The \code{Variable_Map_Wide} sheet, as read by
#'   \code{dict_check_compat()}.
#' @param f_map The \code{Factor_Levels} sheet, as read by
#'   \code{dict_check_compat()}.
#' @param missing_vars Tibble of variables in the data not covered by the
#'   dictionary (as returned by \code{dict_check_compat()}).
#' @param missing_levels Tibble of factor values in the data not present in
#'   \code{Factor_Levels} (as returned by \code{dict_check_compat()}).
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect of
#'   writing the timestamped copy.
#'
#' @keywords internal
write_dict_compat_update <- function(dict_path, wave_col, wide_map, f_map, missing_vars, missing_levels) {

  nothing_to_add <- nrow(missing_vars) == 0 && nrow(missing_levels) == 0
  if (nothing_to_add) {
    cli::cli_alert_info("Nothing to update, no gaps found.")
    return(invisible(NULL))
  }

  # Validate that wave_col exists in Variable_Map_Wide
  if (!wave_col %in% names(wide_map)) {
    cli::cli_abort(c(
      "{.arg wave_col} {.val {wave_col}} not found as a column in {.sheet Variable_Map_Wide}.",
      "i" = "Available wave columns: {.val {setdiff(names(wide_map), c('target_name', 'description'))}}"
    ))
  }

  wb <- openxlsx::loadWorkbook(dict_path)

  # -- A. Variable_Map_Wide -------------------------------------------------
  if (nrow(missing_vars) > 0) {
    wave_col_idx  <- which(names(wide_map) == wave_col)
    first_new_row <- nrow(wide_map) + 2   # +1 for header, +1 to go past last data row

    openxlsx::writeData(
      wb,
      sheet    = "Variable_Map_Wide",
      x        = missing_vars$variable,
      startCol = wave_col_idx,
      startRow = first_new_row,
      colNames = FALSE
    )
  }

  # -- B. Factor_Levels, full table rewrite -----------------------------------
  # openxlsx has no public API to extend an existing table's ref range, so
  # appending rows outside the table breaks filtering/sorting. Fix: read the
  # existing data, combine with new rows, and recreate the sheet as a fresh table.
  if (nrow(missing_levels) > 0) {
    new_rows <- tibble::tibble(
      wave              = wave_col,
      original_variable = vapply(
        missing_levels$target_name,
        function(tgt) {
          v <- wide_map[[wave_col]][wide_map$target_name == tgt]
          if (length(v) == 1 && !is.na(v)) v else NA_character_
        },
        character(1)
      ),
      target_name    = missing_levels$target_name,
      original_level = missing_levels$missing_value,
      standard_code  = NA_character_,
      standard_label = NA_character_
    )

    combined_f_map <- dplyr::bind_rows(f_map, new_rows)

    # Recreate the sheet so all rows fall inside the table range
    fl_pos <- which(names(wb) == "Factor_Levels")
    openxlsx::removeWorksheet(wb, "Factor_Levels")
    openxlsx::addWorksheet(wb, "Factor_Levels")
    openxlsx::writeDataTable(wb, "Factor_Levels", combined_f_map,
                             tableName  = "Table_Factors",
                             tableStyle = "TableStyleMedium2")

    # Restore the original sheet position
    n         <- length(names(wb))
    new_order <- append(seq_len(n - 1L), n, after = fl_pos - 1L)
    openxlsx::worksheetOrder(wb) <- new_order
  }

  # -- Save to timestamped copy ---------------------------------------------
  ts        <- format(Sys.time(), "%y%m%d")
  base      <- tools::file_path_sans_ext(basename(dict_path))
  copy_path <- file.path(dirname(dict_path), paste0(ts, "_", base, "_copy.xlsx"))
  openxlsx::saveWorkbook(wb, copy_path, overwrite = TRUE)

  cli::cli_alert_success("Updated dictionary saved to {.file {copy_path}}")

  invisible(NULL)
}
