#' Check whether an armonia dictionary is applicable to a new dataset
#'
#' @description Compares a data frame against an existing armonia dictionary to
#' determine whether \code{dict_apply()} can be run without issues. Three
#' things are checked: (1) variables present in the data but absent from the
#' dictionary's \code{Variable_Map_Wide}; (2) dictionary variables not found in
#' the data (informational); (3) factor values present in the data but absent
#' from \code{Factor_Levels}. The dictionary structure is validated first via
#' \code{dict_validate()}, which aborts on malformed dictionaries.
#'
#' This function does \emph{not} abort on compatibility issues, it returns a
#' report for the user to act on.
#'
#' @param dict_path Character string. Path to the \code{.xlsx} dictionary file
#'   produced by \code{dict_init()}.
#' @param data A data.frame or tibble to check against the dictionary.
#' @param wave_col Character. Name of the wave identifier column in \code{data}
#'   (excluded from variable coverage checks) and, when \code{update = TRUE},
#'   the corresponding column name in \code{Variable_Map_Wide} where new
#'   variables are appended (e.g. \code{"orig_w1"}). Defaults to
#'   \code{"source_wave"}.
#' @param update Logical. If \code{TRUE}, writes the detected gaps into a
#'   timestamped copy of the dictionary file (same directory, named
#'   \code{yymmdd_<original>_copy.xlsx}). The original file is never modified.
#'   Defaults to \code{FALSE}.
#'
#' @return Invisibly returns a named list with three tibbles:
#' \describe{
#'   \item{\code{$missing_vars}}{Variables in \code{data} not covered by the dictionary.}
#'   \item{\code{$extra_vars}}{Dictionary variables not found in \code{data} (informational).}
#'   \item{\code{$missing_levels}}{Factor values in \code{data} not present in \code{Factor_Levels},
#'     with columns \code{target_name} and \code{missing_value}.}
#' }
#'
#' @seealso \code{\link{dict_validate}} for structural dictionary validation,
#'   \code{\link{dict_apply}} for applying the dictionary.
#'
#' @examples
#' dict_path <- tempfile(fileext = ".xlsx")
#' openxlsx::write.xlsx(
#'   list(
#'     Variable_Map_Wide = data.frame(
#'       target_name = c("id", "gender"),
#'       orig_w1     = c("id", "gender")
#'     ),
#'     Factor_Levels = data.frame(
#'       wave              = "orig_w1",
#'       original_variable = "gender",
#'       target_name       = "gender",
#'       original_level    = c("Female", "Male"),
#'       standard_code     = c(1, 2),
#'       standard_label    = c("Female", "Male")
#'     )
#'   ),
#'   file = dict_path
#' )
#'
#' new_wave <- data.frame(
#'   source_wave = "w1",
#'   id          = c("alice", "bob"),
#'   gender      = c("Female", "Male"),
#'   wellbeing   = c(8, 5)   # not yet in the dictionary
#' )
#'
#' result <- dict_check_compat(dict_path, new_wave)
#' result$missing_vars
#'
#' @export
dict_check_compat <- function(dict_path, data, wave_col = "source_wave", update = FALSE) {

  # 1. Validate inputs ----------------------------------------------------------
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame or tibble.")
  }

  # Reuse dict_validate() for structural check, aborts if dict is malformed
  dict_validate(dict_path)

  # 2. Read dictionary sheets ---------------------------------------------------
  wide_map  <- readxl::read_excel(dict_path, sheet = "Variable_Map_Wide", col_types = "text")
  names(wide_map) <- tolower(names(wide_map))

  f_map <- readxl::read_excel(dict_path, sheet = "Factor_Levels", col_types = "text")
  names(f_map) <- tolower(names(f_map))

  dict_targets <- unique(wide_map$target_name[!is.na(wide_map$target_name)])

  # 3. Variables in data not covered by dict ------------------------------------
  data_cols <- setdiff(names(data), wave_col)

  missing_vars <- tibble::tibble(
    variable = setdiff(data_cols, dict_targets)
  )

  # 4. Variables in dict not found in data (informational) ----------------------
  extra_vars <- tibble::tibble(
    variable = setdiff(dict_targets, data_cols)
  )

  # 5. Factor values in data not present in Factor_Levels ----------------------
  # Only applicable when Factor_Levels is non-trivial
  missing_levels <- tibble::tibble(
    target_name    = character(),
    missing_value  = character()
  )

  has_factors <- !(ncol(f_map) == 1 && names(f_map) == "no factors found")

  if (has_factors && "target_name" %in% names(f_map) && "original_level" %in% names(f_map)) {

    # For each target_name that appears in Factor_Levels AND in data as a character col,
    # check whether all non-NA values in data appear somewhere as an original_level
    factor_targets <- unique(f_map$target_name[!is.na(f_map$target_name)])

    for (tgt in factor_targets) {
      if (!tgt %in% names(data)) next
      if (!is.character(data[[tgt]])) next

      known_levels <- unique(f_map$original_level[f_map$target_name == tgt & !is.na(f_map$original_level)])
      data_values  <- unique(data[[tgt]][!is.na(data[[tgt]])])

      new_vals <- setdiff(data_values, known_levels)
      if (length(new_vals) > 0) {
        missing_levels <- dplyr::bind_rows(
          missing_levels,
          tibble::tibble(target_name = tgt, missing_value = new_vals)
        )
      }
    }
  }

  # 6. CLI summary --------------------------------------------------------------
  cli::cli_h1("Dictionary compatibility report")

  if (nrow(missing_vars) == 0) {
    cli::cli_alert_success("All data variables are covered by the dictionary.")
  } else {
    cli::cli_alert_warning(
      "{nrow(missing_vars)} variable{?s} in data not covered by dictionary ({.field $missing_vars}):"
    )
    cli::cli_ul(missing_vars$variable)
  }

  if (nrow(extra_vars) > 0) {
    cli::cli_alert_info(
      "{nrow(extra_vars)} dictionary variable{?s} not found in data ({.field $extra_vars})."
    )
  }

  if (nrow(missing_levels) == 0) {
    cli::cli_alert_success("All factor values in data are covered by the dictionary.")
  } else {
    n_vars <- dplyr::n_distinct(missing_levels$target_name)
    cli::cli_alert_warning(
      "{nrow(missing_levels)} factor value{?s} across {n_vars} variable{?s} not in Factor_Levels ({.field $missing_levels})."
    )
  }

  # 7. Write-back to a timestamped copy ----------------------------------------
  if (update) {
    write_dict_compat_update(dict_path, wave_col, wide_map, f_map, missing_vars, missing_levels)
  }

  invisible(list(
    missing_vars   = missing_vars,
    extra_vars     = extra_vars,
    missing_levels = missing_levels
  ))
}
