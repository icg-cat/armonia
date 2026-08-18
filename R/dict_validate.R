#' Validate an armonia dictionary file
#'
#' @description Performs a fail-fast schema check on an Excel dictionary
#' produced by \code{dict_init()}. Verifies that required sheets are present,
#' \code{target_name} values are unique and snake_case-compliant, and that the
#' factor levels sheet contains the expected columns and numeric
#' \code{standard_code} values.
#'
#' @param path Character string. Path to the \code{.xlsx} dictionary file.
#'
#' @return Invisibly returns \code{TRUE} on success; aborts with an informative
#' error on the first validation failure.
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
#' dict_validate(dict_path)
#'
#' @export
dict_validate <- function(path) {

  # 1. Pre checks -------------- --------------------------------------------
  if (!file.exists(path)) {
    cli::cli_abort("Dictionary file not found: {.file {path}}")
  }

  cli::cli_alert_info("Validating dictionary structure...")

  # 2. SHEET CHECK ----------------------------------------------------------
  sheets <- readxl::excel_sheets(path)
  required <- c("Variable_Map_Wide", "Factor_Levels")
  missing <- setdiff(required, sheets)

  if (length(missing) > 0) {
    cli::cli_abort("Missing critical sheets: {.val {missing}}")
  }

  # 3. VARIABLE MAP VALIDATION ----------------------------------------------
  wide_map <- readxl::read_excel(path, sheet = "Variable_Map_Wide", col_types = "text")

  # Enforce lowercase headers
  names(wide_map) <- tolower(names(wide_map))

  if (!"target_name" %in% names(wide_map)) {
    cli::cli_abort("Sheet 'Variable_Map_Wide' is missing the {.var target_name} column.")
  }

  # Check for duplicates (imp for Primary Keys)
  targets <- wide_map$target_name[!is.na(wide_map$target_name)]

  if (any(duplicated(targets))) {
    dupes <- unique(targets[duplicated(targets)])
    cli::cli_abort(c(
      "Duplicate {.var target_name} detected.",
      "x" = "Concepts must be unique: {.val {dupes}}"
    ))
  }

  # Check for "snake_case" compliance in target_name
  # Regex: starts with letter, only contains letters, numbers, underscores
  bad_names <- targets[!stringr::str_detect(targets, "^[a-z][a-z0-9_]*$")]
  if (length(bad_names) > 0) {
    cli::cli_alert_danger("The following target_names violate 'snake_case' rules:")
    cli::cli_ul(utils::head(bad_names, 5))
    cli::cli_abort("All target_names must be lowercase, no spaces, no special chars.")
  }

  # 4. FACTOR MAP VALIDATION ------------------------------------------------
  f_map <- readxl::read_excel(path, sheet = "Factor_Levels")
  names(f_map) <- tolower(names(f_map))

  if(ncol(f_map) == 1 && names(f_map) == "no factors found"){
    cli::cli_alert_danger("No factors found in dataset. Skipping review. ")
  } else {


  req_cols <- c("target_name", "standard_code", "standard_label")
  missing_f <- setdiff(req_cols, names(f_map))

  if (length(missing_f) > 0) {
    cli::cli_abort("Sheet 'Factor_Levels' missing columns: {.val {missing_f}}")
  }

  # Ensure standard_code is numeric. readxl might have read it as text if there were typos.
  if (!is.numeric(f_map$standard_code)) {
    # Try to coerce
    coerced <- suppressWarnings(as.numeric(f_map$standard_code))
    if (any(is.na(coerced) & !is.na(f_map$standard_code))) {
      cli::cli_abort("Column {.var standard_code} contains non-numeric values. It must be strictly integers.")
    }
  }

  # Orphan Check: Factor targets that don't exist in Wide Map
  orphans <- setdiff(unique(f_map$target_name), unique(wide_map$target_name))
  if (length(orphans) > 0) {
    cli::cli_alert_warning(
      "Found {length(orphans)} orphaned factor definitions (not in Wide Map): {orphans}. \nThey will be ignored.")
  }

  # Label Conflict Check: each standard_code within a target_name must map to
  # exactly one standard_label across all waves. Conflicts mean assign_standard_labels()
  # would produce ambiguous results.
  # Rows with NA target_name are skipped: in a freshly generated dict the XLOOKUP
  # formula in Factor_Levels has not yet been evaluated by Excel, so readxl returns NA.
  # The check only applies to user-edited dicts where target_name is resolved.
  f_map_linked <- dplyr::filter(f_map, !is.na(.data$target_name))

  label_conflicts <- f_map_linked |>
    dplyr::group_by(.data$target_name, .data$standard_code) |>
    dplyr::summarise(
      n_labels = dplyr::n_distinct(.data$standard_label, na.rm = TRUE),
      .groups  = "drop"
    ) |>
    dplyr::filter(.data$n_labels > 1)

  if (nrow(label_conflicts) > 0) {
    conflict_vars <- unique(label_conflicts$target_name)
    cli::cli_abort(c(
      "Factor label conflict detected in {.sheet Factor_Levels}.",
      "x" = "Variable{?s} with conflicting {.var standard_label}s for the same {.var standard_code}: {.val {conflict_vars}}",
      "i" = "Each {.var standard_code} within a {.var target_name} must map to exactly one {.var standard_label} across all waves."
    ))
  }
 }

  cli::cli_alert_success("Dictionary {.file {basename(path)}} passed validation.")
  return(invisible(TRUE))
}
