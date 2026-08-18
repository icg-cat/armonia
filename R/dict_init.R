#' Initialize a data dictionary
#'
#' @description Scans a list of data frames and generates an Excel dictionary.
#' The workbook contains a wide variable map (one row per
#' concept, one column per wave), a factor levels sheet with pre-populated
#' XLOOKUP formulas linking back to the variable map, a change log, and a
#' read-only snapshot of the original metadata. The user is expected to review
#' and edit the dictionary before passing it to \code{dict_apply()}.
#'
#' @param data_list Named list of data frames, one per wave or data version.
#' @param match_by Character string. Strategy for aligning variables across
#'   waves. \code{"position"} (default) matches by column index, suitable when
#'   waves share the same structure or differ only by language. \code{"name"}
#'   matches alphabetically by variable name, suitable when column order
#'   cannot be trusted.
#' @param type_prefix Character string. Prefix prepended to auto-generated
#'   \code{target_name} values (e.g. \code{"var"} produces \code{var_001},
#'   \code{var_002}, ...).
#' @param save_path Character string. File path for the output \code{.xlsx}
#'   dictionary. Defaults to \code{"dictionary.xlsx"}.
#' @param max_unique overrides maximum number of unique values for
#' detect_factor_potential(). Defaults to NULL (thus applies heuristic rules).
#' See detect_factor_potential() for details.
#'
#' @return Invisibly returns \code{TRUE} on success. The primary output is the
#'   \code{.xlsx} file written to \code{save_path}.
#'
#' @examples
#' wave1 <- data.frame(id = c("alice", "bob"), gender = c("Female", "Male"))
#' wave2 <- data.frame(id = c("carol", "dave"), gender = c("Mujer", "Hombre"))
#'
#' out_path <- tempfile(fileext = ".xlsx")
#' dict_init(list(w1 = wave1, w2 = wave2), save_path = out_path)
#'
#'@importFrom rlang .data
#' @export
dict_init <- function(data_list,
                      match_by = "position",
                      type_prefix = "var",
                      save_path = "dictionary.xlsx",
                      max_unique = NULL) {

  # 1. SETUP & VALIDATION
  match_by <- match.arg(match_by, choices = c("position", "name"))

  if (!is.list(data_list) || length(data_list) == 0) {
    cli::cli_abort("Input {.arg data_list} must be a non-empty list of data frames.")
  }
  if (file.exists(save_path)) {
    cli::cli_alert_warning("File {.file {save_path}} already exists. Overwriting...")
  }

  # 2. SANITIZATION
  cli::cli_alert_info("Sanitizing input names (janitor::clean_names)...")
  clean_list <- list()
  for (n in names(data_list)) {
    clean_list[[n]] <- janitor::clean_names(data_list[[n]])
  }

  # 3. BUILD WIDE MAP
  cli::cli_alert_info("Building map using strategy: {.strong {match_by}}")

  if (match_by == "name") {
    # --- STRATEGY A: MATCH BY NAME (Alphabetical) ---
    all_vars_list <- lapply(clean_list, names)
    unique_vars <- sort(unique(unlist(all_vars_list)))

    wide_map <- data.frame(
      target_name = paste0(type_prefix, "_", sprintf("%03d", seq_along(unique_vars))),
      description = NA_character_,
      stringsAsFactors = FALSE
    )

    for (wave_name in names(clean_list)) {
      col_name <- paste0("orig_", tolower(wave_name))
      is_present <- unique_vars %in% names(clean_list[[wave_name]])
      wide_map[[col_name]] <- ifelse(is_present, unique_vars, NA_character_)
    }

  } else {
    # --- STRATEGY B: MATCH BY POSITION (Structural) ---

    max_cols <- max(vapply(clean_list, ncol, integer(1)))

    wide_map <- data.frame(
      target_name = paste0(type_prefix, "_", sprintf("%03d", 1:max_cols)),
      description = NA_character_,
      stringsAsFactors = FALSE
    )

    for (wave_name in names(clean_list)) {
      col_name <- paste0("orig_", tolower(wave_name))
      current_names <- names(clean_list[[wave_name]])

      length(current_names) <- max_cols
      wide_map[[col_name]] <- current_names
    }
  }

  # 4. FACTOR MINING
  factor_map <- mine_factors(clean_list, wide_map, max_unique = max_unique)

  # 5. EXCEL GENERATION
  write_dictionary_workbook(wide_map, factor_map, match_by, save_path)

  return(invisible(TRUE))
}

