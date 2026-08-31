#' Mine candidate factor variables across waves for the dictionary
#'
#' @description Internal helper for \code{dict_init()}. Scans each wave in
#' \code{clean_list} for character columns that look categorical (via
#' \code{detect_factor_potential()}), and builds the raw \code{Factor_Levels}
#' rows for them: one row per unique value per flagged variable, with a
#' provisional \code{target_name} resolved from \code{wide_map}.
#'
#' @param clean_list Named list of sanitized data frames (post
#'   \code{janitor::clean_names()}), one per wave.
#' @param wide_map The wide variable map built by \code{dict_init()}, used to
#'   resolve each factor row's \code{target_name}.
#' @param max_unique Passed through to \code{detect_factor_potential()}.
#'   Defaults to \code{NULL}.
#'
#' @return A data.frame of candidate factor rows (possibly zero rows), with
#'   columns \code{wave}, \code{original_variable}, \code{target_name},
#'   \code{original_level}, \code{standard_code}, \code{standard_label}.
#'
#' @keywords internal
mine_factors <- function(clean_list, wide_map, max_unique = NULL) {
  factor_rows <- list()
  current_row_idx <- 2

  for (nm in names(clean_list)) {
    df <- clean_list[[nm]]
    # Detect factors
    vars <- names(df)[vapply(df, function(x) is.factor(detect_factor_potential(x, max_unique = max_unique)), logical(1))]

    not_factors <- setdiff(names(df)[sapply(df, is.character)], vars)
    cli::cli_alert_info("The following variables are *not* identified as factors: {not_factors}")

    if (length(vars) > 0) {
      wave_col_name <- paste0("orig_", tolower(nm))
      target_col_idx <- which(names(wide_map) == wave_col_name)
      col_letter <- openxlsx::int2col(target_col_idx)

      for (v in vars) {
        vals <- sort(unique(df[[v]]))
        n <- length(vals)

        factor_rows[[length(factor_rows) + 1]] <- data.frame(
          wave = wave_col_name,
          original_variable = v,
          target_name = NA, # formula goes here
          original_level = as.character(vals),
          standard_code = seq_along(vals),
          standard_label = as.character(vals),
          stringsAsFactors = FALSE
        )
        current_row_idx <- current_row_idx + n
      }
    }
  }

  factor_map <- dplyr::bind_rows(factor_rows)

  # Resolve target_name from wide_map for sorting (column C is NA until XLOOKUP runs in Excel).
  # The XLOOKUP formula written later will override this column, so pre-filling is safe.
  if (nrow(factor_map) > 0) {
    factor_map$target_name <- mapply(function(wave_col, orig_var) {
      matched <- wide_map$target_name[wide_map[[wave_col]] == orig_var]
      if (length(matched) == 1) matched else NA_character_
    }, factor_map$wave, factor_map$original_variable)

    factor_map <- dplyr::arrange(factor_map, .data$target_name, .data$standard_code)
  }

  factor_map
}
