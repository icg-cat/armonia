#' Check class consistency across waves before binding
#'
#' @description Compares the R class of every column across a list of
#' standardized data frames (each produced by \code{dict_apply()}) and reports
#' any \code{target_name} variables whose class differs between waves. Run this
#' after \code{dict_apply()} and before \code{harm_bind_waves()} to catch
#' coercions that \code{dplyr::bind_rows()} would apply silently.
#'
#' @param data_list A named list of data frames, each already processed by
#'   \code{dict_apply()}.
#'
#' @return If no conflicts are found, returns \code{invisible(NULL)} and prints
#'   a success message. If conflicts are found, emits a warning and returns a
#'   tibble with columns \code{target_name}, \code{wave}, and \code{r_class}
#'  , one row per (variable, wave) pair involved in a conflict.
#'
#' @examples
#' wave1 <- data.frame(gender = factor(c("1", "2")), wellbeing = c(8, 5))
#' wave2 <- data.frame(gender = factor(c("2", "1")), wellbeing = c(9, 6))
#' check_bind_ready(list(w1 = wave1, w2 = wave2))
#'
#' @importFrom rlang .data
#' @export
check_bind_ready <- function(data_list) {

  if (!is.list(data_list) || length(data_list) == 0) {
    cli::cli_abort("{.arg data_list} must be a non-empty named list of data frames.")
  }
  if (is.null(names(data_list)) || any(names(data_list) == "")) {
    cli::cli_abort("Every element of {.arg data_list} must be named (use the wave label as the name).")
  }

  # Collect (target_name, wave, r_class) for every column in every wave
  class_rows <- list()
  for (nm in names(data_list)) {
    df <- data_list[[nm]]
    for (col in names(df)) {
      class_rows[[length(class_rows) + 1]] <- data.frame(
        target_name = col,
        wave        = nm,
        r_class     = class(df[[col]])[1],
        stringsAsFactors = FALSE
      )
    }
  }

  class_tbl <- tibble::as_tibble(dplyr::bind_rows(class_rows))

  # Find variables where more than one distinct r_class exists across waves
  conflict_vars <- class_tbl |>
    dplyr::group_by(.data$target_name) |>
    dplyr::summarise(n_classes = dplyr::n_distinct(.data$r_class), .groups = "drop") |>
    dplyr::filter(.data$n_classes > 1) |>
    dplyr::pull(.data$target_name)

  if (length(conflict_vars) == 0) {
    cli::cli_alert_success("All columns have consistent classes across waves. Ready to bind.")
    return(invisible(NULL))
  }

  conflicts <- dplyr::filter(class_tbl, .data$target_name %in% conflict_vars) |>
    dplyr::arrange(.data$target_name, .data$wave)

  cli::cli_warn(c(
    "{length(conflict_vars)} variable{?s} have mismatched classes across waves.",
    "i" = "These will be silently coerced by {.fn harm_bind_waves}.",
    "i" = "Review the returned tibble and add explicit coercions if needed."
  ))

  return(conflicts)
}
