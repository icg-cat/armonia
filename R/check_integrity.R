#' Final integrity check for harmonized data
#'
#' @description Verifies that the master dataset satisfies the following
#' standards: each combination of participant ID and time point must be unique
#' (tidy structure), and all factor columns must carry numeric-string levels as
#' produced by \code{dict_apply()}.
#'
#' @param data The final harmonized tibble, typically the output of
#'   \code{harm_bind_waves()} or \code{harm_add_timepoint()}.
#' @param id_col Character string. Name of the primary key column
#'   (e.g. \code{"pk_hash"}).
#' @param time_col Character string. Name of the column identifying the time
#'   point (e.g. \code{"source_wave"}). Defaults to \code{"timestamp"}. Set to
#'   \code{NULL} for data with no time dimension (e.g. single-wave or
#'   cross-sectional data); uniqueness is then checked on \code{id_col} alone.
#' @param verbose Logical. If \code{TRUE}, prints a success message on passing.
#'   Defaults to \code{TRUE}.
#'
#' @return Invisibly returns \code{TRUE} on success; aborts with an
#'   informative error if the data is non-tidy.
#'
#' @examples
#' data <- data.frame(
#'   id        = c("alice", "bob", "carol"),
#'   timestamp = c("w1", "w1", "w2"),
#'   gender    = factor(c("1", "2", "1"))
#' )
#' check_integrity(data, id_col = "id")
#'
#' # single-wave data with no time dimension
#' cross_sectional <- data.frame(id = c("alice", "bob"), gender = factor(c("1", "2")))
#' check_integrity(cross_sectional, id_col = "id", time_col = NULL)
#'
#' @export
check_integrity <- function(data, id_col, time_col = "timestamp", verbose = TRUE) {

  # 1. Tidy data check: unique ID (and time, unless time_col is NULL)
  key_cols   <- if (is.null(time_col)) id_col else c(id_col, time_col)
  duplicates <- duplicated(data[, key_cols])
  if (any(duplicates)) {
    n_dupes <- sum(duplicates)
    if (is.null(time_col)) {
      cli::cli_abort(c(
        "x" = "Integrity failure: non-tidy data detected.",
        "i" = "{.val {n_dupes}} duplicate values of {.var {id_col}} found.",
        "!" = "Each participant must have exactly one row."
      ))
    } else {
      cli::cli_abort(c(
        "x" = "Integrity failure: non-tidy data detected.",
        "i" = "{.val {n_dupes}} duplicate combinations of {.var {id_col}} and {.var {time_col}} found.",
        "!" = "Each participant must have exactly one row per time point."
      ))
    }
  }

  # 2. Factor coding check
  is_factor_check <- vapply(data, is.factor, logical(1))

  # Check if factor levels are numeric strings
  for (col in names(data)[is_factor_check]) {
    lvls <- levels(data[[col]])
    if (!all(grepl("^[0-9]+$", lvls))) {
      cli::cli_warn("Variable {.var {col}} is a factor but contains non-numeric labels.")
    }
  }

  # 3. Conditional success message
  if (verbose) {
    cli::cli_alert_success("Integrity check passed: Data is tidy and fulfills contract requirements.")
  }
  return(invisible(TRUE))
}
