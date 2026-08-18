#' Detect columns containing multi-value cells
#'
#' @description Scans every character column in a data frame for cells that
#' split into more than one value under \code{sep}, indicating multiple
#' choices stored in a single cell. Prints a message naming any flagged
#' columns, and invisibly returns a named list of each flagged column's
#' unique atomic values (alphabetically sorted), ready to reuse as
#' \code{new_names} in \code{harm_split_multival()}.
#'
#' @param data A data.frame or tibble to scan.
#' @param sep A single regex string used to split cells. Defaults to
#'   \code{"[,;]"} (comma or semicolon).
#' @param trim_ws Logical. Whether to strip leading/trailing whitespace from
#'   each extracted value before taking uniques. Defaults to \code{TRUE}.
#'
#' @return Invisibly returns a named list: one element per flagged column,
#'   each a sorted character vector of its unique values. Empty list if none
#'   are found.
#'
#' @seealso \code{\link{split_multival}}
#'
#' @examples
#' wave1 <- data.frame(
#'   id       = c("alice", "bob", "carol"),
#'   symptoms = c("headache; cramps", "None", "cramps; headache")
#' )
#' detect_multival(wave1)
#'
#' @export
#'
detect_multival <- function(data, sep = "[,;]", trim_ws = TRUE) {
  stopifnot(is.data.frame(data))

  char_cols <- names(data)[vapply(data, is.character, logical(1))]
  values <- list()

  for (col in char_cols) {
    vals  <- data[[col]]
    parts <- strsplit(vals[!is.na(vals)], sep, perl = TRUE)

    if (!any(lengths(parts) > 1)) next  # nothing split into >1 piece here

    atomic_vals <- unlist(parts, use.names = FALSE)
    if (trim_ws) atomic_vals <- trimws(atomic_vals)
    atomic_vals <- atomic_vals[atomic_vals != ""]

    values[[col]] <- sort(unique(atomic_vals))
  }

  if (length(values) > 0) {
    cli::cli_alert_info("Potential multiple-choice columns: {.field {names(values)}}")
  } else {
    cli::cli_alert_success("No multi-value columns detected.")
  }

  invisible(values)
}
