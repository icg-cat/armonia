#' Convert a multi-value column into dummy indicator columns
#'
#' @description Replaces a single column that contains separator-delimited
#' values (e.g. \code{"apple; banana"}) with one logical column per possible
#' category, TRUE/FALSE per row. Matching is by value identity, not position:
#' a value lands in the column bearing its name regardless of where it
#' appeared in the cell. Use \code{harm_detect_multival()} first to obtain the
#' full vocabulary to pass as \code{new_names}.
#'
#' @param data A data.frame or tibble.
#' @param col Character. Name of the column to convert.
#' @param new_names Character vector. The complete set of possible category
#'   values expected in \code{col} (e.g. from \code{harm_detect_multival()}).
#'   One logical output column is created per entry, using \code{new_names}
#'   as column names. Must be unique and must not collide with existing
#'   column names in \code{data} (other than \code{col} itself).
#' @param prefix Character vector. Prefix given to identify the variable of origin for resulting dummy variables
#' @param sep A single regex string used as the split delimiter.
#'   Defaults to \code{"[,;]"} (comma or semicolon).
#' @param trim_ws Logical. Whether to strip leading/trailing whitespace from
#'   each extracted value before matching against \code{new_names}.
#'   Defaults to \code{TRUE}.
#' @param fulldata Logical. Whether the results return the whole dataframe
#' with the dummy variables (TRUE, default), or only the dummies (FALSE).
#'
#' @return A data.frame (or tibble, if the input was a tibble) with \code{col}
#'   replaced by \code{length(new_names)} logical columns. Rows where
#'   \code{col} is \code{NA} produce \code{NA} in every new column. Values in
#'   \code{col} not present in \code{new_names} trigger a warning and are
#'   dropped from the output.
#'
#' @seealso \code{\link{detect_multival}} for obtaining \code{new_names}.
#'
#' @examples
#' wave1 <- data.frame(
#'   id       = c("alice", "bob"),
#'   symptoms = c("headache; cramps", "None")
#' )
#' new_vals <- detect_multival(wave1)
#' split_multival(wave1, col = "symptoms", new_names = new_vals$symptoms, prefix = "sympt")
#'
#' @export
#'
split_multival <- function(data, col, new_names, prefix, sep = "[,;]", trim_ws = TRUE, fulldata = TRUE) {

  # 1. Validate inputs ----------------------------------------------------------
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame or tibble.")
  }
  if (!is.character(col) || length(col) != 1) {
    cli::cli_abort("{.arg col} must be a single column name string.")
  }
  if (!col %in% names(data)) {
    cli::cli_abort("Column {.var {col}} not found in {.arg data}.")
  }
  if (!is.character(new_names) || length(new_names) == 0) {
    cli::cli_abort("{.arg new_names} must be a non-empty character vector.")
  }
  if (anyDuplicated(new_names) > 0) {
    cli::cli_abort("{.arg new_names} must not contain duplicate values.")
  }
  collide <- intersect(new_names, setdiff(names(data), col))
  if (length(collide) > 0) {
    cli::cli_abort(c(
      "{.arg new_names} collide{?s} with existing column name{?s}: {.field {collide}}.",
      "i" = "Rename the conflicting entries in {.arg new_names} before proceeding."
    ))
  }
  if (!is.character(sep) || length(sep) != 1) {
    cli::cli_abort("{.arg sep} must be a single regex string.")
  }
  tryCatch(
    grepl(sep, ""),
    error = function(e) cli::cli_abort("{.arg sep} is not a valid regex: {e$message}")
  )
  if (!is.logical(trim_ws) || length(trim_ws) != 1) {
    cli::cli_abort("{.arg trim_ws} must be TRUE or FALSE.")
  }

  n_new <- length(new_names)
  raw_vals <- data[[col]]

  # 2. Split each cell into atomic values ---------------------------------------
  parts_list <- strsplit(as.character(raw_vals), sep, perl = TRUE)
  parts_list <- lapply(parts_list, function(x) {
    if (trim_ws) x <- trimws(x)
    x[x != ""]
  })

  # 3. Build the logical matrix, row by row --------------------------------------
  ind_matrix <- matrix(NA, nrow = length(raw_vals), ncol = n_new,
                       dimnames = list(NULL, new_names))

  unmatched_rows <- integer(0)
  unmatched_vals <- character(0)

  for (i in seq_along(raw_vals)) {
    if (is.na(raw_vals[i])) next

    row_vals <- parts_list[[i]]
    ind_matrix[i, ] <- new_names %in% row_vals

    stray <- setdiff(row_vals, new_names)
    if (length(stray) > 0) {
      unmatched_rows <- c(unmatched_rows, i)
      unmatched_vals <- c(unmatched_vals, stray)
    }
  }

  if (length(unmatched_rows) > 0) {
    cli::cli_warn(c(
      "{length(unmatched_rows)} row{?s} contain value{?s} not present in {.arg new_names}.",
      "i" = "Unmatched value{?s}: {.val {unique(unmatched_vals)}}.",
      "i" = "Affected row{?s}: {unmatched_rows}."
    ))
  }

  # 4. Assemble new columns -------------------------------------------------------
  new_cols <- as.data.frame(ind_matrix, stringsAsFactors = FALSE)
  names(new_cols) <- paste0(prefix, "_", new_names)

  # 5. Insert in place of original column ------------------------------------------
  if(fulldata){
    col_pos <- which(names(data) == col)
    left    <- if (col_pos > 1) data[, seq_len(col_pos - 1), drop = FALSE] else NULL
    right   <- if (col_pos < ncol(data)) data[, seq(col_pos + 1, ncol(data)), drop = FALSE] else NULL

    result <- dplyr::bind_cols(left, new_cols, right)
  } else {
    result <- new_cols
  }

  if (tibble::is_tibble(data)) result <- tibble::as_tibble(result)

  result
}
