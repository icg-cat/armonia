#' Flag potentially duplicate cases across waves
#'
#' @description Identifies rows from \emph{different} waves that share the same
#' case identifier and whose non-identifier columns are highly similar. This
#' situation arises when the same case is entered in multiple yearly datasets
#' and the rows reappear after binding waves together with \code{harm_bind_waves()}.
#'
#' Similarity is computed pairwise for each group of rows sharing the same
#' \code{id_col} value across different waves:
#' \code{similarity = n_matching_cols / n_total_cols}, where
#' \code{NA == NA} is counted as a match.
#'
#' @param data A data.frame or tibble, typically the output of
#'   \code{harm_bind_waves()}.
#' @param id_col Character. Name of the case identifier column.
#'   Defaults to \code{"id"}.
#' @param wave_col Character. Name of the wave identifier column.
#'   Defaults to \code{"source_wave"}.
#' @param threshold Numeric in \code{[0, 1]}. Minimum similarity for a pair of
#'   rows to be flagged as potential duplicates. Defaults to \code{0.90}.
#'
#' @return The input \code{data} with an added logical column
#'   \code{potential_duplicate}. A summary tibble of flagged pairs (with columns
#'   \code{id}, \code{row_a}, \code{wave_a}, \code{row_b}, \code{wave_b},
#'   \code{similarity}) is attached as the attribute \code{"duplicate_pairs"}.
#'
#' @seealso \code{\link{harm_bind_waves}} for combining waves before deduplication.
#'
#' @examples
#' combined <- data.frame(
#'   id          = c("alice", "alice", "bob"),
#'   gender      = c("Female", "Female", "Male"),
#'   wellbeing   = c(8, 8, 5),
#'   source_wave = c("w1", "w2", "w1")
#' )
#' flag_duplicate_cases(combined)
#'
#' @export
flag_duplicate_cases <- function(data,
                                  id_col    = "id",
                                  wave_col  = "source_wave",
                                  threshold = 0.90) {

  # 1. Validate inputs ----------------------------------------------------------
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame or tibble.")
  }
  if (!id_col %in% names(data)) {
    cli::cli_abort("Column {.var {id_col}} not found in {.arg data}. Set {.arg id_col} correctly.")
  }
  if (!wave_col %in% names(data)) {
    cli::cli_abort("Column {.var {wave_col}} not found in {.arg data}. Set {.arg wave_col} correctly.")
  }
  if (!is.numeric(threshold) || length(threshold) != 1 || threshold < 0 || threshold > 1) {
    cli::cli_abort("{.arg threshold} must be a single number between 0 and 1.")
  }

  # 2. Identify comparison columns ----------------------------------------------
  compare_cols <- setdiff(names(data), c(id_col, wave_col))

  # 3. For each id that appears in more than one wave, compare all row pairs ----
  ids_multiwave <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_col))) |>
    dplyr::summarise(
      n_waves = dplyr::n_distinct(.data[[wave_col]]),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$n_waves > 1) |>
    dplyr::pull(dplyr::all_of(id_col))

  flagged_rows <- integer(0)
  pair_summary <- list()

  for (id_val in ids_multiwave) {
    idx <- which(data[[id_col]] == id_val)

    # All pairs of rows with different waves
    grid <- expand.grid(i = idx, j = idx, stringsAsFactors = FALSE)
    grid <- grid[grid$i < grid$j, ]

    for (k in seq_len(nrow(grid))) {
      ri <- grid$i[k]
      rj <- grid$j[k]

      # Skip same-wave pairs
      if (data[[wave_col]][ri] == data[[wave_col]][rj]) next

      # Similarity: proportion of matching values (NA == NA counts as match)
      row_i <- as.character(unlist(data[ri, compare_cols]))
      row_j <- as.character(unlist(data[rj, compare_cols]))

      n_total    <- length(compare_cols)
      n_match    <- sum(
        (is.na(data[ri, compare_cols]) & is.na(data[rj, compare_cols])) |
        (!is.na(data[ri, compare_cols]) & !is.na(data[rj, compare_cols]) & row_i == row_j)
      )
      similarity <- n_match / n_total

      if (similarity >= threshold) {
        flagged_rows <- unique(c(flagged_rows, ri, rj))
        pair_summary[[length(pair_summary) + 1]] <- tibble::tibble(
          id         = as.character(id_val),
          row_a      = ri,
          wave_a     = as.character(data[[wave_col]][ri]),
          row_b      = rj,
          wave_b     = as.character(data[[wave_col]][rj]),
          similarity = similarity
        )
      }
    }
  }

  # 4. Add flag column to data --------------------------------------------------
  result <- data
  result$potential_duplicate <- seq_len(nrow(data)) %in% flagged_rows

  # 5. Build summary tibble -----------------------------------------------------
  if (length(pair_summary) > 0) {
    summary_tbl <- dplyr::bind_rows(pair_summary)
    n_flagged   <- sum(result$potential_duplicate)
    cli::cli_alert_warning(
      "{n_flagged} row{?s} flagged as potential duplicate{?s} ({length(pair_summary)} pair{?s}, threshold = {threshold})."
    )
  } else {
    summary_tbl <- tibble::tibble(
      id         = character(),
      row_a      = integer(),
      wave_a     = character(),
      row_b      = integer(),
      wave_b     = character(),
      similarity = numeric()
    )
    cli::cli_alert_success("No potential duplicates found (threshold = {threshold}).")
  }

  attr(result, "duplicate_pairs") <- summary_tbl
  result
}
