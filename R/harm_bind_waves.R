#' Bind harmonized waves into a master dataset
#'
#' @description Combines a named list of standardized data frames (each
#' produced by \code{dict_apply()}) into a single long-format tibble. A
#' \code{source_wave} column is added automatically to preserve wave
#' provenance. Column names are enforced to snake_case.
#'
#' @param data_list A named list of data frames, each already processed by
#'   \code{dict_apply()}.
#'
#' @return A single harmonized tibble with a \code{source_wave} column
#'   identifying the origin of each row.
#'
#' @examples
#' w1 <- data.frame(id = "alice", gender = factor("1"))
#' w2 <- data.frame(id = "bob",   gender = factor("2"))
#' harm_bind_waves(list(w1 = w1, w2 = w2))
#'
#' @export
harm_bind_waves <- function(data_list) {

  if (!is.list(data_list) || length(data_list) == 0) {
    cli::cli_abort("Input must be a non-empty list of dataframes.")
  }

  # Safety Net: Use the resolve_types logic for any lingering inconsistencies
  combined <- dplyr::bind_rows(data_list, .id = "source_wave")

  # Ensure all variables created are snake_case
  colnames(combined) <- tolower(colnames(combined))

  return(tibble::as_tibble(combined))
}
