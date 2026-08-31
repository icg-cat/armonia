#' Append a New Longitudinal Timepoint
#'
#' @description Safely joins a new wave of data to an existing master dataset.
#' It automatically handles variable renaming (suffixing) to prevent collisions
#' and uses a full join to preserve all participants (attrition + recruitment).
#'
#' @param master_data A data.frame/tibble representing the current master
#' dataset (Time 1).
#' @param new_data A data.frame/tibble representing the new timepoint (Time N+1).
#'                 Must be already processed by dict_apply().
#' @param by_id Character string. The name of the distinct variable to join by
#' (e.g., "participant_id"). Must exist in both datasets.
#' @param suffix Character string. The suffix to append to new variables
#' (e.g., "_w2", "_t2"). Must start with an underscore/separator to ensure
#' readability and comply with snake_case referencing across objects.
#' @param verbose Logical. If TRUE, prints a summary of the join statistics.
#'
#' @return A single wide-format tibble containing both datasets.
#'
#' @examples
#' master   <- data.frame(id = c("alice", "bob"), wellbeing = c(8, 5))
#' new_wave <- data.frame(id = c("alice", "bob"), wellbeing = c(9, 6))
#' harm_add_timepoint(master, new_wave, by_id = "id", suffix = "_w2")
#'
#' @export
harm_add_timepoint <- function(master_data, new_data, by_id, suffix, verbose = TRUE) {

  # 1. FAIL FAST: Input Validation ------------------------------------------
  if (!is.data.frame(master_data) || !is.data.frame(new_data)) {
    cli::cli_abort("Inputs {.arg master_data} and {.arg new_data} must be data frames.")
  }

  if (!is.character(by_id) || length(by_id) != 1) {
    cli::cli_abort("{.arg by_id} must be a single character string.")
  }

  if (!by_id %in% names(master_data)) {
    cli::cli_abort("ID column {.val {by_id}} not found in {.arg master_data}.")
  }

  if (!by_id %in% names(new_data)) {
    cli::cli_abort("ID column {.val {by_id}} not found in {.arg new_data}.")
  }

  # Check for duplicate IDs in the NEW data (Critical for 1:1 Joining)
  if (any(duplicated(new_data[[by_id]]))) {
    n_dupes <- sum(duplicated(new_data[[by_id]]))
    cli::cli_abort(c(
      "x" = "Duplicate IDs detected in {.arg new_data}.",
      "i" = "Found {n_dupes} duplicate entries for ID {.val {by_id}}.",
      "!" = "Clean or aggregate the new wave before joining."
    ))
  }

  # 2. SUFFIX LOGIC: Isolate the New Wave -----------------------------------

  # Get all columns except the ID
  vars_to_rename <- setdiff(names(new_data), by_id)

  if (length(vars_to_rename) == 0) {
    cli::cli_warn("{.arg new_data} contains only the ID column. Nothing to join.")
    return(tibble::as_tibble(master_data))
  }

  # Apply suffix
  new_data_renamed <- new_data |>
    dplyr::rename_with(
      .fn = ~ paste0(., suffix),
      .cols = dplyr::all_of(vars_to_rename)
    )

  # 3. EXECUTE JOIN: Full Join ----------------------------------------------
  combined_data <- dplyr::full_join(master_data, new_data_renamed, by = by_id)

  # 4. AUDIT ----------------------------------------------------------------
  if (verbose) {
    # Calculate overlap stats
    ids_master <- master_data[[by_id]]
    ids_new    <- new_data[[by_id]]

    n_match    <- length(intersect(ids_master, ids_new))
    n_attrit   <- length(setdiff(ids_master, ids_new)) # Only in Master
    n_recruit  <- length(setdiff(ids_new, ids_master)) # Only in New

    cli::cli_h2("Longitudinal join audit")
    cli::cli_ul(c(
      "Matched (both waves): {.val {n_match}}",
      "Attrition (master only): {.val {n_attrit}}",
      "recruitment (new only): {.val {n_recruit}}",
      "Total participants: {.val {nrow(combined_data)}}"
    ))

    if (n_recruit > 0) {
      cli::cli_alert_info("New participants added. Check if these are valid new recruits or ID typos.")
    }
  }

  return(tibble::as_tibble(combined_data))
}
