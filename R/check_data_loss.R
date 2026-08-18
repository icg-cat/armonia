#' Check for data loss after transformation and binding
#'
#' @description Compares a list of original (pre-\code{dict_apply()}) data
#' frames against the master dataset produced by \code{harm_bind_waves()} to
#' detect four classes of information loss: missing rows, dropped variables,
#' new \code{NA} values introduced in mapped columns, and unexpected category
#' collapse in factor variables. Run this after \code{harm_bind_waves()} and
#' before \code{assign_standard_labels()}.
#'
#' @param data_list A named list of data frames as passed to \code{dict_init()},
#'   \strong{before} \code{dict_apply()}. List names must match the wave labels
#'   used in the dictionary (e.g. \code{list(w1 = ..., w2 = ...)} corresponds
#'   to dictionary columns \code{orig_w1}, \code{orig_w2}).
#' @param master A data frame or tibble, the output of \code{harm_bind_waves()}.
#'   Must contain a \code{source_wave} column.
#' @param wb_path Character string. Path to the \code{.xlsx} dictionary file,
#'   used to read \code{Variable_Map_Wide} for name translation.
#'
#' @return If all checks pass, returns \code{invisible(NULL)} and prints a
#'   success message. If issues are found, emits a warning and returns a tibble
#'   of flagged rows with columns \code{wave}, \code{target_name}, \code{check},
#'   \code{before}, \code{after}, \code{delta}, and \code{status}.
#'
#' @examples
#' wave1 <- data.frame(id = c("alice", "bob"), gender = c("Female", "Male"))
#'
#' dict_path <- tempfile(fileext = ".xlsx")
#' openxlsx::write.xlsx(
#'   list(Variable_Map_Wide = data.frame(
#'     target_name = c("id", "gender"),
#'     orig_w1     = c("id", "gender")
#'   )),
#'   file = dict_path
#' )
#'
#' master <- data.frame(
#'   id          = c("alice", "bob"),
#'   gender      = factor(c("1", "2")),
#'   source_wave = "w1"
#' )
#'
#' check_data_loss(list(w1 = wave1), master, dict_path)
#'
#' @importFrom rlang .data
#' @export
check_data_loss <- function(data_list, master, wb_path) {

  # 1. INPUT VALIDATION --------------------------------------------------------
  if (!is.list(data_list) || length(data_list) == 0) {
    cli::cli_abort("{.arg data_list} must be a non-empty named list of data frames.")
  }
  if (is.null(names(data_list)) || any(names(data_list) == "")) {
    cli::cli_abort("Every element of {.arg data_list} must be named (use the wave label as the name).")
  }
  if (!is.data.frame(master) || !"source_wave" %in% names(master)) {
    cli::cli_abort("{.arg master} must be a data frame with a {.var source_wave} column.")
  }
  missing_waves <- setdiff(names(data_list), unique(master$source_wave))
  if (length(missing_waves) > 0) {
    cli::cli_abort(c(
      "Wave label{?s} in {.arg data_list} not found in {.var source_wave} column of {.arg master}.",
      "x" = "Missing: {.val {missing_waves}}"
    ))
  }
  if (!file.exists(wb_path)) {
    cli::cli_abort("Dictionary file not found: {.file {wb_path}}")
  }

  # 2. LOAD DICTIONARY ---------------------------------------------------------
  w_map <- readxl::read_excel(wb_path, sheet = "Variable_Map_Wide", col_types = "text")
  names(w_map) <- tolower(names(w_map))

  # 3. BUILD REPORT ROWS -------------------------------------------------------
  report_rows <- list()

  for (nm in names(data_list)) {

    df_orig    <- janitor::clean_names(data_list[[nm]])
    wave_col   <- paste0("orig_", tolower(nm))
    master_wave <- master[master$source_wave == nm, , drop = FALSE]

    # Check 1: Row count -------------------------------------------------------
    before_n <- nrow(df_orig)
    after_n  <- nrow(master_wave)

    report_rows[[length(report_rows) + 1]] <- data.frame(
      wave        = nm,
      target_name = "(row count)",
      check       = "row_count",
      before      = as.character(before_n),
      after       = as.character(after_n),
      delta       = as.numeric(after_n - before_n),
      status      = ifelse(after_n == before_n, "ok", "warn"),
      stringsAsFactors = FALSE
    )

    # Resolve wave column in dictionary ----------------------------------------
    if (!wave_col %in% names(w_map)) {
      cli::cli_warn("Column {.val {wave_col}} not found in Variable_Map_Wide. Skipping wave {.val {nm}}.")
      next
    }

    map_sub <- w_map[!is.na(w_map[[wave_col]]) & !is.na(w_map$target_name),
                     c("target_name", wave_col), drop = FALSE]
    colnames(map_sub)[2] <- "orig_col"

    # Check 2: Dropped columns -------------------------------------------------
    dropped <- setdiff(names(df_orig), map_sub$orig_col)

    for (d in dropped) {
      report_rows[[length(report_rows) + 1]] <- data.frame(
        wave        = nm,
        target_name = d,
        check       = "dropped_column",
        before      = d,
        after       = NA_character_,
        delta       = NA_real_,
        status      = "warn",
        stringsAsFactors = FALSE
      )
    }

    # Checks 3 & 4: per mapped variable ----------------------------------------
    for (i in seq_len(nrow(map_sub))) {
      orig_col   <- map_sub$orig_col[i]
      target_col <- map_sub$target_name[i]

      if (!orig_col   %in% names(df_orig)) next
      if (!target_col %in% names(master))  next

      orig_vec   <- df_orig[[orig_col]]
      master_vec <- master_wave[[target_col]]

      # Check 3: NA inflation --------------------------------------------------
      before_na <- sum(is.na(orig_vec))
      after_na  <- sum(is.na(master_vec))
      delta_na  <- after_na - before_na

      report_rows[[length(report_rows) + 1]] <- data.frame(
        wave        = nm,
        target_name = target_col,
        check       = "na_count",
        before      = as.character(before_na),
        after       = as.character(after_na),
        delta       = as.numeric(delta_na),
        status      = ifelse(delta_na > 0, "warn", "ok"),
        stringsAsFactors = FALSE
      )

      # Check 4: Unique value count (factor columns in master only) ------------
      if (is.factor(master_vec)) {
        before_u <- dplyr::n_distinct(orig_vec,   na.rm = TRUE)
        after_u  <- dplyr::n_distinct(master_vec, na.rm = TRUE)
        delta_u  <- after_u - before_u

        report_rows[[length(report_rows) + 1]] <- data.frame(
          wave        = nm,
          target_name = target_col,
          check       = "unique_values",
          before      = as.character(before_u),
          after       = as.character(after_u),
          delta       = as.numeric(delta_u),
          status      = ifelse(delta_u < 0, "warn", "ok"),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # 4. ASSEMBLE AND RETURN -----------------------------------------------------
  report    <- tibble::as_tibble(dplyr::bind_rows(report_rows))
  warn_rows <- dplyr::filter(report, .data$status == "warn")
  n_issues  <- nrow(warn_rows)

  if (n_issues == 0) {
    cli::cli_alert_success("All transformation checks passed. No data loss detected.")
    return(invisible(NULL))
  }

  cli::cli_warn(c(
    "{n_issues} issue{?s} detected in the transformed data.",
    "i" = "Review the returned tibble for details.",
    "i" = "Common cause: factor levels in the data absent from {.sheet Factor_Levels} in the dictionary."
  ))

  return(warn_rows)
}
