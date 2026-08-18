#' Merge two data dictionary workbooks
#'
#' Intended to work with Excel-based data dictionaries created by
#' \code{dict_init()}. Reads two dictionaries, merges their contents sheet by
#' sheet, appends a timestamped entry to the change log, and saves the result as
#'  a new dated workbook.
#' The use-case for this function is having one working version of a dictionary
#' when receiving a new data source to add, which generates a raw version of the
#' dictionary.
#'
#' The following content sheets are merged:
#' \code{Variable_Map_Wide}, \code{Factor_Levels}, and \code{Original_Metadata}.
#' The \code{Change_Log} sheet is always stacked without deduplication to
#' preserve history. All other sheets present in \code{dt2} are written as-is;
#' sheets exclusive to \code{dt1} are not carried over.
#'
#' Merge strategies:
#' Stacks both sources row-wise and retains distinct records across all columns.
#' Equivalent to a union with deduplication.
#'
#' @param dt1_path Character. Path to the first (older, ready) dictionary
#'  \code{.xlsx} file.
#' @param dt2_path Character. Path to the second (newer, raw) dictionary
#' \code{.xlsx} file.
#' @param save_dir Character. Directory where the merged file will be saved.
#' Defaults to \code{here::here()}.
#'
#' @return Invisibly returns the save path of the merged workbook.
#'
#' @examples
#' dict1 <- tempfile(fileext = ".xlsx")
#' openxlsx::write.xlsx(
#'   list(
#'     Variable_Map_Wide = data.frame(target_name = "id", orig_w1 = "id"),
#'     Factor_Levels      = data.frame(target_name = character(), original_level = character())
#'   ),
#'   file = dict1
#' )
#'
#' dict2 <- tempfile(fileext = ".xlsx")
#' openxlsx::write.xlsx(
#'   list(
#'     Variable_Map_Wide = data.frame(target_name = c("id", "gender"), orig_w2 = c("id", "gender")),
#'     Factor_Levels      = data.frame(target_name = character(), original_level = character())
#'   ),
#'   file = dict2
#' )
#'
#' dict_merge(dict1, dict2, save_dir = tempdir())
#'
#' @export
dict_merge <- function(dt1_path, dt2_path, save_dir = here::here()) {

  # --- input validation ---
  if (!file.exists(dt1_path)) cli::cli_abort("File not found: {.file {dt1_path}}")
  if (!file.exists(dt2_path)) cli::cli_abort("File not found: {.file {dt2_path}}")

  # --- read sheets ---
  d1_names <- openxlsx::getSheetNames(dt1_path)
  d2_names <- openxlsx::getSheetNames(dt2_path)

  read_workbook <- function(path, sheet_names) {
    sheets <- purrr::map(sheet_names, \(s) openxlsx::read.xlsx(path, sheet = s))
    purrr::set_names(sheets, sheet_names)
  }

  d1 <- read_workbook(dt1_path, d1_names)
  d2 <- read_workbook(dt2_path, d2_names)

  # --- merge change log (full history, no deduplication) ---
  d2$Change_Log <- dplyr::bind_rows(
    d1$Change_Log,
    d2$Change_Log,
    tibble::tibble(
      timestamp        = as.character(Sys.time()),
      action           = "Merged 2 dictionaries",
      match_strategy   = "full_unique",
      source1          = as.character(dt1_path),
      source2          = as.character(dt2_path)
    )
  )

  # --- declare content sheets ---
  content_sheets <- c("Variable_Map_Wide", "Factor_Levels", "Original_Metadata")


  # --- merge and deduplicate content sheets ---
  merge_sheet <- function(sheet_name) {
    dplyr::bind_rows(d1[[sheet_name]], d2[[sheet_name]]) |>
      dplyr::distinct()
  }

  d2[content_sheets] <- purrr::map(content_sheets, merge_sheet)


  # --- write output workbook ---
  wb <- openxlsx::createWorkbook()

  purrr::walk(d2_names, \(s) {
    openxlsx::addWorksheet(wb, s)
    openxlsx::writeData(wb, s, x = d2[[s]], startCol = 1, startRow = 1)
  })

  save_path <- file.path(save_dir, paste0(format(Sys.Date(), "%y%m%d"), "_merged_dict.xlsx"))

  if (file.exists(save_path)) {
    cli::cli_abort("Output file already exists: {.file {save_path}}")
  }

  openxlsx::saveWorkbook(wb, save_path, overwrite = FALSE)
  cli::cli_alert_success("Dictionary merged and saved at {.file {save_path}}")

  invisible(save_path)
}
