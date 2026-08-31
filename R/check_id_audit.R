#' Audit primary key matching between two ID columns
#'
#' Compares a vector of IDs against a reference vector, identifying exact
#' matches and flagging potential typos via fuzzy string distance metrics.
#' Results are written to a dedicated sheet in an Excel workbook for review.
#'
#' Fuzzy matching uses a composite distance score combining Levenshtein (\code{lv}),
#' Damerau-Levenshtein (\code{dl}), and longest common substring (\code{lcs})
#' distances. A candidate is rejected as a match if the minimum composite score
#' is greater than or equal to the length of the query ID.
#'
#' For best results on data containing accents, special characters, or
#' inconsistent whitespace, normalize inputs with \code{\link{normalize_id}}
#' before calling this function.
#'
#' @param id_col1 A character vector of reference IDs (the source of truth).
#' @param id_col2 A character vector of IDs to audit against \code{id_col1}.
#'   Primary keys must not contain missing values.
#' @param wb_path A string. Path to an existing \code{.xlsx} workbook where
#'   audit results will be written. A sheet named \code{"Review_IDs"} will be
#'   created or overwritten.
#' @param case_sensitive Logical. If \code{TRUE} (default), matching is
#'   case-sensitive. If \code{FALSE}, both vectors are lowercased before
#'   comparison; original casing is preserved in the output.
#' @param verbose Logical. If TRUE, returns distance matrix. Defaults to FALSE.
#'
#' @return Invisibly returns a data frame with the following columns:
#'   \describe{
#'     \item{index}{Position of the ID in \code{id_col2}.}
#'     \item{w2id}{Original value from \code{id_col2}.}
#'     \item{best_match}{Best matching value from \code{id_col1}, or
#'       \code{"no best match found"} if no viable candidate exists.}
#'     \item{all_equal}{Logical. \code{TRUE} if \code{w2id} and
#'       \code{best_match} are equal under the active case sensitivity setting.}
#'   }
#'
#' @examples
#' ref_ids   <- c("alice@uni.edu", "bob@gmail.com")
#' audit_ids <- c("alice@uni.edu", "bob@gmial.com")  # typo: gmial vs gmail
#'
#' wb_path <- tempfile(fileext = ".xlsx")
#' openxlsx::write.xlsx(list(Instructions = data.frame(info = "audit workbook")), file = wb_path)
#'
#' check_id_audit(ref_ids, audit_ids, wb_path = wb_path)
#'
#' # Case-insensitive matching, with upstream normalization
#' check_id_audit(
#'   normalize_id(ref_ids),
#'   normalize_id(audit_ids),
#'   wb_path = wb_path,
#'   case_sensitive = FALSE
#' )
#'
#' @seealso \code{\link{normalize_id}}
#' @importFrom purrr map_dfr
#' @importFrom dplyr bind_cols mutate
#' @importFrom stringdist stringdist
#' @importFrom openxlsx loadWorkbook removeWorksheet addWorksheet writeData saveWorkbook
#' @importFrom cli cli_abort cli_alert_info
#' @export
check_id_audit <- function(id_col1, id_col2, wb_path, case_sensitive = TRUE, verbose = FALSE) {

  # Manage case sensitivity
  id1_compare <- if (!case_sensitive) tolower(id_col1) else id_col1
  id2_compare <- if (!case_sensitive) tolower(id_col2) else id_col2

  results <- purrr::map(.x = seq_along(id2_compare), function(.x) {

    if (is.na(id2_compare[[.x]])) {
      cli::cli_abort("Attention: ID number {.x} is missing. Primary keys cannot be missing. Please review items in id_col2")
    }

    if (id2_compare[[.x]] %in% id1_compare) {
      # Exact match
      match_idx <- which(id1_compare == id2_compare[[.x]])[[1]]  # ties: first candidate taken
      matched <- dplyr::mutate(
        dplyr::bind_cols(
          index      = .x,
          w2id       = id_col2[[.x]],
          best_match = id_col1[[match_idx]]
        ),
        all_equal = if (!case_sensitive) {
          id2_compare[[.x]] == tolower(.data$best_match)
        } else {
          .data$w2id == .data$best_match
        }
      )
      list(matched = matched, distances = NULL)

    } else {
      # Fuzzy match
      idlength <- nchar(id2_compare[[.x]])
      lv       <- stringdist::stringdist(id2_compare[[.x]], id1_compare, method = "lv")
      dl       <- stringdist::stringdist(id2_compare[[.x]], id1_compare, method = "dl")
      lcs      <- stringdist::stringdist(id2_compare[[.x]], id1_compare, method = "lcs")

      distances <- dplyr::mutate(
        dplyr::bind_cols(
          index = .x,
          w2id  = id_col2[[.x]],
          id1   = id_col1,
          lv    = lv,
          dl    = dl,
          lcs   = lcs
        ),
        sum_dist = lv + dl + lcs
      )

      idx <- which(distances$sum_dist == min(distances$sum_dist))[[1]]  # ties: first candidate taken

      matched <- if (min(distances$sum_dist) >= idlength) {
        dplyr::mutate(
          dplyr::bind_cols(
            index      = .x,
            w2id       = id_col2[[.x]],
            best_match = "no best match found"
          ),
          all_equal = FALSE
        )
      } else {
        dplyr::mutate(
          dplyr::bind_cols(
            index      = .x,
            w2id       = id_col2[[.x]],
            best_match = id_col1[[idx]]
          ),
          all_equal = FALSE
        )
      }

      list(matched = matched, distances = distances)
    }
  })

  # Separate matched rows and distance logs
  matched      <- purrr::list_rbind(purrr::map(results, "matched"))
  distance_log <- purrr::list_rbind(
    purrr::keep(purrr::map(results, "distances"), Negate(is.null))
  )

  # Append results to Excel review sheet
  wb <- openxlsx::loadWorkbook(wb_path)
  sheet_name <- "Review_IDs"
  if (sheet_name %in% names(wb)) openxlsx::removeWorksheet(wb, sheet_name)
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, matched)
  openxlsx::saveWorkbook(wb, wb_path, overwrite = TRUE)

  cli::cli_alert_info(
    "ID audit complete, see results in {.file {sheet_name}}. {.val {sum(matched$all_equal == FALSE)}} potential typos identified."
  )

  if (verbose) {
    return(list(matched = matched, distances = distance_log))
  } else {
    return(invisible(matched))
  }
}
