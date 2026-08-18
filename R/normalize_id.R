#' Normalize identifier strings
#'
#' Prepares character vectors for comparison by optionally removing accents and
#' special Latin characters, and/or stripping whitespace. Intended as a
#' preprocessing step before passing primary keys to \code{\link{check_id_audit}}.
#'
#' @param x A character vector to normalize.
#' @param translit Logical. If \code{TRUE} (default), transliterates Latin
#'   script characters with diacritics to their ASCII equivalents (e.g.
#'   \code{"café"} becomes \code{"cafe"}). Uses ICU transliteration rule
#'   \code{"Latin-ASCII"} via \code{stringi}.
#' @param remove_whitespace Logical. If \code{TRUE} (default), removes all
#'   whitespace characters, including non-breaking and other Unicode space
#'   variants.
#'
#' @return A character vector of the same length as \code{x}.
#' @importFrom stringi stri_trans_general stri_replace_all_charclass
#' @export
#' @examples
#' normalize_id(c("caf\u00e9", "jo\u00e3o", "mar\u00eda"))
#' normalize_id(c("john doe", "jane doe"), translit = FALSE)
#' normalize_id(c("caf\u00e9 latte", "jo\u00e3o silva"), translit = TRUE, remove_whitespace = TRUE)
#'
#' @seealso \code{\link{check_id_audit}}
normalize_id <- function(x, translit = TRUE, remove_whitespace = TRUE) {
  if (translit) {
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
  }
  if (remove_whitespace) {
    x <- stringi::stri_replace_all_charclass(x, "\\p{Z}", "")
  }
  x
}
