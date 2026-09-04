#' Clean and standardize email strings
#'
#' @description Prepares email addresses for hashing by trimming whitespace,
#' normalizing case, and correcting common typos (e.g. \code{.co} endings,
#' \code{.con} endings, \code{@gogle} domains). Empty strings and \code{NA}
#' values are preserved as \code{NA_character_} to prevent hashing the literal
#' string \code{"NA"}.
#'
#' @param x A character vector of email addresses.
#'
#' @return A character vector of the same length as \code{x}, standardized and
#'   ready for hashing.
#'
#' @examples
#' emails <- c("alice@uni.edu", "BOB@gmail.co ", NA)
#' anon_clean_email(emails)
#'
#' @export
anon_clean_email <- function(x) {
  if (is.null(x)) return(NULL)

  # Standardize: trim whitespace and lowercase
  clean_x <- trimws(tolower(as.character(x)))

  # Replace common error strings.

  clean_x <- stringr::str_replace(
    string = clean_x,
    pattern = "\\.con$",
    replacement = ".com")

  clean_x <- stringr::str_replace(
    string = clean_x,
    pattern = "@gogle",
    replacement = "@google")

  # Ensure NAs are preserved as NA_character_ to avoid hashing the string "NA"
  clean_x[is.na(x) | x == ""] <- NA_character_


  return(clean_x)
}

#' Generate pseudonymous research IDs via salted hashing
#'
#' @description Creates a 12-character SHA-256 hash from an identifier and a
#' project-specific salt retrieved from the \code{HARMONIZE_SALT} environment
#' variable. Aborts immediately if no salt is found, preventing insecure
#' de-identification with an empty or default key. \code{NA} inputs are passed
#' through as \code{NA_character_}.
#'
#' @param x A character vector of identifiers (e.g. the output of
#'   \code{anon_clean_email()}).
#'
#' @return A character vector of truncated 12-character hex strings, the same
#'   length as \code{x}.
#'
#' @examples
#' old_salt <- Sys.getenv("HARMONIZE_SALT")
#' Sys.setenv(HARMONIZE_SALT = "example-salt")
#' anon_hash(c("alice@uni.edu", NA))
#' Sys.setenv(HARMONIZE_SALT = old_salt)
#'
#' @export
anon_hash <- function(x) {
  hash_length <- 12  # truncated hex length for the pseudonymous ID
  # 1. retrieve salt
  # Option A: Salt managed via .Renviron for security and reproducibility
  salt_key <- Sys.getenv("HARMONIZE_SALT")

  # Fail-Fast: Do not allow insecure defaults
  if (salt_key == "") {
    cli::cli_abort(c(
      "x" = "Security error: no {.var HARMONIZE_SALT} found in environment.",
      "i" = "Please set a salt in your {.file .Renviron} file: {.code HARMONIZE_SALT='your_secret_key'}",
      "!" = "Execution halted to prevent insecure de-identification."
    ))
  }

  # 2. Hashing execution
  # We use SHA-256 via the digest package as current research standard
  hashed_values <- vapply(x, function(val) {
    if (is.na(val)) return(NA_character_)

    # Concatenate value with salt before hashing
    full_hash <- digest::digest(paste0(val, salt_key), algo = "sha256")

    # 3. Formatting: truncate for readability
    substr(full_hash, 1, hash_length)
  }, character(1), USE.NAMES = FALSE)

  return(hashed_values)
}
