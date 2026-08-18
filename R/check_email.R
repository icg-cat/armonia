#' Flag structurally suspicious email addresses
#'
#' @description Runs a set of heuristic checks against each email address and
#' flags addresses that are likely typed incorrectly: stray \code{mailto:}
#' prefixes, surrounding quotes or angle brackets, a missing or duplicated
#' \code{@@}, a comma or semicolon where a dot was meant, consecutive dots, a
#' handle or domain that starts or ends with a dot or hyphen, known domain
#' misspellings (e.g. \code{gmial.com}), and suspicious or misspelled
#' top-level domains. Each address gets an overall \code{flag} of
#' \code{"ok"}, \code{"possible_mistake"}, or \code{"likely_mistake"}, with a
#' semicolon-separated \code{reason} explaining what was flagged.
#'
#' @param x A character vector of email addresses to check.
#'
#' @return A data.frame with one row per element of \code{x} and three
#'   columns: \code{email} (the original address), \code{flag}
#'   (\code{"ok"}, \code{"possible_mistake"}, or \code{"likely_mistake"}),
#'   and \code{reason} (\code{NA} when \code{flag} is \code{"ok"}, otherwise
#'   the flagged issues joined with \code{"; "}).
#'
#' @examples
#' emails <- c("alice@uni.edu", "bob@gmail.co", "carol@yahoo.com")
#' check_email(emails)
#'
#' @export
check_email <- function(x) {

  # Hardcoded known domain misspellings: misspelling = correct
  domain_typos <- c(
    "goggle.com"   = "google.com",
    "gogle.com"    = "google.com",
    "googl.com"    = "google.com",
    "gmial.com"    = "gmail.com",
    "gmai.com"     = "gmail.com",
    "gamil.com"    = "gmail.com",
    "gmmail.com"   = "gmail.com",
    "gmaill.com"   = "gmail.com",
    "hotmal.com"   = "hotmail.com",
    "hotmial.com"  = "hotmail.com",
    "hotnail.com"  = "hotmail.com",
    "hotmaill.com" = "hotmail.com",
    "outlok.com"   = "outlook.com",
    "outloook.com" = "outlook.com",
    "yaho.com"     = "yahoo.com",
    "yahooo.com"   = "yahoo.com",
    "iclod.com"    = "icloud.com",
    "iclould.com"  = "icloud.com",
    "protonmal.com" = "protonmail.com"
  )

  # Valid short TLDs that should not be flagged
  valid_short_tlds <- c("io", "ai", "co", "uk", "de", "fr", "es", "it", "nl",
                        "br", "mx", "ca", "au", "jp", "cn", "in", "ru", "za",
                        # latin america
                        "ar", "bo", "br", "cl", "co", "cr", "cu", "do", "ec",
                        "sv", "gt", "hn", "ni", "pa", "py", "pe", "uy", "ve",
                        # others in whitelist
                        "me", "ws"
                        )

  results <- purrr::map(x, function(email) {

    reasons  <- character(0)
    severity <- character(0)

    # --- Structural checks ---

    # mailto: prefix
    if (grepl("^mailto:", email, ignore.case = TRUE)) {
      reasons  <- c(reasons, "mailto: prefix present")
      severity <- c(severity, "likely_mistake")
    }

    # surrounding quotes
    if (grepl('^["\']|["\']$', email)) {
      reasons  <- c(reasons, "surrounding quotes")
      severity <- c(severity, "likely_mistake")
    }

    # surrounding angle brackets
    if (grepl("^<|>$", email)) {
      reasons  <- c(reasons, "surrounding angle brackets")
      severity <- c(severity, "likely_mistake")
    }

    # missing @ or multiple @
    at_count <- nchar(email) - nchar(gsub("@", "", email, fixed = TRUE))
    if (at_count == 0) {
      reasons  <- c(reasons, "missing @")
      severity <- c(severity, "likely_mistake")
    } else if (at_count > 1) {
      reasons  <- c(reasons, "multiple @ symbols")
      severity <- c(severity, "likely_mistake")
    }

    # separator mistakes: comma or semicolon instead of dot
    if (grepl(",", email, fixed = TRUE)) {
      reasons  <- c(reasons, "comma instead of dot")
      severity <- c(severity, "likely_mistake")
    }
    if (grepl(";", email, fixed = TRUE)) {
      reasons  <- c(reasons, "semicolon instead of dot")
      severity <- c(severity, "likely_mistake")
    }

    # space in address
    if (grepl(" ", email, fixed = TRUE)) {
      reasons  <- c(reasons, "space in address")
      severity <- c(severity, "possible_mistake")
    }

    # double dots
    if (grepl("\\.\\.", email)) {
      reasons  <- c(reasons, "consecutive dots")
      severity <- c(severity, "likely_mistake")
    }

    # Only run remaining checks if @ is present exactly once
    if (at_count == 1) {

      parts  <- strsplit(email, "@", fixed = TRUE)[[1]]
      handle <- parts[[1]]
      domain <- parts[[2]]

      # handle starts or ends with dot
      if (grepl("^\\.|\\.$", handle)) {
        reasons  <- c(reasons, "handle starts or ends with a dot")
        severity <- c(severity, "likely_mistake")
      }

      # domain starts or ends with dot
      if (grepl("^\\.|\\.$", domain)) {
        reasons  <- c(reasons, "domain starts or ends with a dot")
        severity <- c(severity, "likely_mistake")
      }

      # domain starts or ends with hyphen
      if (grepl("^-|-$", domain)) {
        reasons  <- c(reasons, "domain starts or ends with a hyphen")
        severity <- c(severity, "likely_mistake")
      }

      # known domain misspellings
      if (tolower(domain) %in% names(domain_typos)) {
        reasons  <- c(reasons, paste0("possible domain misspelling: ", domain, " (did you mean ", domain_typos[[tolower(domain)]], "?)"))
        severity <- c(severity, "likely_mistake")
      }

      # TLD checks
      tld <- sub(".*\\.", "", domain)

      # no TLD (no dot in domain)
      if (!grepl("\\.", domain)) {
        reasons  <- c(reasons, "no TLD found")
        severity <- c(severity, "likely_mistake")
      } else {

        # likely TLD misspellings
        likely_tld_typos <- c("con", "cmo", "ogr", "ner")
        if (tolower(tld) %in% likely_tld_typos) {
          reasons  <- c(reasons, paste0("likely TLD misspelling: .", tld))
          severity <- c(severity, "likely_mistake")
        }

        # .co flagged as possible .com
        if (tolower(tld) == "co" && !grepl("\\.co\\.[a-z]{2}$", domain)) {
          reasons  <- c(reasons, "possible .co instead of .com")
          severity <- c(severity, "possible_mistake")
        }

        # suspiciously short TLD not in known valid list
        if (nchar(tld) <= 2 && !tolower(tld) %in% valid_short_tlds) {
          reasons  <- c(reasons, paste0("unusually short TLD: .", tld))
          severity <- c(severity, "possible_mistake")
        }
      }
    }

    # Derive overall flag from worst severity found
    flag <- dplyr::case_when(
      "likely_mistake"   %in% severity ~ "likely_mistake",
      "possible_mistake" %in% severity ~ "possible_mistake",
      .default = "ok"
    )

    data.frame(
      email  = email,
      flag   = flag,
      reason = if (length(reasons) == 0) NA_character_ else paste(reasons, collapse = "; ")
    )
  })

  purrr::list_rbind(results)
}
