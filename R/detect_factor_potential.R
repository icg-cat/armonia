# Thresholds for the factor-detection heuristic below.
FACTOR_LIKERT_MAX_UNIQUE    <- 7     # cardinality shortcut: always a factor at or below this
FACTOR_CARDINALITY_MAX      <- 16    # pattern test: cardinality cap for a categorical variable
FACTOR_MIN_SAMPLE_SIZE      <- 30    # pattern test: minimum sample size to trust the heuristic
FACTOR_MIN_CATEGORY_FREQ    <- 3     # pattern test: a category must recur at least this often to count as "repeated"
FACTOR_MIN_REPETITION_RATIO <- 0.20  # pattern test: share of categories that must repeat

#' Detect factor potential in a vector
#'
#' @description Heuristic classifier that decides whether a vector should be
#' coerced to a factor. Uses two strategies: a cardinality shortcut for
#' Likert/binary-style variables (up to 7 unique values), and a stricter
#' pattern test combining cardinality, sample size, and category repetition.
#'
#' @param x A vector of any type.
#' @param max_unique Override maximum of 16 unique values for factors. Defaults to NULL.
#'
#' @return The original vector unchanged, or a factor if the heuristic
#' identifies it as categorical.
#'
#' @keywords internal
detect_factor_potential <- function(x, max_unique = NULL) {
  # 1. isolate non-missing data for logic checks
  ## Work on 'x_clean' to calculate metrics, return original 'x'
  x_clean <- x[!is.na(x)]
  n_total <- length(x_clean)

  # Edge case: If vector is empty or all NA, return original class
  if (n_total == 0) return(x)

  # Calculate key metrics
  unique_vals <- unique(x_clean)
  n_unique    <- length(unique_vals)

  # Early exit: if every non-NA value parses as a number, the variable is already
  # numeric-coded (e.g. Likert stored as "1","2","3"), no factoring needed.
  if (all(!is.na(suppressWarnings(as.numeric(as.character(unique_vals)))))) return(x)

  # OVERRIDE: if the user supplied max_unique, cardinality alone decides; skip sample-size and repetition guards.
  if (!is.null(max_unique)) {
    return(if (n_unique <= max_unique) as.factor(x) else x)
  }

  # STRATEGY 1: 'Likert/Binary' (If cardinality is very low, likely a factor regardless of sample size.
  if (n_unique <= FACTOR_LIKERT_MAX_UNIQUE) {
    return(as.factor(x))
  }

  # STRATEGY 2: 'Pattern' Test. If bypass failed, must meet 3 conditions:
  ## Condition A: Cardinality cap rejects continuous variables
  is_low_cardinality <- n_unique <= FACTOR_CARDINALITY_MAX

  # Condition B: Sample size floor
  is_large_sample <- n_total >= FACTOR_MIN_SAMPLE_SIZE

  # Condition C: Repetition distinguishes factors from IDs (a true factor should have categories that repeat)
  freq_counts <- table(x_clean)
  n_repeats   <- sum(freq_counts >= FACTOR_MIN_CATEGORY_FREQ)
  rep_ratio   <- n_repeats / n_unique

  is_repetitive <- rep_ratio >= FACTOR_MIN_REPETITION_RATIO

  # FINAL DECISION: All strict conditions must be TRUE
  if (is_low_cardinality && is_large_sample && is_repetitive) {
    return(as.factor(x))
  } else {
    return(x)
  }
}
