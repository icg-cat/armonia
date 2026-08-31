#' Assign standard labels to factor variables using the dictionary
#'
#' @description Iterates over all factor columns in \code{data} and replaces
#' their numeric levels with the corresponding \code{standard_label} values
#' from the dictionary. Intended to be called on the master dataset produced
#' by \code{harm_bind_waves()}, after factors have been codified as integers
#' by \code{dict_apply()} and after the dictionary has been validated by
#'  \code{dict_validate()}.
#'
#' @param data A data.frame whose factor columns carry integer-coded levels.
#'   Typically the output of \code{harm_bind_waves()}.
#' @param dict Path to the dictionary containing the Factor_Levels sheet where standard_codes and standard_labels are specified.
#'
#' @return A data.frame identical to \code{data} with factor levels replaced
#'   by their \code{standard_label} strings.
#'
#' @examples
#' data <- data.frame(eye_color = factor(c("1", "2", "1")))
#' dict_path <- tempfile(fileext = ".xlsx")
#' openxlsx::write.xlsx(
#'   list(Factor_Levels = data.frame(
#'     target_name    = c("eye_color", "eye_color"),
#'     standard_code  = c("1", "2"),
#'     standard_label = c("Hazel", "Green")
#'   )),
#'   file = dict_path
#' )
#' assign_standard_labels(data, dict_path)
#'
#' @seealso \code{\link{dict_validate}}
#' @export
assign_standard_labels <- function(data, dict) {
  dict <- readxl::read_excel(dict, sheet = "Factor_Levels", col_types = "text")

  # Identify factor columns
  is_fact <- vapply(data, is.factor, FUN.VALUE = logical(1))
  target_cols <- names(data)[is_fact]

  # Iterate over factor column names
  data[target_cols] <- lapply(target_cols, function(col_name) {

    current_factor <- data[[col_name]]
    current_levels <- levels(current_factor)

    # Subset dictionary for the specific variables
    var_dict <- dict[dict$target_name == col_name, c("target_name", "standard_code", "standard_label")]
    var_dict <- unique(var_dict)

    # INTEGRITY CHECKS: Ensure one-to-one mapping between code and label
    # If any standard_code appears more than once, it means there are conflicting labels
    code_counts <- table(var_dict$standard_code)
    if (any(code_counts > 1)) {
      conflicting_codes <- names(code_counts[code_counts > 1])

      cli::cli_abort(c(
        "x" = "Metadata Conflict: variable {.var {col_name}} has multiple standard_labels defined for the same standard_code.",
        "i" = "Conflicting codes: {.val {conflicting_codes}}.",
        "!" = "Ensure the dictionary has unique labels per code across all waves."
      ))
    }

    # If the variable isn't in the dictionary, return as is with a warning
    if (nrow(var_dict) == 0) {
      cli::cli_warn("Variable {.var {col_name}} not found in dictionary. Returning as-is.")
      return(current_factor)
    }

    # Match the dictionary rows to the existing levels of the factor
    match_idx <- match(current_levels, var_dict$standard_code)

    # Extract unique labels based on the matched indices
    new_labels <- var_dict$standard_label[match_idx]

    # Handle cases where a level in the data has no entry in the dictionary
    if (any(is.na(new_labels))) {
      cli::cli_warn("Some levels in {.var {col_name}} do not have a corresponding standard_label.")
    }

    # Re-factor with the new labels
    factor(
      current_factor,
      levels = current_levels,
      labels = new_labels
    )
  })

  return(data)
}
