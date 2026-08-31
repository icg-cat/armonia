#' Apply dictionary mapping and standardize categorical values
#'
#' @description Renames variables and recodes factor labels to their numeric
#' \code{standard_code} equivalents using the mapping sheets in the dictionary
#' produced by \code{dict_init()}. Input data is sanitized via
#' \code{janitor::clean_names()} before any processing.
#'
#' @param data A data.frame to be standardized.
#' @param wb_path Character string. Path to the \code{.xlsx} dictionary file.
#' @param source_name Character string. The column name in \code{Variable_Map_Wide}
#'   that corresponds to this dataset (e.g. \code{"orig_wave1"}). Must match
#'   the dictionary exactly.
#' @param attributes Logical. If \code{TRUE}, the original variable names and
#'   descriptions from the dictionary are stored as attributes on the returned
#'   tibble. Defaults to \code{FALSE}.
#'
#' @return A tibble with variables renamed to their \code{target_name} and
#'   factor columns recoded to integer-level factors.
#'
#' @examples
#' wave1 <- data.frame(id = c("alice", "bob"), gender = c("Female", "Male"))
#'
#' dict_path <- tempfile(fileext = ".xlsx")
#' openxlsx::write.xlsx(
#'   list(
#'     Variable_Map_Wide = data.frame(
#'       target_name = c("id", "gender"),
#'       orig_w1     = c("id", "gender")
#'     ),
#'     Factor_Levels = data.frame(
#'       wave              = "orig_w1",
#'       original_variable = "gender",
#'       target_name       = "gender",
#'       original_level    = c("Female", "Male"),
#'       standard_code     = c(1, 2),
#'       standard_label    = c("Female", "Male")
#'     )
#'   ),
#'   file = dict_path
#' )
#'
#' dict_apply(wave1, dict_path, source_name = "orig_w1")
#'
#' @export
dict_apply <- function(data, wb_path, source_name, attributes = FALSE) {

  # 1. Initialization and sanitization
  data_clean <- janitor::clean_names(data)
  wave_id <- tolower(source_name)

  # Load mapping sheets as text to avoid silent numeric/date coercion
  w_map <- readxl::read_excel(wb_path, sheet = "Variable_Map_Wide", col_types = "text")
  l_map <- readxl::read_excel(wb_path, sheet = "Factor_Levels", col_types = "text")

  # Force lowercase headers to prevent case-sensitivity crashes
  colnames(w_map) <- tolower(colnames(w_map))
  colnames(l_map) <- tolower(colnames(l_map))

  # Find matching ID column in the dictionary
  wave_col <- intersect(source_name, colnames(w_map))
  if (length(wave_col) == 0) {
    cli::cli_abort("Wave column {.val {wave_col}} not found in Variable_Map_Wide. Check dictionary, source_name must match either column 3 or 4 in sheet 2")
  }

  # 2. Variable renaming
  # Extract mapping for variables present in this wave
  map_sub <- w_map[!is.na(w_map[[wave_col]]), c("target_name", wave_col)]

  # Only keep variables that are explicitly mapped in the dictionary
  valid_cols <- map_sub[[wave_col]] %in% colnames(data_clean)
  map_sub <- map_sub[valid_cols, ]

  if (nrow(map_sub) == 0) {
    cli::cli_warn("No variables in wave {.val {source_name}} matched the dictionary.")
    return(tibble::as_tibble(data_clean))
  }

  # Subset and rename in one step
  data_std <- data_clean[, map_sub[[wave_col]], drop = FALSE]
  colnames(data_std) <- map_sub$target_name

  # add attributes
  if(attributes == TRUE){
    attr(data_std, "labels") <- map_sub[[wave_col]]
    attr(data_std, "descriptions") <- w_map$description[w_map$target_name %in% map_sub$target_name]
  }

  # 3. Categorical transformation
  wave_factors <- l_map[l_map$wave == wave_id, ]

  if (nrow(l_map) > 0) {
    for (orig_var in unique(wave_factors$original_variable)) {
      # Find the target_name associated with this original variable
      target <- map_sub$target_name[map_sub[[wave_col]] == orig_var]

      if (length(target) > 0 && target %in% colnames(data_std)) {
        lev_map <- wave_factors[wave_factors$original_variable == orig_var, ]

        # Mapping original Labels to standard codes
        lookup <- stats::setNames(as.character(lev_map$standard_code),
                                  as.character(lev_map$original_level))

        # Apply the mapping while maintaining the factor class
        data_std[[target]] <- factor(
          lookup[as.character(data_std[[target]])],
          levels = unique(as.character(lev_map$standard_code))
        )
      }
    }
  }

  return(tibble::as_tibble(data_std))
}
