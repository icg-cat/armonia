#' Create Master Research Dictionary
#'
#' @description Generates the 'Rosetta Stone' Excel for harmonization.
#' Creates sheets with Title Case names for readability, but populates them
#' with lowercase column headers to ensure downstream consistency.
#'
#' @param data_list A named list of data.frames (one per wave).
#' @param type_prefix Character prefix for standardized variables (e.g., "hlth").
#' @param file_path Character. Path to save the Excel file.
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData addStyle createStyle saveWorkbook
#' @export
create_survey_xlsx <- function(data_list, type_prefix, file_path = "SILC_Master_Dict.xlsx") {

  wb <- openxlsx::createWorkbook()
  h_style <- openxlsx::createStyle(textDecoration = "bold", bgFill = "#E6E6E6", border = "Bottom")

  # --- 1. Variable_Map_Wide ---
  max_cols <- max(vapply(data_list, ncol, numeric(1)))

  # Initialize with lowercase 'target_name'
  wide_map <- data.frame(
    target_name = paste0(tolower(type_prefix), "_q", sprintf("%03i", seq_len(max_cols))),
    stringsAsFactors = FALSE
  )

  for (n in names(data_list)) {
    orig_names <- colnames(data_list[[n]])
    length(orig_names) <- max_cols
    # Force lowercase 'orig_' prefix
    wide_map[[paste0("orig_", tolower(n))]] <- orig_names
  }
  wide_map$assessment <- "OK"

  openxlsx::addWorksheet(wb, "Variable_Map_Wide")
  openxlsx::writeData(wb, "Variable_Map_Wide", wide_map)
  openxlsx::addStyle(wb, "Variable_Map_Wide", h_style, rows = 1, cols = 1:ncol(wide_map))

  # --- 2. Factor_Levels ---
  factor_map <- do.call(rbind, lapply(names(data_list), function(n) {
    df <- data_list[[n]]
    f_cols <- names(df)[vapply(df, is.factor, logical(1))]
    if(length(f_cols) == 0) return(NULL)

    do.call(rbind, lapply(f_cols, function(col) {
      lvls <- levels(df[[col]])
      data.frame(
        wave = tolower(n),
        original_variable = col,
        target_name = NA_character_, # Placeholder to be filled by sync
        original_level = lvls,
        standard_code = seq_along(lvls),
        standard_label = lvls,
        stringsAsFactors = FALSE
      )
    }))
  }))

  if(!is.null(factor_map)) {
    openxlsx::addWorksheet(wb, "Factor_Levels")
    openxlsx::writeData(wb, "Factor_Levels", factor_map)
    openxlsx::addStyle(wb, "Factor_Levels", h_style, rows = 1, cols = 1:ncol(factor_map))
  }

  # --- 3. Change_Log ---
  openxlsx::addWorksheet(wb, "Change_Log")
  log_df <- data.frame(timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                       action = "init", details = "Dictionary created.", stringsAsFactors = FALSE)
  openxlsx::writeData(wb, "Change_Log", log_df)

  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
}

#' Sync Wide Map Edits to Metadata
#'
#' @description Propagates target_name updates from 'Variable_Map_Wide' to 'Factor_Levels'.
#' Handles mixed capitalization in Excel headers by strictly enforcing lowercase logic internally.
#'
#' @param wb_path Character. Path to the Master Dictionary Excel file.
#'
#' @importFrom openxlsx loadWorkbook read.xlsx writeData saveWorkbook
#' @export
sync_wide_dictionary <- function(wb_path) {

  if (!file.exists(wb_path)) stop("File not found: ", wb_path)

  wb <- openxlsx::loadWorkbook(wb_path)
  s_names <- names(wb)

  # Robust Sheet Selection: Find the index regardless of Case
  v_idx <- which(tolower(s_names) == "variable_map_wide")
  f_idx <- which(tolower(s_names) == "factor_levels")

  if(length(v_idx) == 0 || length(f_idx) == 0) {
    stop("Critical sheets missing. Expected 'Variable_Map_Wide' and 'Factor_Levels'.")
  }

  # Load data
  wide_map <- openxlsx::read.xlsx(wb_path, sheet = v_idx)
  factor_levels <- openxlsx::read.xlsx(wb_path, sheet = f_idx)

  # STRICT ENFORCEMENT: Convert loaded headers to lowercase immediately
  colnames(wide_map) <- tolower(colnames(wide_map))
  colnames(factor_levels) <- tolower(colnames(factor_levels))

  # Build lookup from Wide Map
  orig_cols <- grep("^orig_", colnames(wide_map), value = TRUE)
  if (length(orig_cols) == 0) return(invisible(NULL))

  lookup_list <- lapply(orig_cols, function(col) {
    data.frame(wave = gsub("^orig_", "", col),
               original_variable = wide_map[[col]],
               target_name = wide_map$target_name,
               stringsAsFactors = FALSE)
  })

  lookup_table <- do.call(rbind, lookup_list)
  lookup_table <- lookup_table[!is.na(lookup_table$original_variable), ]

  # Remove existing target_name from Factor_Levels to avoid merge conflicts (.x/.y)
  if("target_name" %in% colnames(factor_levels)) {
    factor_levels$target_name <- NULL
  }

  # Merge using the clean, lowercase keys
  updated_factors <- merge(factor_levels, lookup_table,
                           by = c("wave", "original_variable"), all.x = TRUE)

  # Reorder columns for consistency
  canonical_cols <- c("wave", "original_variable", "target_name",
                      "original_level", "standard_code", "standard_label")

  # Keep any extra columns the user might have added (e.g., notes)
  final_cols <- c(intersect(canonical_cols, colnames(updated_factors)),
                  setdiff(colnames(updated_factors), canonical_cols))

  updated_factors <- updated_factors[, final_cols]

  # Write back to the original sheet index (preserving original sheet name)
  openxlsx::writeData(wb, sheet = s_names[f_idx], x = updated_factors)
  openxlsx::saveWorkbook(wb, wb_path, overwrite = TRUE)

  message(">>> Sync successful: Factor_Levels updated.")
}

#' Transform Wave to Baseline
#'
#' @description Renames variables and recodes factors for a single wave
#' based on the Master Dictionary.
#'
#' @param df Data.frame. The raw wave data.
#' @param wb_path Character. Path to the Master Dictionary.
#' @param wave_name Character. The wave identifier (e.g., "w23").
#'
#' @return A data.frame with standardized variable names and factor levels.
#' @importFrom openxlsx read.xlsx getSheetNames
#' @importFrom stats setNames
#' @export
transform_to_baseline <- function(df, wb_path, wave_name) {

  wave_id <- tolower(wave_name)
  wb_sheets <- openxlsx::getSheetNames(wb_path)

  # Robust Sheet Selection
  v_idx <- which(tolower(wb_sheets) == "variable_map_wide")
  f_idx <- which(tolower(wb_sheets) == "factor_levels")

  if(length(v_idx) == 0 || length(f_idx) == 0) stop("Dictionary sheets not found.")

  # Read and normalize headers
  w_map <- openxlsx::read.xlsx(wb_path, sheet = v_idx)
  l_map <- openxlsx::read.xlsx(wb_path, sheet = f_idx)

  colnames(w_map) <- tolower(colnames(w_map))
  colnames(l_map) <- tolower(colnames(l_map))

  # Identify Wave Column
  wave_col <- paste0("orig_", wave_id)
  if(!wave_col %in% colnames(w_map)) {
    stop(paste("Wave column", wave_col, "not found in Variable_Map_Wide."))
  }

  # 1. Variable Renaming
  # Extract map for this wave only
  map_sub <- w_map[!is.na(w_map[[wave_col]]), c("target_name", wave_col)]

  # Select only columns present in the dataframe
  valid_cols <- map_sub[[wave_col]] %in% colnames(df)
  map_sub <- map_sub[valid_cols, ]

  if (nrow(map_sub) == 0) {
    warning("No matching variables found for this wave.")
    return(data.frame())
  }

  df_std <- df[, map_sub[[wave_col]], drop = FALSE]
  colnames(df_std) <- map_sub$target_name

  # 2. Factor Recoding
  # Filter factor levels for this wave
  wave_factors <- l_map[l_map$wave == wave_id, ]

  if (nrow(wave_factors) > 0) {
    for(var in unique(wave_factors$original_variable)) {

      # Find the new target name for this original variable
      target <- map_sub$target_name[map_sub[[wave_col]] == var]

      if(length(target) > 0 && target %in% colnames(df_std)) {
        lev_map <- wave_factors[wave_factors$original_variable == var, ]

        # Create lookup: Original Label -> Standard Code
        lookup <- stats::setNames(as.character(lev_map$standard_code),
                                  as.character(lev_map$original_level))

        # Apply factor conversion
        df_std[[target]] <- factor(lookup[as.character(df_std[[target]])],
                                   levels = unique(as.character(lev_map$standard_code)))
      }
    }
  }

  return(df_std)
}
