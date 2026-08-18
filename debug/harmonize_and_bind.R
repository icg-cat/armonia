#' Execute Row-Binding based on a Blueprint
#' @param source_df Reference data.frame.
#' @param target_df Data.frame to align and bind.
#' @param blueprint The data.frame output (and edited) from inspect_harmony.
#' @return A combined data.frame with unified column names.
harmonize_and_bind <- function(source_df, target_df, blueprint) {

  # 1. Filter blueprint to only valid mappings
  valid_map <- blueprint[!is.na(blueprint$target_var), ]

  # 2. Subset and Rename target_df to match source_df names
  # This ensures that when we rbind, columns align perfectly.
  target_aligned <- target_df[, valid_map$target_var, drop = FALSE]
  colnames(target_aligned) <- valid_map$source_var

  # 3. Handle columns present in source but missing in target (fill NA)
  missing_in_target <- setdiff(colnames(source_df), valid_map$source_var)
  for (col in missing_in_target) target_aligned[[col]] <- NA

  # 4. Final Bind (keeping all source columns)
  combined <- rbind(source_df, target_aligned[, colnames(source_df)])

  return(combined)
}
