#' Sync Review Edits and Log Changes
#' @param wb_path Path to the dictionary.xlsx.
#' @param lang_b The language to update.
sync_dictionary <- function(wb_path, lang_b) {

  wb <- openxlsx::loadWorkbook(wb_path)
  review_sheet <- paste0("Review_", lang_b)

  if (!review_sheet %in% names(wb)) stop("Review sheet not found.")

  # 1. Load Data
  review_data <- openxlsx::read.xlsx(wb_path, sheet = review_sheet)
  var_map <- openxlsx::read.xlsx(wb_path, sheet = "Variable_Map")

  # 2. Identify actual changes (where proposed != current)
  changes <- review_data[review_data$proposed_standard_name != review_data$standard_code, ]

  if (nrow(changes) > 0) {
    # 3. Create/Update Change Log
    log_entry <- data.frame(
      timestamp = Sys.time(),
      language = lang_b,
      original_variable = changes$original_lang_b,
      old_standard_name = changes$standard_code,
      new_standard_name = changes$proposed_standard_name,
      reason = "Manual Review Adjustment"
    )

    if (!"Change_Log" %in% names(wb)) {
      openxlsx::addWorksheet(wb, "Change_Log")
      openxlsx::writeData(wb, "Change_Log", log_entry)
    } else {
      existing_log <- openxlsx::read.xlsx(wb_path, sheet = "Change_Log")
      updated_log <- rbind(existing_log, log_entry)
      openxlsx::writeData(wb, "Change_Log", updated_log)
    }

    # 4. Apply Changes to the Variable_Map
    for (i in seq_len(nrow(changes))) {
      match_idx <- which(var_map$language == lang_b &
                           var_map$original_name == changes$original_lang_b[i])
      var_map$standard_name[match_idx] <- changes$new_standard_name[i]
    }

    # 5. Save and Cleanup
    openxlsx::writeData(wb, "Variable_Map", var_map)
    # We remove the Review sheet after syncing to signify the 'task is done'
    # This prevents the 'run it again?' confusion.
    openxlsx::removeWorksheet(wb, review_sheet)

    openxlsx::saveWorkbook(wb, wb_path, overwrite = TRUE)
    message("Sync complete. Variable_Map updated and changes logged.")

  } else {
    message("No changes detected in the Review sheet.")
  }
}
