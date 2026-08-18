id_col1 <- raw_w1_uk$email
id_col2 <- raw_w2_uk$email
wb_path <- "dict_w2.xlsx"

check_id_audit <- function(id_col1, id_col2, wb_path, max_dist = 1) {

  # 1. Identify matches
  matched <- purrr::map_dfr(.x = seq_along(id_col2), function(.x){

    if(id_col2[[.x]] %in% id_col1){
      bind_cols(
        index = .x,
        w2id = id_col2[[.x]],
        best_match = id_col1[which(id_col1 == id_col2[[.x]])]
      ) %>%
        mutate(
          all_equal = w2id == best_match
        )
    } else {

    lv <- stringdist::stringdist(.x, id_col1, method = "lv") # number of deletions, insertions and substitutions necessary to turn b into a
    dl <- stringdist::stringdist(.x, id_col1, method = "dl") #like the Levenshtein distance but also allows transposition of adjacent characters & allows for multiple edits on substrings
    lcs <- stringdist::stringdist(.x, id_col1, method = "lcs") # the number of unpaired characters

    distances <- bind_cols(lv = lv,
              dl = dl,
              lcs = lcs) %>%
      mutate(sum_dist = lv+dl+lcs)

    idx <- which(distances$sum_dist == min(distances$sum_dist))

     bind_cols(
       index = .x,
      w2id = id_col2[[.x]],
      best_match = id_col1[[idx]]
    ) %>%
       mutate(
         all_equal = w2id == best_match
       )
     }

    })


    # 2. Append to Excel Review Sheet
    wb <- openxlsx::loadWorkbook(wb_path)
    sheet_name <- "Review_IDs"

    if (sheet_name %in% names(wb)) openxlsx::removeWorksheet(wb, sheet_name)
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(wb, sheet_name, matched)

    # # Styling for Human-in-the-Loop
    # warn_style <- openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
    # openxlsx::conditionalFormatting(wb, sheet_name, cols = 1:ncol(report),
    #                                 rows = 2:(nrow(report)+1),
    #                                 rule = '!= ""', type = "expression", style = warn_style)

    openxlsx::saveWorkbook(wb, wb_path, overwrite = TRUE)
    cli::cli_alert_info("ID Audit complete: {.val {nrow(report)}} potential typos added to {.file {sheet_name}}.")
}








# next --------------------------------------------------------------------


# --- 1. Identify Files ---
# Adjust the index [2:8] as per your specific project needs
files_to_read <- list.files("R", full.names = TRUE)[2:8]
output_file <- "debug/function_dump.rtf"

if (!dir.exists("debug")) dir.create("debug")

# --- 2. Open a Raw Write Connection ---
# Using "wb" (write binary) or "w" ensures we control every character
con <- file(output_file, "w")

# --- 3. Write the SINGLE Mandatory Header ---
# We define f0 as Courier (Monospaced) for code alignment
rtf_header <- "{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0\\fmodern Courier;}}\\f0\\fs18 "
writeLines(rtf_header, con, sep = "")

# --- 4. Loop and Sanitize Content ---
for (f in files_to_read) {
  # Add a Bold File Header
  writeLines(paste0("\\line\\b --- FILE: ", basename(f), " ---\\b0\\line "), con, sep = "")

  # Read the R code
  raw_code <- readLines(f, warn = FALSE)

  # CRITICAL: Escape characters that RTF uses for its own logic
  # We must escape \ first, then { and }
  clean_code <- gsub("\\", "\\\\", raw_code, fixed = TRUE)
  clean_code <- gsub("{", "\\{", clean_code, fixed = TRUE)
  clean_code <- gsub("}", "\\}", clean_code, fixed = TRUE)

  # Write lines with \line to force the RTF reader to respect line breaks
  writeLines(paste0(clean_code, "\\line "), con, sep = "")

  # Add extra spacing between files
  writeLines("\\line ", con, sep = "")
}

# --- 5. Write the SINGLE Mandatory Footer and Close ---
writeLines("}", con, sep = "")
close(con)

message("Success: '", output_file, "' is now a valid, single-header RTF document.")
