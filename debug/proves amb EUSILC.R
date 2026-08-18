# ==============================================================================
# EU-SILC HARMONIZATION PIPELINE (v1.0)
# Expert R implementation for post-doctoral longitudinal research
# ==============================================================================

# 1. SETUP
# ------------------------------------------------------------------------------
library("openxlsx")
library("dplyr")

# 2. DEFENSIVE UTILITIES
# ------------------------------------------------------------------------------
#' Check Tidy Integrity
#' Ensures one observation per ID per Wave. Handles cross-sectional if wave_col is NULL.
source(here::here("R/check_tidy_integrity.R"))
# --- Robust Composite ID Generation ---

generate_composite_id <- function(df, id_col, year_col) {
  # 1. Ensure Year is numeric
  year_val <- as.numeric(as.character(df[[year_col]]))

  # 2. Extract numeric digits from ID (e.g., "H0101" -> "0101")
  id_numeric_str <- gsub("[^0-9]", "", as.character(df[[id_col]]))
  id_val <- as.numeric(id_numeric_str)

  # 3. Apply your formula
  # Using 10^5 ensures a consistent offset for IDs up to 5 digits
  return(year_val * 100000 + id_val)
}

# Implementation in the pipeline:
# final_panel$composite_id <- generate_composite_id(final_panel, "p_q01", "p_q02")

# 3. DICTIONARY ENGINE
# ------------------------------------------------------------------------------
source(here::here("R/create_survey_dictionary.R"))

# 4. TRANSFORMATION ENGINE
# ------------------------------------------------------------------------------
source(here::here("R/transform_to_baseline.R"))
# ==============================================================================
# EXECUTION: 10-ROW LONGITUDINAL SIMULATION
# ==============================================================================

# --- STEP 1: Setup Messy EU-SILC Data ---
# 2023: Standard
w23 <- data.frame(
  PB030 = c("H0101", "H0102"),
  PB010 = 2023,
  PB150 = factor(c("Male", "Female")),
  PY010G = c(25000, 30000),
  stringsAsFactors = FALSE
)

# 2024: SHIFTED. 'NEW_VAR' is inserted at Col 3, pushing Income to Col 5.
w24 <- data.frame(
  PB030 = c("H0101", "H0301"),
  PB010 = 2024,
  NEW_VAR = c("A", "B"), # The intruder
  PB150 = factor(c("1", "2")),
  PY010 = c(26000, 31000), # Renamed and shifted
  stringsAsFactors = FALSE
)

data_list <- list(W23 = w23, W24 = w24)

# STEP B: Create Dictionary
# Initial data integrity checks
lapply(data_list, check_tidy_integrity, id_col = "PB030", wave_col = "PB010")

# Create a metadata dictionary
create_survey_xlsx(data_list, "p", "SILC_Master_Dict.xlsx")

# STEP C: Simulate the Manual Excel Edit
wb <- openxlsx::loadWorkbook("SILC_Master_Dict.xlsx")
wide_map <- openxlsx::read.xlsx(wb, sheet = "Variable_Map_Wide")








#--------------------------
# STEP C: Standardize and Detect Shift
std_23 <- transform_to_baseline(w23, "SILC_Master_Dict.xlsx", "W23")
std_24 <- transform_to_baseline(w24, "SILC_Master_Dict.xlsx", "W24")

# STEP D: Audit structural integrity (The "Manual Intervention" Trigger)
# We compare W24 against the W23 reference
# This will flag that 'p_q04' in W23 (Income) does not match 'p_q04' in W24 (NEW_VAR)
alignment_report <- inspect_alignment_xlsx(std_23, std_24, "SILC_Master_Dict.xlsx", "W24")



# B. Run Initial Checks and Dictionary Setup
lapply(data_list, check_tidy_integrity, id_col = "PB030", wave_col = "PB010")
create_survey_xlsx(data_list, "p", "SILC_Master_Dict.xlsx")

# INTERMISSION: In a real workflow, the researcher would edit the Excel here.
# For this script, we assume the user maps PY010G and PY010 to p_q04 (p_income).

# C. Process and Bind
std_23 <- transform_to_baseline(w23, "SILC_Master_Dict.xlsx", "W23")
std_24 <- transform_to_baseline(w24, "SILC_Master_Dict.xlsx", "W24")

# Using dplyr::bind_rows for Union Join (keeps all distinct columns)
final_panel <- dplyr::bind_rows(list(W23 = std_23, W24 = std_24), .id = "source_wave")

# D. Generate EU-SILC Composite ID
# p_q01 = PB030 (ID), p_q02 = PB010 (Year)
final_panel$composite_id <- paste0(final_panel$p_q02, "_", final_panel$p_q01)

# Result Visualization
print(final_panel)
