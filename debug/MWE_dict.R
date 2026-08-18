# User-in-the-loop MWE
source("R/dict_init.R")
source("R/dict_validate.R")
# --- 0. Setup: Generate "Messy" Raw Data ---
# In a real project, you would read_csv() or read_excel() here.

raw_w1 <- data.frame(
  ID = 1:3,
  Age = c(25, 30, 45),
  Gender = c("Male", "Female", "Male"),
  Satisfaction = c("High", "Low", "High"),
  stringsAsFactors = FALSE
)

raw_w2 <- data.frame(
  p_id = 4:6,
  age_years = c(26, 31, 46),
  sex = c(1, 2, 1), # 1=Male, 2=Female
  sat_score = c(3, 1, 3) # 1=Low, 3=High
)

# Combine into a named list (Required input format)
raw_waves <- list(w1 = raw_w1, w2 = raw_w2)

# --- 1. Initialize the Dictionary (The "Scan") ---
# This function scans your data, standardizes names (clean_names),
# and hunts for factors (text or numeric codes).

dict_path <- "project_dictionary.xlsx"

# Note: We use a 'prefix' so generic columns start as 'demo_001', 'demo_002'...
dict_init(
  raw_waves,
  type_prefix = "demo",
  match_by = "position",
  save_path = dict_path
  )

# OUTPUT:
# v Sanitizing input names (janitor::clean_names)...
# v Scanning 2 datasets for variables and factors...
# v Dictionary initialized at 'project_dictionary.xlsx'

# --- 2. The Human-in-the-Loop (Simulated Editing) ---
# NORMALLY: You would now open 'project_dictionary.xlsx' in Excel.
# You would:
#   1. Rename 'target_name' from 'demo_001' to 'age', 'demo_002' to 'gender'.
#   2. Align 'age' and 'age_years' onto the same row.
#   3. Define that 1=Male and 2=Female in the 'Factor_Levels' sheet.

# FOR THIS EXAMPLE: We will simulate these edits programmatically.
library(openxlsx)
wb <- loadWorkbook(dict_path)

# A. Simulating "Variable_Map_Wide" Edits
# We map w1$age and w2$age_years to the same target 'age'
wide_map <- read.xlsx(dict_path, sheet = "Variable_Map_Wide")

# Let's say row 2 is Age. We set target to 'age'
wide_map$target_name[which(wide_map$orig_w1 == "age")] <- "age"
# We align w2 'age_years' to this row
wide_map$orig_w2[which(wide_map$orig_w1 == "age")] <- "age_years"

# Let's say row 3 is Gender. We set target to 'gender'
wide_map$target_name[which(wide_map$orig_w1 == "gender")] <- "gender"
wide_map$orig_w2[which(wide_map$orig_w1 == "gender")] <- "sex" # Align 'sex' here

writeData(wb, "Variable_Map_Wide", wide_map)

# B. Simulating "Factor_Levels" Edits
# We ensure the numeric codes match our desired output (1=Male, 2=Female)
f_map <- read.xlsx(dict_path, sheet = "Factor_Levels")

# For W1 (Male/Female text), assign them codes 1 and 2
f_map$standard_code[f_map$original_level == "Male"] <- 1
f_map$standard_code[f_map$original_level == "Female"] <- 2

# For W2 (1/2 numeric), they already map to 1/2, so we are good.
# But we ensure they point to the target 'gender'
f_map$target_name[f_map$original_variable %in% c("gender", "sex")] <- "gender"

writeData(wb, "Factor_Levels", f_map)
saveWorkbook(wb, dict_path, overwrite = TRUE)

message(">> User edits simulated.")


# --- 3. Validate the Dictionary (The "Gatekeeper") ---
# Before running any code, we check if our Excel edits broke anything.
# This checks for: Duplicate targets, Snake_case violations, Missing columns.

tryCatch({
  dict_validate(dict_path)
  message(">> Validation Passed! Ready for harmonization.")
}, error = function(e) {
  message("!! Validation Failed: ", e$message)
})

# OUTPUT:
# v Validating dictionary structure...
# v Dictionary 'project_dictionary.xlsx' passed validation.
# >> Validation Passed! Ready for harmonization.
