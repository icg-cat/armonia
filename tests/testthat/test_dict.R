# NOTE: This is Step 2 (Testing). Logic is frozen.

# --- HELPER: Synthetic Data Generator ---
make_synthetic_waves <- function() {
  # Wave 1: "Standard" (Factors as text)
  w1 <- data.frame(
    ID = 1:5,
    Age = c(25, 30, 45, 22, 50),
    Gender = c("Male", "Female", "Male", "Male", "Female"),
    Q1_Sat = c("High", "Low", "High", "Med", "High"),
    stringsAsFactors = FALSE
  )

  # Wave 2: "Chaotic" (Different names, Factors as codes, new cols)
  w2 <- data.frame(
    p_id = 6:10,
    age_yrs = c(26, 31, 46, 23, 51),
    sex = c(1, 2, 1, 1, 2), # 1=Male, 2=Female (coded)
    satisfaction = factor(c("High", "Low", "High", "Med", "High")),
    income = c(1000, 2000, 3000, 4000, 5000)
  )

  return(list(w1 = w1, w2 = w2))
}

# --- TESTS: Module A (Dictionary) ---

test_that("dict_init sorts alphabetically (match_by = 'name')", {

  # Setup: Create variables that would be far apart if not sorted
  # "zebra" would come after "apple" normally.
  # "apple_sauce" should be right next to "apple".
  w1 <- data.frame(zebra = 1, apple = 1, apple_sauce = 1)

  tmp_path <- file.path(tempdir(), "test_sort.xlsx")

  # Run
  dict_init(list(w1 = w1), match_by = "name", save_path = tmp_path)

  # Verify
  wb <- openxlsx::read.xlsx(tmp_path, sheet = "Variable_Map_Wide")

  # target_name should be: var_001, var_002, var_003
  # orig_w1 should be: apple, apple_sauce, zebra (Sorted!)
  expect_equal(wb$orig_w1, c("apple", "apple_sauce", "zebra"))

  unlink(tmp_path)
})

test_that("dict_init aligns positionally (match_by = 'position')", {

  # Setup: Different languages, same structure
  w_en <- data.frame(age = 1, gender = "M")
  w_es <- data.frame(edad = 1, genero = "H") # Same position, diff name

  tmp_path <- file.path(tempdir(), "test_pos.xlsx")

  # Run
  dict_init(list(en = w_en, es = w_es), match_by = "position", save_path = tmp_path)

  # Verify
  wb <- openxlsx::read.xlsx(tmp_path, sheet = "Variable_Map_Wide")

  # Row 1 should have BOTH 'age' and 'edad'
  expect_equal(wb$orig_en[1], "age")
  expect_equal(wb$orig_es[1], "edad")

  # Row 2 should have 'gender' and 'genero'
  expect_equal(wb$orig_en[2], "gender")
  expect_equal(wb$orig_es[2], "genero")

  unlink(tmp_path)
})

test_that("dict_init enforces sanitization and creates valid structure", {

  # Setup
  tmp_dir <- tempdir()
  tmp_path <- file.path(tmp_dir, "test_dict.xlsx")
  raw_data <- make_synthetic_waves()

  # 1. SUCCESS CASE: Initialization
  expect_no_error(
    dict_init(raw_data, type_prefix = "test", save_path = tmp_path)
  )

  expect_true(file.exists(tmp_path))

  # 2. VERIFY CONTENT (The "Rosetta Stone")
  wb <- openxlsx::loadWorkbook(tmp_path)

  # Check Sheets
  sheets <- names(wb)
  expect_true(all(c("Variable_Map_Wide", "Factor_Levels") %in% sheets))

  # Check Variable Map (Sanitization Check)
  wide_map <- openxlsx::read.xlsx(tmp_path, sheet = "Variable_Map_Wide")

  # Names should be lowercased by janitor (e.g., 'Gender' -> 'gender')
  expect_true("gender" %in% wide_map$orig_w1)
  expect_true("sex" %in% wide_map$orig_w2)

  # Target names should follow pattern 'test_001', 'test_002'...
  expect_true(any(grepl("test_00", wide_map$target_name)))

  # Check Factor Map
  f_map <- openxlsx::read.xlsx(tmp_path, sheet = "Factor_Levels")

  # Wave 1 'Gender' should be detected as factor (text)
  w1_factors <- f_map[f_map$wave == "orig_w1", ]
  expect_true("gender" %in% w1_factors$original_variable)
  expect_true("Male" %in% w1_factors$original_level)

  # Wave 2 'sex' (numeric codes 1,2) is NOT flagged as a factor: the
  # detect_factor_potential() numeric early-exit guard (added Session 4)
  # short-circuits before the n_unique threshold for purely numeric vectors.
  w2_factors <- f_map[f_map$wave == "orig_w2", ]
  expect_false("sex" %in% w2_factors$original_variable)

  # Cleanup
  unlink(tmp_path)
})

test_that("dict_validate implements 'Fail Fast' correctly", {

  # Setup
  tmp_dir <- tempdir()
  valid_path <- file.path(tmp_dir, "valid_dict.xlsx")

  # Create a valid base first
  raw <- make_synthetic_waves()
  dict_init(raw, save_path = valid_path)

  # 1. SUCCESS CASE
  expect_true(dict_validate(valid_path) == TRUE)

  # 2. FAILURE: Missing Columns in Map
  bad_map_path <- file.path(tmp_dir, "bad_map.xlsx")
  wb <- openxlsx::loadWorkbook(valid_path)

  df_map <- openxlsx::read.xlsx(valid_path, sheet = "Variable_Map_Wide")
  df_map$target_name <- NULL
  openxlsx::writeData(wb, "Variable_Map_Wide", df_map)
  openxlsx::saveWorkbook(wb, bad_map_path, overwrite = TRUE)

  # Regex matching: "missing" AND "target_name" anywhere in string
  expect_error(dict_validate(bad_map_path), "missing.*target_name")

  # 3. FAILURE: Duplicate Primary Keys (Target Names)
  dupe_path <- file.path(tmp_dir, "dupe_dict.xlsx")
  wb <- openxlsx::loadWorkbook(valid_path)
  df_map <- openxlsx::read.xlsx(valid_path, sheet = "Variable_Map_Wide")

  # Create duplicate
  df_map$target_name[2] <- df_map$target_name[1]
  openxlsx::writeData(wb, "Variable_Map_Wide", df_map)
  openxlsx::saveWorkbook(wb, dupe_path, overwrite = TRUE)

  # Regex matching: "Duplicate" AND "target_name"
  expect_error(dict_validate(dupe_path), "Duplicate.*target_name")

  # 4. FAILURE: Snake Case Violation
  snake_path <- file.path(tmp_dir, "snake_fail.xlsx")
  wb <- openxlsx::loadWorkbook(valid_path)
  df_map <- openxlsx::read.xlsx(valid_path, sheet = "Variable_Map_Wide")

  df_map$target_name[1] <- "Bad Variable Name"
  openxlsx::writeData(wb, "Variable_Map_Wide", df_map)
  openxlsx::saveWorkbook(wb, snake_path, overwrite = TRUE)

  # We expect the error message from cli_abort, NOT the cli_alert preceding it.
  expect_error(dict_validate(snake_path), "must be lowercase")

  # Cleanup
  unlink(c(valid_path, bad_map_path, dupe_path, snake_path))
})

test_that("dict_init handles empty input", {
  expect_error(dict_init(list()), "must be a non-empty list")
})

test_that("dict_init's Factor_Levels sheet has no phantom trailing row", {
  # Regression test: the embedded XLOOKUP formula used to be written one row
  # past the actual table data (an off-by-one in data_row_indices), leaving
  # a spurious all-NA row that broke dict_apply() when chained directly
  # after dict_init() without an intervening Excel round-trip.
  w1 <- data.frame(id = c("alice", "bob"), gender = c("Female", "Male"))
  tmp_path <- file.path(tempdir(), "phantom_row_check.xlsx")
  on.exit(unlink(tmp_path))

  dict_init(list(w1 = w1), save_path = tmp_path)

  f_map <- readxl::read_excel(tmp_path, sheet = "Factor_Levels", col_types = "text")

  # 2 unique values each for id and gender: exactly 4 real rows, no extras.
  expect_equal(nrow(f_map), 4)
  expect_false(any(is.na(f_map$wave)))
})

test_that("dict_validate fails when file does not exist", {
  expect_error(dict_validate("path/to/nonexistent.xlsx"), regexp = "not found")
})

test_that("dict_validate fails when a required sheet is missing", {
  tmp_path <- file.path(tempdir(), "missing_sheet.xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Variable_Map_Wide")
  openxlsx::writeData(wb, "Variable_Map_Wide", data.frame(target_name = "x"))
  # Deliberately omit Factor_Levels sheet
  openxlsx::saveWorkbook(wb, tmp_path, overwrite = TRUE)

  expect_error(dict_validate(tmp_path), regexp = "[Mm]issing.*sheets")
  unlink(tmp_path)
})

test_that("dict_validate fails when standard_label conflicts across waves for the same standard_code", {
  tmp_path <- file.path(tempdir(), "label_conflict.xlsx")
  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb, "Variable_Map_Wide")
  openxlsx::writeData(wb, "Variable_Map_Wide", data.frame(
    target_name = "gender", description = NA_character_
  ))

  # standard_code 1 maps to "Female" in wave 1 but "Mujer" in wave 2, conflict
  openxlsx::addWorksheet(wb, "Factor_Levels")
  openxlsx::writeData(wb, "Factor_Levels", data.frame(
    wave              = c("orig_w1", "orig_w1", "orig_w2", "orig_w2"),
    original_variable = c("gender",  "gender",  "genero",  "genero"),
    target_name       = c("gender",  "gender",  "gender",  "gender"),
    original_level    = c("Female",  "Male",    "Mujer",   "Hombre"),
    standard_code     = c(1, 2, 1, 2),
    standard_label    = c("Female",  "Male",    "Mujer",   "Male")  # code 1: conflict
  ))

  openxlsx::saveWorkbook(wb, tmp_path, overwrite = TRUE)

  expect_error(dict_validate(tmp_path), regexp = "label conflict")
  unlink(tmp_path)
})

test_that("dict_validate fails when standard_code contains non-numeric values", {
  tmp_path <- file.path(tempdir(), "non_numeric.xlsx")
  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb, "Variable_Map_Wide")
  openxlsx::writeData(wb, "Variable_Map_Wide", data.frame(
    target_name = "gender", description = NA_character_
  ))

  openxlsx::addWorksheet(wb, "Factor_Levels")
  openxlsx::writeData(wb, "Factor_Levels", data.frame(
    wave = "orig_w1", original_variable = "sex",
    target_name = "gender", original_level = "Male",
    standard_code = "not_a_number", standard_label = "Male"
  ))

  openxlsx::saveWorkbook(wb, tmp_path, overwrite = TRUE)

  expect_error(dict_validate(tmp_path), regexp = "non-numeric")
  unlink(tmp_path)
})
