# Helper: build a minimal valid armonia dictionary in a temp file
make_test_dict <- function(path) {
  v_map <- data.frame(
    target_name = c("id", "mood", "score"),
    description = c(NA, NA, NA),
    orig_w1     = c("id", "mood", "score"),
    stringsAsFactors = FALSE
  )
  f_levels <- data.frame(
    wave              = c("orig_w1", "orig_w1", "orig_w1"),
    original_variable = c("mood",    "mood",    "mood"),
    target_name       = c("mood",    "mood",    "mood"),
    original_level    = c("Good",    "Neutral", "Bad"),
    standard_code     = c(1,         2,         3),
    standard_label    = c("Good",    "Neutral", "Bad"),
    stringsAsFactors  = FALSE
  )
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Variable_Map_Wide")
  openxlsx::writeData(wb, "Variable_Map_Wide", v_map)
  openxlsx::addWorksheet(wb, "Factor_Levels")
  openxlsx::writeData(wb, "Factor_Levels", f_levels)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}

# ── compatible data ───────────────────────────────────────────────────────────

test_that("dict_check_compat returns empty tibbles for fully compatible data", {
  dict_path <- tempfile(fileext = ".xlsx")
  make_test_dict(dict_path)

  data <- data.frame(
    source_wave = "w1",
    id          = 1,
    mood        = "Good",
    score       = 8,
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(dict_check_compat(dict_path, data))
  expect_equal(nrow(result$missing_vars),   0)
  expect_equal(nrow(result$missing_levels), 0)
})

# ── new variable in data ──────────────────────────────────────────────────────

test_that("dict_check_compat flags variables in data not in dictionary", {
  dict_path <- tempfile(fileext = ".xlsx")
  make_test_dict(dict_path)

  data <- data.frame(
    source_wave   = "w1",
    id            = 1,
    mood          = "Good",
    score         = 8,
    new_variable  = "something",
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(dict_check_compat(dict_path, data))
  expect_true("new_variable" %in% result$missing_vars$variable)
})

# ── new factor value in data ──────────────────────────────────────────────────

test_that("dict_check_compat flags factor values in data not in Factor_Levels", {
  dict_path <- tempfile(fileext = ".xlsx")
  make_test_dict(dict_path)

  data <- data.frame(
    source_wave = "w1",
    id          = 1,
    mood        = "Excellent",   # not in Factor_Levels
    score       = 8,
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(dict_check_compat(dict_path, data))
  expect_true("Excellent" %in% result$missing_levels$missing_value)
  expect_true("mood" %in% result$missing_levels$target_name)
})

# ── extra vars in dict (informational) ───────────────────────────────────────

test_that("dict_check_compat reports dict variables absent from data", {
  dict_path <- tempfile(fileext = ".xlsx")
  make_test_dict(dict_path)

  # data missing 'score' which is in dict
  data <- data.frame(
    source_wave = "w1",
    id          = 1,
    mood        = "Good",
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(dict_check_compat(dict_path, data))
  expect_true("score" %in% result$extra_vars$variable)
})

# ── invalid dict path ─────────────────────────────────────────────────────────

test_that("dict_check_compat aborts on invalid dict path", {
  data <- data.frame(id = 1)
  expect_error(
    dict_check_compat("nonexistent.xlsx", data),
    class = "rlang_error"
  )
})

# ── update = TRUE: nothing to add ─────────────────────────────────────────────

test_that("dict_check_compat update=TRUE emits info message when nothing to add", {
  dict_path <- tempfile(fileext = ".xlsx")
  make_test_dict(dict_path)

  data <- data.frame(
    orig_w1     = "w1",
    id          = 1,
    mood        = "Good",
    score       = 8,
    stringsAsFactors = FALSE
  )

  expect_message(
    dict_check_compat(dict_path, data, wave_col = "orig_w1", update = TRUE),
    regexp = "Nothing to update"
  )
  # no copy file should be created
  ts        <- format(Sys.time(), "%y%m%d")
  copy_path <- file.path(dirname(dict_path),
                         paste0(ts, "_", tools::file_path_sans_ext(basename(dict_path)), "_copy.xlsx"))
  expect_false(file.exists(copy_path))
})

# ── update = TRUE: missing variable written to Variable_Map_Wide ──────────────

test_that("dict_check_compat update=TRUE appends missing variable to Variable_Map_Wide", {
  dict_path <- tempfile(fileext = ".xlsx")
  make_test_dict(dict_path)

  data <- data.frame(
    orig_w1      = "w1",
    id           = 1,
    mood         = "Good",
    score        = 8,
    new_variable = "something",
    stringsAsFactors = FALSE
  )

  suppressMessages(dict_check_compat(dict_path, data, wave_col = "orig_w1", update = TRUE))

  ts        <- format(Sys.time(), "%y%m%d")
  copy_path <- file.path(dirname(dict_path),
                         paste0(ts, "_", tools::file_path_sans_ext(basename(dict_path)), "_copy.xlsx"))
  expect_true(file.exists(copy_path))

  updated_map <- readxl::read_excel(copy_path, sheet = "Variable_Map_Wide", col_types = "text")
  names(updated_map) <- tolower(names(updated_map))
  expect_true("new_variable" %in% updated_map$orig_w1)
  # target_name for newly added row should be NA
  new_row <- updated_map[!is.na(updated_map$orig_w1) & updated_map$orig_w1 == "new_variable", ]
  expect_true(is.na(new_row$target_name))
})

# ── update = TRUE: missing factor level written to Factor_Levels ──────────────

test_that("dict_check_compat update=TRUE appends missing factor level to Factor_Levels", {
  dict_path <- tempfile(fileext = ".xlsx")
  make_test_dict(dict_path)

  data <- data.frame(
    orig_w1 = "w1",
    id      = 1,
    mood    = "Excellent",   # not in Factor_Levels
    score   = 8,
    stringsAsFactors = FALSE
  )

  suppressMessages(dict_check_compat(dict_path, data, wave_col = "orig_w1", update = TRUE))

  ts        <- format(Sys.time(), "%y%m%d")
  copy_path <- file.path(dirname(dict_path),
                         paste0(ts, "_", tools::file_path_sans_ext(basename(dict_path)), "_copy.xlsx"))
  expect_true(file.exists(copy_path))

  fl <- readxl::read_excel(copy_path, sheet = "Factor_Levels", col_types = "text")
  names(fl) <- tolower(names(fl))
  new_rows <- fl[!is.na(fl$original_level) & fl$original_level == "Excellent", ]
  expect_equal(nrow(new_rows), 1L)
  expect_equal(new_rows$wave,          "orig_w1")
  expect_equal(new_rows$target_name,   "mood")
  expect_true(is.na(new_rows$standard_code))
  expect_true(is.na(new_rows$standard_label))
})

# ── update = TRUE: invalid wave_col aborts ────────────────────────────────────

test_that("dict_check_compat update=TRUE aborts when wave_col not in Variable_Map_Wide", {
  dict_path <- tempfile(fileext = ".xlsx")
  make_test_dict(dict_path)

  data <- data.frame(
    bad_wave = "w1",
    id       = 1,
    mood     = "Excellent",
    stringsAsFactors = FALSE
  )

  expect_error(
    suppressMessages(dict_check_compat(dict_path, data, wave_col = "bad_wave", update = TRUE)),
    class = "rlang_error"
  )
})
