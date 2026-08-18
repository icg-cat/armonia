# Helper: write a Variable_Map_Wide data frame to a temp xlsx and return path.
make_loss_dict <- function(wide_map_df) {
  tmp <- tempfile(fileext = ".xlsx")
  wb  <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Variable_Map_Wide")
  openxlsx::writeData(wb, "Variable_Map_Wide", wide_map_df)
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)
  tmp
}

# ── Fixtures ────────────────────────────────────────────────────────────────

make_clean_fixtures <- function() {
  # Two waves with identical structure; master is a clean row-bind
  w1 <- data.frame(id = 1:3, age = c(25L, 30L, 35L), stringsAsFactors = FALSE)
  w2 <- data.frame(id = 4:6, age = c(26L, 31L, 36L), stringsAsFactors = FALSE)

  master <- data.frame(
    source_wave = c("w1", "w1", "w1", "w2", "w2", "w2"),
    id          = 1:6,
    age         = c(25L, 30L, 35L, 26L, 31L, 36L),
    stringsAsFactors = FALSE
  )

  wide_map <- data.frame(
    target_name = c("id", "age"),
    description = NA_character_,
    orig_w1     = c("id", "age"),
    orig_w2     = c("id", "age"),
    stringsAsFactors = FALSE
  )

  dict_path <- make_loss_dict(wide_map)
  list(data_list = list(w1 = w1, w2 = w2), master = master, dict = dict_path)
}

# ── Tests ────────────────────────────────────────────────────────────────────

test_that("check_data_loss returns NULL invisibly when all checks pass", {
  f <- make_clean_fixtures()
  on.exit(unlink(f$dict))

  result <- check_data_loss(f$data_list, f$master, f$dict)
  expect_null(result)
})

test_that("check_data_loss flags row count mismatch", {
  f <- make_clean_fixtures()
  on.exit(unlink(f$dict))

  # Remove one row from wave 1 in the master
  master_short <- f$master[-1, ]

  expect_warning(
    result <- check_data_loss(f$data_list, master_short, f$dict),
    regexp = "issue"
  )

  expect_s3_class(result, "tbl_df")
  row_check <- result[result$check == "row_count" & result$wave == "w1", ]
  expect_equal(nrow(row_check), 1L)
  expect_equal(row_check$status, "warn")
  expect_equal(as.numeric(row_check$delta), -1)
})

test_that("check_data_loss flags NA inflation from unmapped factor levels", {
  # Wave 1 gender: 0 NAs in original; master gets NAs because a level was unmapped
  w1 <- data.frame(gender = c("Male", "Female", "Male"), stringsAsFactors = FALSE)

  # Simulate a master where the recoding produced NAs (one level was absent from dict)
  master <- data.frame(
    source_wave = c("w1", "w1", "w1"),
    gender      = factor(c("1", NA, "1")),   # "Female" had no mapping → NA
    stringsAsFactors = FALSE
  )

  wide_map <- data.frame(
    target_name = "gender",
    description = NA_character_,
    orig_w1     = "gender",
    stringsAsFactors = FALSE
  )
  dict_path <- make_loss_dict(wide_map)
  on.exit(unlink(dict_path))

  expect_warning(
    result <- check_data_loss(list(w1 = w1), master, dict_path),
    regexp = "issue"
  )

  na_row <- result[result$check == "na_count" & result$wave == "w1", ]
  expect_equal(nrow(na_row), 1L)
  expect_equal(na_row$status, "warn")
  expect_equal(as.numeric(na_row$delta), 1)
})

test_that("check_data_loss flags dropped columns", {
  # Original has 3 columns; dict only maps 2 → third is dropped
  w1 <- data.frame(id = 1:3, age = c(25L, 30L, 35L), extra = c("a", "b", "c"),
                   stringsAsFactors = FALSE)

  master <- data.frame(
    source_wave = c("w1", "w1", "w1"),
    id          = 1:3,
    age         = c(25L, 30L, 35L),
    stringsAsFactors = FALSE
  )

  # Dict maps only id and age; extra is not listed
  wide_map <- data.frame(
    target_name = c("id", "age"),
    description = NA_character_,
    orig_w1     = c("id", "age"),
    stringsAsFactors = FALSE
  )
  dict_path <- make_loss_dict(wide_map)
  on.exit(unlink(dict_path))

  expect_warning(
    result <- check_data_loss(list(w1 = w1), master, dict_path),
    regexp = "issue"
  )

  dropped_row <- result[result$check == "dropped_column", ]
  expect_equal(nrow(dropped_row), 1L)
  expect_equal(dropped_row$target_name, "extra")
})

test_that("check_data_loss flags unique value collapse for factor columns", {
  # Original has 3 gender categories; master factor has only 2 (mapping collision)
  w1 <- data.frame(gender = c("Male", "Female", "Other"), stringsAsFactors = FALSE)

  master <- data.frame(
    source_wave = c("w1", "w1", "w1"),
    gender      = factor(c("1", "2", "1")),  # "Other" mapped to same code as "Male"
    stringsAsFactors = FALSE
  )

  wide_map <- data.frame(
    target_name = "gender",
    description = NA_character_,
    orig_w1     = "gender",
    stringsAsFactors = FALSE
  )
  dict_path <- make_loss_dict(wide_map)
  on.exit(unlink(dict_path))

  expect_warning(
    result <- check_data_loss(list(w1 = w1), master, dict_path),
    regexp = "issue"
  )

  u_row <- result[result$check == "unique_values" & result$wave == "w1", ]
  expect_equal(nrow(u_row), 1L)
  expect_equal(u_row$status, "warn")
  expect_true(as.numeric(u_row$delta) < 0)
})

test_that("check_data_loss aborts when master has no source_wave column", {
  f <- make_clean_fixtures()
  on.exit(unlink(f$dict))

  bad_master <- f$master
  bad_master$source_wave <- NULL

  expect_error(
    check_data_loss(f$data_list, bad_master, f$dict),
    regexp = "source_wave"
  )
})
