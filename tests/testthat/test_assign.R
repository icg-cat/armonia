# Testing assign_standard_labels

# Helper: write a Factor_Levels-only dictionary to a temp .xlsx and return its path
make_factor_levels_dict <- function(factor_levels) {
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(Factor_Levels = factor_levels), file = path)
  path
}

test_that("assign_standard_labels replaces integer codes with human-readable labels", {
  data <- data.frame(
    gender = factor(c("1", "2", "1")),
    stringsAsFactors = FALSE
  )
  dict <- make_factor_levels_dict(data.frame(
    target_name    = c("gender", "gender"),
    standard_code  = c("1", "2"),
    standard_label = c("Male", "Female"),
    stringsAsFactors = FALSE
  ))

  result <- assign_standard_labels(data, dict)

  expect_s3_class(result$gender, "factor")
  expect_equal(as.character(result$gender), c("Male", "Female", "Male"))
})

test_that("assign_standard_labels leaves non-factor columns untouched", {
  data <- data.frame(
    age    = c(25, 30, 45),
    gender = factor(c("1", "2", "1")),
    stringsAsFactors = FALSE
  )
  dict <- make_factor_levels_dict(data.frame(
    target_name    = c("gender", "gender"),
    standard_code  = c("1", "2"),
    standard_label = c("Male", "Female"),
    stringsAsFactors = FALSE
  ))

  result <- assign_standard_labels(data, dict)

  expect_equal(result$age, data$age)
  expect_equal(as.character(result$gender), c("Male", "Female", "Male"))
})

test_that("assign_standard_labels warns for factor columns absent from dictionary", {
  data <- data.frame(
    missing_var = factor(c("1", "2")),
    stringsAsFactors = FALSE
  )
  dict <- make_factor_levels_dict(data.frame(
    target_name    = "other_var",
    standard_code  = "1",
    standard_label = "Some label",
    stringsAsFactors = FALSE
  ))

  expect_warning(
    assign_standard_labels(data, dict),
    regexp = "not found in dictionary"
  )
})

test_that("assign_standard_labels errors on conflicting labels for the same standard_code", {
  data <- data.frame(
    gender = factor(c("1", "2")),
    stringsAsFactors = FALSE
  )
  dict <- make_factor_levels_dict(data.frame(
    target_name    = c("gender", "gender"),
    standard_code  = c("1", "1"),      # same code, two different labels -> conflict
    standard_label = c("Male", "Man"),
    stringsAsFactors = FALSE
  ))

  expect_error(
    assign_standard_labels(data, dict),
    regexp = "Metadata Conflict"
  )
})
