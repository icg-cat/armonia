# Testing split_multival

test_that("split_multival creates one logical column per category", {
  data <- data.frame(
    id       = c("alice", "bob"),
    symptoms = c("headache; cramps", "None"),
    stringsAsFactors = FALSE
  )

  result <- split_multival(data, col = "symptoms",
                            new_names = c("headache", "cramps", "None"),
                            prefix = "sympt")

  expect_true(all(c("sympt_headache", "sympt_cramps", "sympt_None") %in% names(result)))
  expect_equal(result$sympt_headache, c(TRUE, FALSE))
  expect_equal(result$sympt_cramps,   c(TRUE, FALSE))
  expect_equal(result$sympt_None,     c(FALSE, TRUE))
})

test_that("split_multival preserves NA rows across all new columns", {
  data <- data.frame(
    id       = c("alice", "bob"),
    symptoms = c("headache", NA),
    stringsAsFactors = FALSE
  )

  result <- split_multival(data, col = "symptoms", new_names = "headache", prefix = "sympt")

  expect_true(is.na(result$sympt_headache[2]))
})

test_that("split_multival can return only the dummy columns when fulldata = FALSE", {
  data <- data.frame(
    id       = "alice",
    symptoms = "headache",
    stringsAsFactors = FALSE
  )

  result <- split_multival(data, col = "symptoms", new_names = "headache",
                            prefix = "sympt", fulldata = FALSE)

  expect_named(result, "sympt_headache")
})

test_that("split_multival warns on values not present in new_names", {
  data <- data.frame(symptoms = "unlisted_symptom", stringsAsFactors = FALSE)

  expect_warning(
    split_multival(data, col = "symptoms", new_names = "headache", prefix = "sympt"),
    regexp = "not present in"
  )
})

test_that("split_multival errors when new_names collides with an existing column", {
  data <- data.frame(headache = 1, symptoms = "headache", stringsAsFactors = FALSE)

  expect_error(
    split_multival(data, col = "symptoms", new_names = "headache", prefix = ""),
    regexp = "collide"
  )
})

test_that("split_multival errors on a column not found in data", {
  data <- data.frame(symptoms = "headache", stringsAsFactors = FALSE)

  expect_error(
    split_multival(data, col = "missing_col", new_names = "headache", prefix = "sympt"),
    regexp = "not found"
  )
})
