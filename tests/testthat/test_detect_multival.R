# Testing detect_multival

test_that("detect_multival flags columns with delimiter-separated values", {
  data <- data.frame(
    id       = c("alice", "bob", "carol"),
    symptoms = c("headache; cramps", "None", "cramps; headache"),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(detect_multival(data))

  expect_named(result, "symptoms")
  expect_setequal(result$symptoms, c("cramps", "headache", "None"))
})

test_that("detect_multival ignores columns with no multi-value cells", {
  data <- data.frame(
    id     = c("alice", "bob"),
    gender = c("Female", "Male"),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(detect_multival(data))

  expect_equal(result, list())
})

test_that("detect_multival trims whitespace from split values by default", {
  data <- data.frame(
    symptoms = c("headache ,  cramps", "None"),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(detect_multival(data))

  expect_setequal(result$symptoms, c("cramps", "headache", "None"))
})

test_that("detect_multival reports no columns found when nothing splits", {
  data <- data.frame(id = c("alice", "bob"), stringsAsFactors = FALSE)

  expect_message(detect_multival(data), regexp = "No multi-value columns detected")
})
