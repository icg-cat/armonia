# Testing internal helper: detect_factor_potential

test_that("detect_factor_potential converts low-cardinality vectors via Likert shortcut", {
  # <= 5 unique values: always converted regardless of sample size
  x <- c("High", "Med", "Low", "High", "Low")
  expect_s3_class(detect_factor_potential(x), "factor")
})

test_that("detect_factor_potential leaves binary numeric vectors unchanged (already numeric-coded)", {
  x <- c(1, 2, 1, 2, 1, 2, 1, 2)
  expect_false(is.factor(detect_factor_potential(x)))
})

test_that("detect_factor_potential leaves character Likert scales unchanged (values parse as numbers)", {
  x <- c("1", "2", "3", "4", "5", "1", "3", "5")
  expect_false(is.factor(detect_factor_potential(x)))
})

test_that("detect_factor_potential converts via pattern test (sufficient cardinality, n, repetition)", {
  # 8 unique values (> 5), n = 50 (>= 30), repetitive categories
  set.seed(1)
  x <- sample(letters[1:8], 50, replace = TRUE)
  expect_s3_class(detect_factor_potential(x), "factor")
})

test_that("detect_factor_potential rejects IDs (no repetition)", {
  # All values unique: rep_ratio = 0, fails pattern test
  x <- paste0("ID_", 1:50)
  expect_false(is.factor(detect_factor_potential(x)))
})

test_that("detect_factor_potential rejects small samples for pattern test", {
  # 8 unique values (> 7 so the Likert/binary shortcut doesn't fire), n = 10 (< 30 floor)
  x <- c("A", "B", "C", "D", "E", "F", "G", "H", "A", "B")
  expect_false(is.factor(detect_factor_potential(x)))
})

test_that("detect_factor_potential returns input unchanged when all values are NA", {
  x <- c(NA_character_, NA_character_, NA_character_)
  expect_equal(detect_factor_potential(x), x)
})
