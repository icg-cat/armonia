# --- TESTS: check_bind_ready() ---

test_that("check_bind_ready returns NULL invisibly when all classes match", {
  w1 <- data.frame(age = c(25L, 30L), score = c(1.1, 2.2), stringsAsFactors = FALSE)
  w2 <- data.frame(age = c(35L, 40L), score = c(3.3, 4.4), stringsAsFactors = FALSE)

  result <- check_bind_ready(list(wave1 = w1, wave2 = w2))
  expect_null(result)
})

test_that("check_bind_ready returns conflict tibble and warns on class mismatch", {
  w1 <- data.frame(age = c(25L, 30L),   score = "high",  stringsAsFactors = FALSE)
  w2 <- data.frame(age = c(35.5, 40.5), score = "low", stringsAsFactors = FALSE)
  # age: integer vs numeric -> conflict; score: character both -> no conflict

  expect_warning(
    result <- check_bind_ready(list(wave1 = w1, wave2 = w2)),
    regexp = "mismatched classes"
  )

  expect_s3_class(result, "tbl_df")
  expect_true("target_name" %in% names(result))
  expect_true("wave" %in% names(result))
  expect_true("r_class" %in% names(result))
  expect_true("age" %in% result$target_name)
  expect_false("score" %in% result$target_name)
})

test_that("check_bind_ready aborts on empty list", {
  expect_error(check_bind_ready(list()), "non-empty")
})

test_that("check_bind_ready aborts on unnamed list", {
  w1 <- data.frame(x = 1)
  w2 <- data.frame(x = 2)
  expect_error(check_bind_ready(list(w1, w2)), "named")
})

test_that("check_bind_ready returns NULL for a single-wave list (nothing to compare)", {
  w1 <- data.frame(age = 25L, score = 1.1, stringsAsFactors = FALSE)
  result <- check_bind_ready(list(wave1 = w1))
  expect_null(result)
})

test_that("check_bind_ready reports all conflicting waves, not just first pair", {
  w1 <- data.frame(x = 1L,   stringsAsFactors = FALSE)
  w2 <- data.frame(x = 1.5,  stringsAsFactors = FALSE)
  w3 <- data.frame(x = "a",  stringsAsFactors = FALSE)

  expect_warning(
    result <- check_bind_ready(list(w1 = w1, w2 = w2, w3 = w3)),
    regexp = "mismatched classes"
  )

  expect_equal(nrow(result), 3)  # one row per (x, wave) pair
  expect_setequal(result$wave, c("w1", "w2", "w3"))
})
