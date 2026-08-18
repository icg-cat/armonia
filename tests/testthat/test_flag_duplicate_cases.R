# Helper: build a small test dataset
make_test_data <- function() {
  tibble::tibble(
    source_wave = c("w1", "w2", "w1", "w2", "w1"),
    id          = c(  1,    1,    2,    2,    3),
    mood        = c("Good", "Good", "Bad", "Good", "Neutral"),
    score       = c(    8,      8,     3,      7,        5)
  )
}

# ── success: cross-wave duplicate flagged ─────────────────────────────────────

test_that("flag_duplicate_cases flags rows with same id, different waves, high similarity", {
  df <- make_test_data()
  result <- suppressMessages(flag_duplicate_cases(df, threshold = 0.90))

  # id=1 in w1 and w2 are identical → should be flagged
  flagged <- result[result$id == 1, ]
  expect_true(all(flagged$potential_duplicate))
})

# ── success: same-wave pair NOT flagged ───────────────────────────────────────

test_that("flag_duplicate_cases does not flag same-wave rows", {
  df <- tibble::tibble(
    source_wave = c("w1", "w1"),
    id          = c(1, 1),
    mood        = c("Good", "Good"),
    score       = c(8, 8)
  )
  result <- suppressMessages(flag_duplicate_cases(df, threshold = 0.90))
  expect_false(any(result$potential_duplicate))
})

# ── success: below-threshold pair NOT flagged ─────────────────────────────────

test_that("flag_duplicate_cases does not flag pairs below threshold", {
  df <- make_test_data()
  # id=2: mood differs across waves → similarity = 0.5 (1 of 2 cols match)
  result <- suppressMessages(flag_duplicate_cases(df, threshold = 0.90))
  flagged_id2 <- result[result$id == 2, ]
  expect_false(any(flagged_id2$potential_duplicate))
})

# ── success: id appearing only in one wave not touched ────────────────────────

test_that("flag_duplicate_cases does not flag single-wave ids", {
  df <- make_test_data()
  result <- suppressMessages(flag_duplicate_cases(df))
  # id=3 only in w1 — should not be flagged
  expect_false(result$potential_duplicate[result$id == 3])
})

# ── duplicate_pairs attribute is attached ────────────────────────────────────

test_that("duplicate_pairs attribute contains the flagged pairs", {
  df <- make_test_data()
  result <- suppressMessages(flag_duplicate_cases(df, threshold = 0.90))
  pairs  <- attr(result, "duplicate_pairs")
  expect_true(is.data.frame(pairs))
  expect_true("similarity" %in% names(pairs))
  expect_true(nrow(pairs) >= 1)
})

# ── failure: missing id_col ───────────────────────────────────────────────────

test_that("flag_duplicate_cases aborts when id_col not found", {
  df <- make_test_data()
  expect_error(
    flag_duplicate_cases(df, id_col = "nonexistent"),
    class = "rlang_error"
  )
})

# ── failure: missing wave_col ─────────────────────────────────────────────────

test_that("flag_duplicate_cases aborts when wave_col not found", {
  df <- make_test_data()
  expect_error(
    flag_duplicate_cases(df, wave_col = "nonexistent"),
    class = "rlang_error"
  )
})

# ── NA == NA counts as match ──────────────────────────────────────────────────

test_that("NA == NA is treated as a match for similarity", {
  df <- tibble::tibble(
    source_wave = c("w1", "w2"),
    id          = c(1, 1),
    mood        = c(NA_character_, NA_character_),
    score       = c(NA_character_, NA_character_)
  )
  result <- suppressMessages(flag_duplicate_cases(df, threshold = 1.0))
  expect_true(all(result$potential_duplicate))
})
