# Testing Module D: Validation & Integrity

# --- Tests: check_id_audit ---

test_that("check_id_audit identifies exact matches and best matches for typos", {
  ids_t1 <- c("user@test.com", "researcher@univ.edu", "admin@site.org")
  ids_t2 <- c("user@test.com", "researcher@unv.edu")  # second has a 1-char typo

  tmp_wb <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(Instructions = data.frame(Info = "Test")), file = tmp_wb)
  on.exit(unlink(tmp_wb))

  result <- check_id_audit(ids_t1, ids_t2, tmp_wb)

  # Exact match: should be flagged as equal
  expect_true(result$all_equal[1])

  # Typo: not equal, but best_match should point to the correct reference ID
  expect_false(result$all_equal[2])
  expect_equal(result$best_match[2], "researcher@univ.edu")

  # Results written to Excel
  wb_content <- openxlsx::read.xlsx(tmp_wb, sheet = "Review_IDs")
  expect_equal(nrow(wb_content), 2)
})

test_that("check_id_audit returns 'no best match found' for completely unrelated IDs", {
  ids_t1 <- c("aaa")
  ids_t2 <- c("zzzzzzzzzzz")  # sum of edit distances far exceeds string length

  tmp_wb <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(Instructions = data.frame(Info = "Test")), file = tmp_wb)
  on.exit(unlink(tmp_wb))

  result <- check_id_audit(ids_t1, ids_t2, tmp_wb)

  expect_equal(result$best_match[1], "no best match found")
})

# --- Tests: check_integrity ---

test_that("check_integrity passes on tidy data and is silent with verbose = FALSE", {
  tidy_df <- data.frame(
    pk_hash     = c("h1", "h2", "h1"),
    source_wave = c("w1", "w1", "w2"),
    category    = factor(c("1", "2", "1")),
    stringsAsFactors = FALSE
  )

  expect_silent(check_integrity(tidy_df, "pk_hash", "source_wave", verbose = FALSE))
})

test_that("check_integrity emits a success message with verbose = TRUE", {
  tidy_df <- data.frame(
    pk_hash     = c("h1", "h2", "h1"),
    source_wave = c("w1", "w1", "w2"),
    category    = factor(c("1", "2", "1")),
    stringsAsFactors = FALSE
  )

  expect_message(check_integrity(tidy_df, "pk_hash", "source_wave", verbose = TRUE))
})

test_that("check_integrity fails on non-tidy data (duplicate id x time combinations)", {
  tidy_df <- data.frame(
    pk_hash     = c("h1", "h2", "h1"),
    source_wave = c("w1", "w1", "w2"),
    stringsAsFactors = FALSE
  )
  dupe_df <- rbind(tidy_df, tidy_df[1, ])

  expect_error(
    check_integrity(dupe_df, "pk_hash", "source_wave"),
    regexp = "[Ii]ntegrity"
  )
})

test_that("check_integrity warns when factor columns have non-numeric levels", {
  df <- data.frame(
    pk_hash     = c("h1", "h2"),
    source_wave = c("w1", "w2"),
    category    = factor(c("Low", "High")),  # not numeric codes
    stringsAsFactors = FALSE
  )

  expect_warning(
    check_integrity(df, "pk_hash", "source_wave", verbose = FALSE),
    regexp = "non-numeric"
  )
})

test_that("check_integrity with time_col = NULL checks uniqueness on id_col alone", {
  cross_sectional <- data.frame(
    pk_hash  = c("h1", "h2"),
    category = factor(c("1", "2")),
    stringsAsFactors = FALSE
  )

  expect_silent(check_integrity(cross_sectional, "pk_hash", time_col = NULL, verbose = FALSE))
})

test_that("check_integrity with time_col = NULL fails on duplicate ids", {
  dupe_ids <- data.frame(
    pk_hash  = c("h1", "h1"),
    category = factor(c("1", "2")),
    stringsAsFactors = FALSE
  )

  expect_error(
    check_integrity(dupe_ids, "pk_hash", time_col = NULL),
    regexp = "duplicate values"
  )
})

# --- Tests: normalize_id ---

test_that("normalize_id transliterates accented characters to ASCII", {
  expect_equal(normalize_id("café"), "cafe")
  expect_equal(normalize_id(c("joão", "maría")), c("joao", "maria"))
})

test_that("normalize_id removes whitespace by default", {
  expect_equal(normalize_id("john doe"), "johndoe")
})

test_that("normalize_id can skip transliteration and whitespace removal independently", {
  expect_equal(normalize_id("café latte", translit = FALSE, remove_whitespace = FALSE), "café latte")
  expect_equal(normalize_id("john doe", translit = FALSE), "johndoe")
})

test_that("normalize_id combined with case-insensitive matching resolves accent-only mismatches", {
  ref_ids   <- normalize_id(c("café", "joão"))
  audit_ids <- normalize_id(c("Café", "JOÃO"))

  tmp_wb <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(Instructions = data.frame(Info = "Test")), file = tmp_wb)
  on.exit(unlink(tmp_wb))

  result <- check_id_audit(ref_ids, audit_ids, tmp_wb, case_sensitive = FALSE)

  expect_true(all(result$all_equal))
})
