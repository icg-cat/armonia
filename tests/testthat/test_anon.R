# Testing Module C: Privacy & Anonymization

test_that("anon_clean_email standardizes whitespace, case, and preserves NA/empty", {
  emails <- c("  USER@Example.com ", "test@domain.com", NA, "")

  cleaned <- anon_clean_email(emails)

  expect_equal(cleaned[1], "user@example.com")
  expect_equal(cleaned[2], "test@domain.com")
  expect_true(is.na(cleaned[3]))
  expect_true(is.na(cleaned[4]))
})

test_that("anon_clean_email corrects common domain typos", {
  emails <- c("user@domain.co", "user@domain.con", "user@gogle.com")

  cleaned <- anon_clean_email(emails)

  expect_equal(cleaned[1], "user@domain.com")
  expect_equal(cleaned[2], "user@domain.com")
  expect_equal(cleaned[3], "user@google.com")
})

test_that("anon_hash fails without a salt", {
  old_salt <- Sys.getenv("HARMONIZE_SALT")
  Sys.setenv(HARMONIZE_SALT = "")
  on.exit(Sys.setenv(HARMONIZE_SALT = old_salt))

  expect_error(anon_hash("test@email.com"), regexp = "[Ss]ecurity [Ee]rror")
})

test_that("anon_hash produces consistent 12-char hashes", {
  old_salt <- Sys.getenv("HARMONIZE_SALT")
  Sys.setenv(HARMONIZE_SALT = "test_secret_123")
  on.exit(Sys.setenv(HARMONIZE_SALT = old_salt))

  input <- "researcher@university.edu"
  hash1 <- anon_hash(input)
  hash2 <- anon_hash(input)

  expect_equal(nchar(hash1), 12)
  expect_equal(hash1, hash2)
  expect_type(hash1, "character")
})

test_that("anon_hash output changes when salt changes", {
  old_salt <- Sys.getenv("HARMONIZE_SALT")
  on.exit(Sys.setenv(HARMONIZE_SALT = old_salt))

  input <- "researcher@university.edu"

  Sys.setenv(HARMONIZE_SALT = "salt_one")
  hash_a <- anon_hash(input)

  Sys.setenv(HARMONIZE_SALT = "salt_two")
  hash_b <- anon_hash(input)

  expect_false(hash_a == hash_b)
})

test_that("anon_hash preserves NAs", {
  old_salt <- Sys.getenv("HARMONIZE_SALT")
  Sys.setenv(HARMONIZE_SALT = "test_secret_123")
  on.exit(Sys.setenv(HARMONIZE_SALT = old_salt))

  input <- c("user1@email.com", NA)
  hashed <- anon_hash(input)

  expect_false(is.na(hashed[1]))
  expect_true(is.na(hashed[2]))
})
