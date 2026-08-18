# Testing check_email

test_that("check_email flags a clean address as ok", {
  result <- check_email("alice@uni.edu")

  expect_equal(result$flag, "ok")
  expect_true(is.na(result$reason))
})

test_that("check_email returns one row per input, in order", {
  result <- check_email(c("alice@uni.edu", "bob@gmail.co", "carol@yahoo.com"))

  expect_equal(nrow(result), 3)
  expect_equal(result$email, c("alice@uni.edu", "bob@gmail.co", "carol@yahoo.com"))
})

test_that("check_email flags a missing @ as likely_mistake", {
  result <- check_email("alice.uni.edu")

  expect_equal(result$flag, "likely_mistake")
  expect_match(result$reason, "missing @", fixed = TRUE)
})

test_that("check_email flags multiple @ symbols as likely_mistake", {
  result <- check_email("alice@@uni.edu")

  expect_equal(result$flag, "likely_mistake")
  expect_match(result$reason, "multiple @ symbols", fixed = TRUE)
})

test_that("check_email flags a known domain misspelling as likely_mistake", {
  result <- check_email("alice@gmial.com")

  expect_equal(result$flag, "likely_mistake")
  expect_match(result$reason, "did you mean gmail.com", fixed = TRUE)
})

test_that("check_email flags a mailto: prefix as likely_mistake", {
  result <- check_email("mailto:alice@uni.edu")

  expect_equal(result$flag, "likely_mistake")
  expect_match(result$reason, "mailto: prefix present", fixed = TRUE)
})

test_that("check_email flags a space in the address as possible_mistake", {
  result <- check_email("alice smith@uni.edu")

  expect_equal(result$flag, "possible_mistake")
  expect_match(result$reason, "space in address", fixed = TRUE)
})

test_that("check_email flags .co as a possible .com typo", {
  result <- check_email("bob@gmail.co")

  expect_equal(result$flag, "possible_mistake")
  expect_match(result$reason, "possible .co instead of .com", fixed = TRUE)
})

test_that("check_email flags an unusually short, unlisted TLD as possible_mistake", {
  result <- check_email("alice@example.zz")

  expect_equal(result$flag, "possible_mistake")
  expect_match(result$reason, "unusually short TLD", fixed = TRUE)
})
