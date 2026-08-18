# --- Setup: Synthetic Dictionary Environment ---
tmp_wb <- tempfile(fileext = ".xlsx")

v_map_test <- data.frame(
  target_name = c("id", "gender", "extra"),
  orig_w1 = c("uid", "sex", NA),
  orig_w2 = c("participant_id", "genero", "sobrante"),
  stringsAsFactors = FALSE
)

# wave column must equal tolower(source_name) as used internally by dict_apply
l_map_test <- data.frame(
  wave = c("orig_w1", "orig_w1", "orig_w2", "orig_w2"),
  original_variable = c("sex", "sex", "genero", "genero"),
  original_level = c("Male", "Female", "Hombre", "Mujer"),
  standard_code = c(1, 2, 1, 2),
  stringsAsFactors = FALSE
)

openxlsx::write.xlsx(
  list(Variable_Map_Wide = v_map_test, Factor_Levels = l_map_test),
  file = tmp_wb
)

df_w1 <- data.frame(uid = "A01", sex = factor("Male"), stringsAsFactors = FALSE)
df_w2 <- data.frame(participant_id = "B02", genero = factor("Mujer"), sobrante = 99, stringsAsFactors = FALSE)

# --- Tests: dict_apply ---

test_that("dict_apply renames variables and recodes factors to standard codes", {
  res1 <- dict_apply(df_w1, tmp_wb, "orig_w1")

  expect_named(res1, c("id", "gender"))  # 'extra' absent: NA for orig_w1
  expect_s3_class(res1$gender, "factor")
  expect_equal(as.character(res1$gender), "1")  # "Male" -> standard_code 1

  res2 <- dict_apply(df_w2, tmp_wb, "orig_w2")

  expect_named(res2, c("id", "gender", "extra"))
  expect_equal(as.character(res2$gender), "2")  # "Mujer" -> standard_code 2
})

test_that("dict_apply fails when source_name is not a column in the dictionary", {
  expect_error(
    dict_apply(df_w1, tmp_wb, "orig_w3"),
    regexp = "Wave column.*not found"
  )
})

# --- Tests: harm_bind_waves ---

test_that("harm_bind_waves row-binds waves and adds source_wave column", {
  res1 <- dict_apply(df_w1, tmp_wb, "orig_w1")
  res2 <- dict_apply(df_w2, tmp_wb, "orig_w2")

  combined <- harm_bind_waves(list(wave1 = res1, wave2 = res2))

  expect_equal(nrow(combined), 2)
  expect_equal(combined$gender, factor(c("1", "2")))
  expect_true("source_wave" %in% names(combined))
})

test_that("harm_bind_waves fails on empty input", {
  expect_error(harm_bind_waves(list()), regexp = "non-empty list")
})

# --- Tests: harm_add_timepoint ---

test_that("harm_add_timepoint suffixes new wave variables and leaves by_id unsuffixed", {
  master   <- data.frame(id = c("A", "B", "C"), score = c(10, 20, 30), stringsAsFactors = FALSE)
  new_wave <- data.frame(id = c("A", "B", "D"), score = c(11, 22, 40), stringsAsFactors = FALSE)

  result <- harm_add_timepoint(master, new_wave, by_id = "id", suffix = "_w2", verbose = FALSE)

  expect_true("score" %in% names(result))      # original master variable
  expect_true("score_w2" %in% names(result))   # new wave variable with suffix
  expect_false("id_w2" %in% names(result))     # by_id must NOT be suffixed
})

test_that("harm_add_timepoint full join preserves all participants (match + attrition + recruited)", {
  master   <- data.frame(id = c("A", "B", "C"), score = c(10, 20, 30), stringsAsFactors = FALSE)
  new_wave <- data.frame(id = c("A", "B", "D"), score = c(11, 22, 40), stringsAsFactors = FALSE)

  result <- harm_add_timepoint(master, new_wave, by_id = "id", suffix = "_w2", verbose = FALSE)

  # A+B matched, C attrited, D recruited = 4 rows
  expect_equal(nrow(result), 4)
  expect_true(all(c("A", "B", "C", "D") %in% result$id))
})

test_that("harm_add_timepoint fails on duplicate IDs in new_data", {
  master <- data.frame(id = c("A", "B"), score = c(10, 20), stringsAsFactors = FALSE)
  duped  <- data.frame(id = c("A", "A"), score = c(11, 12), stringsAsFactors = FALSE)

  expect_error(
    harm_add_timepoint(master, duped, by_id = "id", suffix = "_w2"),
    regexp = "Duplicate"
  )
})

test_that("harm_add_timepoint fails when by_id is not found in master", {
  master   <- data.frame(id = c("A", "B"), score = c(10, 20), stringsAsFactors = FALSE)
  new_wave <- data.frame(id = c("A", "B"), score = c(11, 22), stringsAsFactors = FALSE)

  expect_error(
    harm_add_timepoint(master, new_wave, by_id = "wrong_id", suffix = "_w2"),
    regexp = "not found in"
  )
})

test_that("harm_add_timepoint fails when by_id is not found in new_data", {
  master  <- data.frame(id = c("A", "B"), score = c(10, 20), stringsAsFactors = FALSE)
  no_id   <- data.frame(score = c(11, 22), stringsAsFactors = FALSE)

  expect_error(
    harm_add_timepoint(master, no_id, by_id = "id", suffix = "_w2"),
    regexp = "not found in"
  )
})

test_that("harm_add_timepoint warns when new_data contains only the ID column", {
  master  <- data.frame(id = c("A", "B"), score = c(10, 20), stringsAsFactors = FALSE)
  id_only <- data.frame(id = c("A", "B"), stringsAsFactors = FALSE)

  expect_warning(
    harm_add_timepoint(master, id_only, by_id = "id", suffix = "_w2"),
    regexp = "only the ID"
  )
})
