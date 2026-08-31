# Testing dict_merge

make_min_dict <- function(target_name, orig_col_name, orig_values) {
  path <- tempfile(fileext = ".xlsx")
  wide <- data.frame(target_name = target_name, stringsAsFactors = FALSE)
  wide[[orig_col_name]] <- orig_values
  openxlsx::write.xlsx(
    list(
      Variable_Map_Wide = wide,
      Factor_Levels      = data.frame(target_name = character(), original_level = character()),
      Change_Log         = data.frame(timestamp = as.character(Sys.time()), action = "INIT",
                                       match_strategy = "position", stringsAsFactors = FALSE)
    ),
    file = path
  )
  path
}

test_that("dict_merge unions variables from both dictionaries and drops exact duplicates", {
  dict1 <- make_min_dict("id", "orig_w1", "id")
  dict2 <- make_min_dict(c("id", "gender"), "orig_w2", c("id", "gender"))

  save_dir <- tempfile()
  dir.create(save_dir)

  result_path <- dict_merge(dict1, dict2, save_dir = save_dir)

  expect_true(file.exists(result_path))

  merged_map <- openxlsx::read.xlsx(result_path, sheet = "Variable_Map_Wide")
  expect_true(all(c("id", "gender") %in% merged_map$target_name))
})

test_that("dict_merge records the match_strategy as a string, not a function", {
  dict1 <- make_min_dict("id", "orig_w1", "id")
  dict2 <- make_min_dict("id", "orig_w2", "id")

  save_dir <- tempfile()
  dir.create(save_dir)

  result_path <- dict_merge(dict1, dict2, save_dir = save_dir)

  change_log <- openxlsx::read.xlsx(result_path, sheet = "Change_Log")
  last_entry <- change_log[nrow(change_log), ]

  expect_type(last_entry$match_strategy, "character")
  expect_equal(last_entry$match_strategy, "full_unique")
})

test_that("dict_merge errors when the output file already exists", {
  dict1 <- make_min_dict("id", "orig_w1", "id")
  dict2 <- make_min_dict("id", "orig_w2", "id")

  save_dir <- tempfile()
  dir.create(save_dir)

  dict_merge(dict1, dict2, save_dir = save_dir)

  expect_error(
    dict_merge(dict1, dict2, save_dir = save_dir),
    regexp = "already exists"
  )
})

test_that("dict_merge errors when an input file does not exist", {
  expect_error(
    dict_merge("nonexistent1.xlsx", "nonexistent2.xlsx"),
    regexp = "not found"
  )
})
