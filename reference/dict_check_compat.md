# Check whether an armonia dictionary is applicable to a new dataset

Compares a data frame against an existing armonia dictionary to
determine whether
[`dict_apply()`](https://icg-cat.github.io/armonia/reference/dict_apply.md)
can be run without issues. Three things are checked: (1) variables
present in the data but absent from the dictionary's
`Variable_Map_Wide`; (2) dictionary variables not found in the data
(informational); (3) factor values present in the data but absent from
`Factor_Levels`. The dictionary structure is validated first via
[`dict_validate()`](https://icg-cat.github.io/armonia/reference/dict_validate.md),
which aborts on malformed dictionaries.

This function does *not* abort on compatibility issues, it returns a
report for the user to act on.

## Usage

``` r
dict_check_compat(dict_path, data, wave_col = "source_wave", update = FALSE)
```

## Arguments

- dict_path:

  Character string. Path to the `.xlsx` dictionary file produced by
  [`dict_init()`](https://icg-cat.github.io/armonia/reference/dict_init.md).

- data:

  A data.frame or tibble to check against the dictionary.

- wave_col:

  Character. Name of the wave identifier column in `data` (excluded from
  variable coverage checks) and, when `update = TRUE`, the corresponding
  column name in `Variable_Map_Wide` where new variables are appended
  (e.g. `"orig_w1"`). Defaults to `"source_wave"`.

- update:

  Logical. If `TRUE`, writes the detected gaps into a timestamped copy
  of the dictionary file (same directory, named
  `yymmdd_<original>_copy.xlsx`). The original file is never modified.
  Defaults to `FALSE`.

## Value

Invisibly returns a named list with three tibbles:

- `$missing_vars`:

  Variables in `data` not covered by the dictionary.

- `$extra_vars`:

  Dictionary variables not found in `data` (informational).

- `$missing_levels`:

  Factor values in `data` not present in `Factor_Levels`, with columns
  `target_name` and `missing_value`.

## See also

[`dict_validate`](https://icg-cat.github.io/armonia/reference/dict_validate.md)
for structural dictionary validation,
[`dict_apply`](https://icg-cat.github.io/armonia/reference/dict_apply.md)
for applying the dictionary.

## Examples

``` r
dict_path <- tempfile(fileext = ".xlsx")
openxlsx::write.xlsx(
  list(
    Variable_Map_Wide = data.frame(
      target_name = c("id", "gender"),
      orig_w1     = c("id", "gender")
    ),
    Factor_Levels = data.frame(
      wave              = "orig_w1",
      original_variable = "gender",
      target_name       = "gender",
      original_level    = c("Female", "Male"),
      standard_code     = c(1, 2),
      standard_label    = c("Female", "Male")
    )
  ),
  file = dict_path
)

new_wave <- data.frame(
  source_wave = "w1",
  id          = c("alice", "bob"),
  gender      = c("Female", "Male"),
  wellbeing   = c(8, 5)   # not yet in the dictionary
)

result <- dict_check_compat(dict_path, new_wave)
#> ℹ Validating dictionary structure...
#> ✔ Dictionary file1a1375411781.xlsx passed validation.
#> 
#> ── Dictionary compatibility report ─────────────────────────────────────────────
#> ! 1 variable in data not covered by dictionary ($missing_vars):
#> • wellbeing
#> ✔ All factor values in data are covered by the dictionary.
result$missing_vars
#> # A tibble: 1 × 1
#>   variable 
#>   <chr>    
#> 1 wellbeing
```
