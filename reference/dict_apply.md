# Apply dictionary mapping and standardize categorical values

Renames variables and recodes factor labels to their numeric
`standard_code` equivalents using the mapping sheets in the dictionary
produced by
[`dict_init()`](https://icg-cat.github.io/armonia/reference/dict_init.md).
Input data is sanitized via
[`janitor::clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.html)
before any processing.

## Usage

``` r
dict_apply(data, wb_path, source_name, attributes = FALSE)
```

## Arguments

- data:

  A data.frame to be standardized.

- wb_path:

  Character string. Path to the `.xlsx` dictionary file.

- source_name:

  Character string. The column name in `Variable_Map_Wide` that
  corresponds to this dataset (e.g. `"orig_wave1"`). Must match the
  dictionary exactly.

- attributes:

  Logical. If `TRUE`, the original variable names and descriptions from
  the dictionary are stored as attributes on the returned tibble.
  Defaults to `FALSE`.

## Value

A tibble with variables renamed to their `target_name` and factor
columns recoded to integer-level factors.

## Examples

``` r
wave1 <- data.frame(id = c("alice", "bob"), gender = c("Female", "Male"))

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

dict_apply(wave1, dict_path, source_name = "orig_w1")
#> # A tibble: 2 × 2
#>   id    gender
#>   <chr> <fct> 
#> 1 alice 1     
#> 2 bob   2     
```
