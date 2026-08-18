# Validate an armonia dictionary file

Performs a fail-fast schema check on an Excel dictionary produced by
[`dict_init()`](https://icg-cat.github.io/armonia/reference/dict_init.md).
Verifies that required sheets are present, `target_name` values are
unique and snake_case-compliant, and that the factor levels sheet
contains the expected columns and numeric `standard_code` values.

## Usage

``` r
dict_validate(path)
```

## Arguments

- path:

  Character string. Path to the `.xlsx` dictionary file.

## Value

Invisibly returns `TRUE` on success; aborts with an informative error on
the first validation failure.

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

dict_validate(dict_path)
#> ℹ Validating dictionary structure...
#> ✔ Dictionary file1a137ee9d0b4.xlsx passed validation.
```
