# Assign standard labels to factor variables using the dictionary

Iterates over all factor columns in `data` and replaces their numeric
levels with the corresponding `standard_label` values from the
dictionary. Intended to be called on the master dataset produced by
[`harm_bind_waves()`](https://icg-cat.github.io/armonia/reference/harm_bind_waves.md),
after factors have been codified as integers by
[`dict_apply()`](https://icg-cat.github.io/armonia/reference/dict_apply.md)
and after the dictionary has been validated by
[`dict_validate()`](https://icg-cat.github.io/armonia/reference/dict_validate.md).

## Usage

``` r
assign_standard_labels(data, dict)
```

## Arguments

- data:

  A data.frame whose factor columns carry integer-coded levels.
  Typically the output of
  [`harm_bind_waves()`](https://icg-cat.github.io/armonia/reference/harm_bind_waves.md).

- dict:

  Path to the dictionary containing the Factor_Levels sheet where
  standard_codes and standard_labels are specified.

## Value

A data.frame identical to `data` with factor levels replaced by their
`standard_label` strings.

## See also

[`dict_validate`](https://icg-cat.github.io/armonia/reference/dict_validate.md)

## Examples

``` r
data <- data.frame(eye_color = factor(c("1", "2", "1")))
dict_path <- tempfile(fileext = ".xlsx")
openxlsx::write.xlsx(
  list(Factor_Levels = data.frame(
    target_name    = c("eye_color", "eye_color"),
    standard_code  = c("1", "2"),
    standard_label = c("Hazel", "Green")
  )),
  file = dict_path
)
assign_standard_labels(data, dict_path)
#>   eye_color
#> 1     Hazel
#> 2     Green
#> 3     Hazel
```
