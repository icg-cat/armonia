# Check class consistency across waves before binding

Compares the R class of every column across a list of standardized data
frames (each produced by
[`dict_apply()`](https://icg-cat.github.io/armonia/reference/dict_apply.md))
and reports any `target_name` variables whose class differs between
waves. Run this after
[`dict_apply()`](https://icg-cat.github.io/armonia/reference/dict_apply.md)
and before
[`harm_bind_waves()`](https://icg-cat.github.io/armonia/reference/harm_bind_waves.md)
to catch coercions that
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
would apply silently.

## Usage

``` r
check_bind_ready(data_list)
```

## Arguments

- data_list:

  A named list of data frames, each already processed by
  [`dict_apply()`](https://icg-cat.github.io/armonia/reference/dict_apply.md).

## Value

If no conflicts are found, returns `invisible(NULL)` and prints a
success message. If conflicts are found, emits a warning and returns a
tibble with columns `target_name`, `wave`, and `r_class` , one row per
(variable, wave) pair involved in a conflict.

## Examples

``` r
wave1 <- data.frame(gender = factor(c("1", "2")), wellbeing = c(8, 5))
wave2 <- data.frame(gender = factor(c("2", "1")), wellbeing = c(9, 6))
check_bind_ready(list(w1 = wave1, w2 = wave2))
#> ✔ All columns have consistent classes across waves. Ready to bind.
```
