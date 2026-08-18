# Bind harmonized waves into a master dataset

Combines a named list of standardized data frames (each produced by
[`dict_apply()`](https://icg-cat.github.io/armonia/reference/dict_apply.md))
into a single long-format tibble. A `source_wave` column is added
automatically to preserve wave provenance. Column names are enforced to
snake_case.

## Usage

``` r
harm_bind_waves(data_list)
```

## Arguments

- data_list:

  A named list of data frames, each already processed by
  [`dict_apply()`](https://icg-cat.github.io/armonia/reference/dict_apply.md).

## Value

A single harmonized tibble with a `source_wave` column identifying the
origin of each row.

## Examples

``` r
w1 <- data.frame(id = "alice", gender = factor("1"))
w2 <- data.frame(id = "bob",   gender = factor("2"))
harm_bind_waves(list(w1 = w1, w2 = w2))
#> # A tibble: 2 × 3
#>   source_wave id    gender
#>   <chr>       <chr> <fct> 
#> 1 w1          alice 1     
#> 2 w2          bob   2     
```
