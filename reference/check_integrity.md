# Final integrity check for harmonized data

Verifies that the master dataset satisfies the following standards: each
combination of participant ID and time point must be unique (tidy
structure), and all factor columns must carry numeric-string levels as
produced by
[`dict_apply()`](https://icg-cat.github.io/armonia/reference/dict_apply.md).

## Usage

``` r
check_integrity(data, id_col, time_col = "timestamp", verbose = TRUE)
```

## Arguments

- data:

  The final harmonized tibble, typically the output of
  [`harm_bind_waves()`](https://icg-cat.github.io/armonia/reference/harm_bind_waves.md)
  or
  [`harm_add_timepoint()`](https://icg-cat.github.io/armonia/reference/harm_add_timepoint.md).

- id_col:

  Character string. Name of the primary key column (e.g. `"pk_hash"`).

- time_col:

  Character string. Name of the column identifying the time point (e.g.
  `"source_wave"`). Defaults to `"timestamp"`. Set to `NULL` for data
  with no time dimension (e.g. single-wave or cross-sectional data);
  uniqueness is then checked on `id_col` alone.

- verbose:

  Logical. If `TRUE`, prints a success message on passing. Defaults to
  `TRUE`.

## Value

Invisibly returns `TRUE` on success; aborts with an informative error if
the data is non-tidy.

## Examples

``` r
data <- data.frame(
  id        = c("alice", "bob", "carol"),
  timestamp = c("w1", "w1", "w2"),
  gender    = factor(c("1", "2", "1"))
)
check_integrity(data, id_col = "id")
#> ✔ Integrity check passed: Data is tidy and fulfills contract requirements.

# single-wave data with no time dimension
cross_sectional <- data.frame(id = c("alice", "bob"), gender = factor(c("1", "2")))
check_integrity(cross_sectional, id_col = "id", time_col = NULL)
#> ✔ Integrity check passed: Data is tidy and fulfills contract requirements.
```
