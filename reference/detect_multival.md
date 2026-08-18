# Detect columns containing multi-value cells

Scans every character column in a data frame for cells that split into
more than one value under `sep`, indicating multiple choices stored in a
single cell. Prints a message naming any flagged columns, and invisibly
returns a named list of each flagged column's unique atomic values
(alphabetically sorted), ready to reuse as `new_names` in
`harm_split_multival()`.

## Usage

``` r
detect_multival(data, sep = "[,;]", trim_ws = TRUE)
```

## Arguments

- data:

  A data.frame or tibble to scan.

- sep:

  A single regex string used to split cells. Defaults to `"[,;]"` (comma
  or semicolon).

- trim_ws:

  Logical. Whether to strip leading/trailing whitespace from each
  extracted value before taking uniques. Defaults to `TRUE`.

## Value

Invisibly returns a named list: one element per flagged column, each a
sorted character vector of its unique values. Empty list if none are
found.

## See also

[`split_multival`](https://icg-cat.github.io/armonia/reference/split_multival.md)

## Examples

``` r
wave1 <- data.frame(
  id       = c("alice", "bob", "carol"),
  symptoms = c("headache; cramps", "None", "cramps; headache")
)
detect_multival(wave1)
#> ℹ Potential multiple-choice columns: symptoms
```
