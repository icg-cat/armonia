# Convert a multi-value column into dummy indicator columns

Replaces a single column that contains separator-delimited values (e.g.
`"apple; banana"`) with one logical column per possible category,
TRUE/FALSE per row. Matching is by value identity, not position: a value
lands in the column bearing its name regardless of where it appeared in
the cell. Use `harm_detect_multival()` first to obtain the full
vocabulary to pass as `new_names`.

## Usage

``` r
split_multival(
  data,
  col,
  new_names,
  prefix,
  sep = "[,;]",
  trim_ws = TRUE,
  fulldata = TRUE
)
```

## Arguments

- data:

  A data.frame or tibble.

- col:

  Character. Name of the column to convert.

- new_names:

  Character vector. The complete set of possible category values
  expected in `col` (e.g. from `harm_detect_multival()`). One logical
  output column is created per entry, using `new_names` as column names.
  Must be unique and must not collide with existing column names in
  `data` (other than `col` itself).

- prefix:

  Character vector. Prefix given to identify the variable of origin for
  resulting dummy variables

- sep:

  A single regex string used as the split delimiter. Defaults to
  `"[,;]"` (comma or semicolon).

- trim_ws:

  Logical. Whether to strip leading/trailing whitespace from each
  extracted value before matching against `new_names`. Defaults to
  `TRUE`.

- fulldata:

  Logical. Whether the results return the whole dataframe with the dummy
  variables (TRUE, default), or only the dummies (FALSE).

## Value

A data.frame (or tibble, if the input was a tibble) with `col` replaced
by `length(new_names)` logical columns. Rows where `col` is `NA` produce
`NA` in every new column. Values in `col` not present in `new_names`
trigger a warning and are dropped from the output.

## See also

[`detect_multival`](https://icg-cat.github.io/armonia/reference/detect_multival.md)
for obtaining `new_names`.

## Examples

``` r
wave1 <- data.frame(
  id       = c("alice", "bob"),
  symptoms = c("headache; cramps", "None")
)
new_vals <- detect_multival(wave1)
#> ℹ Potential multiple-choice columns: symptoms
split_multival(wave1, col = "symptoms", new_names = new_vals$symptoms, prefix = "sympt")
#>      id sympt_None sympt_cramps sympt_headache
#> 1 alice      FALSE         TRUE           TRUE
#> 2   bob       TRUE        FALSE          FALSE
```
