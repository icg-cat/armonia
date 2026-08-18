# Initialize a data dictionary

Scans a list of data frames and generates an Excel dictionary. The
workbook contains a wide variable map (one row per concept, one column
per wave), a factor levels sheet with pre-populated XLOOKUP formulas
linking back to the variable map, a change log, and a read-only snapshot
of the original metadata. The user is expected to review and edit the
dictionary before passing it to
[`dict_apply()`](https://icg-cat.github.io/armonia/reference/dict_apply.md).

## Usage

``` r
dict_init(
  data_list,
  match_by = "position",
  type_prefix = "var",
  save_path = "dictionary.xlsx",
  max_unique = NULL
)
```

## Arguments

- data_list:

  Named list of data frames, one per wave or data version.

- match_by:

  Character string. Strategy for aligning variables across waves.
  `"position"` (default) matches by column index, suitable when waves
  share the same structure or differ only by language. `"name"` matches
  alphabetically by variable name, suitable when column order cannot be
  trusted.

- type_prefix:

  Character string. Prefix prepended to auto-generated `target_name`
  values (e.g. `"var"` produces `var_001`, `var_002`, ...).

- save_path:

  Character string. File path for the output `.xlsx` dictionary.
  Defaults to `"dictionary.xlsx"`.

- max_unique:

  overrides maximum number of unique values for
  detect_factor_potential(). Defaults to NULL (thus applies heuristic
  rules). See detect_factor_potential() for details.

## Value

Invisibly returns `TRUE` on success. The primary output is the `.xlsx`
file written to `save_path`.

## Examples

``` r
wave1 <- data.frame(id = c("alice", "bob"), gender = c("Female", "Male"))
wave2 <- data.frame(id = c("carol", "dave"), gender = c("Mujer", "Hombre"))

out_path <- tempfile(fileext = ".xlsx")
dict_init(list(w1 = wave1, w2 = wave2), save_path = out_path)
#> ℹ Sanitizing input names (janitor::clean_names)...
#> ℹ Building map using strategy: position
#> ℹ The following variables are *not* identified as factors: 
#> ℹ The following variables are *not* identified as factors: 
#> ✔ Dictionary initialized at /tmp/RtmpOWQ6Rw/file1a131606f307.xlsx
```
