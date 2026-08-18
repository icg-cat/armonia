# Write detected dictionary gaps to a timestamped copy

Internal helper for `dict_check_compat(update = TRUE)`. Appends the
variables and factor levels missing from the dictionary into a new,
timestamped copy of the workbook (`yymmdd_<original>_copy.xlsx`),
leaving the original file untouched. If neither `missing_vars` nor
`missing_levels` has any rows, no file is written.

## Usage

``` r
write_dict_compat_update(
  dict_path,
  wave_col,
  wide_map,
  f_map,
  missing_vars,
  missing_levels
)
```

## Arguments

- dict_path:

  Character string. Path to the original `.xlsx` dictionary file.

- wave_col:

  Character. Name of the wave identifier column in `Variable_Map_Wide`
  where new variables are appended.

- wide_map:

  The `Variable_Map_Wide` sheet, as read by
  [`dict_check_compat()`](https://icg-cat.github.io/armonia/reference/dict_check_compat.md).

- f_map:

  The `Factor_Levels` sheet, as read by
  [`dict_check_compat()`](https://icg-cat.github.io/armonia/reference/dict_check_compat.md).

- missing_vars:

  Tibble of variables in the data not covered by the dictionary (as
  returned by
  [`dict_check_compat()`](https://icg-cat.github.io/armonia/reference/dict_check_compat.md)).

- missing_levels:

  Tibble of factor values in the data not present in `Factor_Levels` (as
  returned by
  [`dict_check_compat()`](https://icg-cat.github.io/armonia/reference/dict_check_compat.md)).

## Value

Invisibly returns `NULL`. Called for its side effect of writing the
timestamped copy.
