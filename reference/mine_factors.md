# Mine candidate factor variables across waves for the dictionary

Internal helper for
[`dict_init()`](https://icg-cat.github.io/armonia/reference/dict_init.md).
Scans each wave in `clean_list` for character columns that look
categorical (via
[`detect_factor_potential()`](https://icg-cat.github.io/armonia/reference/detect_factor_potential.md)),
and builds the raw `Factor_Levels` rows for them: one row per unique
value per flagged variable, with a provisional `target_name` resolved
from `wide_map`.

## Usage

``` r
mine_factors(clean_list, wide_map, max_unique = NULL)
```

## Arguments

- clean_list:

  Named list of sanitized data frames (post
  [`janitor::clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.html)),
  one per wave.

- wide_map:

  The wide variable map built by
  [`dict_init()`](https://icg-cat.github.io/armonia/reference/dict_init.md),
  used to resolve each factor row's `target_name`.

- max_unique:

  Passed through to
  [`detect_factor_potential()`](https://icg-cat.github.io/armonia/reference/detect_factor_potential.md).
  Defaults to `NULL`.

## Value

A data.frame of candidate factor rows (possibly zero rows), with columns
`wave`, `original_variable`, `target_name`, `original_level`,
`standard_code`, `standard_label`.
