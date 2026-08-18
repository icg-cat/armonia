# Write the dictionary workbook to disk

Internal helper for
[`dict_init()`](https://icg-cat.github.io/armonia/reference/dict_init.md).
Builds the full workbook, the Instructions sheet, the styled
`Variable_Map_Wide` sheet, the `Factor_Levels` sheet (with embedded
`XLOOKUP` formulas resolving `target_name`), the `Change_Log` sheet, and
the `Original_Metadata` snapshot, then saves it to `save_path`.

## Usage

``` r
write_dictionary_workbook(wide_map, factor_map, match_by, save_path)
```

## Arguments

- wide_map:

  The wide variable map built by
  [`dict_init()`](https://icg-cat.github.io/armonia/reference/dict_init.md).

- factor_map:

  The candidate factor rows built by
  [`mine_factors()`](https://icg-cat.github.io/armonia/reference/mine_factors.md)
  (possibly zero rows).

- match_by:

  Character string. The variable-alignment strategy used by
  [`dict_init()`](https://icg-cat.github.io/armonia/reference/dict_init.md)
  (`"position"` or `"name"`), used for the Instructions text and the
  initial `Change_Log` entry.

- save_path:

  Character string. File path for the output `.xlsx` dictionary.

## Value

Invisibly returns `TRUE` on success. The primary output is the `.xlsx`
file written to `save_path`.
