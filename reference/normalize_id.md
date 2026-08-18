# Normalize identifier strings

Prepares character vectors for comparison by optionally removing accents
and special Latin characters, and/or stripping whitespace. Intended as a
preprocessing step before passing primary keys to
[`check_id_audit`](https://icg-cat.github.io/armonia/reference/check_id_audit.md).

## Usage

``` r
normalize_id(x, translit = TRUE, remove_whitespace = TRUE)
```

## Arguments

- x:

  A character vector to normalize.

- translit:

  Logical. If `TRUE` (default), transliterates Latin script characters
  with diacritics to their ASCII equivalents (e.g. `"café"` becomes
  `"cafe"`). Uses ICU transliteration rule `"Latin-ASCII"` via
  `stringi`.

- remove_whitespace:

  Logical. If `TRUE` (default), removes all whitespace characters,
  including non-breaking and other Unicode space variants.

## Value

A character vector of the same length as `x`.

## See also

[`check_id_audit`](https://icg-cat.github.io/armonia/reference/check_id_audit.md)

## Examples

``` r
normalize_id(c("caf\u00e9", "jo\u00e3o", "mar\u00eda"))
#> [1] "cafe"  "joao"  "maria"
normalize_id(c("john doe", "jane doe"), translit = FALSE)
#> [1] "johndoe" "janedoe"
normalize_id(c("caf\u00e9 latte", "jo\u00e3o silva"), translit = TRUE, remove_whitespace = TRUE)
#> [1] "cafelatte" "joaosilva"
```
