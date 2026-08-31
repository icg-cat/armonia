# Flag potentially duplicate cases across waves

Identifies rows from *different* waves that share the same case
identifier and whose non-identifier columns are highly similar. This
situation arises when the same case is entered in multiple yearly
datasets and the rows reappear after binding waves together with
[`harm_bind_waves()`](https://icg-cat.github.io/armonia/reference/harm_bind_waves.md).

Similarity is computed pairwise for each group of rows sharing the same
`id_col` value across different waves:
`similarity = n_matching_cols / n_total_cols`, where `NA == NA` is
counted as a match.

## Usage

``` r
flag_duplicate_cases(
  data,
  id_col = "id",
  wave_col = "source_wave",
  threshold = 0.9
)
```

## Arguments

- data:

  A data.frame or tibble, typically the output of
  [`harm_bind_waves()`](https://icg-cat.github.io/armonia/reference/harm_bind_waves.md).

- id_col:

  Character. Name of the case identifier column. Defaults to `"id"`.

- wave_col:

  Character. Name of the wave identifier column. Defaults to
  `"source_wave"`.

- threshold:

  Numeric in `[0, 1]`. Minimum similarity for a pair of rows to be
  flagged as potential duplicates. Defaults to `0.90`.

## Value

The input `data` with an added logical column `potential_duplicate`. A
summary tibble of flagged pairs (with columns `id`, `row_a`, `wave_a`,
`row_b`, `wave_b`, `similarity`) is attached as the attribute
`"duplicate_pairs"`.

## See also

[`harm_bind_waves`](https://icg-cat.github.io/armonia/reference/harm_bind_waves.md)
for combining waves before deduplication.

## Examples

``` r
combined <- data.frame(
  id          = c("alice", "alice", "bob"),
  gender      = c("Female", "Female", "Male"),
  wellbeing   = c(8, 8, 5),
  source_wave = c("w1", "w2", "w1")
)
flag_duplicate_cases(combined)
#> ! 2 rows flagged as potential duplicates (1 pair, threshold = 0.9).
#>      id gender wellbeing source_wave potential_duplicate
#> 1 alice Female         8          w1                TRUE
#> 2 alice Female         8          w2                TRUE
#> 3   bob   Male         5          w1               FALSE
```
