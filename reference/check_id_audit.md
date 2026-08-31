# Audit primary key matching between two ID columns

Compares a vector of IDs against a reference vector, identifying exact
matches and flagging potential typos via fuzzy string distance metrics.
Results are written to a dedicated sheet in an Excel workbook for
review.

## Usage

``` r
check_id_audit(
  id_col1,
  id_col2,
  wb_path,
  case_sensitive = TRUE,
  verbose = FALSE
)
```

## Arguments

- id_col1:

  A character vector of reference IDs (the source of truth).

- id_col2:

  A character vector of IDs to audit against `id_col1`. Primary keys
  must not contain missing values.

- wb_path:

  A string. Path to an existing `.xlsx` workbook where audit results
  will be written. A sheet named `"Review_IDs"` will be created or
  overwritten.

- case_sensitive:

  Logical. If `TRUE` (default), matching is case-sensitive. If `FALSE`,
  both vectors are lowercased before comparison; original casing is
  preserved in the output.

- verbose:

  Logical. If TRUE, returns distance matrix. Defaults to FALSE.

## Value

Invisibly returns a data frame with the following columns:

- index:

  Position of the ID in `id_col2`.

- w2id:

  Original value from `id_col2`.

- best_match:

  Best matching value from `id_col1`, or `"no best match found"` if no
  viable candidate exists.

- all_equal:

  Logical. `TRUE` if `w2id` and `best_match` are equal under the active
  case sensitivity setting.

## Details

Fuzzy matching uses a composite distance score combining Levenshtein
(`lv`), Damerau-Levenshtein (`dl`), and longest common substring (`lcs`)
distances. A candidate is rejected as a match if the minimum composite
score is greater than or equal to the length of the query ID.

For best results on data containing accents, special characters, or
inconsistent whitespace, normalize inputs with
[`normalize_id`](https://icg-cat.github.io/armonia/reference/normalize_id.md)
before calling this function.

## See also

[`normalize_id`](https://icg-cat.github.io/armonia/reference/normalize_id.md)

## Examples

``` r
ref_ids   <- c("alice@uni.edu", "bob@gmail.com")
audit_ids <- c("alice@uni.edu", "bob@gmial.com")  # typo: gmial vs gmail

wb_path <- tempfile(fileext = ".xlsx")
openxlsx::write.xlsx(list(Instructions = data.frame(info = "audit workbook")), file = wb_path)

check_id_audit(ref_ids, audit_ids, wb_path = wb_path)
#> ℹ ID audit complete, see results in Review_IDs. 1 potential typos identified.

# Case-insensitive matching, with upstream normalization
check_id_audit(
  normalize_id(ref_ids),
  normalize_id(audit_ids),
  wb_path = wb_path,
  case_sensitive = FALSE
)
#> ℹ ID audit complete, see results in Review_IDs. 1 potential typos identified.
```
