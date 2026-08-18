# Check for data loss after transformation and binding

Compares a list of original
(pre-[`dict_apply()`](https://icg-cat.github.io/armonia/reference/dict_apply.md))
data frames against the master dataset produced by
[`harm_bind_waves()`](https://icg-cat.github.io/armonia/reference/harm_bind_waves.md)
to detect four classes of information loss: missing rows, dropped
variables, new `NA` values introduced in mapped columns, and unexpected
category collapse in factor variables. Run this after
[`harm_bind_waves()`](https://icg-cat.github.io/armonia/reference/harm_bind_waves.md)
and before
[`assign_standard_labels()`](https://icg-cat.github.io/armonia/reference/assign_standard_labels.md).

## Usage

``` r
check_data_loss(data_list, master, wb_path)
```

## Arguments

- data_list:

  A named list of data frames as passed to
  [`dict_init()`](https://icg-cat.github.io/armonia/reference/dict_init.md),
  **before**
  [`dict_apply()`](https://icg-cat.github.io/armonia/reference/dict_apply.md).
  List names must match the wave labels used in the dictionary (e.g.
  `list(w1 = ..., w2 = ...)` corresponds to dictionary columns
  `orig_w1`, `orig_w2`).

- master:

  A data frame or tibble, the output of
  [`harm_bind_waves()`](https://icg-cat.github.io/armonia/reference/harm_bind_waves.md).
  Must contain a `source_wave` column.

- wb_path:

  Character string. Path to the `.xlsx` dictionary file, used to read
  `Variable_Map_Wide` for name translation.

## Value

If all checks pass, returns `invisible(NULL)` and prints a success
message. If issues are found, emits a warning and returns a tibble of
flagged rows with columns `wave`, `target_name`, `check`, `before`,
`after`, `delta`, and `status`.

## Examples

``` r
wave1 <- data.frame(id = c("alice", "bob"), gender = c("Female", "Male"))

dict_path <- tempfile(fileext = ".xlsx")
openxlsx::write.xlsx(
  list(Variable_Map_Wide = data.frame(
    target_name = c("id", "gender"),
    orig_w1     = c("id", "gender")
  )),
  file = dict_path
)

master <- data.frame(
  id          = c("alice", "bob"),
  gender      = factor(c("1", "2")),
  source_wave = "w1"
)

check_data_loss(list(w1 = wave1), master, dict_path)
#> ✔ All transformation checks passed. No data loss detected.
```
