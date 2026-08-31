# Merge two data dictionary workbooks

Intended to work with Excel-based data dictionaries created by
[`dict_init()`](https://icg-cat.github.io/armonia/reference/dict_init.md).
Reads two dictionaries, merges their contents sheet by sheet, appends a
timestamped entry to the change log, and saves the result as a new dated
workbook. The use-case for this function is having one working version
of a dictionary when receiving a new data source to add, which generates
a raw version of the dictionary.

## Usage

``` r
dict_merge(dt1_path, dt2_path, save_dir = here::here())
```

## Arguments

- dt1_path:

  Character. Path to the first (older, ready) dictionary `.xlsx` file.

- dt2_path:

  Character. Path to the second (newer, raw) dictionary `.xlsx` file.

- save_dir:

  Character. Directory where the merged file will be saved. Defaults to
  [`here::here()`](https://here.r-lib.org/reference/here.html).

## Value

Invisibly returns the save path of the merged workbook.

## Details

The following content sheets are merged: `Variable_Map_Wide`,
`Factor_Levels`, and `Original_Metadata`. The `Change_Log` sheet is
always stacked without deduplication to preserve history. All other
sheets present in `dt2` are written as-is; sheets exclusive to `dt1` are
not carried over.

Merge strategies: Stacks both sources row-wise and retains distinct
records across all columns. Equivalent to a union with deduplication.

## Examples

``` r
dict1 <- tempfile(fileext = ".xlsx")
openxlsx::write.xlsx(
  list(
    Variable_Map_Wide = data.frame(target_name = "id", orig_w1 = "id"),
    Factor_Levels      = data.frame(target_name = character(), original_level = character())
  ),
  file = dict1
)

dict2 <- tempfile(fileext = ".xlsx")
openxlsx::write.xlsx(
  list(
    Variable_Map_Wide = data.frame(target_name = c("id", "gender"), orig_w2 = c("id", "gender")),
    Factor_Levels      = data.frame(target_name = character(), original_level = character())
  ),
  file = dict2
)

dict_merge(dict1, dict2, save_dir = tempdir())
#> ✔ Dictionary merged and saved at /tmp/RtmpfZcCaP/260831_merged_dict.xlsx
```
