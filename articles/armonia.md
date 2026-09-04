# Harmonizing longitudinal research data with armonia

``` r

library(armonia)
library(dplyr)
library(openxlsx)
library(knitr)
```

## Description

Real-world longitudinal studies collect data across multiple waves,
often with different teams, questionnaire versions, or even languages.
The result is a collection of data frames that *should* be equivalent
but often aren’t: column names differ (`v010` vs `V010`), factor labels
are in different languages (`Good` vs `Bueno`), or accumulate small
typos over time. `armonia` handles this in four phases:

1.  **Dictionary**: scan your waves and generate an Excel reference
    table for you to match variables and assign factor levels.
2.  **Standardize**: apply the dictionary to rename variables and recode
    factor levels to integers.
3.  **Assemble**: stack waves (long format) or join them (wide format
    for longitudinal analysis). Then relabel factors using the
    dictionary.
4.  **Anonymize data**: replace raw IDs with salted hashes.

This vignette walks through each phase using a fictional *WellBeing
Study*: ten participants measured at two waves, with the second wave
administered in Spanish.

------------------------------------------------------------------------

## 1 Toy data

We start with two data frames. Wave 1 was collected in English; Wave 2
was collected in Spanish six months later with slightly different column
names and factor labels.

``` r

wave1 <- data.frame(
  email     = c("alice@uni.edu", "bob@gmail.co", "carol@yahoo.com",
                "david@uni.edu", "eva@outlook.com", "frank@gmail.com",
                "grace@uni.edu", "henry@yahoo.com", "iris@gmail.com",
                "jack@uni.edu"),
  gender    = c("Female", "Male",   "Female", "Male", "Female",
                "Male",   "Female", "Male",   "Female", "Male"),
  mood      = c("Good",    "Neutral", "Bad",  "Good", "Good",
                "Bad",     "Neutral", "Good", "Bad",  "Neutral"),
  wellbeing = c(8, 5, 3, 7, 9, 2, 6, 8, 4, 6),
  symptoms = c("None", "headache; cramps", "headache; cramps", "None", 
               "None", "cramps; headache", "None", "headache", 
               "cramps; headache", "None"),
  stringsAsFactors = FALSE
)

wave2 <- data.frame(
  correo       = c("alice@uni.edu", "bob@gmail.com", "carol@yahoo.com",
                   "david@uni.edu", "eva@outlook.com", "frank@gmail.com",
                   "grace@uni.edu", "henry@yahoo.com", "iris@gmail.com",
                   "newbie@gmail.com"),
  genero       = c("Mujer",  "Hombre", "Mujer",  "Hombre", "Mujer",
                   "Hombre", "Mujer",  "Hombre", "Mujer",  "Hombre"),
  estado_animo = c("Bueno",   "Neutral", "Malo",  "Bueno", "Bueno",
                   "Malo",    "Neutral", "Bueno", "Malo",  "Bueno"),
  bienestar    = c(9, 6, 4, 8, 9, 3, 7, 9, 5, 7),
  síntomas     = c("Ninguno", "dolor de cabeza", "calambres", "Ninguno", 
                   "calambres", "dolor de cabeza; calambres", "calambres", 
                   "Ninguno", "dolor de cabeza; calambres", "Ninguno"),
  stringsAsFactors = FALSE
)
```

At a glance, these frames have several problems that prevent a direct
[`rbind()`](https://rdrr.io/r/base/cbind.html):

| Issue | Wave 1 | Wave 2 |
|----|----|----|
| Email column name | `email` | `correo` |
| Sex variable | `gender` (“Female” / “Male”) | `genero` (“Mujer” / “Hombre”) |
| Mood variable | `mood` (“Good” / “Neutral” / “Bad”) | `estado_animo` (“Bueno” / “Neutral” / “Malo”) |
| Wellbeing column | `wellbeing` | `bienestar` |
| Symptoms columnb | `symptoms` | `síntomas` |
| ID typo | `bob@gmail.co` | `bob@gmail.com` |
| Attrition | Jack present | Jack absent |
| New recruit | .. | `newbie@gmail.com` |

Notice one of the variables is a multiple-choice question, with values
separated by semi-colons. This and any other ad hoc transformations that
the data might require, need to be performed **before** starting the
harmonization process. The functions
[`detect_multival()`](https://icg-cat.github.io/armonia/reference/detect_multival.md)
and
[`split_multival()`](https://icg-cat.github.io/armonia/reference/split_multival.md)
simplify specifically the multiple-response variables needs by creating
a set of dummy variables with all the multiple-choice response options:

``` r

new_vals <- detect_multival(wave1)
#> ℹ Potential multiple-choice columns: symptoms
wave1 <- split_multival(data = wave1, col = "symptoms", new_names = new_vals$symptoms, prefix = "sympt")

new_vals2 <- detect_multival(wave2)
#> ℹ Potential multiple-choice columns: síntomas
wave2 <- split_multival(data = wave2, col = "síntomas", new_names = new_vals2$síntomas, prefix = "sympt")

wave1
#>              email gender    mood wellbeing sympt_cramps sympt_headache
#> 1    alice@uni.edu Female    Good         8        FALSE          FALSE
#> 2     bob@gmail.co   Male Neutral         5         TRUE           TRUE
#> 3  carol@yahoo.com Female     Bad         3         TRUE           TRUE
#> 4    david@uni.edu   Male    Good         7        FALSE          FALSE
#> 5  eva@outlook.com Female    Good         9        FALSE          FALSE
#> 6  frank@gmail.com   Male     Bad         2         TRUE           TRUE
#> 7    grace@uni.edu Female Neutral         6        FALSE          FALSE
#> 8  henry@yahoo.com   Male    Good         8        FALSE           TRUE
#> 9   iris@gmail.com Female     Bad         4         TRUE           TRUE
#> 10    jack@uni.edu   Male Neutral         6        FALSE          FALSE
#>    sympt_None
#> 1        TRUE
#> 2       FALSE
#> 3       FALSE
#> 4        TRUE
#> 5        TRUE
#> 6       FALSE
#> 7        TRUE
#> 8       FALSE
#> 9       FALSE
#> 10       TRUE
```

------------------------------------------------------------------------

## 2 Phase 1: The dictionary

### 2.1 Scanning the data with `dict_init()`

[`dict_init()`](https://icg-cat.github.io/armonia/reference/dict_init.md)
takes a *named* list of data frames and generates an Excel workbook with
a pre-filled mapping skeleton. We use `match_by = "position"` because in
this toy example both waves share the same conceptual column order, they
just differ in names and language.

``` r

dict_path <- tempfile(fileext = ".xlsx")

dict_init(
  data_list = list(w1 = wave1, w2 = wave2),
  match_by  = "position",
  save_path = dict_path
)
#> ℹ Sanitizing input names (janitor::clean_names)...
#> ℹ Building map using strategy: position
#> ℹ The following variables are *not* identified as factors: email
#> ℹ The following variables are *not* identified as factors: correo
#> ✔ Dictionary initialized at /tmp/RtmpLeNPIQ/file1a1ade6eda0.xlsx
```

The workbook contains two key sheets:

- **Variable_Map_Wide** aligns variables across waves by their column
  position: one row per concept, one column per wave:

| target_name | description | orig_w1        | orig_w2               |
|:------------|------------:|:---------------|:----------------------|
| var_001     |          NA | email          | correo                |
| var_002     |          NA | gender         | genero                |
| var_003     |          NA | mood           | estado_animo          |
| var_004     |          NA | wellbeing      | bienestar             |
| var_005     |          NA | sympt_cramps   | sympt_calambres       |
| var_006     |          NA | sympt_headache | sympt_dolor_de_cabeza |
| var_007     |          NA | sympt_none     | sympt_ninguno         |

Variable_Map_Wide (auto-generated skeleton) {.table}

- **Factor_Levels** lists every detected categorical variable and
  auto-assigns integer codes in alphabetical order within each wave:

| wave    | original_variable     | original_level | standard_code | standard_label |
|:--------|:----------------------|:---------------|--------------:|:---------------|
| orig_w1 | gender                | Female         |             1 | Female         |
| orig_w2 | genero                | Hombre         |             1 | Hombre         |
| orig_w1 | gender                | Male           |             2 | Male           |
| orig_w2 | genero                | Mujer          |             2 | Mujer          |
| orig_w1 | mood                  | Bad            |             1 | Bad            |
| orig_w2 | estado_animo          | Bueno          |             1 | Bueno          |
| orig_w1 | mood                  | Good           |             2 | Good           |
| orig_w2 | estado_animo          | Malo           |             2 | Malo           |
| orig_w1 | mood                  | Neutral        |             3 | Neutral        |
| orig_w2 | estado_animo          | Neutral        |             3 | Neutral        |
| orig_w1 | sympt_cramps          | FALSE          |             1 | FALSE          |
| orig_w2 | sympt_calambres       | FALSE          |             1 | FALSE          |
| orig_w1 | sympt_cramps          | TRUE           |             2 | TRUE           |
| orig_w2 | sympt_calambres       | TRUE           |             2 | TRUE           |
| orig_w1 | sympt_headache        | FALSE          |             1 | FALSE          |
| orig_w2 | sympt_dolor_de_cabeza | FALSE          |             1 | FALSE          |
| orig_w1 | sympt_headache        | TRUE           |             2 | TRUE           |
| orig_w2 | sympt_dolor_de_cabeza | TRUE           |             2 | TRUE           |
| orig_w1 | sympt_none            | FALSE          |             1 | FALSE          |
| orig_w2 | sympt_ninguno         | FALSE          |             1 | FALSE          |
| orig_w1 | sympt_none            | TRUE           |             2 | TRUE           |
| orig_w2 | sympt_ninguno         | TRUE           |             2 | TRUE           |

Factor_Levels (auto-generated skeleton) {.table}

💡 **How does the function identify factors?**

[`dict_init()`](https://icg-cat.github.io/armonia/reference/dict_init.md)
will automatically identify variables that could be treated as factors,
by applying the following strategies:

- 1: Skip variables where every non-NA value parses as a number &
  variables where every value is NA
- 2: ‘Likert/Binary’: If the number of unique values is low (\<7), the
  variable is considered a factor regardless of sample size
- 3: If the number of unique values is above 7, three conditions need to
  be met:
  - Less than 16 unique values
  - More than 30 unique cases
  - any give value must be repeated a minimum of 20% of times

The argument `max_unique` overrides this process. If supplied, the
number of unique values alone decides; sample-size and repetition guards
are skipped.

### 2.2 Manually identify matching variables and levels

> **You are now meant to open `dictionary.xlsx` in Excel and fill in two
> things:**
>
> - In **Variable_Map_Wide**: replace the auto-generated `target_name`
>   values (`var_001`, …) with meaningful snake_case names that will
>   appear in your final dataset.
> - In **Factor_Levels**: adjust `standard_code` values so that
>   equivalent concepts across waves share the same integer, and fill in
>   one single `standard_label` for each numeric code.

After editing, **Variable_Map_Wide** should look like this:

| target_name    | description            | orig_w1        | orig_w2               |
|----------------|------------------------|----------------|-----------------------|
| id             | Participant email      | email          | correo                |
| gender         | Self-defined gender    | gender         | genero                |
| mood           | Self-rated mood        | mood           | estado_animo          |
| wellbeing      | Wellbeing score (0–10) | wellbeing      | bienestar             |
| sympt_cramps   | symptoms dummy         | sympt_cramps   | sympt_calambres       |
| sympt_headache | symptoms dummy         | sympt_headache | sympt_dolor_de_cabeza |
| sympt_none     | symptoms dummy         | sympt_none     | sympt_ninguno         |

And **Factor_Levels** needs cross-wave consistency. Notice how `Mujer`
(wave 2) must share the same `standard_code` as `Female` (wave 1):

| wave    | original_variable | original_level | standard_code | standard_label |
|---------|-------------------|----------------|---------------|----------------|
| orig_w1 | gender            | Female         | 1             | Female         |
| orig_w1 | gender            | Male           | 2             | Male           |
| orig_w2 | genero            | Mujer          | 1             | Female         |
| orig_w2 | genero            | Hombre         | 2             | Male           |
| orig_w1 | mood              | Good           | 1             | Good           |
| orig_w1 | mood              | Neutral        | 2             | Neutral        |
| orig_w1 | mood              | Bad            | 3             | Bad            |
| orig_w2 | estado_animo      | Bueno          | 1             | Good           |
| orig_w2 | estado_animo      | Neutral        | 2             | Neutral        |
| orig_w2 | estado_animo      | Malo           | 3             | Bad            |

> **Tip 1:** the auto-generated codes for wave 2 do *not* match wave 1.
> [`dict_init()`](https://icg-cat.github.io/armonia/reference/dict_init.md)
> assigns codes alphabetically within each wave independently, so
> `Bueno` gets code 1 while the equivalent `Good` gets code 2. The
> editing step is where you establish a single coherent standard across
> waves.

> **Tip 2:** the standard-label column can also be used to easily
> translate all the factor levels in the dataset to another language.

For this vignette we simulate the editing step programmatically, but in
practice you would make these changes directly in Excel:

💡 **About the Excel formula that finds variable names**

Notice that Factor_Levels identifies each variable’s `target_name` even
if this has been manually modified. This is achieved using the Excel
formula XLOOKUP. By default, this formula compares datasets by pairs.
[`dict_init()`](https://icg-cat.github.io/armonia/reference/dict_init.md)
does allow building 3+ dataset dictionaries, but then the user will need
to manually change the reference columns in the XLOOKUP function that
assigns a variable `target_name`. The “Instructions” sheet has more
details on how to modify the XLOOKUP function and adapt it to one’s
needs.

### 2.3 Validating the completed dictionary with `dict_validate()`

Before proceeding,
[`dict_validate()`](https://icg-cat.github.io/armonia/reference/dict_validate.md)
performs a fail-fast schema check: required sheets present,
`target_name` values are snake_case and unique, `standard_code` is
numeric, and every `standard_code` within a `target_name` maps to
exactly one `standard_label` across all waves.

``` r

dict_validate(dict_path)
#> ℹ Validating dictionary structure...
#> ✔ Dictionary file1a1ade6eda0.xlsx passed validation.
```

If any of these tests fail, the function aborts immediately with an
informative message. Two examples:

    ✖ All target_names must be lowercase, no spaces, no special chars.

    ✖ Variable(s) with conflicting standard_labels for the same standard_code: "gender"
    ℹ Each standard_code within a target_name must map to exactly one standard_label across all waves.

The second error would occur when `standard_code = 1` for `gender` maps
to `"Female"` in wave 1 but `"Mujer"` in wave 2, a conflict that would
cause
[`assign_standard_labels()`](https://icg-cat.github.io/armonia/reference/assign_standard_labels.md)
to produce ambiguous results downstream. Fix it by aligning all
`standard_label` values for the same code to a single agreed-upon string
(e.g. `"Female"` in both rows).

------------------------------------------------------------------------

## 3 Phase 2: Standardization

### 3.1 Applying the mapping with `dict_apply()`

[`dict_apply()`](https://icg-cat.github.io/armonia/reference/dict_apply.md)
reads the completed dictionary and does two things to each wave:

1.  **Renames** columns to their `target_name`.
2.  **Recodes** factor labels to integer `standard_code` values.

The `source_name` argument must exactly match the column header in
`Variable_Map_Wide`, therefore make sure your always use
`paste0("orig_", <list_name>)`. For `list(w1 = wave1, ...)` this means
`source_name = "orig_w1"`.

``` r

std_w1 <- dict_apply(wave1, wb_path = dict_path, source_name = "orig_w1")
knitr::kable(std_w1, caption = "Wave 1 after dict_apply()")
```

| id                | gender | mood | wellbeing | sympt_cramps | sympt_headache | sympt_none |
|:------------------|:-------|:-----|----------:|:-------------|:---------------|:-----------|
| <alice@uni.edu>   | 1      | 1    |         8 | 1            | 1              | 2          |
| <bob@gmail.co>    | 2      | 2    |         5 | 2            | 2              | 1          |
| <carol@yahoo.com> | 1      | 3    |         3 | 2            | 2              | 1          |
| <david@uni.edu>   | 2      | 1    |         7 | 1            | 1              | 2          |
| <eva@outlook.com> | 1      | 1    |         9 | 1            | 1              | 2          |
| <frank@gmail.com> | 2      | 3    |         2 | 2            | 2              | 1          |
| <grace@uni.edu>   | 1      | 2    |         6 | 1            | 1              | 2          |
| <henry@yahoo.com> | 2      | 1    |         8 | 1            | 2              | 1          |
| <iris@gmail.com>  | 1      | 3    |         4 | 2            | 2              | 1          |
| <jack@uni.edu>    | 2      | 2    |         6 | 1            | 1              | 2          |

Wave 1 after dict_apply() {.table}

``` r

std_w2 <- dict_apply(wave2, wb_path = dict_path, source_name = "orig_w2")
knitr::kable(std_w2, caption = "Wave 2 after dict_apply()")
```

| id | gender | mood | wellbeing | sympt_cramps | sympt_headache | sympt_none |
|:---|:---|:---|---:|:---|:---|:---|
| <alice@uni.edu> | 1 | 1 | 9 | 1 | 1 | 2 |
| <bob@gmail.com> | 2 | 2 | 6 | 1 | 2 | 1 |
| <carol@yahoo.com> | 1 | 3 | 4 | 2 | 1 | 1 |
| <david@uni.edu> | 2 | 1 | 8 | 1 | 1 | 2 |
| <eva@outlook.com> | 1 | 1 | 9 | 2 | 1 | 1 |
| <frank@gmail.com> | 2 | 3 | 3 | 2 | 2 | 1 |
| <grace@uni.edu> | 1 | 2 | 7 | 2 | 1 | 1 |
| <henry@yahoo.com> | 2 | 1 | 9 | 1 | 1 | 2 |
| <iris@gmail.com> | 1 | 3 | 5 | 2 | 2 | 1 |
| <newbie@gmail.com> | 2 | 1 | 7 | 1 | 1 | 2 |

Wave 2 after dict_apply() {.table}

Both waves now share identical column names (`id`, `gender`, `mood`,
`wellbeing`) and `gender`/`mood` are stored as integer-coded factors.
Crucially, `Female` and `Mujer` both become `1`, and `Good` and `Bueno`
both become `1`, so they are now the same concept in the same encoding.

> **Note on what gets kept:**
> [`dict_apply()`](https://icg-cat.github.io/armonia/reference/dict_apply.md)
> retains only the variables that appear in the dictionary. Any unmapped
> columns in the *Variable_Map_Wide* sheet are silently dropped. This is
> the intended behaviour, because the dictionary is the explicit
> contract of what enters the master dataset. For example: columns in
> one dataset that are not present in the other can be kept (with
> partial information), or dropped from the final version by removing
> their line from *Variable_Map_Wide*.

### 3.2 Pre-bind review with `check_bind_ready()`

Before binding, confirm that every shared `target_name` column has the
same R class in both waves.
[`check_bind_ready()`](https://icg-cat.github.io/armonia/reference/check_bind_ready.md)
operates on the *standardized* data, after
[`dict_apply()`](https://icg-cat.github.io/armonia/reference/dict_apply.md)
has recoded factors to integers, so any conflicts it surfaces are real
problems that
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
would coerce silently.

``` r

check_bind_ready(list(w1 = std_w1, w2 = std_w2))
#> ✔ All columns have consistent classes across waves. Ready to bind.
```

A green tick means all columns are class-consistent and the bind is safe
to proceed. If the function returns a conflict tibble instead, inspect
it and add an explicit coercion step before calling
[`harm_bind_waves()`](https://icg-cat.github.io/armonia/reference/harm_bind_waves.md).

### 3.3 Stacking the waves with `harm_bind_waves()`

``` r

stacked <- harm_bind_waves(list(w1 = std_w1, w2 = std_w2))
knitr::kable(head(stacked, 8), caption = "Stacked dataset (first 8 rows)")
```

| source_wave | id | gender | mood | wellbeing | sympt_cramps | sympt_headache | sympt_none |
|:---|:---|:---|:---|---:|:---|:---|:---|
| w1 | <alice@uni.edu> | 1 | 1 | 8 | 1 | 1 | 2 |
| w1 | <bob@gmail.co> | 2 | 2 | 5 | 2 | 2 | 1 |
| w1 | <carol@yahoo.com> | 1 | 3 | 3 | 2 | 2 | 1 |
| w1 | <david@uni.edu> | 2 | 1 | 7 | 1 | 1 | 2 |
| w1 | <eva@outlook.com> | 1 | 1 | 9 | 1 | 1 | 2 |
| w1 | <frank@gmail.com> | 2 | 3 | 2 | 2 | 2 | 1 |
| w1 | <grace@uni.edu> | 1 | 2 | 6 | 1 | 1 | 2 |
| w1 | <henry@yahoo.com> | 2 | 1 | 8 | 1 | 2 | 1 |

Stacked dataset (first 8 rows) {.table}

A `source_wave` column is added automatically to track the origin of
each row. If a variable were present in one wave but not the other,
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
would fill the missing cells with `NA` rather than throwing an error.

### 3.4 Post-transformation quality check with `check_data_loss()`

Before restoring labels, verify that no information was lost during
transformation.
[`check_data_loss()`](https://icg-cat.github.io/armonia/reference/check_data_loss.md)
compares each original wave against its corresponding slice of the
master dataset, using the dictionary to resolve variable names.

``` r

check_data_loss(
  data_list = list(w1 = wave1, w2 = wave2),
  master    = stacked,
  wb_path   = dict_path
)
#> ✔ All transformation checks passed. No data loss detected.
```

Four checks are performed per wave to ensure all modifications are
intentional. Because flags can be triggered by deliberate adjustments
(e.g., dropping columns or collapsing factor levels), they function as a
verification tool rather than a definitive error report:

- row count
- dropped columns
- new `NA` values introduced in mapped variables
- categories collapsed in factor columns

A green tick signifies that the transformation passed all validation
checks. If any changes are flagged, the function returns a tibble where
each row describes an identified issue:

| `check` | Meaning | Common cause |
|----|----|----|
| `row_count` | Rows were lost | Filtering may have been applied unintentionally |
| `dropped_column` | A variable has no mapping in the dictionary | Can be intentional or accidental |
| `na_count` | New `NA`s appeared in a mapped column | A factor level originally present in the data is absent from `Factor_Levels` |
| `unique_values` | A factor variable has fewer distinct values than the original | Two original levels were mapped to the same `standard_code` |

### 3.5 Restoring readable labels with `assign_standard_labels()`

The master dataset stores factors as integers, which simplifies data
binding. For reports and tables labelled variables are preferred. This
function can also be used to translate dataframes between languages.

The recommended procedure is to apply this to a *copy* of the data.

``` r

stacked_labeled <- assign_standard_labels(stacked, dict = dict_path)
knitr::kable(head(stacked_labeled, 8),
             caption = "With standard labels restored (for reporting)")
```

| source_wave | id | gender | mood | wellbeing | sympt_cramps | sympt_headache | sympt_none |
|:---|:---|:---|:---|---:|:---|:---|:---|
| w1 | <alice@uni.edu> | Female | Good | 8 | FALSE | FALSE | TRUE |
| w1 | <bob@gmail.co> | Male | Neutral | 5 | TRUE | TRUE | FALSE |
| w1 | <carol@yahoo.com> | Female | Bad | 3 | TRUE | TRUE | FALSE |
| w1 | <david@uni.edu> | Male | Good | 7 | FALSE | FALSE | TRUE |
| w1 | <eva@outlook.com> | Female | Good | 9 | FALSE | FALSE | TRUE |
| w1 | <frank@gmail.com> | Male | Bad | 2 | TRUE | TRUE | FALSE |
| w1 | <grace@uni.edu> | Female | Neutral | 6 | FALSE | FALSE | TRUE |
| w1 | <henry@yahoo.com> | Male | Good | 8 | FALSE | TRUE | FALSE |

With standard labels restored (for reporting) {.table
style="width:100%;"}

------------------------------------------------------------------------

## 4 Phase 3: Assembly

### 4.1 Auditing IDs before joining with `check_id_audit()`

So far we were able to row-bind two dataframes resulting from one
same(ish) data collection instrument, such as observation T1 in two
languages. Now we need to column-bind this stacked dataset to T2
observations in a longitudinal study, having mostly the same
participants, and new variables. Before column-binding the two
longitudinally, it is worth checking whether all IDs in wave 2 cleanly
match an ID in wave 1.
[`check_id_audit()`](https://icg-cat.github.io/armonia/reference/check_id_audit.md)
compares the two ID vectors using three string-distance metrics
(Levenshtein, Damerau-Levenshtein, LCS) and flags likely typos.

The audit results are written to a `Review_IDs` sheet in the dictionary
workbook so the research team can review and sign off on each match.

``` r

audit <- check_id_audit(
  id_col1 = std_w1$id,
  id_col2 = std_w2$id,
  wb_path = dict_path
)
#> ℹ ID audit complete, see results in Review_IDs. 2 potential typos identified.
knitr::kable(audit, caption = "ID audit: wave-2 IDs vs their best match in wave 1")
```

| index | w2id               | best_match          | all_equal |
|------:|:-------------------|:--------------------|:----------|
|     1 | <alice@uni.edu>    | <alice@uni.edu>     | TRUE      |
|     2 | <bob@gmail.com>    | <bob@gmail.co>      | FALSE     |
|     3 | <carol@yahoo.com>  | <carol@yahoo.com>   | TRUE      |
|     4 | <david@uni.edu>    | <david@uni.edu>     | TRUE      |
|     5 | <eva@outlook.com>  | <eva@outlook.com>   | TRUE      |
|     6 | <frank@gmail.com>  | <frank@gmail.com>   | TRUE      |
|     7 | <grace@uni.edu>    | <grace@uni.edu>     | TRUE      |
|     8 | <henry@yahoo.com>  | <henry@yahoo.com>   | TRUE      |
|     9 | <iris@gmail.com>   | <iris@gmail.com>    | TRUE      |
|    10 | <newbie@gmail.com> | no best match found | FALSE     |

ID audit: wave-2 IDs vs their best match in wave 1 {.table}

Rows where `all_equal` is `FALSE` deserve attention. Here,
`bob@gmail.com` (wave 2) best-matches `bob@gmail.co` (wave 1), a clear
`.co` vs `.com` typo. Without correction, Bob would appear as a *new
participant* rather than a returning one.

Once typos are confirmed, corrections need to be performed before
joining. The helper function
[`anon_clean_email()`](https://icg-cat.github.io/armonia/reference/anon_clean_email.md)
is specifically designed for instances where e-mails act as identifiers,
and it automatically fixes common patterns including the `.co` vs `.com`
typo:

``` r

std_w1_clean      <- std_w1
std_w1_clean$id   <- anon_clean_email(std_w1$id)
```

### 4.2 Building the longitudinal dataset with `harm_add_timepoint()`

[`harm_add_timepoint()`](https://icg-cat.github.io/armonia/reference/harm_add_timepoint.md)
performs a full outer join of the (corrected) wave-1 master against the
wave-2 data, appending a suffix to every new variable so columns do not
collide.

``` r

longitudinal <- harm_add_timepoint(
  master_data = std_w1_clean,
  new_data    = std_w2,
  by_id       = "id",
  suffix      = "_w2"
)
#> 
#> ── Longitudinal join audit ──
#> 
#> • Matched (both waves): 8
#> • Attrition (master only): 2
#> • recruitment (new only): 2
#> • Total participants: 12
#> ℹ New participants added. Check if these are valid new recruits or ID typos.
knitr::kable(longitudinal, caption = "Wide-format longitudinal dataset")
```

| id | gender | mood | wellbeing | sympt_cramps | sympt_headache | sympt_none | gender_w2 | mood_w2 | wellbeing_w2 | sympt_cramps_w2 | sympt_headache_w2 | sympt_none_w2 |
|:---|:---|:---|---:|:---|:---|:---|:---|:---|---:|:---|:---|:---|
| <alice@uni.edu> | 1 | 1 | 8 | 1 | 1 | 2 | 1 | 1 | 9 | 1 | 1 | 2 |
| <bob@gmail.co> | 2 | 2 | 5 | 2 | 2 | 1 | NA | NA | NA | NA | NA | NA |
| <carol@yahoo.com> | 1 | 3 | 3 | 2 | 2 | 1 | 1 | 3 | 4 | 2 | 1 | 1 |
| <david@uni.edu> | 2 | 1 | 7 | 1 | 1 | 2 | 2 | 1 | 8 | 1 | 1 | 2 |
| <eva@outlook.com> | 1 | 1 | 9 | 1 | 1 | 2 | 1 | 1 | 9 | 2 | 1 | 1 |
| <frank@gmail.com> | 2 | 3 | 2 | 2 | 2 | 1 | 2 | 3 | 3 | 2 | 2 | 1 |
| <grace@uni.edu> | 1 | 2 | 6 | 1 | 1 | 2 | 1 | 2 | 7 | 2 | 1 | 1 |
| <henry@yahoo.com> | 2 | 1 | 8 | 1 | 2 | 1 | 2 | 1 | 9 | 1 | 1 | 2 |
| <iris@gmail.com> | 1 | 3 | 4 | 2 | 2 | 1 | 1 | 3 | 5 | 2 | 2 | 1 |
| <jack@uni.edu> | 2 | 2 | 6 | 1 | 1 | 2 | NA | NA | NA | NA | NA | NA |
| <bob@gmail.com> | NA | NA | NA | NA | NA | NA | 2 | 2 | 6 | 1 | 2 | 1 |
| <newbie@gmail.com> | NA | NA | NA | NA | NA | NA | 2 | 1 | 7 | 1 | 1 | 2 |

Wide-format longitudinal dataset {.table style="width:100%;"}

The join audit summarises the study dynamics automatically:

- **Alice, Bob, Carol, David, Eva, Frank, Grace, Henry, Iris**, matched
  across both waves (9 participants).
- **Jack**, present only in wave 1 (attrition); his `_w2` columns are
  `NA`.
- **newbie@gmail.com**, present only in wave 2 (new recruit); his wave-1
  columns are `NA`.

> The alert “New participants added. Check if these are valid new
> recruits or ID typos.” is a reminder to always run
> [`check_id_audit()`](https://icg-cat.github.io/armonia/reference/check_id_audit.md)
> *before* this step.

------------------------------------------------------------------------

## 5 Phase 4: Anonymization

Identifiers in data collection can be sensitive information. Before
sharing the dataset with colleagues or publishing, researchers can
replace them with pseudonymous hashes.

### 5.1 Cleaning emails with `anon_clean_email()`

[`anon_clean_email()`](https://icg-cat.github.io/armonia/reference/anon_clean_email.md)
standardizes email strings, lowercases, trims whitespace, and corrects
common typos, producing a clean, consistent input for hashing. We
already used it above to fix Bob’s ID before the longitudinal join; here
we show it explicitly on the wave-1 emails:

``` r

data.frame(
  raw     = wave1$email,
  cleaned = anon_clean_email(wave1$email)
) |>
  knitr::kable(caption = "Email cleaning: Bob's .co typo corrected automatically")
```

| raw               | cleaned           |
|:------------------|:------------------|
| <alice@uni.edu>   | <alice@uni.edu>   |
| <bob@gmail.co>    | <bob@gmail.co>    |
| <carol@yahoo.com> | <carol@yahoo.com> |
| <david@uni.edu>   | <david@uni.edu>   |
| <eva@outlook.com> | <eva@outlook.com> |
| <frank@gmail.com> | <frank@gmail.com> |
| <grace@uni.edu>   | <grace@uni.edu>   |
| <henry@yahoo.com> | <henry@yahoo.com> |
| <iris@gmail.com>  | <iris@gmail.com>  |
| <jack@uni.edu>    | <jack@uni.edu>    |

Email cleaning: Bob’s .co typo corrected automatically {.table}

Supported corrections: `.co` to `.com`, `.con` to `.com`, `@gogle` to
`@google`. Empty strings and `NA` values are preserved as
`NA_character_`, hashing the literal string `"NA"` would be a silent
data error.

### list of common typos

Common typos and mispellings that the function reviews:

- domain typos
- tlds whitelist
- other reviws

| Misspelling   | Replacement    |
|---------------|----------------|
| goggle.com    | google.com     |
| gogle.com     | google.com     |
| googl.com     | google.com     |
| gmial.com     | gmail.com      |
| gmai.com      | gmail.com      |
| gamil.com     | gmail.com      |
| gmmail.com    | gmail.com      |
| gmaill.com    | gmail.com      |
| hotmal.com    | hotmail.com    |
| hotmial.com   | hotmail.com    |
| hotnail.com   | hotmail.com    |
| hotmaill.com  | hotmail.com    |
| outlok.com    | outlook.com    |
| outloook.com  | outlook.com    |
| yaho.com      | yahoo.com      |
| yahooo.com    | yahoo.com      |
| iclod.com     | icloud.com     |
| iclould.com   | icloud.com     |
| protonmal.com | protonmail.com |
| ————–         | —————–         |

“io”, “ai”, “co”, “uk”, “de”, “fr”, “es”, “it”, “nl”, “br”, “mx”, “ca”,
“au”, “jp”, “cn”, “in”, “ru”, “za”, “ar”, “bo”, “br”, “cl”, “co”, “cr”,
“cu”, “do”, “ec”, “sv”, “gt”, “hn”, “ni”, “pa”, “py”, “pe”, “uy”, “ve”,
“me”, “ws”

- mailto: prefix
- surrounding quotes
- surrounding angle brackets
- missing @ or multiple @
- separator mistakes: comma or semicolon instead of dot
- space in address
- double dots
- handle starts or ends with dot
- domain starts or ends with dot
- domain starts or ends with hyphen
- 

### 5.2 Hashing IDs with `anon_hash()`

[`anon_hash()`](https://icg-cat.github.io/armonia/reference/anon_hash.md)
requires a project-specific salt stored in the `HARMONIZE_SALT`
environment variable. It refuses to run without one, preventing the
creation of insecure, reproducible hashes with an empty key.

``` r

Sys.setenv(HARMONIZE_SALT = "wellbeing-study-2025")

data.frame(
  email     = anon_clean_email(wave1$email),
  pseudo_id = anon_hash(anon_clean_email(wave1$email))
) |>
  knitr::kable(caption = "Email to 12-character SHA-256 pseudo-ID")
```

| email             | pseudo_id    |
|:------------------|:-------------|
| <alice@uni.edu>   | a4007f7aaf6e |
| <bob@gmail.co>    | 627d8d9c612d |
| <carol@yahoo.com> | 2a750aaa0a36 |
| <david@uni.edu>   | 71255bf81315 |
| <eva@outlook.com> | bbff241474fe |
| <frank@gmail.com> | 95dfe9c262ed |
| <grace@uni.edu>   | 57ba6655fb05 |
| <henry@yahoo.com> | 9fdec6d5e5a8 |
| <iris@gmail.com>  | 127f3885ba0a |
| <jack@uni.edu>    | 89b07728a167 |

Email to 12-character SHA-256 pseudo-ID {.table}

Apply to the longitudinal dataset and remove the raw email column:

``` r

longitudinal$pseudo_id <- anon_hash(anon_clean_email(longitudinal$id))
longitudinal$id        <- NULL

knitr::kable(longitudinal, caption = "Final anonymized longitudinal dataset")
```

| gender | mood | wellbeing | sympt_cramps | sympt_headache | sympt_none | gender_w2 | mood_w2 | wellbeing_w2 | sympt_cramps_w2 | sympt_headache_w2 | sympt_none_w2 | pseudo_id |
|:---|:---|---:|:---|:---|:---|:---|:---|---:|:---|:---|:---|:---|
| 1 | 1 | 8 | 1 | 1 | 2 | 1 | 1 | 9 | 1 | 1 | 2 | a4007f7aaf6e |
| 2 | 2 | 5 | 2 | 2 | 1 | NA | NA | NA | NA | NA | NA | 627d8d9c612d |
| 1 | 3 | 3 | 2 | 2 | 1 | 1 | 3 | 4 | 2 | 1 | 1 | 2a750aaa0a36 |
| 2 | 1 | 7 | 1 | 1 | 2 | 2 | 1 | 8 | 1 | 1 | 2 | 71255bf81315 |
| 1 | 1 | 9 | 1 | 1 | 2 | 1 | 1 | 9 | 2 | 1 | 1 | bbff241474fe |
| 2 | 3 | 2 | 2 | 2 | 1 | 2 | 3 | 3 | 2 | 2 | 1 | 95dfe9c262ed |
| 1 | 2 | 6 | 1 | 1 | 2 | 1 | 2 | 7 | 2 | 1 | 1 | 57ba6655fb05 |
| 2 | 1 | 8 | 1 | 2 | 1 | 2 | 1 | 9 | 1 | 1 | 2 | 9fdec6d5e5a8 |
| 1 | 3 | 4 | 2 | 2 | 1 | 1 | 3 | 5 | 2 | 2 | 1 | 127f3885ba0a |
| 2 | 2 | 6 | 1 | 1 | 2 | NA | NA | NA | NA | NA | NA | 89b07728a167 |
| NA | NA | NA | NA | NA | NA | 2 | 2 | 6 | 1 | 2 | 1 | 9d0c43960e3e |
| NA | NA | NA | NA | NA | NA | 2 | 1 | 7 | 1 | 1 | 2 | 8fb49c6fb9f3 |

Final anonymized longitudinal dataset {.table}

The raw email is gone; the `pseudo_id` is deterministic (same email +
same salt == same hash) so records can still be linked within the study,
but the identifier cannot be reversed without the salt.

------------------------------------------------------------------------

## 6 The Complete Pipeline

Below is the full workflow condensed into a single annotated script,
mapping directly onto the four-phase diagram in the package
documentation.

``` r

library(armonia)

# ── 1. RAW DATA ────────────────────────────────────────────────────────────────
wave1 <- data.frame(...)   # English wave
wave2 <- data.frame(...)   # Spanish wave

# ── 2. DICTIONARY ──────────────────────────────────────────────────────────────
dict_path <- "my_study_dictionary.xlsx"

dict_init(list(w1 = wave1, w2 = wave2), save_path = dict_path)
# Open dictionary.xlsx in Excel:
#     Variable_Map_Wide : set meaningful target_name for each row
#     Factor_Levels     : align standard_code across waves, add standard_label

dict_validate(dict_path)

# ── 3. STANDARDIZE ─────────────────────────────────────────────────────────────
std_w1 <- dict_apply(wave1, wb_path = dict_path, source_name = "orig_w1")
std_w2 <- dict_apply(wave2, wb_path = dict_path, source_name = "orig_w2")

# Pre-bind gate: confirm class consistency across waves
check_bind_ready(list(w1 = std_w1, w2 = std_w2))
# Review any warnings before proceeding

stacked <- harm_bind_waves(list(w1 = std_w1, w2 = std_w2))

# Post-bind data quality gate: verify no information was lost in transformation
check_data_loss(data_list = list(w1 = wave1, w2 = wave2),
                master    = stacked,
                wb_path   = dict_path)
# Review any warnings before proceeding

# For modelling: stacked (integer-coded factors)
# For reporting: assign_standard_labels() restores readable labels
stacked_labeled <- assign_standard_labels(stacked, dict = dict_path)

# ── 4. ASSEMBLE ────────────────────────────────────────────────────────────────
check_integrity(stacked, id_col = "id", time_col = "source_wave")

# Audit IDs for typos before the longitudinal join
check_id_audit(id_col1 = std_w1$id, id_col2 = std_w2$id, wb_path = dict_path)
# Review Review_IDs sheet; correct any confirmed typos

std_w1$id   <- anon_clean_email(std_w1$id)   # fix typos in wave-1 IDs

longitudinal <- harm_add_timepoint(
  master_data = std_w1,
  new_data    = std_w2,
  by_id       = "id",
  suffix      = "_w2"
)

# ── 5. ANONYMIZE ───────────────────────────────────────────────────────────────
Sys.setenv(HARMONIZE_SALT = "your-project-secret-salt")  # set once per session

longitudinal$pseudo_id <- anon_hash(anon_clean_email(longitudinal$id))
longitudinal$id        <- NULL
```

------------------------------------------------------------------------

## Summary of Functions

| Function | Phase | Purpose |
|----|----|----|
| [`dict_init()`](https://icg-cat.github.io/armonia/reference/dict_init.md) | Dictionary | Scan waves → generate Excel mapping skeleton |
| [`dict_validate()`](https://icg-cat.github.io/armonia/reference/dict_validate.md) | Dictionary | Fail-fast schema check on completed dictionary |
| [`dict_check_compat()`](https://icg-cat.github.io/armonia/reference/dict_check_compat.md) | Dictionary | Check if an existing dictionary is applicable to a new dataset |
| `harm_detect_multival()` | Pre-processing | Scan a dataframe for columns containing multi-value cells |
| `harm_split_multival()` | Pre-processing | Split a multi-value column into atomic columns |
| [`dict_apply()`](https://icg-cat.github.io/armonia/reference/dict_apply.md) | Standardize | Rename variables and recode factors to integers |
| [`check_bind_ready()`](https://icg-cat.github.io/armonia/reference/check_bind_ready.md) | Standardize | Verify class consistency across waves before binding |
| [`harm_bind_waves()`](https://icg-cat.github.io/armonia/reference/harm_bind_waves.md) | Standardize | Safe row-bind of standardized waves (long format) |
| [`check_data_loss()`](https://icg-cat.github.io/armonia/reference/check_data_loss.md) | Standardize | Verify no information was lost in transformation |
| [`assign_standard_labels()`](https://icg-cat.github.io/armonia/reference/assign_standard_labels.md) | Standardize | Restore readable labels for reporting |
| [`check_integrity()`](https://icg-cat.github.io/armonia/reference/check_integrity.md) | Assemble | Validate tidy structure of the master dataset |
| [`flag_duplicate_cases()`](https://icg-cat.github.io/armonia/reference/flag_duplicate_cases.md) | Assemble | Flag cross-wave rows sharing same ID and ≥ threshold similarity |
| [`check_id_audit()`](https://icg-cat.github.io/armonia/reference/check_id_audit.md) | Assemble | Detect ID typos across waves using string distance |
| [`harm_add_timepoint()`](https://icg-cat.github.io/armonia/reference/harm_add_timepoint.md) | Assemble | Full-join a new wave into a wide longitudinal dataset |
| [`anon_clean_email()`](https://icg-cat.github.io/armonia/reference/anon_clean_email.md) | Anonymize | Normalize emails (typos, case, whitespace) |
| [`anon_hash()`](https://icg-cat.github.io/armonia/reference/anon_hash.md) | Anonymize | Replace IDs with salted SHA-256 pseudo-IDs |
