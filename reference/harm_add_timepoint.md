# Append a New Longitudinal Timepoint

Safely joins a new wave of data to an existing master dataset. It
automatically handles variable renaming (suffixing) to prevent
collisions and uses a full join to preserve all participants
(attrition + recruitment).

## Usage

``` r
harm_add_timepoint(master_data, new_data, by_id, suffix, verbose = TRUE)
```

## Arguments

- master_data:

  A data.frame/tibble representing the current master dataset (Time 1).

- new_data:

  A data.frame/tibble representing the new timepoint (Time N+1). Must be
  already processed by dict_apply().

- by_id:

  Character string. The name of the distinct variable to join by (e.g.,
  "participant_id"). Must exist in both datasets.

- suffix:

  Character string. The suffix to append to new variables (e.g., "\_w2",
  "\_t2"). Must start with an underscore/separator to ensure readability
  and comply with snake_case referencing across objects.

- verbose:

  Logical. If TRUE, prints a summary of the join statistics.

## Value

A single wide-format tibble containing both datasets.

## Examples

``` r
master   <- data.frame(id = c("alice", "bob"), wellbeing = c(8, 5))
new_wave <- data.frame(id = c("alice", "bob"), wellbeing = c(9, 6))
harm_add_timepoint(master, new_wave, by_id = "id", suffix = "_w2")
#> 
#> ── Longitudinal join audit ──
#> 
#> • Matched (both waves): 2
#> • Attrition (master only): 0
#> • recruitment (new only): 0
#> • Total participants: 2
#> # A tibble: 2 × 3
#>   id    wellbeing wellbeing_w2
#>   <chr>     <dbl>        <dbl>
#> 1 alice         8            9
#> 2 bob           5            6
```
