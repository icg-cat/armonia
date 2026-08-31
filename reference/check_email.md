# Flag structurally suspicious email addresses

Runs a set of heuristic checks against each email address and flags
addresses that are likely typed incorrectly: stray `mailto:` prefixes,
surrounding quotes or angle brackets, a missing or duplicated `@`, a
comma or semicolon where a dot was meant, consecutive dots, a handle or
domain that starts or ends with a dot or hyphen, known domain
misspellings (e.g. `gmial.com`), and suspicious or misspelled top-level
domains. Each address gets an overall `flag` of `"ok"`,
`"possible_mistake"`, or `"likely_mistake"`, with a semicolon-separated
`reason` explaining what was flagged.

## Usage

``` r
check_email(x)
```

## Arguments

- x:

  A character vector of email addresses to check.

## Value

A data.frame with one row per element of `x` and three columns: `email`
(the original address), `flag` (`"ok"`, `"possible_mistake"`, or
`"likely_mistake"`), and `reason` (`NA` when `flag` is `"ok"`, otherwise
the flagged issues joined with `"; "`).

## Examples

``` r
emails <- c("alice@uni.edu", "bob@gmail.co", "carol@yahoo.com")
check_email(emails)
#>             email             flag                       reason
#> 1   alice@uni.edu               ok                         <NA>
#> 2    bob@gmail.co possible_mistake possible .co instead of .com
#> 3 carol@yahoo.com               ok                         <NA>
```
