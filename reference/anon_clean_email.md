# Clean and standardize email strings

Prepares email addresses for hashing by trimming whitespace, normalizing
case, and correcting common typos (e.g. `.co` endings, `.con` endings,
`@gogle` domains). Empty strings and `NA` values are preserved as
`NA_character_` to prevent hashing the literal string `"NA"`.

## Usage

``` r
anon_clean_email(x)
```

## Arguments

- x:

  A character vector of email addresses.

## Value

A character vector of the same length as `x`, standardized and ready for
hashing.

## Examples

``` r
emails <- c("alice@uni.edu", "BOB@gmail.co ", NA)
anon_clean_email(emails)
#> [1] "alice@uni.edu" "bob@gmail.com" NA             
```
