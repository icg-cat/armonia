# Generate pseudonymous research IDs via salted hashing

Creates a 12-character SHA-256 hash from an identifier and a
project-specific salt retrieved from the `HARMONIZE_SALT` environment
variable. Aborts immediately if no salt is found, preventing insecure
de-identification with an empty or default key. `NA` inputs are passed
through as `NA_character_`.

## Usage

``` r
anon_hash(x)
```

## Arguments

- x:

  A character vector of identifiers (e.g. the output of
  [`anon_clean_email()`](https://icg-cat.github.io/armonia/reference/anon_clean_email.md)).

## Value

A character vector of truncated 12-character hex strings, the same
length as `x`.

## Examples

``` r
old_salt <- Sys.getenv("HARMONIZE_SALT")
Sys.setenv(HARMONIZE_SALT = "example-salt")
anon_hash(c("alice@uni.edu", NA))
#> [1] "03674ba2a7eb" NA            
Sys.setenv(HARMONIZE_SALT = old_salt)
```
