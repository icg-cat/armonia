# Detect factor potential in a vector

Heuristic classifier that decides whether a vector should be coerced to
a factor. Uses two strategies: a cardinality shortcut for
Likert/binary-style variables (up to 7 unique values), and a stricter
pattern test combining cardinality, sample size, and category
repetition.

## Usage

``` r
detect_factor_potential(x, max_unique = NULL)
```

## Arguments

- x:

  A vector of any type.

- max_unique:

  Override maximum of 16 unique values for factors. Defaults to NULL.

## Value

The original vector unchanged, or a factor if the heuristic identifies
it as categorical.
