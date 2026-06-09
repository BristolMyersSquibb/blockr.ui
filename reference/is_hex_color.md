# Validate a hex colour string

Tests whether `x` is a single `#rgb` (3-digit) or `#rrggbb` (6-digit)
hex colour string. `blockr.core` has no colour predicate, so this one
lives in `blockr.ui`; it is exported so consumers (e.g. `blockr.dock`)
validate colours against the same rule the stack menu uses instead of
re-implementing it.

## Usage

``` r
is_hex_color(x)
```

## Arguments

- x:

  Object to test.

## Value

A length-1 logical.

## Examples

``` r
is_hex_color("#66c2a5")
#> [1] TRUE
is_hex_color("nope")
#> [1] FALSE
```
