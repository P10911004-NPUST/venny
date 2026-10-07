# Fixed Length Vector

Create a desired length of vector by trimming/extending the `x` vector.

## Usage

``` r
fixed_length(x, len, fill_with = NULL)
```

## Arguments

- x:

  A vector.

- len:

  Desired length.

- fill_with:

  Element used to extend the vector length. If set to `NULL`, extend by
  itself (default: NULL).

## Value

A vector

## Examples

``` r
# Extend `x` to fulfill the `len` requirement
fixed_length(1:5, 7)
#> [1] 1 2 3 4 5 1 2
# Trim `x` to fulfill the `len` requirement
fixed_length(1:5, 3)
#> [1] 1 2 3
```
