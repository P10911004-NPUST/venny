# How many subsets

Calculate the number of combinations that can be derived from the given
character vector.

## Usage

``` r
how_many_subsets(x, detail = FALSE)
```

## Arguments

- x:

  A character vector.

- detail:

  Logical (default: FALSE). Whether to output the combinations.

## Value

An integer value or a list.

## Examples

``` r
how_many_subsets(c("qqa", "bnk", "sdf", "123"))
#> [1] 15
```
