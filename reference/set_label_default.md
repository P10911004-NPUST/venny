# Set labels

This is an internal function for
[`venny()`](https://p10911004-npust.github.io/venny/reference/venny.md)
to automatically generate set labels, when the input list is unnamed.

## Usage

``` r
set_label_default(n_sets)

subset_label_default(n_sets)
```

## Arguments

- n_sets:

  An integer.

## Value

A list.

## Examples

``` r
set_label_default(3)
#> $A
#> [1] "Set_A"
#> 
#> $B
#> [1] "Set_B"
#> 
#> $C
#> [1] "Set_C"
#> 
```
