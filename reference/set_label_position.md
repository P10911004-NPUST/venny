# Set label position

Generate the coordinates for the set labels. This is passed into the
[`venny()`](https://p10911004-npust.github.io/venny/reference/venny.md)'s
`set.label.position` arguments.

## Usage

``` r
set_label_position(hjust = 0, vjust = 0, show = NULL, hide = NULL)

subset_label_position(hjust = 0, vjust = 0, show = NULL, hide = NULL)

subset_count_position(hjust = 0, vjust = 0, show = NULL, hide = NULL)

subset_percentage_position(hjust = 0, vjust = 0, show = NULL, hide = NULL)
```

## Arguments

- hjust:

  A numeric vector (default: 0). Horizontal adjustment, adjust the
  x-axis coordinates of the set labels.

- vjust:

  A numeric vector (default: 0). Vertical adjustment, adjust the y-axis
  coordinates of the set labels.

- show:

  A character vector (default: NULL). Show only the specified set
  labels. By default, show all labels.

- hide:

  A character vector (default: NULL). Do not show the specified set
  labels. By default, show all labels.

## Value

A list contains 3 named list. Each named list contains the x, y
coordinates of the set labels for different venn diagram (2, 3, or 4
ellipses), respectively.

## Examples

``` r
set_label_position()
#> [[1]]
#> [[1]]$A
#> [1] -0.50  1.15
#> 
#> [[1]]$B
#> [1] 0.50 1.15
#> 
#> 
#> [[2]]
#> [[2]]$A
#> [1] 0.00 1.65
#> 
#> [[2]]$B
#> [1] -1.4  0.3
#> 
#> [[2]]$C
#> [1] 1.4 0.3
#> 
#> 
#> [[3]]
#> [[3]]$A
#> [1] -2.45 -1.50
#> 
#> [[3]]$B
#> [1] -1.3  2.4
#> 
#> [[3]]$C
#> [1] 1.3 2.4
#> 
#> [[3]]$D
#> [1]  2.45 -1.50
#> 
#> 
#> attr(,"hjust")
#> [1] 0
#> attr(,"vjust")
#> [1] 0
```
