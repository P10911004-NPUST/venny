# Ellipse positions

This is an internal function used to generate the ellipse parameters
that was used by the
[`generate_ellipse_path()`](https://p10911004-npust.github.io/venny/reference/generate_ellipse_path.md)
function. It returns a list encompassing 3 lists. Each list consists of
the x0, y0, long-arm, short-arm, and angle default values for generating
2, 3, or 4 ellipses.

## Usage

``` r
ellipse_position()
```

## Value

A two-layer nested list.

## Examples

``` r
ellipse_position()
#> [[1]]
#> [[1]]$e1
#>    x0    y0     a     b angle 
#> -0.45  0.00  1.00  1.00  0.00 
#> 
#> [[1]]$e2
#>    x0    y0     a     b angle 
#>  0.45  0.00  1.00  1.00  0.00 
#> 
#> 
#> [[2]]
#> [[2]]$e1
#>    x0    y0     a     b angle 
#>  0.00  0.45  1.00  1.00  0.00 
#> 
#> [[2]]$e2
#>    x0    y0     a     b angle 
#> -0.45 -0.45  1.00  1.00  0.00 
#> 
#> [[2]]$e3
#>    x0    y0     a     b angle 
#>  0.45 -0.45  1.00  1.00  0.00 
#> 
#> 
#> [[3]]
#> [[3]]$e1
#>      x0      y0       a       b   angle 
#>  -1.090  -0.569   2.500   1.350 -45.000 
#> 
#> [[3]]$e2
#>    x0    y0     a     b angle 
#>   0.0   0.0   2.5   1.2 -50.0 
#> 
#> [[3]]$e3
#>    x0    y0     a     b angle 
#>   0.0   0.0   2.5   1.2  50.0 
#> 
#> [[3]]$e4
#>     x0     y0      a      b  angle 
#>  1.090 -0.569  2.500  1.350 45.000 
#> 
#> 
```
