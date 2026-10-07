# Control the ellipse textures

This is used to generate the ellipse line color and transparency
parameters, for passing into the
[`venny()`](https://p10911004-npust.github.io/venny/reference/venny.md)s
`ellipse.line` argument.

## Usage

``` r
ellipse_line(
  linetype = "blank",
  linewidth = 0.5,
  color = c("#CC79A7", "#009E73", "#E69F00", "#56B4E9"),
  alpha = 0.5
)

ellipse_fill(
  color = c("#CC79A7", "#009E73", "#E69F00", "#56B4E9"),
  alpha = 0.2
)
```

## Arguments

- linetype:

  A character. The options are: blank, solid, dashed, dotted, dotdash,
  longdash, twodash.

- linewidth:

  A number (default: 0.5).

- color:

  A character vector (default:
  `c("#009E73", "#E69F00", "#CC79A7", "#56B4E9")`).

- alpha:

  A number range from 0 to 1 (default: 0.5). Set the line transparency.

## Value

A list contains "color" and "alpha" values.

## See also

`ellipse_fill()`

## Examples

``` r
ellipse_line()
#> $linetype
#> [1] "blank"
#> 
#> $linewidth
#> [1] 0.5
#> 
#> $color
#> [1] "#CC79A7" "#009E73" "#E69F00" "#56B4E9"
#> 
#> $alpha
#> [1] 0.5
#> 
```
