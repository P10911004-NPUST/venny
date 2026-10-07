# Draw polygons to highlight subsets

A handy wrapper for the
[`ggplot2::geom_polygon()`](https://ggplot2.tidyverse.org/reference/geom_polygon.html)
used to represent the result of set operations.

## Usage

``` r
highlight(
  venn,
  setops,
  color = "black",
  linewidth = 1.2,
  linetype = "solid",
  fill = "black",
  alpha = 0.2,
  ...
)
```

## Arguments

- venn:

  Venn diagram produced from
  [`venny::venny()`](https://p10911004-npust.github.io/venny/reference/venny.md).

- setops:

  The result of set operations.

- color:

  Character (default: "transparent"). The polygon line color.

- linewidth:

  Numeric (default: 1.5). The polygon linewidth.

- linetype:

  Character (default: "solid"). The polygon linetype.

- fill:

  Character (default: "black"). The polygon color.

- alpha:

  Numeric (default: 0.4). The polygon transparency (0-1).

- ...:

  The other arguments passed into
  [`ggplot2::geom_polygon`](https://ggplot2.tidyverse.org/reference/geom_polygon.html).

## Value

A ggplot object.

## Examples

``` r
data <- list(
    Set_A = c(10:100, 500:600),
    Set_B = c(5:150, 550:650),
    Set_C = c(80:180, 580:680),
    Set_D = c(120:220, 520:620)
)
out <- venny(data, detail = TRUE)
p0 <- out$venn
ep <- out$ellipse_path
res <- intersect(ep$Set_A, ep$Set_B, ep$Set_D)
highlight(p0, res)
```
