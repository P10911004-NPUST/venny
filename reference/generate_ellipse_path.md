# Generate Ellipse Path

Generate Ellipse Path

## Usage

``` r
generate_ellipse_path(x0 = 0, y0 = 0, a = 2, b = 1, angle = 0, density = 200)
```

## Arguments

- x0:

  The coordinate x of the polygon center point (default: 0).

- y0:

  The coordinate y of the polygon center point (default: 0).

- a:

  Long arm (default: 2).

- b:

  Short arm (default: 1).

- angle:

  Rotation angle in degree (default: 0).

- density:

  Greater amount of points yield smoother ellipse (default: 200).

## Value

A data.frame with the point coordinates (x, y) to construct the polygon

## Examples

``` r
library(ggplot2)
# Draw a circle
circle <- generate_ellipse_path(a = 1, b = 1)
ggplot(circle, aes(x, y)) +
    geom_polygon() +
    coord_fixed()

# Draw an ellipse
ellipse <- generate_ellipse_path(a = 2, b = 1, angle = 45)
ggplot(ellipse, aes(x, y)) +
    geom_polygon() +
    coord_fixed()
```
