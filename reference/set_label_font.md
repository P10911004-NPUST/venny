# Set label font

Generate a list of the available set label font parameters.

## Usage

``` r
set_label_font(
  family = "sans",
  face = "bold",
  size = 5,
  color = c("#CC79A7", "#009E73", "#E69F00", "#56B4E9"),
  angle = NULL
)

subset_label_font(
  family = "sans",
  face = "bold.italic",
  size = 4,
  color = "black",
  angle = 0
)

subset_count_font(
  family = "sans",
  face = "plain",
  size = 4,
  color = "grey20",
  angle = 0
)

subset_percentage_font(
  family = "sans",
  face = "plain",
  size = 4,
  color = "grey20",
  angle = 0
)
```

## Arguments

- family:

  Character (default: "sans").

- face:

  Character (default: "bold").

- size:

  Numeric (default: 5).

- color:

  Character (default: c("#009E73", "#E69F00", "#CC79A7", "#56B4E9")).

- angle:

  Numeric \| NULL (default: NULL).

## Value

A list.

## Examples

``` r
set_label_font()
#> $family
#> [1] "sans"
#> 
#> $face
#> [1] "bold"
#> 
#> $size
#> [1] 5
#> 
#> $color
#> [1] "#CC79A7" "#009E73" "#E69F00" "#56B4E9"
#> 
#> $angle
#> NULL
#> 
```
