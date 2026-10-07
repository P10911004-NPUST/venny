# Venn diagram

Venn diagram

## Usage

``` r
venny(
  data,
  detail = FALSE,
  ellipse.line = ellipse_line(),
  ellipse.fill = ellipse_fill(),
  ellipse.density = 200L,
  set.label = TRUE,
  set.label.position = set_label_position(),
  set.label.font = set_label_font(),
  subset.label = TRUE,
  subset.label.position = subset_label_position(),
  subset.label.font = subset_label_font(),
  subset.count = TRUE,
  subset.count.position = subset_count_position(),
  subset.count.font = subset_count_font(),
  subset.percentage = TRUE,
  subset.percentage.position = subset_percentage_position(),
  subset.percentage.font = subset_percentage_font(),
  subset.percentage.rounding = 1L
)
```

## Arguments

- data:

  A list with 2 to 4 vectors

- detail:

  Logical (default: TRUE). If TRUE, output a list contains the venn
  diagram and summary table. Otherwise, output only venn diagram.

- ellipse.line:

  A list. See
  [`ellipse_line()`](https://p10911004-npust.github.io/venny/reference/ellipse_line.md).

- ellipse.fill:

  A list. See
  [`ellipse_fill()`](https://p10911004-npust.github.io/venny/reference/ellipse_line.md).

- ellipse.density:

  An integer (default: 200L). Higher value yield smoother ellipse.

- set.label:

  Logical (default: TRUE). If TRUE, show the set labels.

- set.label.position:

  A list. See
  [`set_label_position()`](https://p10911004-npust.github.io/venny/reference/set_label_position.md).

- set.label.font:

  A list. See
  [`set_label_font()`](https://p10911004-npust.github.io/venny/reference/set_label_font.md).

- subset.label:

  Logical (default: TRUE). If TRUE, show the subset labels. If a named
  list is provided, then the selected subset name will be renamed. For
  example, `list(AB = "new_AB")` will change the original subset AB name
  to "new_AB".

- subset.label.position:

  A list. See
  [`subset_label_position()`](https://p10911004-npust.github.io/venny/reference/set_label_position.md).

- subset.label.font:

  A list. See
  [`subset_label_font()`](https://p10911004-npust.github.io/venny/reference/set_label_font.md).

- subset.count:

  Logical (default: TRUE). If TRUE, show the element counts for each
  subset.

- subset.count.position:

  A list. See
  [`subset_count_position()`](https://p10911004-npust.github.io/venny/reference/set_label_position.md).

- subset.count.font:

  A list. See
  [`subset_count_font()`](https://p10911004-npust.github.io/venny/reference/set_label_font.md).

- subset.percentage:

  Logical (default: TRUE). If TRUE, show the percentages of the counts.

- subset.percentage.position:

  A list. See
  [`subset_percentage_position()`](https://p10911004-npust.github.io/venny/reference/set_label_position.md).

- subset.percentage.font:

  A list. See
  [`subset_percentage_font()`](https://p10911004-npust.github.io/venny/reference/set_label_font.md).

- subset.percentage.rounding:

  An integer (default: 1L). How many decimal points.

## Value

A venn diagram which is a `ggplot` object.

## Examples

``` r
lst <- LGL23$DEGs
setLabelPosition <- set_label_position(hjust = c(0.2, -0.5, 0.5, -0.2))
venny(lst, set.label.position = setLabelPosition)
```
