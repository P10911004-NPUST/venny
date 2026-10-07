# Create a bits matrix

Produce a bits matrix for all possible combinations of the input sets.

## Usage

``` r
bits_encoding(x, rownames = TRUE, sep = "")
```

## Arguments

- x:

  A character vector.

- rownames:

  Logical (default: TRUE). Whether to show rownames.

- sep:

  A character used to separate the group names (default is `""`). Only
  working when `rownames = TRUE`.

## Value

A numeric matrix with values of 0 or 1.

## Examples

``` r
bits_encoding(c("A", "B", "C", "D"))
#>      A B C D
#> A    1 0 0 0
#> B    0 1 0 0
#> AB   1 1 0 0
#> C    0 0 1 0
#> AC   1 0 1 0
#> BC   0 1 1 0
#> ABC  1 1 1 0
#> D    0 0 0 1
#> AD   1 0 0 1
#> BD   0 1 0 1
#> ABD  1 1 0 1
#> CD   0 0 1 1
#> ACD  1 0 1 1
#> BCD  0 1 1 1
#> ABCD 1 1 1 1
```
