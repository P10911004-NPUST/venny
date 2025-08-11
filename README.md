# venny

`venny` is an R package for gene expression analysis with a focus on Venn diagram visualization (UpSet plot support not yet available).  
It offers a simple and efficient way to extract target genes after DEG analysis, and can produce publication-ready plots out of the box.  
For more advanced or customizable visualizations, consider other packages such as `ggVennDiagram`, `ggvenn`, or `UpSetR`.

## Installation

You can install the development version of `venny` from [GitHub](https://github.com/P10911004-NPUST/venny) with:

``` r
if ( ! require(devtools) ) install.packages("devtools")
devtools::install_github("P10911004-NPUST/venny")
```

## Quick start

```r
library(venny)
s <- 1:1000
lst <- list(
    Set_A = sample(s, 500),
    Set_B = sample(s, 700),
    Set_C = sample(s, 900),
    Set_D = sample(s, 600)
)
e3 <- venny(lst[1:3])
e3$plot
e4 <- venny(lst[1:4])
e4$plot
```