# venny

This is an R package for gene expression analysis by inferencing the venn diagram (upset plot is not yet provided).  
The primary goal of `venny` is to provide a convenient way to extract target genes after differentially expressed genes (DEG) analysis.
Although the plotting function of `venny` may not as robust as other packages, it already sufficient for producing publication-ready plot.  
If more advance plot is required, you may consider other packages such as `ggVennDiagram`, `ggvenn`, and `UpSetR`.

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
venny(lst[1:3])
```