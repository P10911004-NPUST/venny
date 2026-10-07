# RNA-seq data

A list of essential information for RNA-seq analysis and an example for
demonstration.

## Usage

``` r
LGL23
```

## Format

An object of class `list` of length 4.

## Details

- sample_info: the information for the sample IDs noted in the count
  matrix.

- GFD: A data.frame of gene functional description.

- count_matrix: A matrix of the gene expression count reads for each
  sample. The row names are gene IDs; the column names are sample IDs.

- DEGs: An example demonstrating differentially expressed genes across
  four conditions.
