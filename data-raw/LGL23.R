library(tidyverse)

genotype_lvl <- c("WT", "KO", "OE")
treatment_lvl <- c("mock", "low", "high")
group_lvl <- apply(
    X = expand.grid(genotype_lvl, treatment_lvl),
    MARGIN = 1,
    FUN = \(x) paste(x, collapse = "_")
)

cmat <- read.csv("./data-raw/count_matrix.csv", row.names = 1)
cmat <- as.matrix(cmat[rowSums(cmat) > 10, , drop = FALSE])

sample_info <- read.csv("./data-raw/sample_info.csv", row.names = 1)
sample_info <- sample_info %>%
    mutate(
        genotype = factor(genotype, levels = genotype_lvl),
        treatment = factor(treatment, levels = treatment_lvl),
        group = factor(group, levels = group_lvl)
    )

stopifnot(identical(colnames(cmat), rownames(sample_info)))

GFD <- read.csv("./data-raw/gene_func_desc.csv")

LGL23 <- list(
    count_matrix = cmat,
    sample_info = sample_info,
    GFD = GFD
)

usethis::use_data(LGL23, overwrite = TRUE)
