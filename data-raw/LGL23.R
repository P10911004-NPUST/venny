if (!require(tidyr)) install.packages("tidyr")
if (!require(dplyr)) install.packages("dplyr")
if (!require(BiocManager)) install.packages("BiocManager")
if (!require(DESeq2)) BiocManager::install("DESeq2", ask = FALSE)

genotype_lvl <- c("WT", "KO", "OE")
treatment_lvl <- c("mock", "low", "high")
group_lvl <- apply(
    X = expand.grid(genotype_lvl, treatment_lvl),
    MARGIN = 1,
    FUN = \(x) paste(x, collapse = "_")
)

cmat <- read.csv("./data-raw/count_matrix.csv", row.names = 1)
cmat <- as.matrix(cmat[rowSums(cmat) > 0, , drop = FALSE])

sample_info <- read.csv("./data-raw/sample_info.csv", row.names = 1)
sample_info <- sample_info %>%
    dplyr::mutate(
        genotype = factor(genotype, levels = genotype_lvl),
        treatment = factor(treatment, levels = treatment_lvl),
        group = factor(group, levels = group_lvl)
    )

stopifnot(identical(colnames(cmat), rownames(sample_info)))

GFD <- read.csv("./data-raw/gene_func_desc.csv")

#----------------------- DEGs -----------------------# 
dds <- DESeq2::DESeqDataSetFromMatrix(
    countData = cmat,
    colData = sample_info,
    design = ~ group
)
dds <- DESeq2::DESeq(dds)

DEGs <- lapply(
    list("KO_low", "KO_high", "WT_mock", "OE_mock"),
    function(`_`)
    {
        res <- DESeq2::results(dds, contrast = c("group", `_`, "KO_mock"))
        res <- as.data.frame(res)
        res <- res %>% 
            tidyr::drop_na(padj) %>% 
            dplyr::filter(padj < 0.05)
        return(rownames(res))
    }
)
names(DEGs) <- c("KO_low vs KO_mock", 
                 "KO_high vs KO_mock",
                 "WT_mock vs KO_mock",
                 "OE_mock vs KO_mock")

LGL23 <- list(
    count_matrix = cmat,
    sample_info = sample_info,
    GFD = GFD, 
    DEGs = DEGs
)

usethis::use_data(LGL23, overwrite = TRUE)
