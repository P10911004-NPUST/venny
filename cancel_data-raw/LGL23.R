# rm(list = ls())
#
# library(tidyverse)
# library(DESeq2)
#
# cmat <- read_tsv("./count_matrix.tsv", show_col_types = FALSE)
# cmat <- cmat %>%
#     column_to_rownames("gene_id")
# colnames(cmat) <- str_replace(colnames(cmat), ".*NZ(\\d\\d).*", "LGL23-\\1")
# cmat <- cmat[, sort(colnames(cmat))]
# cmat <- cmat %>%
#     dplyr::mutate(across(everything(), function(x) as.integer(ceiling(x))))
#
# cmat <- as.matrix(cmat)
#
# sample_info <- read.csv("C:/jklai/project/LGL23_Plant-Physiology/sample_info.csv") %>%
#     dplyr::select("sample_id", "genotype", "RGF1_treatment..nM.") %>%
#     dplyr::rename("treatment" = "RGF1_treatment..nM.") %>%
#     dplyr::mutate(
#         sample_id = str_replace(sample_id, "NZ", "LGL23-"),
#         genotype = case_when(genotype == "plt2" ~ "KO",
#                              genotype == "gPLT2" ~ "WT",
#                              genotype == "C212S" ~ "OE"),
#         treatment = case_when(treatment == 0 ~ "mock",
#                               treatment == 0.3 ~ "mid",
#                               treatment == 5 ~ "high")
#     ) %>%
#     dplyr::mutate(group = as.factor(paste(genotype, treatment, sep = "_"))) %>%
#     tidyr::drop_na() %>%
#     tibble::column_to_rownames("sample_id")
#
# save(cmat, sample_info, file = "./LGL23.rda")
#
# dds <- DESeq2::DESeqDataSetFromMatrix(
#     countData = cmat,
#     colData = sample_info,
#     design = ~ group
# )
#
# dds <- DESeq2::DESeq(dds)
#
# identical(rownames(sample_info), colnames(cmat))
