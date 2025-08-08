# lst_4sets <- list(
#     set_1 = letters[1:26],
#     set_2 = letters[3:17],
#     set_3 = letters[11:24],
#     set_4 = letters[1:20]
# )
#
# s <- runif(10000)
# sample(as.integer(rnorm(10000, 10, 3)), 300)
#
# data("LGL23")
#
# dds <- DESeq2::DESeqDataSetFromMatrix(
#     countData = cmat[rowSums(cmat) > 0, ],
#     colData = sample_info,
#     design = ~ group
# )
#
# dds <- DESeq(dds)
# dds$group
# DESeq2::resultsNames(dds)
#
# ncomb <- combn(levels(dds$group), 2)
#
# venny.data <- list()
#
# res <- DESeq2::results(dds, contrast = c("group", "WT_mid", "WT_mock"))
# res <- as.data.frame(res) %>%
#     dplyr::filter(padj < 0.05)
#
# up_reg <- res %>% dplyr::filter(log2FoldChange > 0)
# down_reg <- res %>% dplyr::filter(log2FoldChange < 0)
#
# data_venn4 <- list(
#     KO_up = rownames(up_reg),
#     KO_down = rownames(down_reg)
# )
#
# res <- DESeq2::results(dds, contrast = c("group", "OE_mock", "WT_mock"))
# res <- as.data.frame(res) %>%
#     dplyr::filter(padj < 0.05)
#
# up_reg <- res %>% dplyr::filter(log2FoldChange > 0)
# down_reg <- res %>% dplyr::filter(log2FoldChange < 0)
#
# data_venn4[["OE_up"]] <- rownames(up_reg)
# data_venn4[["OE_down"]] <- rownames(down_reg)
#
# str(data_venn4)
#
# generate_ellipse_4(
#     data = list(
#         KO_down = data_venn4$KO_down,
#         KO_up = data_venn4$KO_up,
#         OE_up = data_venn4$OE_up,
#         OE_down = data_venn4$OE_down
#     )
# )
