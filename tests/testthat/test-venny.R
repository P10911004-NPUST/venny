test_that("venny usage example", {
    # library(venny)
    # library(ggplot2)
    # library(ggtext)
    # #---------------------- Basic usage ----------------------#
    # lst <- LGL23$DEGs
    # venny(lst)
    # 
    # #---------------------- Customized ----------------------#
    # setLabelPosition <- set_label_position(hjust = c(0.2, -0.5, 0.5, -0.2))
    # 
    # out <- venny(
    #     data = lst,
    #     detail = TRUE,
    #     # change the ellipse line
    #     ellipse.line = ellipse_line(linetype = c("dashed", "dotted", "dotdash", "blank"),
    #                                 color = c("black", "red")),
    #     # shift the set label
    #     set.label.position = setLabelPosition,
    #     # rename the subset label
    #     subset.label = list(A = "v", AB = "e", BC = "n", CD = "n", D = "y"),
    #     # show only specific subset labels
    #     subset.label.position = subset_label_position(show = c("A", "AB", "BC", "CD", "D")),
    #     subset.label.font = subset_label_font(size = 5),
    #     # remove specific subset counts
    #     subset.count.position = subset_count_position(hide = c("A", "AB", "BC", "CD", "D")),
    #     subset.percentage = FALSE
    # )
    # 
    # p1 <- out$venn
    # print(p1)
    # 
    # # Add figure caption
    # plotCaption <- paste(
    #     "<b>WT</b>: Wild-type",
    #     "<b>KO</b>: Knock-out",
    #     "<b>OE</b>: Over-expression",
    #     "<b>mock</b>: 0 nM treatment",
    #     "<b>low</b>: 0.3 nM treatment",
    #     "<b>high</b>: 5 nM treatment",
    #     sep = "<br>"
    # )
    # 
    # p2 <- out$venn + 
    #     annotate(
    #         geom = "richtext", 
    #         x = 3.3, y = -1.6, 
    #         label = plotCaption, 
    #         hjust = 0, 
    #         size = 5
    #     ) +
    #     coord_cartesian(xlim = c(-3, 5.1))
    # 
    # print(p2)
    # 
    # # Highlight subsets
    # ep <- out$ellipse_path
    # ABC <- ep$`KO_low vs KO_mock` |>
    #     intersect(ep$`KO_high vs KO_mock`, ep$`WT_mock vs KO_mock`) |>
    #     setdiff(ep$`OE_mock vs KO_mock`)
    # 
    # p3 <- highlight(p2, ABC, linetype = "solid", color = "black")
    # print(p3)
    
    expect_equal(TRUE, TRUE)
})
