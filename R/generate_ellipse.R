#' Generate Ellipse Path
#'
#' @param x0 (default: 0) The coordinate x of the polygon center point.
#' @param y0 (default: 0) The coordinate y of the polygon center point.
#' @param a (default: 2) Long arm.
#' @param b (default: 1) Short arm.
#' @param angle (default: 0) Rotation angle (degree).
#' @param density (default: 100)
#'      Generate how many points? Greater amount of points yield smoother ellipse.
#'
#' @returns A data.frame with the point coordinates (x, y) to construct the polygon
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' library(ggplot2)
#' # Draw a circle
#' circle <- generate_ellipse_path(a = 1, b = 1, density = 100)
#' ggplot(circle, aes(x, y)) +
#'     geom_polygon() +
#'     coord_fixed()
#' # Draw an ellipse
#' ellipse <- generate_ellipse_path(a = 2, b = 1, angle = 45)
#' ggplot(ellipse, aes(x, y)) +
#'     geom_polygon() +
#'     coord_fixed()
generate_ellipse_path <- function(
        x0 = 0,  # center point x
        y0 = 0,  # center point y
        a = 2,  # long-arm
        b = 1,  # short-arm
        angle = 0,
        density = 100
) {
    density <- density + 1
    radian <- (angle %% 360) * pi / 180
    # c <- sqrt(a ^ 2 - b ^ 2)  # distance from the center to a focus
    # e <- 1 - sqrt( (b ^ 2) / (a ^ 2) )  # eccentricity
    theta <- c(seq(0, 2 * pi, length.out = density), 0)

    # (x, y) = (a*cos(theta), b*sin(theta))
    x1 <- a * cos(theta) * cos(radian) - b * sin(theta) * sin(radian) + x0
    y1 <- b * sin(theta) * cos(radian) + a * cos(theta) * sin(radian) + y0

    mat0 <- as.matrix(data.frame(x = x1, y = y1))
    return(mat0)
}


#' @title
#' Plot a 4 sets venn diagram
#'
#' @description
#' Generate a list which contains:
#' 1. Venn plot;
#' 2. Polygon path of each ellipses;
#' 3. a summary table
#' 4. Elements for each sets
#' 5. Elements for each subsets
#'
#' @param data A named list with at least length of 2.
#' @param x0 The x coordinates for the center points of the ellipses (default: c(-1.1, 0, 0, 1.1)).
#' @param y0 The y coordinates for the center points of the ellipses (default: c(-.6, 0, 0, -.6)).
#' @param a The length of the long arm of the ellipses (default: c(2.5, 2.5, 2.5, 2.5)).
#' @param b The length of the short arm of the ellipses (default: c(1.35, 1.2, 1.2, 1.35)).
#' @param ellipse_angle Rotation angle (degree) of the ellipses (default: c(-45, -50, 50, 45)).
#' @param ellipse_density Smoothness of the ellipses. Higher value yield smoother shape, but compute slower (default: 100).
#' @param ellipse_line_color (default: rep("transparent", 4))
#' @param ellipse_line_width (default: 0)
#' @param ellipse_fill_color (default: c("#CC79A7", "#009E73", "#E69F00", "#56B4E9"))
#' @param ellipse_fill_alpha (default: 0.15)
#' @param show_set_label Logical. Show the set names? (default: TRUE)
#' @param set_label_position (default: list(x = c(-2.5, -1.3, 1.3, 2.5), y = c(-1.5, 2.4, 2.4, -1.5)))
#' @param set_label_family (default: "sans")
#' @param set_label_size (default: 5)
#' @param set_label_face (default: "bold")
#' @param set_label_color (default: c("#CC79A7", "#009E73", "#E69F00", "#56B4E9"))
#' @param set_label_angle (default: c(-45, 0, 0, 45))
#' @param show_zone_label Logical. Show the zone names? (default: TRUE)
#' @param zone_label A character vector with length of 15. The zone names.
#' @param zone_label_position A list contains two numeric vectors with length of 15, each for x and y coordinates. Similar with `set_label_position`.
#' @param zone_label_family (default: "sans")
#' @param zone_label_size (default: 3)
#' @param zone_label_face (default: "bold.italic")
#' @param zone_label_color (default: "black")
#' @param zone_label_angle (default: 0)
#' @param show_zone_count Logical. Show the number of elements for each zone? (default: TRUE)
#' @param zone_count_position Similar with `zone_label_position`.
#' @param zone_count_hjust Horizontally adjust the zone_count label position.
#' @param zone_count_vjust Vertically adjust the zone_count label position.
#' @param zone_count_family (default: "sans")
#' @param zone_count_size (default: 4)
#' @param zone_count_face (default: "plain")
#' @param zone_count_color (default: "grey30")
#' @param zone_count_angle (default: 0)
#' @param show_zone_percentage Logical. Show the percentage of `zone_count` for each zone? (default: TRUE).
#' @param zone_percentage_digits Rounding for the percentage (default: 2).
#' @param zone_percentage_position Similar with `zone_count_position`.
#' @param zone_percentage_hjust Similar with `zone_count_hjust`.
#' @param zone_percentage_vjust Similar with `zone_count_vjust`.
#' @param zone_percentage_family (default: "sans")
#' @param zone_percentage_size (default: 3.5)
#' @param zone_percentage_face (default: "plain")
#' @param zone_percentage_color (default: "grey50")
#' @param zone_percentage_angle (default: 0)
#'
#' @returns
#' A list which contains:
#' 1. plot: A venn diagram;
#' 2. ellipse_path: Polygon path of each ellipses;
#' 3. venny_df: A summary table;
#' 4. set_elements: Elements for each sets;
#' 5. zone_elements: Elements for each subsets.
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' s <- 1:1000
#' venn_list <- list(
#'     Set_A = sample(s, 500),
#'     Set_B = sample(s, 800),
#'     Set_C = sample(s, 700),
#'     Set_D = sample(s, 600)
#' )
#' out <- generate_ellipse_4(venn_list)
#' out$plot
generate_ellipse_4 <- function(
        data = list(
            "Set A" = c(1, 2, 3, 4, 5, 7, 9),
            "Set B" = c(3, 5, 7, 9, 12, 15),
            "Set C" = c(1, 4, 5, 7, 9, 10, 13),
            "Set D" = c(3, 4, 6, 10, 12, 14, 15)
        ),
        x0 = c(-1.1, 0, 0, 1.1),
        y0 = c(-.6, 0, 0, -.6),
        a = c(2.5, 2.5, 2.5, 2.5),
        b = c(1.35, 1.2, 1.2, 1.35),
        ellipse_angle = c(-45, -50, 50, 45),
        ellipse_density = c(100, 100, 100, 100),
        ellipse_line_color = rep("transparent", 4),
        ellipse_line_width = 0,
        ellipse_fill_color = c("#CC79A7", "#009E73", "#E69F00", "#56B4E9"),
        ellipse_fill_alpha = 0.15,
        ## Set label, 4 sets in total ====
        show_set_label = TRUE,
        set_label_position = list(x = c(-2.5, -1.3, 1.3, 2.5),
                                  y = c(-1.5, 2.4, 2.4, -1.5)),
        set_label_family = rep("sans", times = 4),
        set_label_size = rep(5, times = 4),
        set_label_face = rep("bold", times = 4),
        set_label_color = c("#CC79A7", "#009E73", "#E69F00", "#56B4E9"),
        set_label_angle = c(-45, 0, 0, 45),
        ## Zone label, 15 zone labels in total ====
        show_zone_label = TRUE,
        zone_label = c("A", "B", "C", "D",
                       "AB", "BC", "CD",
                       "AC", "AD", "BD",
                       "ABC", "BCD", "ACD", "ABD",
                       "ABCD"),
        zone_label_position = list(
            x = c("A" = -2.4, "B" = -1.3, "C" = 1.3, "D" = 2.4,
                  "AB" = -1.6, "BC" = 0, "CD" = 1.6,
                  "AC" = -1.4, "AD" = 0, "BD" = 1.4,
                  "ABC" = -0.7, "BCD" = 0.7, "ACD" = -0.7, "ABD" = 0.7,
                  "ABCD" = 0),
            y = c("A" = 1.15, "B" = 1.9, "C" = 1.9, "D" = 1.15,
                  "AB" = 1.15, "BC" = 1.4, "CD" = 1.15,
                  "AC" = -0.5, "AD" = -1.8, "BD" = -0.5,
                  "ABC" = 0.5, "BCD" = 0.5, "ACD" = -1.1, "ABD" = -1.1,
                  "ABCD" = -0.3)
        ),
        zone_label_family = rep("sans", times = 15),
        zone_label_size = rep(3, times = 15),
        zone_label_face = rep("bold.italic", times = 15),
        zone_label_color = rep("black", times = 15),
        zone_label_angle = rep(0, times = 15),
        ## Zone count ====
        show_zone_count = TRUE,
        zone_count_position = list(
            x = c("A" = -2.4, "B" = -0.9, "C" = 0.9, "D" = 2.4,
                  "AB" = -1.5, "BC" = 0, "CD" = 1.5,
                  "AC" = -1.4, "AD" = 0, "BD" = 1.4,
                  "ABC" = -0.7, "BCD" = 0.7, "ACD" = -0.7, "ABD" = 0.7,
                  "ABCD" = 0),
            y = c("A" = 0.5, "B" = 1.7, "C" = 1.7, "D" = 0.5,
                  "AB" = 0.8, "BC" = 1.1, "CD" = 0.8,
                  "AC" = -0.8, "AD" = -2.1, "BD" = -0.8,
                  "ABC" = 0.2, "BCD" = 0.2, "ACD" = -1.4, "ABD" = -1.4,
                  "ABCD" = -0.6)
        ),
        zone_count_hjust = 0,
        zone_count_vjust = 0,
        zone_count_family = rep("sans", times = 15),
        zone_count_size = rep(4, times = 15),
        zone_count_face = rep("plain", times = 15),
        zone_count_color = rep("grey30", times = 15),
        zone_count_angle = rep(0, times = 15),
        ## Zone percentage
        show_zone_percentage = TRUE,
        zone_percentage_digits = 2,
        zone_percentage_position = list(
            x = zone_count_position[["x"]],
            y = zone_count_position[["y"]] - 0.3
        ),
        zone_percentage_hjust = 0,
        zone_percentage_vjust = 0,
        zone_percentage_family = rep("sans", times = 15),
        zone_percentage_size = rep(3.5, times = 15),
        zone_percentage_face = rep("plain", times = 15),
        zone_percentage_color = rep("grey50", times = 15),
        zone_percentage_angle = rep(0, times = 15)
) {
    if (is.null(data) || length(data) < 2 || !is.list(data)) {
        warning("`data` should be a list with at least two sets.")
        return(NULL)
    }

    n_sets <- length(data)
    set_label <- names(data)

    if (is.null(set_label)) {
        show_set_label <- FALSE
        set_label <- LETTERS[ 1 : n_sets ]
    }

    venny_df_ <- venny_df(data)
    zone_order_default <- c("A", "B", "C", "D", "AB", "BC", "CD", "AC", "AD", "BD", "ABC", "BCD", "ACD", "ABD", "ABCD")
    venny_df_ <- venny_df_[match(rownames(venny_df_), zone_order_default), ]

    ellipse_path <- vector("list", length(data))
    for (i in seq_along(data))
    {
        ellipse_path[[i]] <- generate_ellipse_path(
            x0[i], y0[i], a[i], b[i],
            ellipse_angle[i], ellipse_density[i]
        )
    }
    names(ellipse_path) <- set_label

    plot_df <- data.frame()
    for (i in seq_along(ellipse_path))
    {
        tmp_df <- as.data.frame(ellipse_path[[i]])
        tmp_df["set_label"] <- names(ellipse_path[i])
        plot_df <- rbind(plot_df, tmp_df)
    }

    # Assign `x` and `y` as `NULL` is just for avoiding the
    # "undefined global variable x, y" message,
    # which may be caused by the NSE style of `ggplot2`.
    x <- NULL
    y <- NULL

    # Plot ellipses
    p0 <- ggplot2::ggplot(plot_df, ggplot2::aes(x, y, color = set_label, fill = set_label)) +
        ggplot2::theme_void() +
        ggplot2::coord_fixed() +
        ggplot2::geom_polygon(alpha = ellipse_fill_alpha) +
        ggplot2::scale_color_manual(values = ellipse_line_color) +
        ggplot2::scale_fill_manual(values = ellipse_fill_color) +
        ggplot2::theme(
            axis.title = ggplot2::element_blank(),
            legend.position = "none"
        )

    # Set label
    if (isTRUE(show_set_label))
    {
        p0 <- p0 +
            ggplot2::annotate(
                geom = "text",
                x = set_label_position[["x"]],
                y = set_label_position[["y"]],
                label = set_label,
                family = set_label_family,
                fontface = set_label_face,
                size = set_label_size,
                color = set_label_color,
                angle = set_label_angle
            )
    }

    # Zone label
    if (isTRUE(show_zone_label))
    {
        p0 <- p0 +
            ggplot2::annotate(
                geom = "text",
                x = zone_label_position[["x"]],
                y = zone_label_position[["y"]],
                label = zone_label,
                family = zone_label_family,
                fontface = zone_label_face,
                size = zone_label_size,
                color = zone_label_color,
                angle = zone_label_angle
            )
    }

    # Zone count value
    if (isTRUE(show_zone_count))
    {
        p0 <- p0 +
            ggplot2::annotate(
                geom = "text",
                x = zone_count_position[["x"]] + zone_count_hjust,
                y = zone_count_position[["y"]] + zone_count_vjust,
                label = venny_df_[["n_elements"]],
                family = zone_count_family,
                fontface = zone_count_face,
                size = zone_count_size,
                color = zone_count_color,
                angle = zone_count_angle
            )
    }

    # Zone percentage value
    if (isTRUE(show_zone_percentage))
    {
        percentage <- round(venny_df_[["percentage"]], zone_percentage_digits)
        percentage <- paste0("(", percentage, "%)")
        p0 <- p0 +
            ggplot2::annotate(
                geom = "text",
                x = zone_percentage_position[["x"]] + zone_percentage_hjust,
                y = zone_percentage_position[["y"]] + zone_percentage_vjust,
                label = percentage,
                family = zone_percentage_family,
                fontface = zone_percentage_face,
                size = zone_percentage_size,
                color = zone_percentage_color,
                angle = zone_percentage_angle
            )
    }

    # Ellipses path
    ellipse_path <- vector("list", length(set_label))
    names(ellipse_path) <- set_label
    for (i in seq_along(set_label))
    {
        df0 <- plot_df[plot_df[["set_label"]] == set_label[i], , drop = FALSE]
        mat0 <- as.matrix(df0[c("x", "y")])
        rownames(mat0) <- NULL
        ellipse_path[[set_label[i]]] <- mat0
    }

    # Return as list
    ret <- list(
        plot = p0,
        ellipse_path = ellipse_path,
        venny_df = venny_df_,
        set_elements = attr(venny_df_, "set_elements"),
        zone_elements = attr(venny_df_, "zone_elements")
    )
    class(ret) <- c(class(ret), "venny", "ellipses")

    return(ret)

    # # Test
    # if (FALSE)
    # {
    #     s <- 1:1000
    #     venn_list <- list(
    #         Set_A = sample(s, 500),
    #         Set_B = sample(s, 800),
    #         Set_C = sample(s, 700),
    #         Set_D = sample(s, 600)
    #     )
    #     generate_ellipse_4(venn_list)
    # }
}

