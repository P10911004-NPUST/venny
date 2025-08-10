#' @title Generate Ellipse Path
#'
#' @param x0 The coordinate x of the polygon center point (default: 0).
#' @param y0 The coordinate y of the polygon center point (default: 0).
#' @param a Long arm (default: 2).
#' @param b Short arm (default: 1).
#' @param angle Rotation angle in degree (default: 0).
#' @param density Greater amount of points yield smoother ellipse (default: 200).
#'
#' @returns A data.frame with the point coordinates (x, y) to construct the polygon
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' library(ggplot2)
#' # Draw a circle
#' circle <- generate_ellipse_path(a = 1, b = 1)
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
        density = 200
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

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
## 2 Ellipses ====
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#' @title Generate Venn Plot and Relevant
#'
#' @description
#' Generate a list which contains:
#' 1. Venn plot;
#' 2. Polygon path of each ellipses;
#' 3. A summary table
#' 4. Elements for each sets
#' 5. Elements for each subsets (zones)
#'
#' @param data A named list.
#' @param x0 The x coordinates for the center points of the ellipses.
#' @param y0 The y coordinates for the center points of the ellipses.
#' @param a The length of the long arm of the ellipses.
#' @param b The length of the short arm of the ellipses.
#' @param ellipse_angle Rotation angle in degree for the ellipses.
#' @param ellipse_density Number of points to generate the ellipses.
#' @param ellipse_line_color Ellipses boundary color.
#' @param ellipse_line_width Ellipses boundary width.
#' @param ellipse_fill_color Ellipses fill color.
#' @param ellipse_fill_alpha Ellipses transparency.
#' @param show_set_label Logical. Show the set names (default: TRUE).
#' @param set_label_position A list of length two with the xy coordinates (exp., list(x = 1:10, y = 1:10)).
#' @param set_label_family Font family for the set labels.
#' @param set_label_size Font size for the set labels.
#' @param set_label_face Font face for the set labels.
#' @param set_label_color Font color for the set labels.
#' @param set_label_angle Font rotation angle in degree for the set labels.
#' @param show_zone_label Logical. Show the zone names? (default: TRUE)
#' @param zone_label A character vector with length of 15. The zone names.
#' @param zone_label_position Similar with `set_label_position`.
#' @param zone_label_family Font family for the zone labels.
#' @param zone_label_size Font size for the zone labels.
#' @param zone_label_face Font face for the zone labels.
#' @param zone_label_color Font color for the zone labels.
#' @param zone_label_angle Font rotation angle in degree for the zone labels.
#' @param show_zone_count Logical. Show the number of elements for each zone (default: TRUE).
#' @param zone_count_position Similar with `zone_label_position`.
#' @param zone_count_hjust Horizontally adjust the zone count number position.
#' @param zone_count_vjust Vertically adjust the zone count number position.
#' @param zone_count_family Font family for the zone count number.
#' @param zone_count_size Font size for the zone count number.
#' @param zone_count_face Font face for the zone count number.
#' @param zone_count_color Font color for the zone count number.
#' @param zone_count_angle Font rotation angle in degree for the zone count number (default: 0).
#' @param show_zone_percentage Logical. Show the percentage of the elements count for each zone (default: TRUE).
#' @param zone_percentage_digits Rounding for the percentages (default: 2).
#' @param zone_percentage_position Similar with `zone_count_position`.
#' @param zone_percentage_hjust Similar with `zone_count_hjust`.
#' @param zone_percentage_vjust Similar with `zone_count_vjust`.
#' @param zone_percentage_family Font family for the zone percentage.
#' @param zone_percentage_size Font size for the zone percentage.
#' @param zone_percentage_face Font face for the zone percentage.
#' @param zone_percentage_color Font color for the zone percentage.
#' @param zone_percentage_angle Font rotation angle in degree for the zone percentage.
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
#' lst <- list(
#'     Set_A = sample(s, 500),
#'     Set_B = sample(s, 700),
#'     Set_C = sample(s, 900),
#'     Set_D = sample(s, 600)
#' )
#'
#' e2 <- generate_ellipse_2(lst[1:2])
#' e2$plot
#'
#' e3 <- generate_ellipse_3(lst[1:3])
#' e3$plot
#'
#' e4 <- generate_ellipse_4(lst[1:4])
#' e4$plot
generate_ellipse_2 <- function(
        data = list(
            "Set A" = c(1, 2, 3, 4, 5, 7, 9, 10),
            "Set B" = c(3, 5, 7, 9, 11, 12, 15)
        ),
        x0 = c(-0.45, 0.45),
        y0 = 0,
        a = 1,
        b = 1,
        ellipse_angle = 0,
        ellipse_density = 200,
        ellipse_line_color = "transparent",
        ellipse_line_width = 0,
        ellipse_fill_color = c("#D55E00", "#0072B2"),
        ellipse_fill_alpha = 0.2,
        show_set_label = TRUE,
        set_label_position = list(
            x = c(-0.5, 0.5),
            y = c(1.15, 1.15)
        ),
        set_label_family = "sans",
        set_label_size = 7,
        set_label_face = "bold",
        set_label_color = c("#D55E00", "#0072B2"),
        set_label_angle = 0,
        show_zone_label = TRUE,
        zone_label = c("A", "B", "AB"),
        zone_label_position = list(
            x = c("A" = -1, "B" = 1, "AB" = 0),
            y = c("A" = 0.3, "B" = 0.3, "AB" = 0.3)
        ),
        zone_label_family = "sans",
        zone_label_size = 6,
        zone_label_face = "bold.italic",
        zone_label_color = "black",
        zone_label_angle = 0,
        show_zone_count = TRUE,
        zone_count_position = list(
            x = zone_label_position[["x"]],
            y = zone_label_position[["y"]] - 0.3
        ),
        zone_count_hjust = 0,
        zone_count_vjust = 0,
        zone_count_family = "sans",
        zone_count_size = 6,
        zone_count_face = "plain",
        zone_count_color = "grey30",
        zone_count_angle = 0,
        show_zone_percentage = TRUE,
        zone_percentage_digits = 2,
        zone_percentage_position = list(
            x = zone_count_position[["x"]],
            y = zone_count_position[["y"]] - 0.2
        ),
        zone_percentage_hjust = 0,
        zone_percentage_vjust = 0,
        zone_percentage_family = "sans",
        zone_percentage_size = 5,
        zone_percentage_face = "plain",
        zone_percentage_color = "grey40",
        zone_percentage_angle = 0
) {
    if (is.null(data) || length(data) != 2 || !is.list(data)) {
        warning("`data` should be a list with 2 sets.")
        return(NULL)
    }

    n_sets <- length(data)
    set_label <- names(data)
    n_zones <- 3

    x0 <- fixed_length(x0, n_sets)  # from ./utils.R
    y0 <- fixed_length(y0, n_sets)
    a <- fixed_length(a, n_sets)
    b <- fixed_length(b, n_sets)
    ellipse_angle <- fixed_length(ellipse_angle, n_sets)
    ellipse_density <- fixed_length(ellipse_density, n_sets)
    ellipse_line_color <- fixed_length(ellipse_line_color, n_sets)
    ellipse_line_width <- fixed_length(ellipse_line_width, n_sets)
    ellipse_fill_color <- fixed_length(ellipse_fill_color, n_sets)
    ellipse_fill_alpha <- ellipse_fill_alpha[1]

    if (is.null(set_label)) {
        show_set_label <- FALSE
        set_label <- LETTERS[ 1 : n_sets ]
    }

    zone_label_default <- c("A", "B", "AB")
    if (is.null(zone_label))
        zone_label <- zone_label_default

    venny_df_ <- venny_df(data)
    venny_df_ <- venny_df_[match(rownames(venny_df_), zone_label_default), ]

    # Ellipse path
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
    # "undefined global variable x, y" message during `check()`,
    # which may be caused by the NSE style of `ggplot2`.
    x <- y <- NULL

    # Plot ellipses
    p0 <- ggplot2::ggplot(plot_df, ggplot2::aes(x, y, color = set_label, fill = set_label)) +
        ggplot2::theme_void() +
        # ggplot2::theme_linedraw() +
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
                family = fixed_length(set_label_family, n_sets),
                fontface = fixed_length(set_label_face, n_sets),
                size = fixed_length(set_label_size, n_sets),
                color = fixed_length(set_label_color, n_sets),
                angle = fixed_length(set_label_angle, n_sets)
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
                family = fixed_length(zone_label_family, n_zones),
                fontface = fixed_length(zone_label_face, n_zones),
                size = fixed_length(zone_label_size, n_zones),
                color = fixed_length(zone_label_color, n_zones),
                angle = fixed_length(zone_label_angle, n_zones)
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
                family = fixed_length(zone_count_family, n_zones),
                fontface = fixed_length(zone_count_face, n_zones),
                size = fixed_length(zone_count_size, n_zones),
                color = fixed_length(zone_count_color, n_zones),
                angle = fixed_length(zone_count_angle, n_zones)
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
                family = fixed_length(zone_percentage_family, n_zones),
                fontface = fixed_length(zone_percentage_face, n_zones),
                size = fixed_length(zone_percentage_size, n_zones),
                color = fixed_length(zone_percentage_color, n_zones),
                angle = fixed_length(zone_percentage_angle, n_zones)
            )
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
}

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
## 3 Ellipses ====
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#' @rdname generate_ellipse_2
#' @export
generate_ellipse_3 <- function(
        data = list(
            "Set A" = c(1, 2, 3, 4, 5, 7, 9, 10),
            "Set B" = c(3, 5, 7, 9, 11, 12, 15),
            "Set C" = c(1, 4, 5, 7, 9, 10, 13)
        ),
        x0 = c(0, -0.45, 0.45),
        y0 = c(0.45, -0.45, -0.45),
        a = 1,
        b = 1,
        ellipse_angle = 0,
        ellipse_density = 200,
        ellipse_line_color = "transparent",
        ellipse_line_width = 0,
        ellipse_fill_color = c("#CC79A7", "#E69F00", "#56B4E9"),
        ellipse_fill_alpha = 0.2,
        show_set_label = TRUE,
        set_label_position = list(
            x = c(0, -1.4, 1.4),
            y = c(1.65, 0.3, 0.3)
        ),
        set_label_family = "sans",
        set_label_size = 5,
        set_label_face = "bold",
        set_label_color = c("#CC79A7", "#E69F00", "#56B4E9"),
        set_label_angle = c(0, 45, -45),
        show_zone_label = TRUE,
        zone_label = c("A", "B", "C", "AB", "AC", "BC", "ABC"),
        zone_label_position = list(
            x = c("A" = 0, "B" = -1.2, "C" = 1.2, "AB" = -.6, "AC" = .6, "BC" = 0, "ABC" = 0),
            y = c("A" = 1.3, "B" = -.2, "C" = -.2, "AB" = .4, "AC" = .4, "BC" = -.65, "ABC" = .2)
        ),
        zone_label_family = "sans",
        zone_label_size = 3,
        zone_label_face = "bold.italic",
        zone_label_color = "black",
        zone_label_angle = 0,
        show_zone_count = TRUE,
        zone_count_position = list(
            x = c("A" = 0, "B" = -.9, "C" = .9, "AB" = -.65, "AC" = .65, "BC" = 0, "ABC" = 0),
            y = c("A" = 1, "B" = -.5, "C" = -.5, "AB" = .15, "AC" = .15, "BC" = -.9, "ABC" = -.1)
        ),
        zone_count_hjust = 0,
        zone_count_vjust = 0,
        zone_count_family = "sans",
        zone_count_size = 4,
        zone_count_face = "plain",
        zone_count_color = "grey30",
        zone_count_angle = 0,
        show_zone_percentage = TRUE,
        zone_percentage_digits = 2,
        zone_percentage_position = list(
            x = zone_count_position[["x"]],
            y = zone_count_position[["y"]] - 0.2
        ),
        zone_percentage_hjust = 0,
        zone_percentage_vjust = 0,
        zone_percentage_family = "sans",
        zone_percentage_size = 3.5,
        zone_percentage_face = "plain",
        zone_percentage_color = "grey40",
        zone_percentage_angle = 0
) {
    if (is.null(data) || length(data) != 3 || !is.list(data)) {
        warning("`data` should be a list with 3 sets.")
        return(NULL)
    }

    n_sets <- length(data)
    set_label <- names(data)
    n_zones <- 7

    x0 <- fixed_length(x0, n_sets)  # from ./utils.R
    y0 <- fixed_length(y0, n_sets)
    a <- fixed_length(a, n_sets)
    b <- fixed_length(b, n_sets)
    ellipse_angle <- fixed_length(ellipse_angle, n_sets)
    ellipse_density <- fixed_length(ellipse_density, n_sets)
    ellipse_line_color <- fixed_length(ellipse_line_color, n_sets)
    ellipse_line_width <- fixed_length(ellipse_line_width, n_sets)
    ellipse_fill_color <- fixed_length(ellipse_fill_color, n_sets)
    ellipse_fill_alpha <- ellipse_fill_alpha[1]

    if (is.null(set_label)) {
        show_set_label <- FALSE
        set_label <- LETTERS[ 1 : n_sets ]
    }

    zone_label_default <- c("A", "B", "C", "AB", "AC", "BC", "ABC")
    if (is.null(zone_label))
        zone_label <- zone_label_default

    venny_df_ <- venny_df(data)
    venny_df_ <- venny_df_[match(rownames(venny_df_), zone_label_default), ]

    # Ellipse path
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
    # "undefined global variable x, y" message during `check()`,
    # which may be caused by the NSE style of `ggplot2`.
    x <- y <- NULL

    # Plot ellipses
    p0 <- ggplot2::ggplot(plot_df, ggplot2::aes(x, y, color = set_label, fill = set_label)) +
        ggplot2::theme_void() +
        # ggplot2::theme_linedraw() +
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
                family = fixed_length(set_label_family, n_sets),
                fontface = fixed_length(set_label_face, n_sets),
                size = fixed_length(set_label_size, n_sets),
                color = fixed_length(set_label_color, n_sets),
                angle = fixed_length(set_label_angle, n_sets)
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
                family = fixed_length(zone_label_family, n_zones),
                fontface = fixed_length(zone_label_face, n_zones),
                size = fixed_length(zone_label_size, n_zones),
                color = fixed_length(zone_label_color, n_zones),
                angle = fixed_length(zone_label_angle, n_zones)
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
                family = fixed_length(zone_count_family, n_zones),
                fontface = fixed_length(zone_count_face, n_zones),
                size = fixed_length(zone_count_size, n_zones),
                color = fixed_length(zone_count_color, n_zones),
                angle = fixed_length(zone_count_angle, n_zones)
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
                family = fixed_length(zone_percentage_family, n_zones),
                fontface = fixed_length(zone_percentage_face, n_zones),
                size = fixed_length(zone_percentage_size, n_zones),
                color = fixed_length(zone_percentage_color, n_zones),
                angle = fixed_length(zone_percentage_angle, n_zones)
            )
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
}


##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
## 4 Ellipses ====
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#' @rdname generate_ellipse_2
#' @export
generate_ellipse_4 <- function(
        data = list(
            "Set A" = c(1, 2, 3, 4, 5, 7, 9),
            "Set B" = c(3, 5, 7, 9, 12, 15),
            "Set C" = c(1, 4, 5, 7, 9, 10, 13),
            "Set D" = c(3, 4, 6, 10, 12, 14, 15)
        ),
        x0 = c(-1.1, 0, 0, 1.1),
        y0 = c(-.569, 0, 0, -.569),
        a = c(2.5, 2.5, 2.5, 2.5),
        b = c(1.35, 1.2, 1.2, 1.35),
        ellipse_angle = c(-45, -50, 50, 45),
        ellipse_density = c(200, 200, 200, 200),
        ellipse_line_color = rep("transparent", 4),
        ellipse_line_width = 0,
        ellipse_fill_color = c("#CC79A7", "#009E73", "#E69F00", "#56B4E9"),
        ellipse_fill_alpha = 0.2,
        show_set_label = TRUE,
        set_label_position = list(x = c(-2.5, -1.3, 1.3, 2.5),
                                  y = c(-1.5, 2.4, 2.4, -1.5)),
        set_label_family = rep("sans", times = 4),
        set_label_size = rep(5, times = 4),
        set_label_face = rep("bold", times = 4),
        set_label_color = c("#CC79A7", "#009E73", "#E69F00", "#56B4E9"),
        set_label_angle = c(-45, 0, 0, 45),
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
        zone_label_family = "sans",
        zone_label_size = 3,
        zone_label_face = "bold.italic",
        zone_label_color = "black",
        zone_label_angle = 0,
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
        zone_count_family = "sans",
        zone_count_size = 4,
        zone_count_face = "plain",
        zone_count_color = "grey30",
        zone_count_angle = 0,
        show_zone_percentage = TRUE,
        zone_percentage_digits = 2,
        zone_percentage_position = list(
            x = zone_count_position[["x"]],
            y = zone_count_position[["y"]] - 0.25
        ),
        zone_percentage_hjust = 0,
        zone_percentage_vjust = 0,
        zone_percentage_family = "sans",
        zone_percentage_size = 3.5,
        zone_percentage_face = "plain",
        zone_percentage_color = "grey40",
        zone_percentage_angle = 0
) {
    if (is.null(data) || length(data) != 4 || !is.list(data)) {
        warning("`data` should be a list with 4 sets.")
        return(NULL)
    }

    n_sets <- length(data)
    set_label <- names(data)
    n_zones <- 15

    x0 <- fixed_length(x0, n_sets)  # from ./utils.R
    y0 <- fixed_length(y0, n_sets)
    a <- fixed_length(a, n_sets)
    b <- fixed_length(b, n_sets)
    ellipse_angle <- fixed_length(ellipse_angle, n_sets)
    ellipse_density <- fixed_length(ellipse_density, n_sets)
    ellipse_line_color <- fixed_length(ellipse_line_color, n_sets)
    ellipse_line_width <- fixed_length(ellipse_line_width, n_sets)
    ellipse_fill_color <- fixed_length(ellipse_fill_color, n_sets)
    ellipse_fill_alpha <- ellipse_fill_alpha[1]

    if (is.null(set_label)) {
        show_set_label <- FALSE
        set_label <- LETTERS[ 1 : n_sets ]
    }

    zone_label_default <- c(
        "A", "B", "C", "D",
        "AB", "BC", "CD", "AC", "AD", "BD",
        "ABC", "BCD", "ACD", "ABD", "ABCD"
    )

    if (is.null(zone_label))
        zone_label <- zone_label_default

    venny_df_ <- venny_df(data)
    venny_df_ <- venny_df_[match(rownames(venny_df_), zone_label_default), ]

    # Ellipse path
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
    # "undefined global variable x, y" message during `check()`,
    # which may be caused by the NSE style of `ggplot2`.
    x <- y <- NULL

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
                family = fixed_length(set_label_family, n_sets),
                fontface = fixed_length(set_label_face, n_sets),
                size = fixed_length(set_label_size, n_sets),
                color = fixed_length(set_label_color, n_sets),
                angle = fixed_length(set_label_angle, n_sets)
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
                family = fixed_length(zone_label_family, n_zones),
                fontface = fixed_length(zone_label_face, n_zones),
                size = fixed_length(zone_label_size, n_zones),
                color = fixed_length(zone_label_color, n_zones),
                angle = fixed_length(zone_label_angle, n_zones)
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
                family = fixed_length(zone_count_family, n_zones),
                fontface = fixed_length(zone_count_face, n_zones),
                size = fixed_length(zone_count_size, n_zones),
                color = fixed_length(zone_count_color, n_zones),
                angle = fixed_length(zone_count_angle, n_zones)
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
                family = fixed_length(zone_percentage_family, n_zones),
                fontface = fixed_length(zone_percentage_face, n_zones),
                size = fixed_length(zone_percentage_size, n_zones),
                color = fixed_length(zone_percentage_color, n_zones),
                angle = fixed_length(zone_percentage_angle, n_zones)
            )
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
}

