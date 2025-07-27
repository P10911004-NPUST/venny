#' Generate Ellipse Path
#'
#' @param x0 (default: 0) The coordinate x of the ellipse center point.
#' @param y0 (default: 0) The coordinate y of the ellipse center point.
#' @param a (default: 2) Long arm.
#' @param b (default: 1) Short arm.
#' @param angle (default: 0) Rotation angle (degree).
#' @param density (default: 300) Higher value generate smoother ellipse.
#' @param group (default: "e1") Name for this ellipse.
#'
#' @returns A data.frame with two columns, x and y.
#' The coordinates of the ellipse points to construct the ellipse.
#'
#' @import ggplot2 sf
#' @export
#'
#' @examples
#' library(ggplot2)
#' e1 <- generate_ellipse_1(x0 = 1, angle = 20, group = "e1")
#' e2 <- generate_ellipse_1(group = "e2")
#' p1 <- rbind(e1, e2)
#' ggplot(p1, aes(x, y, color = group)) +
#'     geom_polygon(fill = NA) +
#'     coord_fixed()
generate_ellipse_1 <- function(
        x0 = 0,  # center point x
        y0 = 0,  # center point y
        a = 2,  # long-arm
        b = 1,  # short-arm
        angle = 0,
        density = 300,
        set_label = "e1"
) {
    radian <- (angle %% 360) * pi / 180
    c <- sqrt(a ^ 2 - b ^ 2)  # distance from the center to a focus
    e <- 1 - sqrt( (b ^ 2) / (a ^ 2) )  # eccentricity
    theta <- c(seq(0, 2 * pi, length.out = density), 0)

    # (x, y) = (a*cos(t), b*sin(t))
    x1 <- a * cos(theta) * cos(radian) - b * sin(theta) * sin(radian) + x0
    y1 <- b * sin(theta) * cos(radian) + a * cos(theta) * sin(radian) + y0

    df0 <- data.frame(set_label = set_label, x = x1, y = y1)
    return(df0)
}


generate_ellipse_4 <- function(
        data = list(),

        x0 = c(-1.1, 0, 0, 1.1),
        y0 = c(-.6, 0, 0, -.6),
        a = c(2.5, 2.5, 2.5, 2.5),
        b = c(1.35, 1.2, 1.2, 1.35),
        angle = c(-45, -50, 50, 45),
        density = c(100, 100, 100, 100),

        ellipse_line_color = rep("transparent", 4),
        ellipse_line_width = c(),
        ellipse_fill_color = c("#CC79A7", "#009E73", "#E69F00", "#56B4E9"),
        ellipse_fill_alpha = 0.15,

        # Total is 4 sets
        set_label = c("Set 1", "Set 2", "Set 3", "Set 4"),
        set_label_position = list(x = c(-2.5, -1.2, 1.2, 2.5),
                                  y = c(1.6, 2.3, 2.3, 1.6)),
        set_label_family = rep("sans", times = 4),
        set_label_size = rep(5, times = 4),
        set_label_face = rep("bold", times = 4),
        set_label_color = c("#CC79A7", "#009E73", "#E69F00", "#56B4E9"),
        set_label_angle = c(0, 0, 0, 0),

        # Total is 15 zone labels
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

        zone_count = TRUE,
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

        zone_percentage = TRUE,
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
    venny_df <- venny_init(data)
    venny_df <- venny_df[match(venny_df[["zone_label"]], zone_label), ]

    lst0 <- vector("list", 4)
    for (i in 1:4)
    {
        tmp_df <- generate_ellipse_1(  # from ./utils.R
            x0[i], y0[i], a[i], b[i],
            angle[i], density[i], set_label[i]
        )
        lst0[[i]] <- as.list(tmp_df)
    }
    names(lst0) <- set_label

    ellipse_path <- do.call(rbind.data.frame, lst0)

    # This is just for silencing the "undefined global variable x, y" warnings,
    # which may be caused by the NSE style of `ggplot2`
    if (TRUE) {
        x <- NULL
        y <- NULL
    }


    p0 <- ggplot(ellipse_path, aes(x, y, color = set_label, fill = set_label)) +
        theme_void() +
        coord_fixed() +
        geom_polygon(alpha = ellipse_fill_alpha) +
        scale_color_manual(values = ellipse_line_color) +
        scale_fill_manual(values = ellipse_fill_color) +
        theme(
            axis.title = element_blank(),
            legend.position = "none"
        )

    # Set label
    if (is.character(set_label))
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
    if (is.character(zone_label))
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
    if (isTRUE(zone_count))
    {
        p0 <- p0 +
            ggplot2::annotate(
                geom = "text",
                x = zone_count_position[["x"]] + zone_count_hjust,
                y = zone_count_position[["y"]] + zone_count_vjust,
                label = venny_df[["length"]],
                family = zone_count_family,
                fontface = zone_count_face,
                size = zone_count_size,
                color = zone_count_color,
                angle = zone_count_angle
            )
    }

    # Zone percentage value
    if (isTRUE(zone_percentage))
    {
        percentage <- round(venny_df[["percentage"]], zone_percentage_digits)
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

    ret <- list(
        ellipse_path = ellipse_path,
        data = venny_df,
        plot = p0
    )
    return(ret)
}
