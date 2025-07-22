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
#' e1 <- generate_ellipse_path(x0 = 1, angle = 20, group = "e1")
#' e2 <- generate_ellipse_path(group = "e2")
#' p1 <- rbind(e1, e2)
#' ggplot(p1, aes(x, y, color = group)) +
#'     geom_polygon(fill = NA) +
#'     coord_fixed()
generate_ellipse_path <- function(
        x0 = 0,  # center point x
        y0 = 0,  # center point y
        a = 2,  # long-arm
        b = 1,  # short-arm
        angle = 0,
        density = 300,
        group = "e1"
) {
    radian <- (angle %% 360) * pi / 180
    c <- sqrt(a ^ 2 - b ^ 2)  # distance from the center to a focus
    e <- 1 - sqrt( (b ^ 2) / (a ^ 2) )  # eccentricity
    theta <- c(seq(0, 2 * pi, length.out = density), 0)

    # (x, y) = (a*cos(t), b*sin(t))
    x1 <- a * cos(theta) * cos(radian) - b * sin(theta) * sin(radian) + x0
    y1 <- b * sin(theta) * cos(radian) + a * cos(theta) * sin(radian) + y0

    df0 <- data.frame(group = group, x = x1, y = y1)
    return(df0)
}


generate_ellipse_4 <- function(
        x0 = c(-0.75, 0, 0, 0.75),
        y0 = c(-0.5, -0.15, -0.15, -0.5),
        a = c(2, 2, 2, 2),
        b = c(1, 1, 1, 1),
        angle = c(-30, -30, 30, 30),
        density = c(300, 300, 300, 300),
        group = c("e1", "e2", "e3", "e4"),
        zone_label_pos = c(),
        zone_label_size = c(),
        zone_label_face = c(),
        zone_label_color = c()
) {
    lst0 <- vector("list", 4)
    for (i in 1:4)
        lst0[[i]] <- generate_ellipse_path(x0[i], y0[i], a[i], b[i], angle[i], density[i], group[i])
    names(lst0) <- group

    df0 <- do.call(rbind.data.frame, lst0)

    p0 <- ggplot(df0, aes(x, y, color = group)) +
        theme_light() +
        coord_fixed() +
        geom_polygon(fill = NA) +
        theme(
            axis.title = element_blank(),
            legend.position = "none"
        )

    return(p0)
}
