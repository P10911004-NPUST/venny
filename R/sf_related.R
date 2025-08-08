as_polygon <- function(mat = matrix())
{
    if (inherits(mat, "XY")) return(mat)
    if (is.list(mat)) return(lapply(mat, as_polygon))
    if ( ! is.atomic(mat) & length(dim(mat)) != 2 ) return(NULL)
    if (ncol(mat) != 2 | length(mat) == 0) return(NULL)
    if ( ! is.numeric(mat) ) return(NULL)
    if (is.data.frame(mat)) mat <- as.matrix(mat)

    return(sf::st_polygon(list(mat)))
}


#' @title Polygon manipulation
#'
#' @description
#' Perform union, intersection, and difference between two polygons.
#'
#' @param poly1 The polygon acts as the subject. The input should be a matrix.
#' The first column is the x coordinates, while
#' the second column is the y coordinates.
#' @param poly2 The polygon which performs the manipulation.
#' The first column is the x coordinates, while
#' the second column is the y coordinates.
#'
#' @returns A matrix.
#' @export
#'
#' @examples
#' library(ggplot2)
#' s <- 1:1000
#' venn_list <- list(
#'     Set_A = sample(s, 500),
#'     Set_B = sample(s, 800),
#'     Set_C = sample(s, 700),
#'     Set_D = sample(s, 600)
#' )
#' venn <- generate_ellipse_4(venn_list, ellipse_fill_alpha = 0.1)
#' e4 <- venn$ellipse_path
#' poly <- polygon_union(e4$`Set_A`, e4$`Set_C`) %>%
#'     polygon_intersect(e4$`Set_B`) %>%
#'     polygon_setdiff(e4$`Set_D`)
#'
#' venn$plot +
#'     geom_polygon(
#'         inherit.aes = FALSE,
#'         data = poly,
#'         mapping = aes(x, y),
#'         fill = "red",
#'         alpha = 0.4,
#'         color = "maroon",
#'         linewidth = 1
#'     )
polygon_union <- function(poly1, poly2)
{
    poly1 <- as_polygon(poly1)
    poly2 <- as_polygon(poly2)

    if (is.null(poly1) & is.null(poly2)) return(NULL)
    if (is.null(poly1)) return(poly2)
    if (is.null(poly2)) return(poly1)

    lst <- sf::st_union(poly1, poly2)
    # If it is a `MULTIPOLYGON` class
    if (length(lst) > 1)
        lst <- do.call(rbind, lst)

    mat <- lst[[1]]
    colnames(mat) <- c("x", "y")
    return(mat)
}

#' @rdname polygon_union
polygon_intersect <- function(poly1, poly2)
{
    poly1 <- as_polygon(poly1)
    poly2 <- as_polygon(poly2)

    if (is.null(poly1) & is.null(poly2)) return(NULL)
    if (is.null(poly1)) return(poly2)
    if (is.null(poly2)) return(poly1)

    lst <- sf::st_intersection(poly1, poly2)
    # If it is a `MULTIPOLYGON` class
    if (length(lst) > 1)
        lst <- do.call(rbind, lst)

    mat <- lst[[1]]
    colnames(mat) <- c("x", "y")
    return(mat)
}

#' @rdname polygon_union
polygon_setdiff <- function(poly1, poly2)
{
    poly1 <- as_polygon(poly1)
    poly2 <- as_polygon(poly2)

    if (is.null(poly1) & is.null(poly2)) return(NULL)
    if (is.null(poly1)) return(poly2)
    if (is.null(poly2)) return(poly1)

    lst <- sf::st_difference(poly1, poly2)
    # If it is a `MULTIPOLYGON` class
    if (length(lst) > 1)
        lst <- do.call(rbind, lst)

    mat <- lst[[1]]
    colnames(mat) <- c("x", "y")
    return(mat)
}




