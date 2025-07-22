union_n <- function(lst) unique(unlist(lst))

intersect_n <- function(lst)
{
    vct <- c()
    for (i in seq_along(lst)) {
        if (i == 1) vct <- lst[[i]]
        vct <- intersect(vct, lst[[i]])
    }
    return(vct)
}

bits_encoding <- function(set_names)
{
    n <- length(set_names)  # How many sets?
    comb_n <- (2 ^ n) - 1  # How many combinations?

    # Encode each combinations
    bits_mat <- vapply(
        1:comb_n,
        function(`_`) {
            as.integer(intToBits(`_`)[1:n])
        },
        integer(n)
    )
    bits_mat <- t(bits_mat)
    colnames(bits_mat) <- set_names
    rownames(bits_mat) <- apply(
        X = (bits_mat == 1),
        MARGIN = 1,
        FUN = function(`_`) paste(set_names[`_`], collapse = "")
    )

    return(bits_mat)
}

#' Initialize an `venny` object
#'
#' @param sets A list contains 2 ~ 26 sets of elements.
#' For example, `list(set1 = 1:5, set2 = 3:7)`.
#'
#' @returns A data frame contains necessary information for subsequent inferences and plotting.
#'
#' @import ggplot2 sf
#' @export
#'
#' @examples
#' lst_4sets <- list(
#' set_1 = letters[1:26],
#' set_2 = letters[3:17],
#' set_3 = letters[11:24],
#' set_4 = letters[1:20]
#' )
#' df0 <- venny_init(lst_4sets)
#' head(df0)
#' #>    A B C D length percentage elements
#' #> A  1 0 0 0      2   7.692308      y,z
#' #> B  0 1 0 0      0   0.000000
#' #> AB 1 1 0 0      0   0.000000
#' #> C  0 0 1 0      0   0.000000
#' #> AC 1 0 1 0      4  15.384615  u,v,w,x
#' #> BC 0 1 1 0      0   0.000000
venny_init <- function(sets = list())
{
    if ( ! inherits(sets, "list") ) stop("`sets` should be a list.")
    if (length(sets) < 2) stop("No less than 2 sets.")
    if (length(sets) > 26) stop("No more than 26 sets.")

    set_labels <- LETTERS[seq_along(sets)]
    df0 <- as.data.frame(bits_encoding(set_labels))  # from ./utils.R

    zone_elements <- vector("list", nrow(df0))
    names(zone_elements) <- rownames(df0)
    for (i in 1:nrow(df0))
    {
        zone <- rownames(df0)[i]
        on_set <- sets[unname(df0[i, ] == 1)]
        off_set <- sets[unname(df0[i, ] == 0)]
        zone_elements[[zone]] <- setdiff(
            intersect_n(on_set),  # from ./utils.R
            union_n(off_set)  # from ./utils.R
        )
    }
    elements <- lapply(zone_elements, function(`_`) paste(`_`, collapse = ","))
    elements <- do.call(rbind.data.frame, elements)[[1]]
    elements_length <- lapply(zone_elements, length)
    elements_length <- do.call(rbind.data.frame, elements_length)[[1]]

    df0["length"] <- elements_length
    df0["percentage"] <- proportions(elements_length) * 100
    df0["elements"] <- elements

    ret <- structure(
        .Data = df0,
        nsets = length(sets),
        set_names = stats::setNames(names(sets), set_labels),
        class = c(class(df0), "venny")
    )
    return(ret)
}


# data <- data.frame(
#     x0 = c(0, 0),
#     y0 = c(0, 0),
#     a = c(5, 3),
#     b = c(10, 20),
#     angle = c(45, 35),
#     Ellipse = c("Ell1", "Ell2")
# )
#
# make_ellipse <- function(x0, y0, a, b, angle) {
#     angle <- angle/360 * 2 * pi
#     theta <- c(seq(0, 2 * pi, length.out = 360), 0)
#
#     crds <- cbind(a * cos(theta) * cos(angle) - b * sin(theta) * sin(angle) + x0,
#                   a * cos(theta) * sin(angle) + b * sin(theta) * cos(angle) + y0)
#     crds
#     sf::st_polygon(list(crds))
# }
#
# a <- do.call(make_ellipse, data[1, 1:5])
# b <- do.call(make_ellipse, data[2, 1:5])
#
# intersection <- sf::st_intersection(a, b)
# plot(intersection)

# ggvenn::ggvenn(lst_4sets, fill_alpha = 0.1, show_percentage = FALSE, text_size = 0, set_name_size = 0) +
#     geom_hline(yintercept = -0.5, linetype = "dashed", color = "blue", linewidth = .3) +
#     geom_vline(xintercept = -0.75, linetype = "dashed", color = "blue", linewidth = .3) +
#     geom_hline(yintercept = -0.15, linetype = "dashed", color = "red", linewidth = .3) +
#     geom_vline(xintercept = -0, linetype = "dashed", color = "red", linewidth = .3) +
#     theme_light() +
#     theme(axis.title = element_blank())
# ggplot2::ggsave("ggvenn.jpg")
