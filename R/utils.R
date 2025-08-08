union_n <- function(lst) unique(unlist(lst))

intersect_n <- function(lst)
{
    lst <- lapply(lst, as.character)
    max_length <- max(unlist(lapply(lst, length)))
    vct <- vector("character", max_length)
    for (i in seq_along(lst)) {
        if (i == 1) vct <- lst[[i]]
        vct <- intersect(vct, lst[[i]])
    }
    return(vct)
}

setdiff_n <- function(lst)
{
    # The first polygon in `lst` is the subject,
    # and the others are clipper
    lst <- lapply(lst, as.character)
    max_length <- max(unlist(lapply(lst, length)))
    vct <- vector("character", max_length)
    for (i in seq_along(lst))
    {
        if (i == 1)
            vct <- lst[[i]]
        vct <- setdiff(vct, lst[[i]])
    }
    return(vct)
}


# Calculate how many zones will be derived from the given set_names
calc_zone_n <- function(set_names)
{
    if (length(set_names) < 2)
        stop("Require at least 2 sets.")

    adder <- 0
    comb_n <- vector("list", length(set_names))
    for (i in seq_along(set_names))
    {
        lst0 <- utils::combn(set_names, i, simplify = FALSE)
        adder <- adder + length(lst0)
        comb_n[[i]] <- unlist(lapply(lst0, function(`_`) paste(`_`, collapse = "")))

    }

    ret <- structure(
        .Data = adder,
        combinations = comb_n
    )
    return(ret)
}


bits_encoding <- function(set_names)
{
    n <- length(set_names)
    comb_n <- calc_zone_n(set_names)  # from ./utils.R

    # Encode each combinations
    bits_mat <- vapply(
        X = 1:comb_n,
        FUN = function(`_`) {
            as.integer(intToBits(`_`)[1:n])
        },
        FUN.VALUE = integer(n)
    )
    bits_mat <- t(bits_mat)
    colnames(bits_mat) <- set_names
    rownames(bits_mat) <- apply(
        X = (bits_mat == 1),
        MARGIN = 1,
        FUN = function(`_`) paste(set_names[`_`], collapse = "")
    )

    # Double check the combinations
    stopifnot(
        all(
            rownames(bits_mat) %in% unlist(attr(comb_n, "combinations"))
        )
    )

    return(bits_mat)
}

#' @title Create a `venny` data.frame
#'
#' @description
#' A data.frame consists of each combination of the input sets.
#'
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
#'     set_1 = letters[1:26],
#'     set_2 = letters[3:17],
#'     set_3 = letters[11:24],
#'     set_4 = letters[1:20]
#' )
#' df0 <- venny_df(lst_4sets)
#' head(df0)
#' #>    A B C D length percentage elements
#' #> A  1 0 0 0      2   7.692308      y,z
#' #> B  0 1 0 0      0   0.000000
#' #> AB 1 1 0 0      0   0.000000
#' #> C  0 0 1 0      0   0.000000
#' #> AC 1 0 1 0      4  15.384615  u,v,w,x
#' #> BC 0 1 1 0      0   0.000000
venny_df <- function(sets = list())
{
    if ( ! inherits(sets, "list") ) stop("`sets` should be a list.")
    if (length(sets) < 2) stop("Should be more than 1 sets.")
    if (length(sets) > 26) stop("Should be less than 27 sets.")

    zone_labels <- LETTERS[seq_along(sets)]
    df0 <- as.data.frame(bits_encoding(zone_labels))  # from ./utils.R

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

    df0["zone_label"] <- rownames(df0)
    df0["n_elements"] <- elements_length
    df0["percentage"] <- proportions(elements_length) * 100
    df0["zone_elements"] <- elements

    ret <- structure(
        .Data = df0,
        set_elements = sets,
        zone_elements = zone_elements,
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
