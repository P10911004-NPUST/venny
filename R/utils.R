#' Create a `venny` summary data.frame
#'
#' A data.frame consists of each combination of the input sets.
#'
#' @param data A named list contains 2 ~ 26 sets of elements.
#'      For example, `list(set1 = 1:5, set2 = 3:7)`.
#' @param show_elements Logical (default: FALSE).
#'      List out the elements of each zone in the dataframe.
#'
#' @returns A data frame contains necessary information for subsequent inferences and plotting.
#' @export
#' @examples
#' s <- 1:150
#' venn_list <- list(
#'     sample(s, 50),
#'     sample(s, 37),
#'     sample(s, 46),
#'     sample(s, 41)
#' )
#' df0 <- venn_summary(venn_list)
#' df0
venn_summary <- function(
        data = list(),
        show_elements = TRUE
) {
    if ( ! inherits(data, "list") ) stop("`data` should be a list.")
    if (length(data) < 2) stop("Should be more than 1 sets.")
    s <- seq_along(data)
    if (is.null(names(data))) names(data) <- s

    orig_set_names <- names(data)
    dummy_set_names <- c(LETTERS, s)[s]
    set_names_ref <- data.frame(
        original = orig_set_names,
        dummy = dummy_set_names
    )

    names(data) <- dummy_set_names

    bits_mat <- bits_encoding(dummy_set_names, sep = "")

    lst0 <- vector("list", nrow(bits_mat))
    names(lst0) <- rownames(bits_mat)
    for (i in 1:nrow(bits_mat))
    {
        on_set <- data[unname(bits_mat[i, , drop = TRUE] == 1)]
        off_set <- data[unname(bits_mat[i, , drop = TRUE] == 0)]

        subset_names <- rownames(bits_mat)[i]
        subset_elements <- setdiff(intersect_n(on_set), union_n(off_set))
        if (is.null(subset_elements)) subset_elements <- list(NULL)

        lst0[[subset_names]] <- subset_elements
    }

    elements <- lapply(lst0, function(`_`) paste(`_`, collapse = ","))
    elements <- do.call(rbind.data.frame, elements)[[1]]

    elements_length <- lapply(lst0, length)
    elements_length <- do.call(rbind.data.frame, elements_length)[[1]]

    df0 <- as.data.frame(bits_mat)
    df0["subset"] <- rownames(df0)
    df0["n_elements"] <- elements_length
    df0["percentage"] <- round(proportions(elements_length) * 100, 1)

    if (isTRUE(show_elements))
    {
        df0["elements"] <- elements
        ret <- list(
            table = df0,
            set_names_ref = set_names_ref,
            subset_elements = lst0
        )
    } else {
        ret <- list(
            table = df0,
            set_names_ref = set_names_ref
        )
        return(ret)
    }
}


#' Create a bits matrix
#'
#' Produce a bits matrix for all possible combinations of the input sets.
#'
#' @param x A character vector.
#' @param sep A character used to separate the group names (default is `:`).
#'
#' @return A numeric matrix with values of 0 or 1.
#' @export
#'
#' @examples
#' bits_encoding(c("qqa", "bnk", "sdf", "123"))
bits_encoding <- function(x, sep = ":")
{
    n <- length(x)
    n_comb <- how_many_subsets(x)

    # Encode each combinations
    bits_mat <- vapply(
        X = 1:n_comb,
        function(`_`) {
            as.integer(intToBits(`_`)[1:n])
        },
        FUN.VALUE = integer(n)
    )
    bits_mat <- t(bits_mat)
    colnames(bits_mat) <- x
    rownames(bits_mat) <- apply(
        X = (bits_mat == 1),
        MARGIN = 1,
        FUN = function(`_`) paste(x[`_`], collapse = sep)
    )

    return(bits_mat)
}


#' How many subsets
#'
#' Calculate the number of combinations that can be derived from the given character vector.
#'
#' @param x A character vector.
#' @param detail Logical (default: FALSE). Whether to output the combinations.
#'
#' @return An integer value or a list.
#' @export
#'
#' @examples
#' how_many_subsets(c("qqa", "bnk", "sdf", "123"))
how_many_subsets <- function(x, detail = FALSE)
{
    if (length(x) < 2) stop("Require at least 2 sets.")

    n <- 0
    comb_n <- vector("list", length(x))

    for (i in seq_along(x))
    {
        lst0 <- utils::combn(x, i, simplify = FALSE)
        n <- n + length(lst0)
        comb_n[[i]] <- unlist(lapply(lst0, function(`_`) paste(`_`, collapse = "")))
    }

    if (isTRUE(detail))
        return(list("N" = n, "combinations" = unlist(comb_n)))
    else
        return(n)
}


##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
## Create a vector to a fixed length
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#' Fixed Length Vector
#'
#' Create a desired length of vector by trimming/extending the `x` vector.
#'
#' @param x A vector.
#' @param len Desired length.
#' @param fill_with Element used to extend the vector length.
#'      If set to `NULL`, extend by itself (default: NULL).
#'
#' @return A vector
#' @export
#'
#' @examples
#' # Extend `x` to fulfill the `len` requirement
#' fixed_length(1:5, 7)
#' # Trim `x` to fulfill the `len` requirement
#' fixed_length(1:5, 3)
fixed_length <- function(x, len, fill_with = NULL)
{
    len <- as.integer(len)
    if ( is.null(x) || ! is.atomic(x) || ! is.null(dim(x)) ) stop("Accept only vector.")
    if ( ! is.integer(len) ) stop("`length` should be an integer greater than 0.")
    if (len < 1) len <- 1L

    # If length(x) == len, then do nothing
    if (length(x) == len)
        return(x)

    # If length(x) > len, then trim the `x`
    if (length(x) > len)
        return(x[1:len])

    # If length(x) < len, then extend the `x` with either itself or NAs
    if (length(x) < len)
    {
        if (is.null(fill_with))
            x <- rep(x, times = ceiling(len / length(x)))
        else
            x <- c(x, rep(fill_with, length.out = len - length(x)))

        return(x[1:len])
    }
}


##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
## Set operation
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#' Iterative set operation
#'
#' Proceed set operation along a list.
#'
#' @name iter_setops
#'
#' @param lst A list with at least length of 2.
#'
#' @returns An atomic vector.
#'
#' @examples
#' intersect_n(list(1:3, 3:5, 7:9))
#' union_n(list(1:3, 3:5, 7:9))
#' setdiff_n(list(1:3, 3:5, 7:9))
NULL


#' @rdname iter_setops
#' @export
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


#' @rdname iter_setops
#' @export
union_n <- function(lst)
{
    lst <- lapply(lst, as.character)
    vct <- unique(unlist(lst))
    return(vct)
}


#' @rdname iter_setops
#' @export
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
    radian <- (angle %% 360) * pi / 180
    theta <- c(seq(0, 2 * pi, length.out = density), 0)
    # c <- sqrt(a ^ 2 - b ^ 2)  # distance from the center to a focus
    # e <- 1 - sqrt( (b ^ 2) / (a ^ 2) )  # eccentricity

    mat0 <- matrix(nrow = length(theta), ncol = 2)
    colnames(mat0) <- c("x", "y")

    # Ellipse
    x1 <- a * cos(theta) * cos(radian)
    y1 <- b * sin(theta) * cos(radian)

    # Rotate and shift
    x1 <- x1 - b * sin(theta) * sin(radian) + x0
    y1 <- y1 + a * cos(theta) * sin(radian) + y0

    mat0[, 1] <- x1
    mat0[, 2] <- y1

    ret <- structure(
        .Data = mat0,
        class = c(class(mat0), "venny_ep", "venny_setops")
    )

    return(ret)
}



