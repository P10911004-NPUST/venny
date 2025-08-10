##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
## venny_df ====
## Initialize a summary table
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
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
#' s <- 1:1000
#' venn_list <- list(
#'     Set_A = sample(s, 500),
#'     Set_B = sample(s, 800),
#'     Set_C = sample(s, 700),
#'     Set_D = sample(s, 600)
#' )
#' df0 <- venny_df(venn_list)
#' df0
venny_df <- function(sets = list())
{
    if ( ! inherits(sets, "list") ) stop("`sets` should be a list.")
    if (length(sets) < 2) stop("Should be more than 1 sets.")
    if (length(sets) > 26) stop("Should be less than 27 sets.")

    zone_label <- LETTERS[seq_along(sets)]
    df0 <- as.data.frame(bits_encoding(zone_label))  # from ./utils.R

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


##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
## bits_encoding ====
## Create bits matrix
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#' @title Bits matrix
#'
#' @description
#' Produce a bits matrix for all possible combinations of the input sets.
#'
#'
#' @param set_names Character vector
#'
#' @return A numeric matrix with values of 0 or 1.
#' @export
#'
#' @examples
#' bits_encoding(c("A", "B", "C", "D"))
bits_encoding <- function(set_names)
{
    n <- length(set_names)
    n_comb <- calc_zone_n(set_names)  # from ./utils.R

    # Encode each combinations
    bits_mat <- vapply(
        X = 1:n_comb,
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
            rownames(bits_mat) %in% unlist(attr(n_comb, "combinations"))
        )
    )

    return(bits_mat)
}

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
## calc_zone_n ====
## Calculate how many zones will be derived from the given set_names
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#' @rdname bits_encoding
#' @export
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


##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
## fixed_length ====
## Propagate fixed length vector
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#' @title Fixed Length Vector
#'
#' @description
#' Propagate desired length of vector by trimming/extending the `x` vector.
#'
#' @param x A vector
#' @param len Desired length
#'
#' @return A vector
#' @export
#'
#' @examples
#' # Extend `x` to fulfill the `len` requirement
#' fixed_length(1:5, 7)
#' # Trim `x` to fulfill the `len` requirement
#' fixed_length(1:5, 3)
fixed_length <- function(x, len)
{
    len <- as.integer(len)
    if ( is.null(x) || ! is.atomic(x) || ! is.null(dim(x)) ) stop("Accept only vector.")
    if ( ! is.integer(len) ) stop("`length` should be an integer greater than 0.")
    if (len < 1) len <- 1L
    if (length(x) == len) return(x)
    if (length(x) < len) x <- rep(x, times = ceiling(len / length(x)))
    return(x[1:len])
}


##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
## union_n ====
## Set operation
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#' @title Set Operation
#'
#' @description
#' Proceed iterative set operation along a list.
#'
#'
#' @param lst A list with at least length of 2.
#'
#' @return An atomic vector
#' @export
#'
#' @examples
#' union_n(list(1:3, 3:5, 7:9))
#' @seealso [intersect_n], [setdiff_n]
union_n <- function(lst)
{
    lst <- lapply(lst, as.character)
    vct <- unique(unlist(lst))
    return(vct)
}

#' @rdname union_n
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

#' @rdname union_n
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


