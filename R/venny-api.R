#' @title Venn Diagram
#'
#' @description
#' A wrapper for generate_ellipse_*() functions.
#'
#'
#' @param data A named list consists of at least two sets of elements.
#' @param ... Arguments pass to generate_ellipse_*() functions.
#'
#' @return A list which contains:
#' 1. plot: A venn diagram;
#' 2. ellipse_path: Polygon path of each ellipses;
#' 3. venny_df: A summary table;
#' 4. set_elements: Elements for each sets;
#' 5. zone_elements: Elements for each subsets.
#'
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
#' e3 <- venny(lst[1:3])
#' e3$plot
#' e4 <- venny(lst[1:4], show_zone_percentage = FALSE)
#' e4$plot
#' @seealso [generate_ellipse_2], [generate_ellipse_3], [generate_ellipse_4]
venny <- function(data, ...)
{
    n_sets <- length(data)
    if ( ! is.recursive(data) ) stop("`data` should be a named list.")
    if (n_sets < 2) stop("`data` requires at least two sets.")
    if (n_sets > 4) stop("Not yet")

    fun <- switch(
        n_sets,
        NA,
        generate_ellipse_2,
        generate_ellipse_3,
        generate_ellipse_4
    )

    ret <- fun(data, ...)

    return(ret)
}
