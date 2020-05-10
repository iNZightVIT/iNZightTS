#' Compare multiple time series - DEPRECATED
#'
#' @param x iNZightMTS object containing data
#' @param ... Further arguments to be passed to `plot()`
#' @return No return value, called for the side effect of drawing a plot.
#'
#' @export
multiseries <- function(x,...) {
    if (!any(grepl("^iNZightMTS$", class(x))))
        stop("x is not an iNZightMTS object")
    warning("Deprecated. Use plot(x, compare = FALSE) instead.\n")
    plot(x, ..., compare = FALSE)
}
