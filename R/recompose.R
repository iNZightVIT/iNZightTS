#' Recompose a time series object, with optional animation.
#'
#' @title Recompose a decomposed time series
#' @param ... additional arguments, ignored
#' @return the recomposed series
#' @author iNZight
#' @export
recompose <- function(...) {
    warning("Deprecated: please use `plot(decompose(obj), recompose = TRUE)`")
}
