#' Plot a raw time series together with it's fitted curve and add
#' forecasts and prediction intervals to the end.
#'
#' The predictions and prediction intervals are the result of models
#' fitted by the Holt-Winters method. The amount of predicted
#' observations is calculated by 2 * \code{freq}, where \code{freq} is
#' the frequency of the time series object.
#'
#' @title Forecast plot - DEPRECATED
#'
#' @param x \code{iNZightTS} object
#' @param ... additional arguments passed on
#'
#' @return Called for the side effect of drawing a plot.
#'         The constructed \code{ggplot} object is returned invisibly.
#'
#' @export
forecastplot <-
    function(x, ...) {

    warning("Deprecated. Use plot(x, forecast = n) instead.\n")
    return(plot(x, ..., forecast = 2 * x$freq))

}
