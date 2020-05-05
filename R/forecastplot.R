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
#' @param ... args passed on
#'
#' @return NULL
#'
#' @export
forecastplot <-
    function(x, ...) {

    warning("Deprecated. Use plot(x, forecast = n) instead.\n")
    return(plot(x, ..., forecast = 2 * x$freq))

}
