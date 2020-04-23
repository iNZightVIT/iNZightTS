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
#' @return A multiple time series of the predicted values with columns fit,
#' lwr and upr for the predicted values and the lower and upper bounds
#' respectively.
#'
#' @references C.C Holt (1957)
#' Forecasting seasonals and trends by exponentially weighted
#' moving averages,
#' ONR Research Memorandum, Carnigie Institute 52.
#'
#' P.R Winters (1960)
#' Forecasting sales by exponentially weighted moving averages,
#' \emph{Management Science} \bold{6}, 324--342.
#'
#' @seealso \code{\link{iNZightTS}},
#' \code{\link{HoltWinters}}
#'
#'
#' @export
forecastplot <-
    function(x, ...) {

    warning("Deprecated. Use plot(x, forecast = n) instead.\n")
    return(plot(x, ..., forecast = 2 * x$freq))

}
