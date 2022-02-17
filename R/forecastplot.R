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


log_if <- fabletools::new_transformation(
    transformation = function(x, mult_fit) {
        dplyr::case_when(mult_fit ~ log(x), TRUE ~ as.numeric(x))
    },
    inverse = function(x, mult_fit) {
        dplyr::case_when(mult_fit ~ exp(x), TRUE ~ as.numeric(x))
    }
)


#' @export
predict.inz_ts <- function(x, var = NULL, h = "2 years", mult_fit = FALSE,
                           pred_model = fable::ARIMA, confint_width = .95) {
    var <- guess_plot_var(x, !!enquo(var))

    y_obs <- unlist(lapply(dplyr::case_when(
        length(as.character(var)) > 2 ~ as.character(var)[-1],
        TRUE ~ dplyr::last(as.character(var))
    ), function(i) x[[i]]))
    if (any(y_obs <= 0) & mult_fit) {
        mult_fit <- !mult_fit
        rlang::warn("Non-positive obs detected, setting `mult_fit = FALSE`")
    }

    fit <- fabletools::model(x, Prediction = pred_model(log_if(!!var, !!mult_fit)))

    fit %>%
        fabletools::forecast(h = h) %>%
        dplyr::mutate(
            lower = quantile(!!var, p = (1 - confint_width) / 2),
            upper = quantile(!!var, p = (1 + confint_width) / 2)
        ) %>%
        tsibble::as_tsibble() %>%
        dplyr::select(-(!!var)) %>%
        dplyr::rename(!!var := .mean) %>%
        dplyr::bind_rows(dplyr::mutate(
            dplyr::rename(fitted(fit), !!var := .fitted),
            .model = "Fitted"
        )) %>%
        dplyr::bind_rows(dplyr::mutate(
            dplyr::select(x, !!var),
            .model = "Raw data"
        )) %>%
        structure(class = c("inz_frct", class(.)))
}


#' @export
plot.inz_frct <- function(x) {
    
}
