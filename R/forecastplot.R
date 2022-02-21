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
    if (length(var) < 3) {
        var <- dplyr::last(as.character(var))
    } else {
        var <- as.character(var)[-1]
    }

    inzightts_forecast_ls <- lapply(var, function(y_var) {
        predict_inzightts_var(x, sym(y_var), h, mult_fit, pred_model, confint_width)
    })

    dplyr::bind_rows(!!!inzightts_forecast_ls) %>%
        structure(class = c("inz_frct", class(.)))
}


predict_inzightts_var <- function(x, var, h, mult_fit, pred_model, confint_width) {
    fit <- fabletools::model(x, Prediction = pred_model(log_if(!!var, !!mult_fit)))

    fit %>%
        fabletools::forecast(h = h) %>%
        dplyr::mutate(
            .lower = quantile(!!var, p = (1 - confint_width) / 2),
            .upper = quantile(!!var, p = (1 + confint_width) / 2)
        ) %>%
        tsibble::as_tsibble() %>%
        dplyr::select(-(!!var)) %>%
        dplyr::bind_rows(dplyr::mutate(
            dplyr::rename(fitted(fit), .mean := .fitted),
            .model = "Fitted"
        )) %>%
        dplyr::bind_rows(dplyr::mutate(
            dplyr::select(dplyr::rename(x, .mean = !!var), .mean),
            .model = "Raw data"
        )) %>%
        dplyr::mutate(.var = !!as.character(var)) %>%
        dplyr::select(.var, .model, index, .mean, .lower, .upper) %>%
        tsibble::update_tsibble(key = c(.var, .model))
}


#' @export
plot.inz_frct <- function(x, xlab = NULL, ylab = NULL, title = NULL, plot = TRUE) {
    if (is.null(xlab)) {
        xlab <- dplyr::case_when(
            is.numeric(x$index) ~ "Year",
            TRUE ~ stringr::str_to_title(class(x$index)[1])
        )
    }
    x <- dplyr::rename(x, !!dplyr::first(xlab) := index)

    if (is.null(ylab)) ylab <- unique(x$.var)

    if (!isTRUE(all.equal(length(unique(x$.var)), length(ylab)))) {
        paste0("ylab should be of length ", length(unique(x$.var)), ".") %>%
            rlang::abort()
    }
    if (is.null(title)) {
        title <- dplyr::case_when(
            length(unique(x$.var)) > 1 ~ "",
            TRUE ~ unique(x$.var)
        )
    }

    if (length(unique(x$.var)) == 1) {
        p <- plot_forecast_var(x, sym(unique(x$.var)), xlab, ylab, title)
    } else {
        p_ls <- lapply()
    }

    if (plot) print(p)

    invisible(p)
}


plot_forecast_var <- function(x, var, xlab, ylab, title) {
    x <- dplyr::filter(x, .var == as.character(var))

    pred_data <- x %>%
        dplyr::filter(.model == "Fitted") %>%
        dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
        dplyr::bind_rows(dplyr::filter(x, .model == "Prediction")) %>%
        dplyr::mutate(
            .lower = tidyr::replace_na(.lower, .$.mean[1]),
            .upper = tidyr::replace_na(.upper, .$.mean[1]),
            .model = "Prediction"
        )

    l_spec <- aes(!!sym(xlab), .mean)

    index_obs <- pred_data[[as.character(xlab)]]
    if (lubridate::is.Date(index_obs) | inherits(index_obs, "vctrs_vctr")) {
        vline_xintercept <- as.Date(index_obs)[1]
    } else {
        vline_xintercept <- as.numeric(index_obs)[1]
    }

    p <- x %>%
        dplyr::select(-.var) %>%
        dplyr::filter(.model != "Raw data") %>%
        fabletools::autoplot(.mean) +
        geom_ribbon(
            mapping = aes(!!sym(xlab), ymin = .lower, ymax = .upper),
            data = pred_data, fill = "#ffdbdb", col = NA
        ) +
        geom_line(l_spec, dplyr::filter(x, .model == "Raw data"), size = 1) +
        geom_line(l_spec, pred_data) +
        geom_line(aes(!!sym(xlab), .lower), pred_data, linetype = "dashed") +
        geom_line(aes(!!sym(xlab), .upper), pred_data, linetype = "dashed") +
        geom_vline(xintercept = vline_xintercept, linetype = "dashed", alpha = .8) +
        scale_colour_manual(values = c("#0e8c07", "#b50000", "black")) +
        ggplot2::labs(title = title, y = ylab) +
        ggplot2::theme(
            legend.position = "bottom",
            legend.title = element_blank()
        )
}
