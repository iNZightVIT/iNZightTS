#' Log-transform the input if \code{mult_fit = TRUE}, else return the original input.
#'
#' @title Toggleable log logarithmic transformation
#'
#' @param x a \code{numeric}
#' @param mult_fit whether or not to apply logarithmic transformation to the input
#' @return a \code{numeric}
#'
#' @rdname log_if
#'
#' @seealso \code{\link[fabletools]{new_transformation}}
#'
#' @examples
#' x <- runif(1e4, 1, 100)
#' all.equal(log_if(x, TRUE), log(x))
#' all.equal(log_if(x, FALSE), x)
#'
#' @export
log_if <- fabletools::new_transformation(
    transformation = function(x, mult_fit) {
        if (mult_fit) log(x) else as.numeric(x)
    },
    inverse = function(x, mult_fit) {
        if (mult_fit) exp(x) else as.numeric(x)
    }
)


#' Produce future predictions of the time series from an inzightts object.
#'
#' The output object includes the predicted mean and prediction intervals,
#' as well as the raw data and fitted values.
#'
#' @title Produce forecasts for inzightts objects
#'
#' @param object an inzightts (\code{inz_ts}) object
#' @param var a character vector of the variable(s) to forecast, or \code{NULL}
#' @param h The forecast horison
#' @param mult_fit If \code{TRUE}, a multiplicative model is used, otherwise
#'        an additive model is used by default.
#' @param pred_model a \code{fable} model function
#' @param confint_width a decimal, the width of the prediction interval
#' @param t_range range of data to be plotted, specified as dates or years
#' @param model_range range of data to be fitted for forecasts, specified as
#'        dates or years, if part of \code{model_range} specified is outside
#'        the range of \code{t_range}, the exceeding proportion is ignored.
#' @param ... additional arguments (ignored)
#' @return an \code{inz_frct} object
#'
#' @rdname forecastplot
#'
#' @seealso \code{\link[fable]{fable-package}}
#'
#' @examples
#' t <- inzightts(visitorsQ, var = c(2, 4))
#' ## The following two examples are equivalent
#' pred <- predict(t, names(t)[-1], h = "2 years")
#' pred <- predict(t, names(t)[-1], h = 8)
#' plot(pred)
#'
#' @export
predict.inz_ts <- function(object, var = NULL, h = "2 years", mult_fit = FALSE,
                           pred_model = fable::ARIMA, confint_width = .95,
                           t_range = NULL, model_range = NULL, ...) {
    var <- guess_plot_var(object, !!enquo(var), use = "Predict")

    if (all(is.na(t_range))) t_range <- NULL
    if (all(is.na(model_range))) model_range <- NULL

    if (length(tsibble::key(object)) > 0) {
        rlang::abort("prediction for inz_ts objects with key is not supported.")
    }
    y_obs <- unlist(lapply(ifelse(
        length(as.character(var)) > 2,
        c("", as.character(var)[-1]),
        dplyr::last(as.character(var))
    ), function(i) object[[i]]))
    if (any(y_obs <= 0) & mult_fit) {
        mult_fit <- !mult_fit
        rlang::warn("Non-positive obs detected, setting `mult_fit = FALSE`")
    }
    if (!is.null(t_range)) {
        if (!is.null(model_range) & class(model_range)[1] != class(t_range)[1]) {
            rlang::abort("model_range and t_range must have the same primary class.")
        }
        if (!all(length(t_range) == 2, any(is.numeric(t_range), methods::is(t_range, "Date")))) {
            rlang::abort("t_range must be a numeric or Date vector of length 2.")
        }
        na_i <- which(is.na(t_range))[1]
        if (!is.numeric(object[[tsibble::index_var(object)]]) & is.numeric(t_range)) {
            t_range[na_i] <- lubridate::year(dplyr::case_when(
                as.logical(na_i - 1) ~ dplyr::last(object$index),
                TRUE ~ object$index[1]
            ))
            t_range <- lubridate::ymd(paste0(t_range, c("0101", "1231")))
            object <- dplyr::filter(object, dplyr::between(lubridate::as_date(index), t_range[1], t_range[2]))
        } else if (is.numeric(object[[tsibble::index_var(object)]]) & methods::is(t_range, "Date")) {
            t_range[na_i] <- lubridate::ymd(paste0(ifelse(na_i - 1, dplyr::last(object$index), object$index[1]), "0101"))
            object <- dplyr::filter(object, dplyr::between(index, lubridate::year(t_range[1]), lubridate::year(t_range[2])))
        } else {
            t_range[na_i] <- dplyr::case_when(
                as.logical(na_i - 1) ~ dplyr::last(object$index),
                TRUE ~ object$index[1]
            )
            object <- dplyr::filter(object, dplyr::between(index, t_range[1], t_range[2]))
        }
    }
    if (!is.null(model_range)) {
        if (!all(length(model_range) == 2, any(is.numeric(model_range), methods::is(model_range, "Date")))) {
            rlang::abort("model_range must be a numeric or Date vector of length 2.")
        }
        na_i <- which(is.na(model_range))[1]
        if (!is.numeric(object[[tsibble::index_var(object)]]) & is.numeric(model_range)) {
            model_range[na_i] <- lubridate::year(dplyr::case_when(
                as.logical(na_i - 1) ~ dplyr::last(object$index),
                TRUE ~ object$index[1]
            ))
            model_range <- lubridate::ymd(paste0(model_range, c("0101", "1231")))
            x <- dplyr::filter(object, dplyr::between(lubridate::as_date(index), model_range[1], model_range[2]))
        } else if (is.numeric(object[[tsibble::index_var(object)]]) & methods::is(model_range, "Date")) {
            model_range[na_i] <- lubridate::ymd(paste0(ifelse(na_i - 1, dplyr::last(object$index), object$index[1]), "0101"))
            x <- dplyr::filter(object, dplyr::between(index, lubridate::year(model_range[1]), lubridate::year(model_range[2])))
        } else {
            model_range[na_i] <- dplyr::case_when(
                as.logical(na_i - 1) ~ dplyr::last(object$index),
                TRUE ~ object$index[1]
            )
            x <- dplyr::filter(object, dplyr::between(index, model_range[1], model_range[2]))
        }
    } else {
        x <- object
    }
    if (length(var) < 3) {
        var <- dplyr::last(as.character(var))
    } else {
        var <- as.character(var)[-1]
    }

    inzightts_forecast_ls <- lapply(var, function(y_var) {
        predict_inzightts_var(x, sym(y_var), h, mult_fit, pred_model, confint_width)
    })

    object %>%
        dplyr::select(index, !!!var) %>%
        tidyr::pivot_longer(!index, names_to = ".var", values_to = ".mean") %>%
        dplyr::mutate(.model = "Raw data") %>%
        tibble::as_tibble() %>%
        dplyr::bind_rows(!!!inzightts_forecast_ls) %>%
        dplyr::filter(
            !tsibble::are_duplicated(., index = index, key = c(.var, .model))
        ) %>%
        tsibble::as_tsibble(index = index, key = c(.var, .model)) %>%
        structure(
            class = c("inz_frct", class(.)),
            fit = lapply(inzightts_forecast_ls, function(x) {
                dplyr::rename(
                    attributes(x)$fit,
                    !!unique(x$.var) := Prediction
                )
            })
        )
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
        tsibble::update_tsibble(key = c(.var, .model)) %>%
        structure(fit = fit)
}


#' @param x an \code{inz_frct} object
#' @param xlab a title for the x axis
#' @param ylab a title for the y axis
#' @param title a title for the graph
#' @param plot logical, if \code{FALSE}, the graph isn't drawn
#' @param ... additional arguments (ignored)
#'
#' @rdname decomposition
#'
#' @import patchwork
#'
#' @export
plot.inz_frct <- function(x, xlab = NULL, ylab = NULL, title = NULL, plot = TRUE, ...) {
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
        p_ls <- lapply(seq_along(unique(x$.var)), function(i) {
            y_var <- unique(x$.var)[i]
            plot_forecast_var(x, sym(y_var), xlab, ylab[i], "")
        })
        p <- expr(patchwork::wrap_plots(!!!p_ls)) %>%
            rlang::new_quosure() %>%
            rlang::eval_tidy() +
            patchwork::plot_layout(ncol = 1, guides = "collect") +
            patchwork::plot_annotation(title = title) &
            ggplot2::theme(legend.position = "bottom")
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


#' \code{summary} method for objects of class \code{inz_frct}.
#'
#' @title Summarising forecasts for inzightts objects
#'
#' @param object an \code{inz_frct} object
#' @param var a character vector of length one, or \code{NULL}
#' @param ... additional arguments (ignored)
#' @return a \code{summary_inz_frct} object, which consists of the first few
#'         forecast observations, the model used for forecasting and the model
#'         details (call, coefficients and goodness of fit statistics).
#'
#' @rdname forecastsummary
#'
#' @seealso \code{\link[iNZightTS]{predict.inz_ts}}
#'
#' @examples
#' library(dplyr)
#' s <- visitorsQ %>%
#'     inzightts(var = 2:5) %>%
#'     predict("Japan") %>%
#'     summary("Japan")
#' s
#' print(s, show_details = TRUE)
#'
#' @export
summary.inz_frct <- function(object, var = NULL, ...) {
    if (is.null(var)) {
        var <- unique(object$.var)[1]
        rlang::inform(sprintf(
            "Summary variable not specified, automatically selected `var = %s`",
            var
        ))
    }
    if (length(var) > 1) {
        var <- dplyr::first(var)
        rlang::warn(sprintf(
            "Please specify one variable, automatically selected `var = %s`",
            var
        ))
    }
    fit <- attributes(object)$fit
    i <- which(unlist(lapply(fit, names)) == var)
    mod_spec <- fabletools::model_sum(fit[[i]][[1]][[1]])
    pred <- object %>%
        tibble::as_tibble() %>%
        dplyr::filter(.var == !!var, .model == "Prediction") %>%
        dplyr::select(-c(.var, .model)) %>%
        head()

    with(fit[[i]][[1]][[1]], list(
        head_pred = pred,
        spec = mod_spec,
        model = fit$model
    )) %>%
        structure(
            class = "summary_inz_frct",
            model = class(fit[[i]][[1]][[1]]$fit)
        )
}


#' @param x a \code{summary_inz_frct} object
#' @param show_details logical, if \code{TRUE} the model details will be shown,
#'        only if \code{pred_model = fable::ARIMA} in the \code{predict} call.
#' @param ... additional arguments (ignored)
#'
#' @rdname forecastsummary
#'
#' @export
print.summary_inz_frct <- function(x, show_details = FALSE, ...) {
    cat("\nThe first few forecasted observations:\n")
    print(as.data.frame(x$head_pred), row.names = FALSE)
    cat("\nModel:\n")
    cat(x$spec)
    cat("\n")
    if (show_details & inherits(attributes(x)$model, "ARIMA")) {
        print(x$model)
    }
}
