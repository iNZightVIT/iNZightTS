#' Apply logarithmic transformation
#'
#' Log-transforms the input `x` if `mult_fit` is TRUE; otherwise, returns the
#' original input `x` unchanged.
#'
#' @param x A numeric vector to be transformed.
#' @param mult_fit Logical; set to TRUE to apply logarithmic transformation,
#'        and FALSE to keep the original input.
#' @return A `numeric` vector after applying the logarithmic transformation (if
#'         `mult_fit = TRUE`); otherwise, it returns the original input.
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
#' @md
log_if <- fabletools::new_transformation(
    transformation = function(x, mult_fit) {
        if (mult_fit) log(x) else as.numeric(x)
    },
    inverse = function(x, mult_fit) {
        if (mult_fit) exp(x) else as.numeric(x)
    }
)


get_model <- function(x) {
    UseMethod("get_model")
}


#' @export
get_model.function <- function(x) x


#' @export
get_model.character <- function(x) {
    if (tolower(x) == "auto") {
        ARIMA_lite
    } else {
        getFromNamespace(toupper(x), "fable")
    }
}


ARIMA_lite <- function(formula,
                       order_constraint = p + q + P + Q <= 3 & (constant + d + D <= 2)) {
    fable::ARIMA(!!enquo(formula), order_constraint = !!enquo(order_constraint))
}


#' Forecast future observations
#'
#' Generates future predictions of the time series from an `inzightts` object.
#' The output object includes predicted means, prediction intervals, raw data,
#' and fitted values.
#'
#' @param object An `inzightts` object representing the time series.
#' @param var A character vector specifying the variable(s) to forecast, or set
#'        to `NULL` to forecast all variables.
#' @param h The forecast horizon, either the number of observations to predict,
#'        or a character string specifying the time interval to predict
#'        (e.g., `"2 years"`).
#' @param mult_fit Logical; set to `TRUE` for a multiplicative model, or
#'        `FALSE` for the default additive model.
#' @param pred_model The name of a `fable` model function or `"auto"`.
#' @param confint_width A decimal representing the width of the prediction
#'        interval.
#' @param model_range The range of data to be used for fitting forecasts,
#'        specified as dates or years.
#' @param ... Additional arguments (ignored).
#' @return An \code{inz_frct} object containing the forecasts.
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
#'
#' \dontrun{
#' plot(pred)
#' }
#'
#' @export
#' @md
predict.inz_ts <- function(object, var = NULL, h = 8, mult_fit = FALSE,
                           pred_model = "auto", confint_width = .95,
                           model_range = NULL, ...) {
    var <- guess_plot_var(object, !!enquo(var), use = "Predict")
    pred_model <- get_model(pred_model)
    if (all(is.na(model_range))) model_range <- NULL
    y_obs <- unlist(lapply(ifelse(
        length(as.character(var)) > 2,
        c("", as.character(var)[-1]),
        dplyr::last(as.character(var))
    ), function(i) object[[i]]))
    if (any(y_obs <= 0) && mult_fit) {
        mult_fit <- !mult_fit
        rlang::warn("Non-positive obs detected, setting `mult_fit = FALSE`")
    }
    if (!is.null(model_range)) {
        if (!all(length(model_range) == 2, any(is.numeric(model_range), inherits(model_range, "Date")))) {
            rlang::abort("model_range must be a numeric or Date vector of length 2.")
        }
        na_i <- which(is.na(model_range))[1]
        if (!is.numeric(object[[tsibble::index_var(object)]]) && is.numeric(model_range)) {
            model_range[na_i] <- lubridate::year(
                if (!is.na(na_i) && as.logical(na_i - 1)) {
                    dplyr::last(object$index)
                } else {
                    object$index[1]
                }
            )
            model_range <- lubridate::ymd(paste0(model_range, c("0101", "1231")))
            x <- dplyr::filter(object, dplyr::between(lubridate::as_date(index), model_range[1], model_range[2]))
        } else if (is.numeric(object[[tsibble::index_var(object)]]) && inherits(model_range, "Date")) {
            model_range[na_i] <- lubridate::ymd(paste0(ifelse(na_i - 1, dplyr::last(object$index), object$index[1]), "0101"))
            x <- dplyr::filter(object, dplyr::between(index, lubridate::year(model_range[1]), lubridate::year(model_range[2])))
        } else {
            model_range[na_i] <- if (!is.na(na_i) && as.logical(na_i - 1)) dplyr::last(object$index) else object$index[1]
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
    if (tsibble::n_keys(x) > 1) {
        key_data <- tsibble::key_data(x)
        key_vars <- tsibble::key_vars(x)
        max_t <- max(x[[tsibble::index(x)]])
        valid_key <- na.omit(unlist(lapply(seq_len(nrow(key_data)), function(i) {
            x_k <- dplyr::left_join(key_data[i, ], x, by = key_vars, multiple = "all")
            diff_t <- max_t - max(x_k[[tsibble::index(x)]])
            ifelse(diff_t <= tsibble::guess_frequency(x[[tsibble::index(x)]]), i, NA)
        })))
        min_t <- min(do.call("c", lapply(valid_key, function(i) {
            max(dplyr::left_join(key_data[i, ], x, by = key_vars, multiple = "all")$index)
        })))
        interval <- getFromNamespace("interval_to_period", "feasts")(tsibble::interval(x))
        if (is.character(h)) {
            o_h <- as.integer(round(lubridate::as.period(h) / interval))
            h <- interval * as.integer(max_t - min_t) + lubridate::as.period(h)
        } else {
            o_h <- as.integer(round(h))
            h <- as.integer(max_t - min_t) + h
        }
    }
    inzightts_forecast_ls <- lapply(var, function(y_var) {
        predict_inzightts_var(x, sym(y_var), h, mult_fit, pred_model, confint_width)
    })
    pred <- object |>
        (\(.) dplyr::select(., index, !!!var, !!tsibble::key_vars(.)))() |>
        (\(.) tidyr::pivot_longer(., !c(index, !!tsibble::key_vars(.)), names_to = ".var", values_to = ".mean"))() |>
        dplyr::mutate(.model = "Raw data") |>
        tibble::as_tibble() |>
        dplyr::bind_rows(!!!inzightts_forecast_ls) |>
        (\(.) dplyr::filter(
            .,
            !tsibble::are_duplicated(., index = index, key = c(.var, .model, !!tsibble::key_vars(x)))
        ))() |>
        tsibble::as_tsibble(index = index, key = c(.var, .model, !!tsibble::key_vars(x))) |>
        (\(.) structure(.,
            class = c("inz_frct", class(.)),
            fit = lapply(inzightts_forecast_ls, function(x) {
                dplyr::rename(
                    attributes(x)$fit,
                    !!unique(x$.var) := Prediction
                )
            })
        ))()
    if (tsibble::n_keys(x) > 1) {
        pred <- pred |>
            dplyr::filter(.model != "Prediction") |>
            dplyr::bind_rows(pred |>
                dplyr::filter(.model == "Prediction" & index > max_t & index <= max_t + o_h) |>
                dplyr::right_join(key_data[valid_key, ], by = key_vars, multiple = "all") |>
                tsibble::as_tsibble(index = index, key = !!tsibble::key_vars(pred)) |>
                dplyr::select(!.rows))
    }
    structure(
        pred,
        class = c("inz_frct", class(pred)),
        confint_width = confint_width,
        fit = lapply(inzightts_forecast_ls, function(x) {
            dplyr::rename(
                attributes(x)$fit,
                !!unique(x$.var) := Prediction
            )
        })
    )
}


use_urca <- function() {
    urca::plot
}


predict_inzightts_var <- function(x, var, h, mult_fit, pred_model, confint_width) {
    fit <- fabletools::model(x,
        Prediction = pred_model(log_if(!!var, !!mult_fit))
    )
    fit |>
        fabletools::forecast(h = h) |>
        dplyr::mutate(
            .lower = quantile(!!var, p = (1 - confint_width) / 2),
            .upper = quantile(!!var, p = (1 + confint_width) / 2)
        ) |>
        tsibble::as_tsibble() |>
        dplyr::select(-(!!var)) |>
        dplyr::bind_rows(dplyr::mutate(
            dplyr::rename(fitted(fit), .mean := .fitted),
            .model = "Fitted"
        )) |>
        dplyr::bind_rows(dplyr::mutate(
            dplyr::select(dplyr::rename(x, .mean = !!var), .mean),
            .model = "Raw data"
        )) |>
        dplyr::mutate(.var = !!as.character(var)) |>
        dplyr::select(.var, .model, index, .mean, .lower, .upper) |>
        tsibble::update_tsibble(key = c(.var, .model, !!tsibble::key_vars(x))) |>
        structure(fit = fit)
}


#' @param x An `inz_frct` object containing the forecasts.
#' @param t_range The range of data to be plotted, specified as dates or years.
#' @param xlab A title for the x-axis of the plot.
#' @param ylab A title for the y-axis of the plot.
#' @param title A title for the graph.
#'
#' @param ... Additional arguments (ignored).
#'
#' @rdname forecastplot
#'
#' @import patchwork
#'
#' @export
#' @md
plot.inz_frct <- function(x, t_range = NULL, xlab = NULL, ylab = NULL, title = NULL, ...) {
    if (all(is.na(t_range))) t_range <- NULL
    if (!is.null(t_range)) {
        if (!all(length(t_range) == 2, any(is.numeric(t_range), inherits(t_range, "Date")))) {
            rlang::abort("t_range must be a numeric or Date vector of length 2.")
        }
        na_i <- which(is.na(t_range))[1]
        if (!is.numeric(x[[tsibble::index_var(x)]]) && is.numeric(t_range)) {
            t_range[na_i] <- lubridate::year(
                if (!is.na(na_i) && as.logical(na_i - 1)) {
                    dplyr::last(x$index)
                } else {
                    x$index[1]
                }
            )
            t_range <- lubridate::ymd(paste0(t_range, c("0101", "1231")))
            x <- x |> dplyr::filter(
                dplyr::between(lubridate::as_date(index), t_range[1], t_range[2]) | .model == "Prediction"
            )
        } else if (is.numeric(x[[tsibble::index_var(x)]]) && inherits(t_range, "Date")) {
            t_range[na_i] <- lubridate::ymd(paste0(ifelse(na_i - 1, dplyr::last(x$index), x$index[1]), "0101"))
            x <- x |> dplyr::filter(
                dplyr::between(index, lubridate::year(t_range[1]), lubridate::year(t_range[2])) | .model == "Prediction"
            )
        } else {
            t_range[na_i] <- if (!is.na(na_i) && as.logical(na_i - 1)) dplyr::last(x$index) else x$index[1]
            x <- x |> dplyr::filter(
                dplyr::between(index, t_range[1], t_range[2]) | .model == "Prediction"
            )
        }
        idx <- unique(dplyr::filter(tsibble::fill_gaps(x), .model != "Fitted")$index)
        test <- tsibble::tsibble(idx = idx, y = seq_along(idx), index = idx)
        if (tsibble::has_gaps(test)$.gaps) {
            rlang::abort("Upper bound of `model_range` exceeds `t_range`.")
        }
    }
    if (is.null(xlab)) {
        xlab <- if (is.numeric(x$index)) "Year" else stringr::str_to_title(class(x$index)[1])
    }
    x <- dplyr::rename(x, !!dplyr::first(xlab) := index)
    if (is.null(ylab)) ylab <- unique(x$.var)
    if (!isTRUE(all.equal(length(unique(x$.var)), length(ylab)))) {
        paste0("ylab should be of length ", length(unique(x$.var)), ".") |>
            rlang::abort()
    }
    if (is.null(title)) {
        title <- if (length(unique(x$.var)) > 1) "" else unique(x$.var)
    }
    (if (length(unique(x$.var)) == 1) {
        plot_forecast_var(x, sym(unique(x$.var)), xlab, ylab, title)
    } else {
        p_ls <- lapply(seq_along(unique(x$.var)), function(i) {
            y_var <- unique(x$.var)[i]
            plot_forecast_var(x, sym(y_var), xlab, ylab[i], "")
        })
        rlang::inject(patchwork::wrap_plots(!!!p_ls)) +
            patchwork::plot_layout(ncol = 1, guides = "collect") +
            patchwork::plot_annotation(title = title) &
            ggplot2::theme(legend.position = "bottom")
    }) |> (\(.) structure(., use.plotly = ggplotable(.)))()
}


plot_forecast_var <- function(x, var, xlab, ylab, title) {
    x <- dplyr::filter(x, .var == as.character(var))
    n_keys <- tsibble::n_keys(x)
    pred_data <- x |>
        dplyr::filter(.model == "Fitted") |>
        (\(.) dplyr::filter(., dplyr::row_number() %in% do.call(
            "c", lapply(tsibble::key_data(.)$.rows, dplyr::last)
        )))() |>
        dplyr::bind_rows(dplyr::filter(x, .model == "Prediction")) |>
        dplyr::mutate(.model = "Prediction")
    ik <- which(is.na(pred_data)) %% nrow(pred_data)
    pred_data$.lower[ik] <- pred_data$.mean[ik]
    pred_data$.upper[ik] <- pred_data$.mean[ik]
    if (n_keys > 3) {
        key_vars <- tsibble::key_vars(x)
        x <- dplyr::mutate(x,
            .key = rlang::inject(interaction(!!!({
                key_vars <- tsibble::key_vars(x)
                lapply(key_vars[!key_vars %in% c(".var", ".model")], function(i) x[[i]])
            }), sep = "/"))
        ) |>
            tsibble::update_tsibble(key = c(.var, .model, .key))
        pred_data <- pred_data |>
            (\(.) dplyr::mutate(.,
                .key = rlang::inject(interaction(!!!({
                    key_vars <- tsibble::key_vars(.)
                    lapply(key_vars[!key_vars %in% c(".var", ".model")], function(i) .[[i]])
                }), sep = "/"))
            ))() |>
            tsibble::update_tsibble(key = c(.var, .model, .key))
    }
    r_spec <- list(
        mapping = aes(
            x = !!sym(xlab), ymin = .lower, ymax = .upper,
            group = !!(if (n_keys > 3) sym(".key") else NULL),
            col = !!(if (n_keys > 3) sym(".key") else NULL),
            fill = !!(if (n_keys > 3) sym(".key") else NULL)
        ),
        data = pred_data,
        lwd = ifelse(n_keys > 3, 0, .5)
    )
    if (n_keys == 3) {
        r_spec <- c(r_spec, fill = "#ffdbdb", col = NA)
    } else {
        r_spec <- c(r_spec, alpha = .4)
    }
    l_spec <- list(x = sym(xlab), y = sym(".mean"))
    if (n_keys > 3) {
        l_spec <- c(l_spec, group = sym(".key"), col = sym(".key"))
    }
    l_spec <- rlang::inject(aes(!!!l_spec))
    index_obs <- dplyr::filter(x, .model == "Raw data")[[as.character(xlab)]]
    if (lubridate::is.Date(index_obs) || inherits(index_obs, "vctrs_vctr")) {
        xi <- max(as.Date(index_obs))
    } else {
        xi <- max(as.numeric(index_obs))
    }
    p <- x |>
        dplyr::select(-.var) |>
        dplyr::filter(.model != "Raw data") |>
        fabletools::autoplot(.mean, linetype = ifelse(n_keys > 3, "dashed", "solid"))
    if (n_keys > 3) {
        p$mapping$group <- p$mapping$colour
        p$mapping$colour <- NULL
        p$layers[[1]] <- NULL
    }
    p <- rlang::inject(p + geom_ribbon(!!!r_spec, show.legend = FALSE)) +
        geom_line(l_spec, dplyr::filter(x, .model == "Raw data"), linewidth = 1) +
        geom_line(l_spec, pred_data, linetype = ifelse(n_keys > 3, 2, 1)) +
        geom_vline(xintercept = xi, linetype = "dashed", alpha = .4, lwd = .4) +
        ggplot2::labs(title = title, y = ylab) +
        ggplot2::theme(
            legend.position = "bottom",
            legend.title = element_blank()
        )
    if (n_keys == 3) {
        p + geom_line(aes(!!sym(xlab), .lower), pred_data, linetype = "dashed") +
            geom_line(aes(!!sym(xlab), .upper), pred_data, linetype = "dashed") +
            scale_colour_manual(values = c("#0e8c07", "#b50000", "black"))
    } else {
        p + geom_line(
            mapping = aes(col = .key),
            data = dplyr::filter(x, .model == "Fitted"),
            linetype = "dashed"
        )
    }
}


#' Summarise iNZightTS forecasts
#'
#' Summary method for objects of class `inz_frct`.
#'
#' @param object An `inz_frct` object representing the forecasts.
#' @param var A character vector specifying the variable to summarize,
#'        or set to `NULL` to summarize all variables.
#' @param ... Additional arguments (ignored).
#'
#' @return A `summary_inz_frct` object containing the first few forecast
#'         observations, the forecasting model used, and its details (such as
#'         call, coefficients, and goodness of fit statistics).
#'
#' @rdname forecastsummary
#'
#' @seealso \code{\link[iNZightTS]{predict.inz_ts}}
#'
#' @examples
#' ts <- inzightts(visitorsQ, var = 2:5)
#' p <- predict(ts, "Japan")
#' s <- summary(p, "Japan")
#' s
#' print(s, show_details = TRUE)
#'
#' @export
#' @md
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
    i <- which(unlist(lapply(fit, function(x) dplyr::last(names(x)))) == var)
    mod_spec <- fit[[i]]
    pred <- object |>
        dplyr::filter(.var == !!var, .model == "Prediction") |>
        dplyr::select(-c(.var, .model)) |>
        (\(.) dplyr::select(
            .,
            !!tsibble::key_vars(.),
            !!tsibble::index(.),
            !!tsibble::measured_vars(.)
        ))()
    if (tsibble::n_keys(object) > 3 * length(unique(object$.var))) {
        model <- lapply(fit[[i]][[ncol(fit[[i]])]], function(x) {
            x$fit$model
        })
        names(model) <- unique(rlang::inject(interaction(!!!({
            key_vars <- tsibble::key_vars(object)
            lapply(key_vars[!key_vars %in% c(".var", ".model")], function(i) object[[i]])
        }), sep = "/")))
    } else {
        model <- fit[[i]][[ncol(fit[[i]])]][[1]]$fit$model
    }
    list(pred = pred, spec = mod_spec, model = model) |> structure(
        class = "summary_inz_frct",
        model = class(fit[[i]][[ncol(fit[[i]])]][[1]]$fit),
        ci = attributes(object)$confint_width * 100
    )
}


#' @param x A `summary_inz_frct` object containing forecast summaries.
#' @param show_details Logical; set to `TRUE` to show model details only when
#'        `pred_model` is an "ARIMA" model.
#' @param ... Additional arguments (ignored).
#'
#' @rdname forecastsummary
#'
#' @export
print.summary_inz_frct <- function(x, show_details = FALSE, ...) {
    cat(sprintf("\n%0.f%% Prediction Interval\n", attributes(x)$ci))
    print(dplyr::rename(x$pred,
        Time = index, Fitted = .mean, Lower = .lower, Upper = .upper
    ), n = Inf)
    cat("\nModel:\n")
    print(x$spec)
    cat("\n")
    if (show_details && attributes(x)$model == "ARIMA") {
        print(x$model)
    }
}
