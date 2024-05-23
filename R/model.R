#' @export
fit_model <- function(data, x, method = c("arima", "ets"), ...) {
    method <- match.arg(method)
    fit <- switch(method,
        "arima" = fabletools::model(data, fit = fable::ARIMA(rlang::enquo(x), ...)),
        "ets" = fabletools::model(data, fit = fable::ETS(rlang::enquo(x), ...))
    )
    attr(fit, "method") <- method
    attr(fit, "checked") <- FALSE
    class(fit) <- c("inzts_fit", class(fit))
    fit
}

#' @export
print.inzts_fit <- function(x, ...) {
    cat("Model: ", attr(x, "method"), "\n")
    print.default(x, ...)

    if (!attr(x, "checked")) {
        cat("Warning: model has not been checked for assumptions.\n")
    }
}
