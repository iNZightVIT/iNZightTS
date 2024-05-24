#' @export
fit_model <- function(data, x, method = c("arima", "ets"), ...) {
    method <- match.arg(method)
    fit <- switch(method,
        "arima" = fabletools::model(data,
            fit = fable::ARIMA(!!rlang::enquo(x), ...)
        ),
        "ets" = fabletools::model(data,
            fit = fable::ETS(!!rlang::enquo(x), ...)
        )
    )
    attr(fit, "method") <- method
    attr(fit, "checked") <- FALSE
    class(fit) <- c("inzts_fit", class(fit))
    fit
}

#' @export
print.inzts_fit <- function(x, ...) {
    cli::cli_h3("Model of { attr(x, 'response') }")
    # print.default(x, ...)

    if (!attr(x, "checked")) {
        cli::cli_warn("Model has not been checked for assumptions.\n")
    }
}

#' @export
plot.inzts_fit <- function(x, ...) {
    f <- x[[1]][[1]]
    d <- dplyr::bind_cols(f$data, f$fit$est)

    p <- ggplot2::autoplot(d) +
        ggplot2::geom_line(
            ggplot2::aes(y = .fitted),
            col = "green4"
        )

    if (!attr(x, "checked")) {
        p <- p + ggplot2::labs(
            subtitle = "Model has not been checked for assumptions."
        ) +
            ggplot2::theme(
                plot.subtitle = ggplot2::element_text(
                    face = "italic",
                    color = "red"
                )
            )
    }

    p
}

check <- function(x, ...) {
    if (attr(x, "checked")) {
        return(x)
    }
}
