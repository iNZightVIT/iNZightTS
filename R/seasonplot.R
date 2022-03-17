#' This function plots the seasonal components of a time series together
#' with the estimated seasonal effects of that series.
#'
#' The resulting window will contain two plots. On the left, every
#' seasonal subseries of the time series is plotted. On the right will be
#' the average seasonal effect of the series.
#'
#' @title Plot Seasonal Subseries from a Time Series
#'
#' @param x an inzightts (\code{inz_ts}) object
#' @param ... further arguments to be passed onto specific methods
#'            and the \code{gg_season} function
#' @return A \code{patchwork} object of seasonal plots
#'
#' @rdname seasonplot
#'
#' @seealso \code{\link[feasts]{gg_season}}
#'
#' @examples
#' seasonplot(inzightts(visitorsQ))
#'
#' @export
seasonplot <- function(x, ...) {
    UseMethod("seasonplot")
}


#' @export
seasonplot.inz_ts <- function(x, var = NULL, mult_fit = FALSE, plot = TRUE, ...) {
    var <- guess_plot_var(x, !!enquo(var))
    spec <- list(...)

    if (!is.null(spec$labels)) {
        l <- spec$labels
        spec <- within(spec, rm(labels))
    } else {
        l <- "both"
    }
    if (length(var) < 3) {
        var <- dplyr::last(as.character(var))
    } else {
        var <- var[-1]
    }

    x_dcmp_ls <- season_effect(x, var, mult_fit)
    y_span <- unlist(lapply(seq_along(var), function(i) {
        diff(extendrange(x_dcmp_ls[[i]][[as.character(var)[i]]])) %>%
            max(diff(extendrange(x[[as.character(var)[i]]])))
    }))
    if (mult_fit) {
        eff_y_span <- seq_along(var) %>%
            lapply(function(i) abs(range(x_dcmp_ls[[i]]$season_effect) - 1)) %>%
            unlist() %>%
            max()
        eff_y_lim <- replicate(length(var), 1 + c(-1.05, 1.05) * eff_y_span, FALSE)
    } else {
        eff_y_lim <- lapply(seq_along(var), function(i) c(-.5, .5) * y_span[i])
    }

    if (length(var) < 2) {
        p1 <- expr(feasts::gg_season(x, !!sym(var), labels = l, !!!spec)) %>%
            rlang::new_quosure() %>%
            rlang::eval_tidy() +
            geom_point() +
            ggplot2::ylim(mean(range(x[[var]])) + c(-.5, .5) * y_span) +
            ggplot2::labs(title = "Seasonal plot", x = "")
        p2 <- x_dcmp_ls[[1]] %>%
            plot(ylim = eff_y_lim[[1]], title = "Additive seasonal effects")
        p <- patchwork::wrap_plots(p1, p2, nrow = 1)
    } else {
        p_ls <- lapply(seq_along(var), function(i) {
            p1 <- expr(feasts::gg_season(x, !!sym(var[i]), labels = l, !!!spec)) %>%
                rlang::new_quosure() %>%
                rlang::eval_tidy() +
                geom_point() +
                ggplot2::ylim(mean(range(x[[var[i]]])) + c(-.5, .5) * y_span[i]) +
                ggplot2::labs(title = ifelse(i == 1L, "Seasonal plot", ""), x = "")
            p2 <- x_dcmp_ls[[i]] %>%
                plot(ylim = eff_y_lim[[i]], title = "Additive seasonal effects")
            patchwork::wrap_plots(p1, p2, nrow = 1)
        })
        p <- expr(patchwork::wrap_plots(!!!p_ls)) %>%
            rlang::new_quosure() %>%
            rlang::eval_tidy() +
            patchwork::plot_layout(ncol = 1)
    }

    if (plot) print(p)

    invisible(p)
}


season_effect <- function(x, var, mult_fit = FALSE) {
    lapply(as.character(var), function(v) {
        x_dcmp <- decomp(x, v, "stl", mult_fit)
        season <- sym(names(attributes(x_dcmp)$seasons))
        x_dcmp %>%
            dplyr::mutate(season_effect = dplyr::case_when(
                mult_fit ~ !!season * remainder,
                TRUE ~ !!season + remainder
            )) %>%
            structure(class = c("seas_ts", class(.)), mult_fit = mult_fit)
    })
}


#' @export
plot.seas_ts <- function(x, ylim = NULL, title = NULL, ...) {
    seas_aes <- aes(!!tsibble::index(x), !!sym(names(attributes(x)$seasons)))
    p <- feasts::gg_season(x, season_effect, ...) +
        geom_hline(
            yintercept = as.numeric(attributes(x)$mult_fit),
            colour = "gray", linetype = 2
        ) +
        geom_line(seas_aes) +
        geom_point(seas_aes, pch = 21, fill = "white", stroke = 1.5) +
        scale_y_continuous(limits = ylim) +
        ggplot2::labs(title = title, x = "", y = tsibble::measured_vars(x)[1])

    p$mapping$colour <- NULL
    p$layers[[1]]$aes_params$alpha <- .2

    p
}
