#' Plot seasonal subseries from a time series
#'
#' This function plots the seasonal components of a time series together
#' with the estimated seasonal effects of that series.
#'
#' The resulting window will contain two plots. On the left, every
#' seasonal subseries of the time series is plotted. On the right will be
#' the average seasonal effect of the series.
#'
#' @param x An `inzightts` (`inz_ts`) object representing the time series.
#' @param ... Further arguments to be passed onto specific methods and the
#'        `gg_season` function.
#' @return A `patchwork` object of seasonal plots.
#'
#' @rdname seasonplot
#'
#' @seealso \code{\link[feasts]{gg_season}}
#'
#' @examples
#' \dontrun{
#' seasonplot(inzightts(visitorsQ))
#' }
#'
#' @export
seasonplot <- function(x, ...) {
    UseMethod("seasonplot")
}


#' @export
seasonplot.inz_ts <- function(x, var = NULL, t = 0, mult_fit = FALSE,
                              model_range = NULL, ...) {
    var <- guess_plot_var(x, !!enquo(var))
    spec <- list(...)
    if (all(is.na(model_range))) model_range <- NULL
    if (!is.null(spec$labels)) {
        l <- spec$labels
        spec <- within(spec, rm(labels))
    } else {
        l <- "none"
    }
    if (!is.null(model_range)) {
        if (!all(length(model_range) == 2, any(is.numeric(model_range), inherits(model_range, "Date")))) {
            rlang::abort("model_range must be a numeric or Date vector of length 2.")
        }
        na_i <- which(is.na(model_range))[1]
        t_range <- range(x$index)
        if (!is.numeric(x[[tsibble::index_var(x)]]) && is.numeric(model_range)) {
            model_range[na_i] <- lubridate::year(
                if (!is.na(na_i) && as.logical(na_i - 1)) {
                    dplyr::last(x$index)
                } else {
                    x$index[1]
                }
            )
            model_range <- lubridate::ymd(paste0(model_range, c("0101", "1231")))
            x <- dplyr::filter(x, dplyr::between(lubridate::as_date(index), model_range[1], model_range[2]))
        } else if (is.numeric(x[[tsibble::index_var(x)]]) && inherits(model_range, "Date")) {
            model_range[na_i] <- lubridate::ymd(paste0(ifelse(na_i - 1, dplyr::last(x$index), x$index[1]), "0101"))
            x <- dplyr::filter(x, dplyr::between(index, lubridate::year(model_range[1]), lubridate::year(model_range[2])))
        } else {
            model_range[na_i] <- if (!is.na(na_i) && as.logical(na_i - 1)) dplyr::last(x$index) else x$index[1]
            x <- dplyr::filter(x, dplyr::between(index, model_range[1], model_range[2]))
        }
    }
    if (length(var) < 3) {
        var <- dplyr::last(as.character(var))
    } else {
        var <- var[-1]
    }
    x_dcmp_ls <- season_effect(x, var, t, mult_fit)
    y_span <- unlist(lapply(seq_along(var), function(i) {
        diff(extendrange(x_dcmp_ls[[i]][[as.character(var)[i]]])) |>
            max(diff(extendrange(x[[as.character(var)[i]]])))
    }))

    # quick fix: Compute y-axis limits based on actual season_effect values for each variable
    # This ensures all data points are visible with 5% padding on each side
    eff_y_lim <- lapply(seq_along(var), function(i) {
        season_eff_range <- range(x_dcmp_ls[[i]]$season_effect, na.rm = TRUE)
        extendrange(season_eff_range, f = 0.05)
    })
    if (length(var) < 2) {
        p1 <- rlang::inject(ggtime::gg_season(x, !!sym(var), labels = l, !!!spec)) +
            geom_point() +
            ggplot2::ylim(mean(range(x[[var]])) + c(-.5, .5) * y_span) +
            ggplot2::labs(title = "Seasonal plot", x = "")
        if (tsibble::n_keys(x) > 1) {
            p1$facet$params$rows -> cols
            p1$facet$params$cols -> rows
            p1$facet$params$rows <- rows
            p1$facet$params$cols <- cols
        }
        p2 <- x_dcmp_ls[[1]] |>
            plot(ylim = eff_y_lim[[1]], title = "Seasonal effects")
        p <- patchwork::wrap_plots(p1, p2, nrow = 1)
    } else {
        p_ls <- lapply(seq_along(var), function(i) {
            p1 <- rlang::inject(ggtime::gg_season(x, !!sym(var[i]), labels = l, !!!spec)) +
                geom_point() +
                ggplot2::ylim(mean(range(x[[var[i]]])) + c(-.5, .5) * y_span[i]) +
                ggplot2::labs(title = ifelse(i == 1L, "Seasonal plot", ""), x = "")
            if (tsibble::n_keys(x) > 1) {
                p1$facet$params$rows -> cols
                p1$facet$params$cols -> rows
                p1$facet$params$rows <- rows
                p1$facet$params$cols <- cols
            }
            p2 <- x_dcmp_ls[[i]] |>
                plot(ylim = eff_y_lim[[i]], title = "Seasonal effects")
            patchwork::wrap_plots(p1, p2, nrow = 1)
        })
        p <- rlang::inject(patchwork::wrap_plots(!!!p_ls)) +
            patchwork::plot_layout(ncol = 1)
    }
    if (tsibble::n_keys(x) > 1) {
        p <- p + patchwork::plot_layout(guides = "collect", widths = c(1.6, 1)) &
            ggplot2::theme(legend.position = "bottom")
    }
    p
}


season_effect <- function(x, var, t, mult_fit = FALSE) {
    if (tsibble::n_keys(x) > 1) {
        x <- dplyr::mutate(x, dplyr::across(
            !!tsibble::key_vars(x), function(x) {
                factor(x, levels = sort(unique(x)))
            }
        ))
    }
    lapply(as.character(var), function(v) {
        if (tsibble::n_keys(x) > 1) {
            x_dcmp <- decomp_key(x, v, "stl", mult_fit, t = t)
        } else {
            x_dcmp <- decomp(x, v, "stl", mult_fit, t = t)
        }
        season <- sym(names(attributes(x_dcmp)$seasons))
        x_dcmp |>
            dplyr::mutate(season_effect = if (mult_fit) !!season * remainder else !!season + remainder) |>
            (\(.) structure(.,
                class = c("seas_ts", class(.)),
                mult_fit = mult_fit,
                seasons = attributes(x_dcmp)$seasons
            ))()
    })
}


#' @export
plot.seas_ts <- function(x, ylim = NULL, title = NULL, ...) {
    mult_fit <- attributes(x)$mult_fit
    seas_aes <- aes(
        x = !!tsibble::index(x),
        y = !!sym(names(attributes(x)$seasons)),
        col = !!(if (tsibble::n_keys(x) > 1) sym(".key") else NULL),
        group = !!(if (tsibble::n_keys(x) > 1) sym(".key") else NULL)
    )
    if (tsibble::n_keys(x) > 1) {
        x <- dplyr::mutate(x,
            .key = rlang::inject(interaction(!!!({
                key_vars <- tsibble::key_vars(x)
                lapply(key_vars[key_vars != ".model"], function(i) x[[i]])
            }), sep = "/"))
        ) |>
            tsibble::update_tsibble(key = .key)
    }
    p <- ggtime::gg_season(x, season_effect, ...) +
        ggplot2::labs(y = if (mult_fit) "Multiplicative effect" else "Additive effect")
    if (tsibble::n_keys(x) > 1) {
        p$layers[[1]] <- NULL
        p <- p + geom_line(aes(index, season_effect, group = interaction(id, .key)))
    }
    p$mapping$colour <- NULL
    p <- p + geom_hline(
        yintercept = as.numeric(attributes(x)$mult_fit),
        colour = "gray", linetype = 2
    ) +
        geom_line(seas_aes) +
        geom_point(seas_aes, pch = 21, fill = "white", stroke = 1.5) +
        scale_y_continuous(limits = ylim) +
        ggplot2::labs(title = title, x = "")
    if (tsibble::n_keys(x) > 1) {
        p <- suppressMessages(p + scale_colour_discrete(drop = FALSE)) +
            facet_wrap(NULL) +
            ggplot2::theme(
                strip.background = element_blank(),
                strip.text = element_blank()
            )
        p$mapping$colour <- p$layers[[3]]$mapping$colour
    }
    p$layers[[1]]$aes_params$alpha <- .2
    p
}
