#' This function plots the seasonal components of a time series together
#' with the estimated seasonal effects of that series.
#'
#' The resulting window will contain two plots. On the left, every
#' seasonal subseries of the time series is plotted. On the right will be
#' the average seasonal effect of the series.
#'
#' @title Plot Seasonal Subseries from a Time Series
#'
#' @param obj an \code{iNZightTS} object
#' @param ... Further arguments to be passed onto specific methods.
#' @return No return value, called for the side effect of drawing a plot.
#'
#' @seealso \code{\link{iNZightTS}}
#'
#' @examples
#' ts <- iNZightTS(visitorsQ)
#' seasonplot(ts)
#'
#' @export
seasonplot <- function(obj, ...)
    UseMethod("seasonplot")

#' @export
seasonplot.iNZightTS <- function(obj, multiplicative = FALSE, t = 10, model.lim = NULL,
                                 ylab = obj$currVar, ...) {

    # if there is no season component to the ts, can't create season plot
    if (length(obj$start) == 1)
        return("Time Series does not have a seasonal component")

    multiplicative <- is_multiplicative(obj$tsObj, multiplicative)

    ## Convert tsobject to a dataframe
    freq <- obj$freq

    if (freq == 12) {
        labs <- month.abb
        xlab <- "Month"
    } else if (freq == 4) {
        # labs <- sprintf("%s - %s",
        #     month.abb[c(1, 4, 7, 10)],
        #     month.abb[c(3, 6, 9, 12)]
        # )
        labs <- paste0("Q", 1:4)
        xlab <- "Quarter"
    } else if (freq == 7) {
        labs <- c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
        xlab <- "Day"
    } else {
        labs <- 1:freq
        xlab <- "Season"
    }

    obj <- decompose(obj, ylab = ylab,
        multiplicative = multiplicative,
        t = t,
        model.lim = model.lim
    )

    td <- data.frame(
        Date = as.numeric(time(obj$tsObj)),
        value = as.matrix(obj$tsObj),
        trend = as.numeric(obj$decompVars$components[, "trend"]),
        seasonal = as.numeric(obj$decompVars$components[, "seasonal"]),
        residual = as.numeric(obj$decompVars$components[, "remainder"]),
        stringsAsFactors = TRUE
    )
    td <- dplyr::mutate(td,
            effect = if (multiplicative) {
                .data$value / .data$trend
            } else {
                .data$value - .data$trend
            },
            a = floor(.data$Date) - obj$start[1] + 1,
            b = .data$Date %% 1 * freq + 1
        )

    p1 <- ggplot(td, aes_(~b, ~value, colour = ~a, group = ~a)) +
        geom_point() +
        geom_path() +
        scale_colour_gradient(
            low = "darkorange", high = "blue", guide = "none"
        ) +
        labs(
            title = sprintf("Seasonal plot for %s", obj$currVar),
            x = xlab,
            y = ylab
        ) +
        scale_x_continuous(
            breaks = seq_along(labs),
            minor_breaks = NULL,
            labels = labs
        ) +
        geom_text(
            aes_(label = ~floor(Date)),
            data = td %>% dplyr::filter(.data$b == 1),
            nudge_x = -0.25
        ) +
        geom_text(
            aes_(label = ~floor(Date)),
            data = td %>% dplyr::filter(.data$b == freq),
            nudge_x = 0.25
        )

    ## RHS: seasonal effects
    s <- obj$start[2]
    if (!is.null(model.lim)) {
        tt <- time(obj$decompVars$components)
        s <- (tt[1] - floor(tt[1])) * freq + 1
    }
    season <-
        if (s > 1) td$season[-(1:(freq + 1 - s))][1:freq]
        else td$season[1:freq]
    season <- data.frame(b = 1:freq, effect = season, a = 1, stringsAsFactors = TRUE)

    p2 <- ggplot(td, aes_(
            ~b,
            ~effect - as.integer(multiplicative),
            group = ~a
        )) +
        geom_path(colour = "gray") +
        geom_path(data = season) +
        geom_point(data = season, pch = 21, fill = "white",
            stroke = 1.5, size = 1.5) +
        geom_hline(yintercept = 0, colour = "gray", linetype = 2) +
        labs(
            title = sprintf("%s seasonal effects",
                ifelse(multiplicative, "Multiplicative", "Additive")
            ),
            x = xlab,
            y = ylab
        ) +
        scale_x_continuous(
            breaks = seq_along(labs),
            minor_breaks = NULL,
            labels = labs
        )
    if (multiplicative)
        p2 <- p2 + scale_y_continuous(
            labels = function(y) y + 1
        )

    dev.hold()
    on.exit(dev.flush())

    ##############################################
    ## There's a bug in egg/windows cairoDevice
    ## resulting in `egg::ggarrange()` not showing
    ## one or both of the graphs (when the plot device
    ## is small).
    ## As a temporary fix, using `grid.arrange()` which
    ## has no issues since both graphs have the same
    ## x-axis ticks + labels (so panel sizes are equal!)
    # egg::ggarrange(p1, p2, nrow = 1)
    gridExtra::grid.arrange(p1, p2, nrow = 1)
}


#' @export
seasonplot.inz_ts <- function(x, var = NULL, mult_fit = FALSE, plot = TRUE, ...) {
    var <- guess_plot_var(x, !!enquo(var))
    spec <- list(...) ## Additional arguments passed to feasts::gg_season()

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

    if (length(var) < 2) {
        p1 <- expr(feasts::gg_season(x, !!sym(var), labels = l, !!!spec)) %>%
            rlang::new_quosure() %>%
            rlang::eval_tidy() +
            geom_point() +
            ggplot2::ylim(mean(range(x[[var]])) + c(-.5, .5) * y_span) +
            ggplot2::labs(title = "Seasonal plot", x = "")
        p2 <- x_dcmp_ls[[1]] %>%
            plot(
                ylim = (if (mult_fit) NULL else c(-.5, .5) * y_span),
                title = "Additive seasonal effects"
            )
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
                plot(
                    ylim = (if (mult_fit) NULL else c(-.5, .5) * y_span[i]),
                    title = ifelse(i == 1L, "Additive seasonal effects", "")
                )
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
                mult_fit ~ !!season * remainder - 1,
                TRUE ~ !!season + remainder
            )) %>%
            structure(class = c("seas_ts", class(.)), mult_fit = mult_fit)
    })
}


#' @export
plot.seas_ts <- function(x, ylim = NULL, title = NULL) {
    seas_aes <- aes(
        x = !!tsibble::index(x),
        y = !!sym(names(attributes(x)$seasons)) - attributes(x)$mult_fit
    )
    p <- feasts::gg_season(x, season_effect) +
        geom_hline(yintercept = 0, colour = "gray", linetype = 2) +
        geom_line(seas_aes) +
        geom_point(seas_aes, pch = 21, fill = "white", stroke = 1.5) +
        scale_y_continuous(limits = ylim) +
        ggplot2::labs(title = title, x = "", y = tsibble::measured_vars(x)[1])

    p$mapping$colour <- NULL
    p$layers[[1]]$aes_params$alpha <- .2

    p
}
