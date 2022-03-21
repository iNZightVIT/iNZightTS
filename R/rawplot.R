guess_plot_var <- function(x, var, tidy = FALSE, use = "Plot") {
    if (rlang::quo_is_null(enquo(var))) {
        mv <- tsibble::measured_vars(x)
        pos <- which(vapply(x[mv], is.numeric, logical(1L)))
        if (rlang::is_empty(pos)) {
            rlang::abort("Could not automatically identify an appropriate plot variable, please specify the variable to plot.")
        }
        rlang::inform(sprintf(
            paste(use, "variable not specified, automatically selected `var = %s`"),
            mv[pos[1]]
        ))
        sym(mv[pos[1]])
    } else if (tidy) {
        rlang::get_expr(enexpr(var))
    } else {
        c("", as.character(rlang::eval_tidy(enexpr(var))))
    }
}


#' Draws a plot of a given inzightts (\code{inz_ts}) object with the trend superimposed.
#'
#' @title Draw a simple time series plot
#'
#' @param x an inzightts (\code{inz_ts}) object
#' @param var a character vector of the variable(s) to be plotted, or \code{NULL}
#' @param xlab a title for the x axis
#' @param ylab a title for the y axis
#' @param title a title for the graph
#' @param plot logical, if \code{FALSE}, the graph isn't drawn
#' @param xlim axis limits, specified as dates or years
#' @param aspect the aspect ratio of the plot;
#'        it will be about \code{aspect} times wider than it is high
#' @param compare logical (not used)
#' @param smoother logical, if \code{TRUE} the smoother will be drawn
#' @param sm_model the smoothing method to be used
#' @param mult_fit If \code{TRUE}, a multiplicative model is used, otherwise
#'        an additive model is used by default.
#' @param ... additional arguments (ignored)
#' @return a time series plot (constructed with ggplot2) is returned invisibly,
#'         which can be added to if desired.
#'
#' @rdname rawplot
#'
#' @keywords timeseries
#'
#' @import ggplot2
#'
#' @examples
#' t <- inzightts(visitorsQ, var = c(2, 4))
#' plot(t)
#' plot(t, var = names(t)[-1])
#' plot(t, var = "Japan")
#' plot(t, mult_fit = TRUE)
#'
#' @export
plot.inz_ts <- function(x, var = NULL, xlab = NULL, ylab = NULL, title = NULL,
                        plot = TRUE, xlim = NULL, aspect = NULL, compare = TRUE,
                        smoother = TRUE, sm_model = "stl", mult_fit = FALSE, ...) {
    var <- guess_plot_var(x, !!enquo(var))
    if (all(is.na(xlim))) xlim <- NULL

    if (!compare) { ## Placeholder, to be implemented
        compare <- TRUE
        rlang::warn("Feature compare = FALSE is to be implemented.")
    }

    y_obs <- unlist(lapply(dplyr::case_when(
        length(as.character(var)) > 2 ~ as.character(var)[-1],
        TRUE ~ dplyr::last(as.character(var))
    ), function(i) x[[i]]))
    if (any(y_obs <= 0) & mult_fit) {
        mult_fit <- !mult_fit
        rlang::warn("Non-positive obs detected, setting `mult_fit = FALSE`")
    }
    if (!is.null(xlim)) {
        if (!all(length(xlim) == 2, any(is.numeric(xlim), methods::is(xlim, "Date")))) {
            rlang::abort("xlim must be a numeric or Date vector of length 2.")
        }
        na_i <- which(is.na(xlim))[1]
        t_range <- range(x$index)
        if (!is.numeric(x[[tsibble::index_var(x)]]) & is.numeric(xlim)) {
            xlim[na_i] <- lubridate::year(dplyr::case_when(
                as.logical(na_i - 1) ~ dplyr::last(x$index),
                TRUE ~ x$index[1]
            ))
            xlim <- lubridate::ymd(paste0(xlim, c("0101", "1231")))
            x <- dplyr::filter(x, dplyr::between(lubridate::as_date(index), xlim[1], xlim[2]))
        } else if (is.numeric(x[[tsibble::index_var(x)]]) & methods::is(xlim, "Date")) {
            xlim[na_i] <- lubridate::ymd(paste0(ifelse(na_i - 1, dplyr::last(x$index), x$index[1]), "0101"))
            x <- dplyr::filter(x, dplyr::between(index, lubridate::year(xlim[1]), lubridate::year(xlim[2])))
        } else {
            xlim[na_i] <- dplyr::case_when(
                as.logical(na_i - 1) ~ dplyr::last(x$index),
                TRUE ~ x$index[1]
            )
            x <- dplyr::filter(x, dplyr::between(index, xlim[1], xlim[2]))
        }
    }
    if (is.null(xlab)) {
        xlab <- dplyr::case_when(
            is.numeric(x$index) ~ "Year",
            TRUE ~ stringr::str_to_title(class(x$index)[1])
        )
    }
    x <- dplyr::rename(x, !!dplyr::first(xlab) := index)

    if (is.null(ylab)) {
        ylab <- as.character(var)
        if (length(var) > 1) ylab <- ylab[-1]
    }
    if (is.null(title)) {
        title <- dplyr::case_when(
            tsibble::n_keys(x) > 1 | length(var) > 2 ~ "",
            TRUE ~ dplyr::last(as.character(var))
        )
    }

    if (length(var) > 2) {
        if (!isTRUE(all.equal(length(var) - 1, length(ylab)))) {
            rlang::abort("var and ylab should have the same length.")
        }
        if (!is.null(aspect) | (!compare & tsibble::n_keys(x) > 1)) {
            rlang::warn("Aspect ratio is automatic for multi-series plot.")
        }
    }

    if (length(var) < 3) {
        if (!compare & tsibble::n_keys(x) > 1) aspect <- NULL
        var <- sym(dplyr::last(as.character(var)))
        p <- plot_inzightts_var(
            x, var, xlab, ylab, title, aspect,
            compare, smoother, sm_model, mult_fit
        )
    } else {
        p_ls <- lapply(seq_len(length(var) - 1), function(i) {
            y_var <- as.character(var)[i + 1]
            plot_inzightts_var(
                x, sym(y_var), xlab, ylab[i], "", NULL,
                compare, smoother, sm_model, mult_fit
            )
        })
        p <- expr(patchwork::wrap_plots(!!!p_ls)) %>%
            rlang::new_quosure() %>%
            rlang::eval_tidy() +
            patchwork::plot_layout(ncol = 1, guides = "collect") +
            patchwork::plot_annotation(title = title)
    }

    if (plot) print(p)

    invisible(p)
}


plot_inzightts_var <- function(x, var, xlab, ylab, title, aspect,
                               compare, smoother, sm_model, mult_fit) {
    p <- fabletools::autoplot(x, !!var, size = 1) +
        ggplot2::labs(y = ylab, title = title) +
        ggplot2::theme(
            legend.position = dplyr::case_when(compare ~ "right", TRUE ~ "none"),
            legend.title = element_blank()
        )

    if (!is.null(aspect)) {
        y_var <- dplyr::last(as.character(var))
        p <- p + coord_fixed(
            ratio = diff(range(lubridate::as_date(x[[xlab]]), na.rm = TRUE)) /
                diff(range(x[[y_var]], na.rm = TRUE)) / aspect
        )
    }

    if (smoother) {
        smoother_spec <- list(
            mapping = aes(!!tsibble::index(x), trend),
            data = decomp(x, as.character(var), sm_model, mult_fit),
            linetype = ifelse(compare & tsibble::n_keys(x) > 1, "dashed", "solid"),
            size = ifelse(compare & tsibble::n_keys(x) > 1, 1, .5)
        ) %>% c(
            col = if (compare & tsibble::n_keys(x) > 1) NULL else "red"
        )
        p <- expr(p + geom_line(!!!smoother_spec)) %>%
            rlang::new_quosure() %>%
            rlang::eval_tidy()
    }

    p
}
