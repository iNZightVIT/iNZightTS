guess_plot_var <- function(x, var, tidy = FALSE, use = "Plot") {
    if (rlang::quo_is_null(enquo(var))) {
        mv <- tsibble::measured_vars(x)
        pos <- which(vapply(x[mv], is.numeric, logical(1L)))
        if (rlang::is_empty(pos)) {
            rlang::abort("Could not automatically identify an appropriate plot variable, please specify the variable to plot.")
        }
        rlang::inform(sprintf(
            paste(use, 'variable not specified, automatically selected `var = "%s"`'),
            mv[pos[1]]
        ))
        sym(mv[pos[1]])
    } else if (tidy) {
        rlang::get_expr(enexpr(var))
    } else {
        ## !IMPORTANT! A dummy empty string `""` is added before `var` if
        ## it is passed to the `var` argument from a character vector
        c("", as.character(rlang::eval_tidy(enexpr(var))))
    }
}


cat_x_axis <- function(x) {
    idx <- x[[tsibble::index(x)]]
    if (!any(inherits(idx, "vctrs_vctr"), is.numeric(idx))) {
        rlang::abort("Unsupported index format for categorical plot.")
    }
    if (is.numeric(idx)) {
        fun <- function(x) x
    } else {
        fun <- function(x) lubridate::year(x)
    }
    list(fun = fun, name = "Year")
}


cat_y_axis <- function(x) {
    idx <- x[[tsibble::index(x)]]
    if (!any(inherits(idx, "vctrs_vctr"), is.numeric(idx))) {
        rlang::abort("Unsupported index format for categorical plot.")
    }
    if (is.numeric(idx)) {
        fun <- function(x) rep("", length(x))
        name <- ""
    } else {
        if (inherits(idx, "yearquarter")) {
            fun <- function(x) lubridate::quarter(x)
        } else if (inherits(idx, "yearmonth")) {
            fun <- function(x) lubridate::month(x, label = TRUE)
        } else if (inherits(idx, "yearweek")) {
            fun <- function(x) lubridate::week(x)
        }
        name <- is.numeric(idx) %>%
            ifelse("", substring(class(idx)[1], 5)) %>%
            stringr::str_to_title() %>%
            paste("of Year")
    }
    list(fun = fun, name = name)
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
#' @param xlim axis limits, specified as dates or years
#' @param aspect the aspect ratio of the plot;
#'        it will be about \code{aspect} times wider than it is high
#' @param compare logical, whether to plot the key levels in a single plot
#' @param pal (only if a categorical variable is passed to \code{var}): the
#'        colour palette for the categorical plot, the palette vector should
#'        be in the same order per the rows of \code{tsibble::key_data(x)}.
#' @param smoother logical, if \code{TRUE} the smoother will be drawn
#' @param sm_model the smoothing method to be used
#' @param mult_fit If \code{TRUE}, a multiplicative model is used, otherwise
#'        an additive model is used by default.
#' @param emphasise integer vector to specify the key level(s) to focus in the
#'        plot. The integer maps to the specific key level(s) corresponding to
#'        the ith row of \code{tsibble::key_data(x)}.
#' @param non_emph_opacity numeric. If \code{(0, 1]}, this argument determines
#'        the opacity of the series other than the focused one(s) (to highlight
#'        the focused series). If \code{non_emph_opacity = 0}, the plot draws
#'        the focused series in its own scales.
#' @param show_iso_obs logical, whether to plot isolated observations between
#'        time series gaps or not, if there is any
#' @param iso_obs_size numeric, scaling the size of isolated observations, if
#'        \code{show_iso_obs = TRUE} and they exists
#' @param ... additional arguments (ignored)
#' @return a time series plot (constructed with ggplot2) is returned, which
#'         can be added to if desired.
#'
#' @seealso \code{\link[tsibble]{key_data}}
#'
#' @rdname rawplot
#'
#' @keywords timeseries
#'
#' @import ggplot2
#'
#' @examples
#' t <- inzightts(visitorsQ, var = c(2, 4))
#'
#' \dontrun{
#' plot(t)
#' plot(t, var = names(t)[-1])
#' plot(t, var = "Japan")
#' plot(t, mult_fit = TRUE)
#' }
#'
#' @export
plot.inz_ts <- function(x, var = NULL, xlab = NULL, ylab = NULL, title = NULL,
                        xlim = NULL, aspect = NULL, compare = TRUE, pal = NULL,
                        smoother = TRUE, sm_model = "stl", mult_fit = FALSE,
                        emphasise = NULL, non_emph_opacity = .2,
                        show_iso_obs = TRUE, iso_obs_size = 1, ...) {
    var <- guess_plot_var(x, !!enquo(var))
    if (all(is.na(xlim))) xlim <- NULL
    var_has_num <- any(unlist(lapply(var, function(v) is.numeric(x[[v]]))))
    var_has_cat <- any(unlist(lapply(var, function(v) {
        is.factor(x[[v]]) | is.character(x[[v]])
    })))
    if (!show_iso_obs) iso_obs_size <- -1

    if (tsibble::n_keys(x) > 1) {
        x <- x %>%
            dplyr::mutate(dplyr::across(
                !!tsibble::key_vars(.), function(x) {
                    factor(x, levels = sort(unique(x)))
                }
            ))
    }
    if (var_has_cat) {
        if (var_has_num) {
            rlang::abort("Please pass only numeric or character/factor to `var`.")
        }
        if (length(var) > 2) {
            var <- c("", dplyr::first(var[var != ""]))
            rlang::warn("Please specify one character/factor at a time.")
        }
        x <- x %>% dplyr::mutate(
            !!dplyr::last(var) := rlang::eval_tidy(rlang::new_quosure(expr(interaction(!!!({
                c(lapply(tsibble::key_vars(.), function(i) .[[i]]), list(.[[dplyr::last(var)]]))
            }), sep = "/"))))
        )
        if (tsibble::n_keys(x) > 1 & is.null(emphasise)) {
            emphasise <- 1L
            rlang::warn("Key detected when plotting category, setting `emphasise = 1L`")
        }
        non_emph_opacity <- 0
    } else {
        y_obs <- unlist(lapply(ifelse(
            length(as.character(var)) > 2,
            c("", as.character(var)[-1]),
            dplyr::last(as.character(var))
        ), function(i) x[[i]]))
        if (any(y_obs <= 0, na.rm = TRUE) & mult_fit) {
            mult_fit <- !mult_fit
            rlang::warn("Non-positive obs detected, setting `mult_fit = FALSE`")
        }
    }
    if (!is.null(xlim) & !var_has_cat) {
        if (!all(length(xlim) == 2, any(is.numeric(xlim), inherits(xlim, "Date")))) {
            rlang::abort("xlim must be a numeric or Date vector of length 2.")
        }
        na_i <- which(is.na(xlim))[1]
        if (!is.numeric(x[[tsibble::index_var(x)]]) & is.numeric(xlim)) {
            xlim[na_i] <- lubridate::year(dplyr::case_when(
                as.logical(na_i - 1) ~ dplyr::last(x$index),
                TRUE ~ x$index[1]
            ))
            xlim <- lubridate::ymd(paste0(xlim, c("0101", "1231")))
            x <- dplyr::filter(x, dplyr::between(lubridate::as_date(index), xlim[1], xlim[2]))
        } else if (is.numeric(x[[tsibble::index_var(x)]]) & inherits(xlim, "Date")) {
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
            length(var) > 2 ~ "",
            TRUE ~ dplyr::last(as.character(var))
        )
    }
    if (!any(is.null(emphasise), is.null(non_emph_opacity), tsibble::n_keys(x) == 1)) {
        emph <- list(
            data = tsibble::key_data(x)[emphasise, ],
            opacity = non_emph_opacity
        )
    } else {
        emph <- NULL
    }

    if (length(var) > 2) {
        if (!isTRUE(all.equal(length(var) - 1, length(ylab)))) {
            rlang::abort("var and ylab should have the same length.")
        }
        if (!is.null(aspect) | (!compare & tsibble::n_keys(x) > 1)) {
            rlang::warn("Aspect ratio is automatic for multi-series plot.")
        }
    }
    if (!is.null(aspect) & var_has_cat) {
        rlang::warn("Aspect ratio is automatic for categorical plot.")
    }

    (if (length(var) < 3) {
        if (!compare & tsibble::n_keys(x) > 1) aspect <- NULL
        var <- sym(dplyr::last(as.character(var)))
        plot_inzightts_var(
            x, var, xlab, ylab, title, aspect, emph, pal,
            compare, smoother, sm_model, mult_fit, iso_obs_size
        )
    } else {
        p_ls <- lapply(seq_len(length(var) - 1), function(i) {
            y_var <- as.character(var)[i + 1]
            plot_inzightts_var(
                x, sym(y_var), xlab, ylab[i], "", NULL, emph, pal,
                compare, smoother, sm_model, mult_fit, iso_obs_size
            )
        })
        expr(patchwork::wrap_plots(!!!p_ls)) %>%
            rlang::new_quosure() %>%
            rlang::eval_tidy() +
            patchwork::plot_layout(ncol = 1, guides = "collect") +
            patchwork::plot_annotation(title = title) &
            ggplot2::theme(legend.position = "bottom")
    }) %>% structure(use.plotly = ggplotable(.))
}


plot_inzightts_var <- function(x, var, xlab, ylab, title, aspect, emph, pal,
                               compare, smoother, sm_model, mult_fit, iso) {
    if (!is.null(emph)) {
        emph_data <- emph$data %>%
            dplyr::left_join(x, by = tsibble::key_vars(x), multiple = "all") %>%
            tsibble::as_tsibble(
                index = !!tsibble::index(x),
                key = !!tsibble::key_vars(x)
            )
        if (emph$opacity == 0) x <- emph_data
    }
    op <- ifelse(!is.null(emph), emph$opacity, 1)

    if (is.factor(x[[var]]) | is.character(x[[var]])) {
        return(plot_cat_var(x, var, title, pal))
    }

    p <- fabletools::autoplot(x, !!var, linewidth = 1, alpha = op) +
        ggplot2::labs(y = ylab, title = title) +
        ggplot2::theme(
            legend.position = dplyr::case_when(compare ~ "bottom", TRUE ~ "none"),
            legend.title = element_blank()
        )

    if (!is.null(emph)) {
        p <- suppressMessages(p + scale_colour_discrete(drop = FALSE)) +
            geom_line(data = emph_data, linewidth = 1)
    }

    if (!compare & tsibble::n_keys(x) > 1) {
        p <- p + facet_wrap(vars(!!!tsibble::key(x)), nrow = 1)
    }

    if (!is.null(aspect)) {
        y_var <- dplyr::last(as.character(var))
        p <- p + coord_fixed(
            ratio = diff(range(lubridate::as_date(x[[xlab]]), na.rm = TRUE)) /
                diff(range(x[[y_var]], na.rm = TRUE)) / aspect
        )
    }

    if (smoother) {
        if (tsibble::n_keys(x) > 1) {
            sm_data <- decomp_key(x, as.character(var), sm_model, mult_fit)
        } else {
            sm_data <- decomp(x, as.character(var), sm_model, mult_fit)
        }
        smoother_spec <- list(
            mapping = aes(!!tsibble::index(x), trend),
            data = sm_data,
            linetype = ifelse(compare & tsibble::n_keys(x) > 1, "dashed", "solid"),
            linewidth = .5,
            alpha = op
        )
        if (tsibble::n_keys(x) == 1) {
            smoother_spec <- c(smoother_spec, col = "red")
        }
        if (!all(is.na(sm_data$trend))) {
            p <- expr(p + geom_line(!!!smoother_spec)) %>%
                rlang::new_quosure() %>%
                rlang::eval_tidy()
        } else {
            rlang::warn("Time gaps in all (key) levels, turning off smoothers.")
        }
        if (!is.null(emph)) {
            emph_sm <- emph$data %>%
                dplyr::left_join(sm_data, by = tsibble::key_vars(x), multiple = "all")
            emph_sm_spec <- within(smoother_spec, rm(alpha, data))
            p <- expr(p + geom_line(data = emph_sm, !!!emph_sm_spec)) %>%
                rlang::new_quosure() %>%
                rlang::eval_tidy()
        }
    }

    iso_i <- which(diff(diff(is.na(c(NA, x[[var]])))) == 2)
    if (iso != -1 & length(iso_i) > 0) {
        p <- p + geom_point(data = x[iso_i, ], size = iso / 2) +
            ggplot2::labs(caption = "Isolated observations are plotted as dots")
    }

    p
}


plot_cat_var <- function(x, var, title, pal) {
    ini_row <- dplyr::select(x[1, ], !!var)
    idx <- tsibble::index(x)
    if (!is.numeric(x[[idx]])) {
        floor_idx <- suppressWarnings(ini_row[[idx]] %>%
            lubridate::year() %>%
            as.character() %>%
            getFromNamespace(class(x[[idx]])[1], "tsibble")())
        if (ini_row[[idx]] != floor_idx) {
            ini_row[[idx]] <- floor_idx
            ini_row[[var]] <- NA
            x <- x %>%
                tsibble::update_tsibble(key = NULL) %>%
                dplyr::bind_rows(ini_row) %>%
                tsibble::fill_gaps()
        }
    }
    p <- x %>%
        dplyr::mutate(
            .x = forcats::fct_inorder(as.character(
                cat_x_axis(.)$fun(!!tsibble::index(x))
            )),
            .y = forcats::fct_inorder(as.character(
                cat_y_axis(.)$fun(!!tsibble::index(x))
            )) %>% forcats::fct_rev()
        ) %>%
        dplyr::filter(!is.na(!!var)) %>%
        ggplot(aes(.x, .y, col = !!var)) +
        geom_tile(aes(fill = after_scale(col))) +
        ggplot2::theme(
            panel.grid = element_blank(),
            legend.title = element_blank(),
            legend.position = "bottom",
            axis.ticks = element_blank()
        ) +
        ggplot2::labs(
            x = cat_x_axis(x)$name,
            y = cat_y_axis(x)$name,
            title = title
        )
    if (!is.null(pal)) {
        if (is.function(pal)) {
            pal <- pal(length(unique(x[[var]])))
        }
        p <- p + scale_colour_manual(values = pal)
    }
    p
}


#' Check if a plot generated by iNZightTS2 can be passed to plotly::ggplotly().
#'
#' @title Preliminary check for a plotly::ggplotly() call
#'
#' @param x a \code{ggplot} object produced by iNZightTS2
#' @return a \code{logical}
#'
#' @rdname ggplotable
#'
#' @seealso \code{\link[plotly]{ggplotly}}
#'
#' @examples
#' x <- inzightts(visitorsQ)
#' \dontrun{
#' ggplotable(plot(x))
#' ggplotable(plot(x, names(x)[-1]))
#' }
#'
#' @export
ggplotable <- function(x) {
    !inherits(x, "patchwork")
}
