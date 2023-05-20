#' A time series plot faceted by seasonal period.
#'
#' @title Seasonal subseries plots for inzightts
#'
#' @param x an inzightts (\code{inz_ts}) or tsibble (\code{tbl_ts}) object
#' @param var a character vector of the variable(s) to be plotted, or \code{NULL}
#' @param show_mean logical, if \code{FALSE}, the mean line isn't drawn
#' @param xlab a title for the x axis
#' @param ylab a title for the y axis
#' @param title a title for the graph
#' @return a \code{numeric}
#'
#' @rdname subseries
#'
#' @seealso \code{\link[feasts]{gg_subseries}}
#'
#' @examples
#' t <- inzightts(visitorsQ)
#' \dontrun{
#' subseries(t)
#' }
#'
#' @export
subseries <- function(x, var = NULL, show_mean = TRUE, xlab = NULL,
                      ylab = NULL, title = NULL) {
    var <- guess_plot_var(x, !!enquo(var))
    if (tsibble::n_keys(x) > 1) {
        x <- dplyr::mutate(x,
            .key = rlang::eval_tidy(rlang::new_quosure(expr(interaction(!!!(
                lapply(tsibble::key_vars(x), function(i) x[[i]])
            ), sep = "/"))))
        ) |>
            tsibble::update_tsibble(key = .key)
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
    if (length(var) > 2) {
        if (!isTRUE(all.equal(length(var) - 1, length(ylab)))) {
            rlang::abort("var and ylab should have the same length.")
        }
    }
    (if (length(var) < 3) {
        var <- sym(dplyr::last(as.character(var)))
        plot_subseries(x, var, show_mean, ylab, title)
    } else {
        p_ls <- lapply(seq_len(length(var) - 1), function(i) {
            y_var <- as.character(var)[i + 1]
            plot_subseries(x, sym(y_var), show_mean, ylab[i], title)
        })
        expr(patchwork::wrap_plots(!!!p_ls)) |>
            rlang::new_quosure() |>
            rlang::eval_tidy() +
            patchwork::plot_layout(ncol = 1, guides = "collect") +
            patchwork::plot_annotation(title = title) &
            ggplot2::theme(legend.position = "bottom")
    }) |> (\(.) structure(., use.plotly = ggplotable(.)))()
}


plot_subseries <- function(x, var, show_mean, ylab, title) {
    p <- feasts::gg_subseries(x, !!var) +
        ggplot2::labs(y = ylab, title = title)
    if (!show_mean || tsibble::n_keys(x) > 1) {
        p$layers[[2]] <- NULL
    }
    if (tsibble::n_keys(x) > 1) {
        if (show_mean) {
            p <- p + geom_hline(aes(yintercept = .yint, col = .key))
        }
        p$mapping$group <- rlang::new_quosure(expr(.key))
        p$mapping$colour <- p$mapping$group
        p$facet$params$rows <- NULL
        p <- p + ggplot2::theme(
            legend.position = "bottom",
            legend.title = element_blank()
        )
    }
    p
}
