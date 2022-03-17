#' @export
.decomp <- function(use_method, ...) {
    UseMethod(".decomp")
}


#' Decompose a time series object
#'
#' @param x an inzightts (\code{inz_ts}) object
#' @param var a character vector of length one, or \code{NULL}
#' @param sm_model the smoothing method to be used
#' @param mult_fit If \code{TRUE}, a multiplicative model is used, otherwise
#'        an additive model is used by default.
#' @param ... additional arguments (ignored)
#' @return a decomp (\code{inz_dcmp}) object, a sub-class of dable
#'
#' @rdname decomposition
#'
#' @seealso \code{\link[fabletools]{dable}}
#'
#' @examples
#' library(dplyr)
#' d <- visitorsQ %>%
#'     inzightts() %>%
#'     decomp() %>%
#'     plot()
#'
#' @references
#' R. B. Cleveland, W. S. Cleveland, J.E. McRae, and I. Terpenning (1990)
#' STL: A Seasonal-Trend Decomposition Procedure Based on Loess.
#' Journal of Official Statistics, 6, 3iV73.
#' @export
decomp <- function(x, var = NULL, sm_model = c("stl"), mult_fit = FALSE, ...) {
    var <- dplyr::last(as.character(guess_plot_var(x, !!enquo(var), use = "Decomp")))

    mismatch_err <- gsub(
        "'arg'", "`sm_model`",
        evaluate::try_capture_stack(match.arg(sm_model))$message
    )
    if (inherits(try(match.arg(sm_model), silent = TRUE), "try-error")) {
        rlang::abort(mismatch_err)
    }
    decomp_spec <- list(...)

    expr(.decomp(use_decomp_method(sm_model), x, var, mult_fit, !!!decomp_spec)) %>%
        rlang::new_quosure() %>%
        rlang::eval_tidy() %>%
        structure(class = c("inz_dcmp", class(.)), mult_fit = mult_fit)
}


use_decomp_method <- function(method) {
    structure(method, class = sprintf("use_%s", method))
}


#' @export
.decomp.use_stl <- function(use_method, data, var, mult_fit, ...) {
    if (is.character(var)) var <- sym(var)

    stl_spec <- list(...)
    if (!is.null(stl_spec$s.window)) {
        s.window <- stl_spec$s.window
        stl_spec <- within(stl_spec, rm(s.window))
    } else {
        s.window <- "periodic"
    }
    if (any(data[[var]] <= 0) & mult_fit) {
        mult_fit <- !mult_fit
        rlang::warn("Non-positive obs detected, setting `mult_fit = FALSE`")
    }
    if (mult_fit) data <- dplyr::mutate(data, !!var := log(!!var))

    expr(fabletools::model(data, feasts::STL(
        !!var ~ trend() + season(window = s.window),
        !!!stl_spec
    ))) %>%
        rlang::new_quosure() %>%
        rlang::eval_tidy() %>%
        fabletools::components() %>%
        dplyr::mutate(dplyr::across(
            !!var | trend | remainder | dplyr::contains("season"), function(x) {
                dplyr::case_when(mult_fit ~ exp(x), TRUE ~ as.numeric(x))
            }
        ))
}


as_year <- function(x) {
    UseMethod("as_year")
}


as_year.vctrs_vctr <- function(x) {
    1970 + as.numeric(x) / dplyr::case_when(
        inherits(x, "yearmonth") ~ 12,
        inherits(x, "yearquarter") ~ 4,
        inherits(x, "yearweek") ~ 365.25 / 7,
        TRUE ~ NA_real_
    )
}


as_year.Date <- function(x) {
    1970 + as.numeric(x) / 365.25
}


as_year.numeric <- function(x) x


as_year.character <- function(x) as.numeric(x)


back_transform <- function(x, var, mult_fit) {
    is_annual <- tsibble::guess_frequency(x[[tsibble::index_var(x)]]) == 1
    if (is_annual) {
        attributes(x)$seasons$season_null <- 1
        x <- dplyr::mutate(x, season_null = as.numeric(mult_fit))
    }
    if (mult_fit) {
        season <- sym(names(attributes(x)$seasons))
        x %>% dplyr::mutate(
            !!season := (!!season - 1) * trend,
            remainder = !!var - trend - !!season
        )
    } else {
        x
    }
}


#' @param x a decomp (\code{inz_dcmp}) object
#' @param recompose.progress if recompose is \code{TRUE}, this shows how
#'        much to show (for animation!). Length 2 numeric: the first
#'        is 0 for seasonal, and 1 for residual; second component is
#'        how many observations have been recomposed so far
#' @param recompose logical as to whether the recomposition is shown or not
#' @param ylab the label for the y axis
#' @param xlab the label for the x axis
#' @param title the title for the plot
#' @param xlim the x axis limits
#' @param colour vector of three colours for trend, seasonal, and residuals, respectively
#' @param ... additional arguments (ignored)
#'
#' @rdname decomposition
#'
#' @import patchwork
#' 
#' @export
plot.inz_dcmp <- function(x, recompose.progress = c(0, 0),
                          recompose = any(recompose.progress > 0),
                          ylab = NULL, xlab = "Date",
                          title = NULL, xlim = c(NA, NA),
                          colour = c("#1B9E46", "#45a8ff", "orangered"),
                          ...) {
    var <- suppressMessages(guess_plot_var(x, NULL))
    if (is.null(xlim)) xlim <- as_year(range(x[[tsibble::index_var(x)]]))
    if (is.null(ylab)) ylab <- as.character(var)

    td <- x %>%
        back_transform(var, attributes(x)$mult_fit) %>%
        dplyr::mutate(
            Date = as_year(!!tsibble::index(x)),
            value = !!var,
            seasonal = !!sym(names(attributes(.)$seasons)),
            residual = remainder
        ) %>%
        tibble::as_tibble(x) %>%
        dplyr::select(Date, value, trend, seasonal, residual)

    if (recompose && all(recompose.progress == 0)) {
        recompose.progress <- c(1, nrow(td))
    }
    
    
    ## Create ONE SINGLE plot
    ## but transform the SEASONAL and RESIDUAL components below the main data
    
    yrange <- range(td$value)
    ydiff <- diff(yrange)
    srange <- range(td$seasonal)
    sdiff <- diff(srange)
    rrange <- range(td$residual)
    rdiff <- diff(rrange)
    
    # ratios
    total <- ydiff + sdiff + rdiff
    rr <- 1
    if (rdiff < 0.05 * total) {
        rdiff <- 0.05 * total
        rr <- rdiff / diff(rrange)
        total <- ydiff + sdiff + rdiff
    }
    ratios <- c(ydiff, sdiff, rdiff) / total
    
    datarange <- with(td,
                      c(
                          max(trend, trend + seasonal, value),
                          min(trend, trend + seasonal, value)
                      )
    )
    
    p <- ggplot(td, aes_(~Date))
    p0 <- p +
        theme(
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
        )
    
    if (is.null(title)) title <- sprintf("Decomposition: %s", as.character(var))
    
    lcolour <- colorspace::lighten(colour, 0.5)
    FINAL <- all(recompose.progress == c(1L, nrow(td)))
    pdata <- p0 +
        geom_path(aes_(y = ~value), colour = "gray") +
        geom_path(
            aes_(y = ~trend),
            colour = colour[1],
            alpha = ifelse(FINAL, 0.5, 1)
        ) +
        # Observed data = trend + seasonal swing + residuals
        labs(
            title = title,
            y = ylab,
            subtitle = sprintf(
                "%s = %s + %s + %s",
                ifelse(FINAL,
                       "<span style='color:black'>Observed data</span>",
                       "<span style='color:gray'>Observed data</span>"
                ),
                ifelse(sum(recompose.progress) == 0,
                       glue::glue("<span style='color:{colour[1]}'>**Trend**</span>"),
                       glue::glue("<span style='color:{colour[1]}'>Trend</span>")
                ),
                ifelse(sum(recompose.progress) == 0,
                       glue::glue("<span style='color:{lcolour[2]}'>seasonal swing</span>"),
                       ifelse(recompose.progress[1] == 0,
                              glue::glue("<span style='color:{colour[2]}'>**seasonal swing**</span>"),
                              glue::glue("<span style='color:{colour[2]}'>seasonal swing</span>")
                       )
                ),
                ifelse(recompose.progress[1] == 0,
                       glue::glue("<span style='color:{lcolour[3]}'>residuals</span>"),
                       ifelse(!FINAL,
                              glue::glue("<span style='color:{colour[3]}'>**residuals**</span>"),
                              glue::glue("<span style='color:{colour[3]}'>residuals</span>")
                       )
                )
            )
        ) +
        ylim(extendrange(datarange, f = 0.05))
    if (recompose && any(recompose.progress > 0)) {
        ri <- ifelse(recompose.progress[1] == 0,
                     recompose.progress[2],
                     nrow(td)
        )
        rtd <- td %>%
            dplyr::mutate(
                z = ifelse(1:nrow(td) < ri,
                           .data$trend + .data$seasonal,
                           td$trend[ri] + .data$seasonal
                )
            )
        pdata <- pdata +
            geom_path(
                aes_(y = ~z),
                data = rtd,
                colour = colour[2],
                alpha = ifelse(FINAL, 0.5, 1)
            )
        if (recompose.progress[1] == 1 && recompose.progress[2] > 0) {
            ri <- recompose.progress[2]
            rtd <- td %>%
                dplyr::mutate(
                    z = ifelse(1:nrow(td) < ri,
                               .data$value,
                               .data$trend[ri] + .data$seasonal[ri] +
                                   .data$residual
                    )
                )
            if (!FINAL)
                pdata <- pdata +
                geom_path(
                    aes_(y = ~z),
                    data = rtd[-(1:(ri-1)),],
                    colour = colour[3]
                )
            
            pdata <- pdata +
                geom_path(
                    aes_(y = ~value),
                    data = rtd[1:ri,],
                    colour = if (FINAL) "black" else colour[3]
                )
        }
    }
    
    pdata <- pdata +
        theme(
            plot.title.position = "plot",
            plot.subtitle = ggtext::element_markdown()
        )
    
    pseason <- p0 +
        geom_path(aes_(y = ~seasonal), colour = colour[2]) +
        labs(subtitle = "Seasonal Swing", y = "") +
        theme(
            # panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()
        )
    
    presid <- p +
        geom_path(aes_(y = ~residual), colour = colour[3]) +
        # geom_segment(
        #     aes_(y = ~residual, yend = 0, xend = ~Date),
        #     colour = colour[3]
        # ) +
        labs(subtitle = "Residuals", y = "") +
        ylim(extendrange(rrange, f = rr/2)) +
        theme(
            # panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()
        )
    
    pfinal <- pdata + pseason + presid +
        plot_layout(ncol = 1, heights = ratios)
    
    dev.hold()
    on.exit(dev.flush())
    print(pfinal)
    
    invisible(x)
}
