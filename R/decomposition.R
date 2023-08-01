.decomp <- function(use_method, ...) {
    UseMethod(".decomp")
}


#' Decompose a time series object
#'
#' Decomposes a time series represented by an `inz_ts` object into its
#' seasonal, trend, and remainder components using the specified smoothing
#' method.
#'
#' @param x An `inz_ts` object representing the time series.
#' @param var A character vector of length one, or `NULL`.
#' @param sm_model The smoothing method to be used.
#'        Currently on "stl" is available.
#' @param mult_fit If `TRUE`, a multiplicative model is used; otherwise,
#'        an additive model is used by default.
#' @param model_range The range of data to be decomposed by the model. It can be
#'        specified as dates or years. If part of `model_range` lies
#'        outside the range of the data, the exceeding proportion is ignored.
#' @param ... Additional arguments passed to the decomposition method
#'        defined by `sm_model`.
#' @return An `inz_dcmp` object, which is a sub-class of `dable`,
#'         representing the decomposed components of the time series.
#'
#' @rdname decomposition
#' @seealso \code{\link[fabletools]{dable}}
#'
#' @examples
#' ts <- inzightts(visitorsQ)
#' d <- decomp(ts)
#'
#' \dontrun{
#' plot(d)
#' }
#'
#' @references
#' R. B. Cleveland, W. S. Cleveland, J.E. McRae, and I. Terpenning (1990)
#' STL: A Seasonal-Trend Decomposition Procedure Based on Loess.
#' Journal of Official Statistics, 6, 3iV73.
#' @export
#' @md
decomp <- function(x, var = NULL, sm_model = c("stl"),
                   mult_fit = FALSE, model_range = NULL, ...) {
    var <- dplyr::last(as.character(guess_plot_var(x, !!enquo(var), use = "Decomp")))
    if (tsibble::n_keys(x) > 1) {
        rlang::abort("Cannot decompose multiple time series.")
    }
    if (all(is.na(model_range))) model_range <- NULL
    if (!is.null(model_range)) {
        if (!all(length(model_range) == 2, any(is.numeric(model_range), inherits(model_range, "Date")))) {
            rlang::abort("model_range must be a numeric or Date vector of length 2.")
        }
        na_i <- which(is.na(model_range))[1]
        if (!is.numeric(x[[tsibble::index_var(x)]]) && is.numeric(model_range)) {
            model_range[na_i] <- lubridate::year(dplyr::case_when(
                as.logical(na_i - 1) ~ dplyr::last(x$index),
                TRUE ~ x$index[1]
            ))
            model_range <- lubridate::ymd(paste0(model_range, c("0101", "1231")))
            x <- dplyr::filter(x, dplyr::between(lubridate::as_date(index), model_range[1], model_range[2]))
        } else if (is.numeric(x[[tsibble::index_var(x)]]) && inherits(model_range, "Date")) {
            model_range[na_i] <- lubridate::ymd(paste0(ifelse(na_i - 1, dplyr::last(x$index), x$index[1]), "0101"))
            x <- dplyr::filter(x, dplyr::between(index, lubridate::year(model_range[1]), lubridate::year(model_range[2])))
        } else {
            model_range[na_i] <- dplyr::case_when(
                as.logical(na_i - 1) ~ dplyr::last(x$index),
                TRUE ~ x$index[1]
            )
            x <- dplyr::filter(x, dplyr::between(index, model_range[1], model_range[2]))
        }
    }
    mismatch_err <- gsub(
        "'arg'", "`sm_model`",
        evaluate::try_capture_stack(match.arg(sm_model))$message
    )
    if (inherits(try(match.arg(sm_model), silent = TRUE), "try-error")) {
        rlang::abort(mismatch_err)
    }
    decomp_spec <- list(...)
    rlang::inject(.decomp(use_decomp_method(sm_model), x, var, mult_fit, !!!decomp_spec)) |>
        (\(.) structure(., class = c("inz_dcmp", class(.)), mult_fit = mult_fit))()
}


decomp_key <- function(x, var, sm_model, mult_fit, ...) {
    key_data <- tsibble::key_data(x)
    key_vars <- tsibble::key_vars(x)
    dplyr::bind_rows(!!!lapply(
        seq_len(nrow(key_data)),
        function(i) {
            .x <- key_data[i, ] |>
                dplyr::left_join(x, by = key_vars, multiple = "all") |>
                tsibble::as_tsibble(
                    index = !!tsibble::index(x),
                    key = !!key_vars
                ) |>
                decomp(as.character(var), sm_model, mult_fit, ...) |>
                tibble::as_tibble()
            if (".model" %in% names(.x)) {
                .x
            } else {
                dplyr::mutate(.x, .model = paste(key_data[i, ], collapse = "."))
            }
        }
    )) |>
        (\(.) dplyr::filter(., dplyr::if_all(
            !!key_vars[key_vars %in% names(.)],
            ~ !is.na(.x)
        )))() |>
        (\(.) tsibble::as_tsibble(.,
            index = !!tsibble::index(x),
            key = c(!!key_vars[key_vars %in% names(.)], .model)
        ))() |>
        (\(.) structure(., seasons = {
            n <- names(.)[grep("season_", names(.))]
            n <- n[n != "season_adjust"]
            n <- ifelse(length(n) > 1, n[n != "season_null"], n)
            as.list(tibble::tibble(!!n := 1))
        }))()
}


use_decomp_method <- function(method) {
    structure(method, class = sprintf("use_%s", method))
}


#' @export
.decomp.use_stl <- function(use_method, data, var, mult_fit, t = 0, ...) {
    if (is.character(var)) var <- sym(var)
    stl_spec <- list(...)
    if (!is.null(stl_spec$s.window)) {
        s.window <- stl_spec$s.window
        stl_spec <- within(stl_spec, rm(s.window))
    } else {
        s.window <- "periodic"
    }
    if (!is.null(stl_spec$t.window) && t != 0) {
        rlang::warn("`t.window` overrides `t`.")
    } else {
        if (!dplyr::between(t, 0, 100)) {
            rlang::abort("`t` must be a number between 0 and 100.")
        }
        stl_spec$t.window <- (1.5 * frequency(data) / (1 - 1.5 / (10 * nrow(data) + 1)) +
            0.5 * frequency(data) * t) |>
            ceiling() |>
            round() |>
            (\(x) x + (x %% 2 == 0))() |>
            as.integer()
    }
    if (any(data[[var]] <= 0, na.rm = TRUE) && mult_fit) {
        mult_fit <- !mult_fit
        rlang::warn("Non-positive obs detected, setting `mult_fit = FALSE`")
    }
    if (mult_fit) data <- dplyr::mutate(data, !!var := log(!!var))
    if (any(is.na(data[[var]]))) {
        rlang::warn("Time gaps detected, STL returning NULL model.")
        return(data |>
            dplyr::select(!!tsibble::index(data), !!var) |>
            dplyr::mutate(
                trend = NA_real_,
                season_null = NA_real_,
                remainder = NA_real_
            ) |>
            structure(null_mdl = 1, seasons = list(season_null = 1)))
    }
    rlang::inject(fabletools::model(data, feasts::STL(
        !!var ~ trend() + season(window = s.window),
        !!!stl_spec
    ))) |>
        fabletools::components() |>
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
        x |> dplyr::mutate(
            !!season := (!!season - 1) * trend,
            remainder = !!var - trend - !!season
        )
    } else {
        x
    }
}


#' @param x An `inz_dcmp` object representing the decomposed time series.
#' @param recompose.progress A numeric vector of length 2, controlling the
#'        display of recomposition progress when `recompose` is `TRUE`.
#'        The first component shows the progress for the seasonal component
#'        (0 to 1), and the second component tracks the number of observations
#'        recomposed so far.
#' @param recompose Logical indicating whether the recomposition should be
#'        displayed or not.
#' @param ylab The label for the y-axis of the plot.
#' @param title  The title for the plot.
#' @param colour A vector of three colors specifying the colors for the trend,
#'        seasonal, and residuals components, respectively.
#' @param ... Additional arguments (ignored).
#'
#' @rdname decomposition
#'
#' @import patchwork
#'
#' @export
plot.inz_dcmp <- function(x, recompose.progress = c(0, 0),
                          recompose = any(recompose.progress > 0),
                          ylab = NULL, title = NULL,
                          colour = c("#1B9E46", "#45a8ff", "orangered"), ...) {
    var <- suppressMessages(guess_plot_var(x, NULL))
    if (is.null(xlim)) xlim <- as_year(range(x[[tsibble::index_var(x)]]))
    if (is.null(ylab)) ylab <- as.character(var)
    if (!is.null(attributes(x)$null_mdl)) {
        return(suppressWarnings(plot.inz_ts(x, tsibble::measured_vars(x)[1])))
    }
    td <- x |>
        back_transform(var, attributes(x)$mult_fit) |>
        (\(.) dplyr::mutate(.,
            Date = as_year(!!tsibble::index(x)),
            value = !!var,
            seasonal = !!sym(names(attributes(.)$seasons)),
            residual = remainder
        ))() |>
        tibble::as_tibble(x) |>
        dplyr::select(Date, value, trend, seasonal, residual)

    ## FIXME: ALL CODES BELOW NEED TO BE OPTIMISED

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

    datarange <- with(
        td,
        c(
            max(trend, trend + seasonal, value),
            min(trend, trend + seasonal, value)
        )
    )

    p <- ggplot(td, aes(Date))
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
        geom_path(aes(y = value), colour = "gray") +
        geom_path(
            aes(y = trend),
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
        rtd <- td |>
            dplyr::mutate(
                z = ifelse(1:nrow(td) < ri,
                    .data$trend + .data$seasonal,
                    td$trend[ri] + .data$seasonal
                )
            )
        pdata <- pdata +
            geom_path(
                aes(y = z),
                data = rtd,
                colour = colour[2],
                alpha = ifelse(FINAL, 0.5, 1)
            )
        if (recompose.progress[1] == 1 && recompose.progress[2] > 0) {
            ri <- recompose.progress[2]
            rtd <- td |>
                dplyr::mutate(
                    z = ifelse(1:nrow(td) < ri,
                        .data$value,
                        .data$trend[ri] + .data$seasonal[ri] +
                            .data$residual
                    )
                )
            if (!FINAL) {
                pdata <- pdata +
                    geom_path(
                        aes(y = z),
                        data = rtd[-(1:(ri - 1)), ],
                        colour = colour[3]
                    )
            }

            pdata <- pdata +
                geom_path(
                    aes(y = value),
                    data = rtd[1:ri, ],
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
        geom_path(aes(y = seasonal), colour = colour[2]) +
        labs(subtitle = "Seasonal Swing", y = "") +
        theme(
            # panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()
        )

    presid <- p +
        geom_path(aes(y = residual), colour = colour[3]) +
        # geom_segment(
        #     aes(y = residual, yend = 0, xend = Date),
        #     colour = colour[3]
        # ) +
        labs(subtitle = "Residuals", y = "") +
        ylim(extendrange(rrange, f = rr / 2)) +
        theme(
            # panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()
        )

    pdata + pseason + presid +
        plot_layout(ncol = 1, heights = ratios)
}
