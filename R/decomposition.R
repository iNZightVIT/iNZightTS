#' Decompose a time series object
#'
#' @param obj an iNZightTS object
#' @param multiplicative fit a multiplicative time series model?
#' @param t the smoothing parameter
#' @param model.lim limits for the time series model
#' @param data.name the name of the data
#' @param ... other, ignored, arguments
#' @return an \code{inzdecomp} object (this is the original
#'         object with an additional \code{decompVars} component)
#'
#' @examples
#' t <- iNZightTS(visitorsQ)
#' decomp.ts <- decompose(t, data.name = "Visitors")
#' plot(decomp.ts)
#'
#' @export
#' @references
#' R. B. Cleveland, W. S. Cleveland, J.E. McRae, and I. Terpenning (1990)
#' STL: A Seasonal-Trend Decomposition Procedure Based on Loess.
#' Journal of Official Statistics, 6, 3iV73.
decompose <- function(obj, multiplicative = FALSE, t = 10, model.lim = NULL,
                      data.name = NULL, ...) {
     if (!is.null(model.lim)) {
        model.lim <- ifelse(is.na(model.lim),
            range(time(obj$tsObj)), model.lim)
        ts.sub <- try({
            window(obj$tsObj, model.lim[1], model.lim[2])
        }, TRUE)
        if (inherits(ts.sub, "try-error")) {
            warning("Invalid modelling window - ignoring.")
        } else {
            obj$tsObj <- ts.sub
        }
    }

    multiplicative <- is_multiplicative(obj$tsObj, multiplicative)
    xlist <- get.x(obj$tsObj)
    x <- xlist$x
    x.units <- xlist$x.units

    n <- length(obj$data)

    if (multiplicative)
        tsObj <- log(obj$tsObj)
    else
        tsObj <- obj$tsObj

    if (obj$freq > 1) {
        ### t.window is the smallest odd integer ranges from about 1.5*frequceny to 2*frequency
        ### the actual minimum value is  1.5 * frequency/(1 - 1.5/s.window)
        ### where s.window = 10* number of observation +1 by putting 'periodic'
        ### t is set to be proportion of 0.5 *frequency
        ### when t =0, the t.window takes the default value/ minimum value -the least smoothness
        ### when t = 1. the t.window takes the maximum value - the most smoothnuess
        decomp <- stl(tsObj,
            "periodic",
            t.window =
                nextodd(
                    ceiling(
                        1.5 * frequency(data) / (1 - 1.5 / (10*n + 1)) +
                            0.5 * frequency(data) * t
                    )
                )
            )
    } else {
        ## freq == 1, non seasonal fitted.
        if (multiplicative)  {
            ### according to internet, the span value varies from about 0.1 to 2
            ### 0.1 gives nearly no smoothness, while 2 gives nearly maximum smoothness
            ### therefore here, the span ranges from 0.1 to 2
            ### the default is 0.75
            trend.comp <-
                loess(
                    log(obj$data[1:length(obj$tsObj), obj$currVar]) ~ x,
                    span = 0.1 + 1.9*t
                )$fitted + obj$tsObj * 0

            residuals.comp <- log(obj$tsObj) - trend.comp
            seasons.comp <- obj$tsObj * 0
            decomp <- list()
            decomp$time.series <- as.ts(
                data.frame(
                    seasonal = seasons.comp,
                    trend = trend.comp,
                    remainder = residuals.comp,
                    stringsAsFactors = TRUE
                )
            )
        } else {
            trend.comp <-
                loess(
                    obj$data[1:length(obj$tsObj), obj$currVar] ~ x
                )$fitted + obj$tsObj * 0

            residuals.comp <- obj$tsObj - trend.comp
            seasons.comp <- obj$tsObj * 0
            decomp <- list()
            decomp$time.series <- as.ts(
                data.frame(
                    seasonal = seasons.comp,
                    trend = trend.comp,
                    remainder = residuals.comp,
                    stringsAsFactors = TRUE
                )
            )
        }
        tsp(decomp$time.series) <- c(obj$start[1], obj$end[1], obj$freq)
    }

    decompData <- decomp$time.series    # returns matrix
    if (multiplicative) {
        Y <- log(as.numeric(as.matrix(tsObj)))
        trend_log <- as.numeric(decompData[, "trend"])
        trend <- exp(trend_log)
        seasonal_log <- as.numeric(decompData[, "seasonal"])
        seasonal <- exp(trend_log + seasonal_log) - trend
        residual <- exp(Y - (trend_log + seasonal_log))
        decompData[, "trend"] <- trend
        decompData[, "seasonal"] <- exp(seasonal_log)
        decompData[, "remainder"] <- residual
    }

    obj$decompVars <- list(
        data.name = data.name,
        raw = obj$tsObj@.Data,
        components = decompData,
        multiplicative = multiplicative
    )
    class(obj) <- c("inzdecomp", class(obj))
    obj
}

#' @param x an inzdecomp object (from decompose(ts))
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
#' @return Invisibly returns the original decomposition object. Mainly called
#'         to plot the decomposition.
#'
#' @describeIn decompose Plot a time series decomposition
#' @export
#' @import patchwork
plot.inzdecomp <- function(x, recompose.progress = c(0, 0),
                           recompose = any(recompose.progress > 0),
                           ylab = x$currVar, xlab = "Date",
                           title = NULL, xlim = c(NA, NA),
                           colour = c("#1B9E46", "#45a8ff", "orangered"),
                           ...) {
    ## Convert to a dataframe
    xlim <- ifelse(is.na(xlim), range(time(x$tsObj)), xlim)
    td <- data.frame(
        Date = as.matrix(time(x$tsObj)),
        value = as.matrix(x$tsObj),
        trend = as.numeric(x$decompVars$components[, "trend"]),
        seasonal = as.numeric(x$decompVars$components[, "seasonal"]),
        residual = as.numeric(x$decompVars$components[, "remainder"]),
        stringsAsFactors = TRUE
    )
    if (x$decompVars$multiplicative) {
        td <- dplyr::mutate(td,
            residual = as.numeric(x$tsObj) -
                exp(log(.data$trend) + log(.data$seasonal)),
            seasonal = exp(log(.data$trend) + log(.data$seasonal)) -
                .data$trend
        )
    }
    td <- dplyr::filter(td, dplyr::between(td$Date, xlim[1], xlim[2]))

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

    if (is.null(title)) {
        title <- sprintf("Decomposition%s: %s",
            ifelse(is.null(x$decompVars$data.name),
                "", paste(" of", x$decompVars$data.name)),
            x$currVar
        )
    }

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


#' Decomposes a time series into trend, seasonal and residual components
#' using \code{loess}.
#'
#' If the frequency is greater than 1, the components are found using the
#' \code{\link{stl}} function with \code{s.window} set to \code{TRUE}
#' (effectively replacing smoothing by taking the mean).
#' If the frequency is 1, the trend component is found directly by using
#' \code{\link{loess}} and the residuals are the difference between trend
#' and actual values.
#' The trend, seasonal and residual components are plotted on the same
#' scale allowing for easy visual analysis.
#'
#' @title Plot a Time Series Decomposition
#'
#' @param ... additional arguments, ignored
#'
#' @return The original \code{iNZightTS} object with an item \code{decompVars}
#' appended, containing results from the decomposition.
#'
#' @references R. B. Cleveland, W. S. Cleveland, J.E. McRae, and I. Terpenning (1990) STL: A Seasonal-Trend Decomposition Procedure Based on Loess. Journal of Official Statistics, 6, 3iV73.
#'
#' @seealso \code{\link{stl}}, \code{\link{loess}}, \code{\link{iNZightTS}}
#'
#' @export
decompositionplot <- function(...) {
    warning("Deprecated: please use `plot(decompose(obj))`")
}
