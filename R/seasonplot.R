##' This function plots the seasonal components of a time series together
##' with the estimated seasonal effects of that series.
##'
##' The resulting window will contain two plots. On the left, every
##' seasonal subseries of the time series is plotted. On the right will be
##' the average seasonal effect of the series.
##'
##' @title Plot Seasonal Subseries from a Time Series
##'
##' @param obj an \code{iNZightTS} object
##'
##' @param ... Further arguments to be passed onto specific methods.
##'
##' @seealso \code{\link{iNZightTS}}
##'
##' @examples
##' \dontrun{
##' x <- iNZightTS(UKgas)
##' seasonplot(x)
##' }
##'
##' @export
seasonplot <-
function(obj, ...)
    UseMethod("seasonplot")

#' @export
seasonplot.iNZightTS <-
function(obj, multiplicative = FALSE, t = 10, model.lim = NULL,
         ylab = obj$currVar, ...) {

    # if there is no season component to the ts, can't create season plot
    if (length(obj$start) == 1)
        return("Time Series does not have a seasonal component")

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

    obj <- decomposition(obj, ylab = ylab,
        multiplicative = multiplicative,
        t = t,
        model.lim = model.lim
    )

    td <- data.frame(
        Date = as.numeric(time(obj$tsObj)),
        value = as.matrix(obj$tsObj),
        trend = as.numeric(obj$decompVars$components[, "trend"]),
        seasonal = as.numeric(obj$decompVars$components[, "seasonal"]),
        residual = as.numeric(obj$decompVars$components[, "remainder"])
    ) %>%
        dplyr::mutate(
            effect =
                if (multiplicative) .data$seasonal * .data$residual
                else .data$seasonal + .data$residual,
            a = floor(.data$Date) - obj$start[1] + 1,
            b = .data$Date %% 1 * freq + 1
        )

    p1 <- ggplot(td, aes_(~b, ~value, colour = ~a, group = ~a)) +
        geom_point() +
        geom_path() +
        scale_colour_gradient(
            low = "darkorange", high = "blue", guide = FALSE
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
    season <- data.frame(b = 1:freq, effect = season, a = 1)

    p2 <- ggplot(td, aes_(~b, ~effect - as.integer(multiplicative), group = ~a)) +
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
        ) +
        scale_y_continuous(
            labels = function(y) y + 1
        )

    dev.hold()
    on.exit(dev.flush())

    egg::ggarrange(p1, p2, nrow = 1)
}
