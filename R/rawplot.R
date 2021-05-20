#' Draws a plot of a given \code{iNZightTS} object with the trend superimposed.
#'
#' If animate is set to \code{TRUE}, a scatterplot of all points in the
#' time series will appear followed by slowly drawn lines connecting the
#' points, simulating the drawing of a time series by hand.
#'
#' @title Draw a simple time series plot
#'
#' @param x an \code{iNZightTS} object
#' @param multiplicative logical. If \code{TRUE}, a multiplicative model is used,
#' otherwise an additive model is used by default.
#' @param ylab a title for the y axis
#' @param xlab a title for the x axis
#' @param title a title for the graph
#' @param animate logical, if true the graph is animated
#' @param t smoothing parameter
#' @param smoother logical, if \code{TRUE} the smoother will be drawn
#' @param aspect the aspect ratio of the plot;
#'        it will be about ASPECT times wider than it is high
#' @param plot logical, if \code{FALSE}, the graph isn't drawn
#' @param col the colour of the smoothed trend line
#' @param xlim axis limits, specified as dates
#' @param model.lim limits of the series to use for modelling/forecast
#' @param seasonal.trend logical, if \code{TRUE} seasonal+trend curve added
#' @param forecast numeric, how many observations ahead to forecast (default is 0, no forecast)
#' @param ... additional arguments (not used)
#' @return a time series plot (constructed with ggplot2) is returned invisibly,
#'         which can be added to if desired.
#'
#' @keywords timeseries
#'
#' @import ggplot2
#'
#' @section Forecast:
#' The predictions and prediction intervals are the result of models
#' fitted by the Holt-Winters method. The amount of predicted
#' observations is specified by the value of `forecast`.
#'
#' @references
#' C.C Holt (1957)
#' Forecasting seasonals and trends by exponentially weighted
#' moving averages,
#' ONR Research Memorandum, Carnegie Institute 52.
#'
#' P.R Winters (1960)
#' Forecasting sales by exponentially weighted moving averages,
#' \emph{Management Science} \bold{6}, 324--342.
#'
#' @examples
#' t <- iNZightTS(visitorsQ)
#' plot(t)
#'
#' # Forecast plot (8 quarterly forecasts):
#' plot(t, forecast = 8)
#'
#' @export
plot.iNZightTS <- function(x, multiplicative = FALSE, ylab = obj$currVar, xlab = "Date",
                           title = "%var",
                           animate = FALSE,
                           t = 10, smoother = TRUE,
                           aspect = 3,
                           plot = TRUE,
                           col = ifelse(forecast > 0, "#0e8c07", "red"),
                           xlim = c(NA, NA),
                           model.lim = NULL,
                           seasonal.trend = FALSE,
                           forecast = 0,
                           ...) {

    ### x and y coordinates of the time series tsObj
    obj <- x
    freq <- x$freq
    tsObj <- obj$tsObj
    xlist <- get.x(tsObj)
    x <- xlist$x
    x.units = xlist$x.units
    y <- tsObj@.Data
    y.units <- unit(y, "native")
    multiplicative <- is_multiplicative(tsObj, multiplicative)

    xlim <- ifelse(is.na(xlim), range(time(tsObj)), xlim)
    if (!is.null(model.lim)) {
        model.lim <- ifelse(is.na(model.lim),
            c(min(time(tsObj)), xlim[2]),
            model.lim
        )
        if (model.lim[2] > xlim[2]) {
            warning("Upper modelling limit cannot be greater than upper x limit")
            model.lim[2] <- xlim[2]
        }
    } else {
        model.lim <- c(min(time(tsObj)), xlim[2])
    }

    multiseries <- inherits(obj, "iNZightMTS")

    if (multiseries && forecast > 0) {
        warning("Forecasting not available for multiplots")
        forecast <- 0
    }
    if (freq == 1 && forecast > 0) {
        warning("Forecasting not available for annual data")
        forecast <- 0
    }

    ### We want a trend line, so do a decomposition
    if (!smoother) {
        smooth <- NULL
    } else if (!multiseries) {
        if (forecast > 0) {
            AtsObj <- tsObj
            if (!is.null(model.lim)) {
                AtsObj <- window(AtsObj, model.lim[1], model.lim[2])
            }
            if (multiplicative)
                AtsObj <- log(AtsObj)
            hw.fit <- try(HoltWinters(AtsObj), TRUE)
            if (inherits(hw.fit, "try-error"))
                stop("Holt-Winters could not converge.")
            smooth <- hw.fit$fitted[, 1]
            if (multiplicative) smooth <- exp(smooth)
            smooth <- data.frame(
                time = as.numeric(time(hw.fit$fitted)),
                smooth = smooth,
                stringsAsFactors = TRUE
            )
        } else {
            decomp <- decompose(obj,
                ylab = "",
                multiplicative = multiplicative,
                t = t,
                model.lim = model.lim)$decompVars
            if (multiplicative)
              smooth <- exp(log(decomp$components[,"trend"]))
            else
              smooth <- decomp$components[,"trend"]
            smooth <- as.matrix(smooth)[, 1]

            dt <- time(decomp$components)
            # print(dt)
            # due to rounding, the limits might not be exact ...
            smooth <- smooth[dt - xlim[1] > -1e-12 & dt - xlim[2] < 1e-12]

            if (seasonal.trend) {
                ssn <- decomp$components[, "seasonal"]
                ssn <- ssn[dt - xlim[1] > -1e-12 & dt - xlim[2] < 1e-12]
                ssn <- if (multiplicative) smooth * ssn else smooth + ssn
            }
        }
    } else {
        smoothList <- vector("list", length(obj$currVar))
        names(smoothList) <- obj$currVar
        for (v in obj$currVar) {
            subts <- obj
            subts$tsObj <- obj$tsObj[, v]
            subts$currVar <- v
            class(subts) <- "iNZightTS"
            smoothList[[v]] <- decompose(subts,
                ylab = "",
                multiplicative = multiplicative,
                t = t,
                model.lim = model.lim)$decompVars
        }
        smooth <- do.call(c, lapply(smoothList, function(s) {
            if (multiplicative)
                z <- exp(log(s$components[, "trend"]))
            else
                z <- s$components[, "trend"]
            dt <- time(s$components)
            z[dt - xlim[1] > -1e-12 & dt - xlim[2] < 1e-12]
        }))
    }

    ### Height of the plotting viewport needs to be scale.factor times the height
    ### of the trend viewport in the decomposition plot

    value <- obj$currVar
    ts.df <- data.frame(Date = as.numeric(time(tsObj)),
                        value = as.matrix(tsObj),
                        stringsAsFactors = TRUE)
    ts.df <- ts.df %>%
        tidyr::gather(key = "variable", value = "value",
                      -.data$Date, factor_key = TRUE)
    ts.df <-
        dplyr::mutate(ts.df, variable =
            forcats::lvls_revalue(ts.df$variable,
                                  gsub("value.", "",
                                        levels(ts.df$variable))))
    ## x-axis limits
    if (!all(is.na(xlim))) {
        # if (forecast == 0)
        #     smooth <- smooth[ts.df$Date >= xlim[1] & ts.df$Date <= xlim[2]]
        ts.df <- ts.df[ts.df$Date - xlim[1] > -1e-12 & ts.df$Date - xlim[2] < 1e-12, ]
    }

    fit.df <- ts.df
    if (!is.null(model.lim)) {
        fit.df <-
            fit.df[fit.df$Date - model.lim[1] > -1e-12 &
                   fit.df$Date - model.lim[2] < 1e-12, ]
    }
    if (forecast > 0) {
        # remove first season from the smoother
        # fit.df <- fit.df[-(1:freq),]
        fit.df <- fit.df %>%
            dplyr::filter(
                dplyr::between(.data$Date, min(smooth$time), max(smooth$time))
            )
        smooth <- smooth %>%
            dplyr::filter(
                dplyr::between(.data$time, min(fit.df$Date), max(fit.df$Date))
            )
        fit.df$smooth <- smooth$smooth

        # create prediction df
        pred <- predict(hw.fit,
            n.ahead = forecast,
            prediction.interval = TRUE
        )
        if (multiplicative) {
            pred <- exp(pred)
        }
        pred.df <- rbind(
            fit.df[nrow(fit.df), c("Date", "variable", "value")] %>%
                dplyr::mutate(lower = .data$value, upper = .data$value),
            data.frame(
                Date = as.numeric(time(pred)),
                variable = "value",
                value = as.numeric(pred[, "fit"]),
                lower = as.numeric(pred[, "lwr"]),
                upper = as.numeric(pred[, "upr"]),
                stringsAsFactors = TRUE
            )
        )
    } else if (!is.null(smooth)) {
        fit.df$smooth <- smooth
        if (seasonal.trend) fit.df$season.smooth <- ssn
    }

    if (grepl("%var", title))
        title <- gsub("%var", paste(obj$currVar, collapse = ", "), title)


    tsplot <- ggplot(ts.df, aes_(x = ~Date, y = ~value,
                                 group = ~variable, colour = ~variable)) +
        xlab(xlab) + ylab(ylab) + ggtitle(title)
    if (!is.null(aspect)) {
        xr <- diff(range(ts.df$Date))
        yr <- diff(range(ts.df$value))
        asp <- xr / yr / aspect
        tsplot <- tsplot + coord_fixed(ratio = asp)
    }

    if (!multiseries && forecast == 0)
        tsplot <- tsplot +
            scale_colour_manual(
                values = c(
                    Fitted = col,
                    "Raw data" = "black",
                    if (seasonal.trend) "Trend + Seasonal" = "green4" else NULL
                ),
                guide = "none"
            )

    if (plot && animate && !multiseries) {
        ## Do a bunch of things to animate the plot ...

        dev.hold()
        print(tsplot + geom_point(aes(colour = NULL)))
        dev.flush()

        Sys.sleep(1)
        for (i in 2:nrow(ts.df)) {
            dev.hold()
            print(
                tsplot +
                    geom_point(aes(colour = NULL), colour = "black") +
                    geom_line(aes(colour = NULL),
                        data = ts.df[1:i, ], colour = "black"
                    )
            )
            dev.flush()
            Sys.sleep(ifelse(i > 9, 0.05, 0.5))
        }
        Sys.sleep(1)
    }

    if (forecast > 0) {
        tsplot <- tsplot +
            geom_vline(xintercept = max(fit.df$Date),
                col = "#555555", lty = "dashed") +
            geom_ribbon(aes_(ymin = ~lower, ymax = ~upper),
                data = pred.df,
                fill = "#ffdbdb",
                col = NA
            ) +
            geom_line(aes_(y = ~lower, col = "Prediction"), data = pred.df,
                lty = "dashed", lwd = 0.4) +
            geom_line(aes_(y = ~upper, col = "Prediction"), data = pred.df,
                lty = "dashed", lwd = 0.4) +
            geom_line(data = pred.df, col = "#b50000")
    }

    if (multiseries)
        tsplot <- tsplot + geom_line(lwd = 1)
    else
        tsplot <- tsplot + geom_line(aes(colour = "Raw data"), lwd = 1)
    if (!is.null(smooth)) {
        if (seasonal.trend)
            tsplot <- tsplot +
                geom_path(aes_(x = ~Date, y = ~season.smooth, color = "Trend + Seasonal"),
                    data = fit.df, na.rm = TRUE,
                    lwd = 0.5)

        tsplot <-
            if (multiseries)
                tsplot + geom_line(aes_(x = ~Date, y = ~smooth, color = ~variable),
                    data = fit.df, na.rm = TRUE,
                    linetype = "22", lwd = 1) +
                geom_point(aes_(x = ~Date, y = ~smooth, shape = ~variable, color = ~variable),
                           data = fit.df[fit.df$Date == max(fit.df$Date), ],
                           size = 2, stroke = 2) +
                labs(color = "", shape = "")
            else
                tsplot + geom_line(aes_(x = ~Date, y = ~smooth, col = "Fitted"),
                    data = fit.df)
    }

    if (forecast > 0) {
        tsplot <- tsplot + scale_colour_manual(
            name = "",
            values = c(
                "Raw data" = "black",
                "Fitted" = col,
                "Prediction" = "#b50000"
            )
        ) +
            theme(legend.position = "bottom")
    }

    if (plot) {
        dev.hold()
        print(tsplot)
        dev.flush()
    }

    if (forecast > 0) {
        attr(tsplot, "predictions") <- pred
    }

    attr(tsplot, "use.plotly") <- TRUE
    invisible(tsplot)
}

#' Time series plot - depreciated
#' @param ... arguments passed to `plot` method
#' @export
#' @return Called to draw a plot. Invisibly returns a \code{ggplot} object.
rawplot <- function(...) {
    warning("Depreciated: use `plot()` instead.\n")
    plot(...)
}

#' Get forecast prediction values
#' @param x the forecast object (a plot with predictions)
#' @return a time series forecasts object
#' @export
pred <- function(x) attr(x, "predictions")
