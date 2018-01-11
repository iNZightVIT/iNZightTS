##' Draws a plot of a given \code{iNZightTS} object with the trend superimposed.
##'
##' If animate is set to \code{TRUE}, a scatterplot of all points in the
##' time series will appear followed by slowly drawn lines connecting the
##' points, simulating the drawing of a time series by hand.
##'
##' @title Draw a simple time series plot
##'
##' @param obj an \code{iNZightTS} object
##' @param multiplicative logical. If \code{TRUE}, a multiplicative model is used,
##' otherwise an additive model is used by default.
##' @param ylab a title for the y axis
##' @param xlab a title for the x axis
##' @param tilte a title for the graph
##' @param animate logical, if true the graph is animated
##' @param t smoothing parameter
##' @param aspect the aspect ratio of the plot; 
##'        it will be about ASPECT times wider than it is high
##' @param plot logical, if \code{FALSE}, the graph isn't drawn
##'
##' @keywords timeseries
##'
##' @import ggplot2
##'
##' @export
plot.iNZightTS <- 
  function(x, multiplicative = FALSE, ylab = obj$currVar, xlab = "Date",
           title = "%var",
           animate = FALSE, t = 10, aspect = 3,
           plot = TRUE) {

    # if (any(grepl("^iNZightMTS$", class(data))))
    #     stop("Time-Series must be univariate")

    ### x and y coordinates of the time series tsObj
    obj <- x
    tsObj = obj$tsObj
    xlist = get.x(tsObj)
    x = xlist$x
    x.units = xlist$x.units
    y = tsObj@.Data
    y.units = unit(y, "native")

    multiseries <- inherits(obj, "iNZightMTS")

    ### We want a trend line, so do a decomposition
    if (!multiseries) {
        if (frequency(tsObj) > 1) {
            decomp = decomposition(obj, ylab = "", multiplicative = multiplicative, t = t)$decompVars
            if (multiplicative)
              smooth = exp(log(decomp$components[,"trend"]))
            else
              smooth = decomp$components[,"trend"]
        } else {
            smooth = loess(obj$data[1:length(obj$tsObj), obj$currVar] ~ x)$fitted
        }
        smooth <- as.matrix(smooth)
    } else {
        if (frequency(tsObj) > 1) {
            smoothList <- vector("list", length(obj$currVar))
            names(smoothList) <- obj$currVar
            for (v in obj$currVar) {
                subts <- obj
                subts$tsObj <- obj$tsObj[, v]
                class(subts) <- "iNZightTS"
                smoothList[[v]] <- decomposition(subts, ylab = "", multiplicative = multiplicative, t = t)$decompVars
            }
            smooth <- do.call(c, lapply(smoothList, function(s) {
                if (multiplicative)
                    return(exp(log(s$components[, "trend"])))
                else
                    return(s$components[, "trend"])
            }))
        } else {
            smooth <- NULL
        }
    }

    ### Height of the plotting viewport needs to be scale.factor times the height
    ### of the trend viewport in the decomposition plot

    value <- obj$currVar
    ts.df <- data.frame(Date = as.numeric(time(tsObj)),
                        value = as.matrix(tsObj))
    ts.df <- ts.df %>%
        tidyr::gather(key = "variable", value = "value",
                      colnames(ts.df)[-1], factor_key = TRUE) %>%
        dplyr::mutate(variable = forcats::lvls_revalue(variable, 
                                gsub("value.", "", levels(variable))))

    if (!is.null(smooth))
        ts.df$smooth <- smooth

    if (grepl("%var", title))
        title <- gsub("%var", paste(obj$currVar, collapse = ", "), title)

    xr <- diff(range(ts.df$Date))
    yr <- diff(range(ts.df$value))
    asp <- xr / yr / aspect # + ifelse(multiseries, 2, 0))

    tsplot <- ggplot(ts.df, aes(x = Date, y = value, 
                                group = variable, colour = variable)) +
        coord_fixed(ratio = asp) +
        xlab(xlab) + ylab(ylab) + ggtitle(title)
    if (!multiseries) tsplot <- tsplot + scale_colour_manual(values = "black")

    if (plot && animate && !multiseries) {
        ## Do a bunch of things to animate the plot ...
        dev.hold()
        print(tsplot + geom_point())
        dev.flush()

        Sys.sleep(1)
        for (i in 2:nrow(ts.df)) {
            dev.hold()
            print(tsplot + geom_point() + geom_line(data = ts.df[1:i, ]))
            dev.flush()
            Sys.sleep(ifelse(i > 9, 0.05, 0.5))
        }
        Sys.sleep(1)
    }


    tsplot <- tsplot + geom_line(lwd = 1)
    
    if (!is.null(smooth))
        tsplot <- 
            if (multiseries)
                tsplot + geom_line(aes(x = Date, y = smooth, color = variable),
                          linetype = "22", lwd = 1) +
                geom_point(aes(x = Date, y = smooth, shape = variable, color = variable), 
                           data = ts.df[ts.df$Date == max(ts.df$Date), ],
                           size = 2, stroke = 2)
                # scale_shape_discrete(seq_along(obj$currVar), solid = FALSE, guide = FALSE)
            else
                tsplot + geom_line(aes(x = Date, y = smooth), color = "red")

    if (plot) {
        dev.hold()
        print(tsplot)
        dev.flush()
    }

    invisible(tsplot)
}

##' Time series plot - depreciated
##' @export
rawplot <- function(...) {
    cat("Depreciated: use `plot()` instead.\n")
    plot(...)
}