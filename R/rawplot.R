##' Draws a plot of a given \code{iNZightTS} object with the trend superimposed.
##'
##' If animate is set to \code{TRUE}, a scatterplot of all points in the
##' time series will appear followed by slowly drawn lines connecting the
##' points, simulating the drawing of a time series by hand.
##'
##' @title Draw a simple time series plot
##'
##' @param obj an \code{iNZightTS} object
##'
##' @param multiplicative logical. If \code{TRUE}, a multiplicative model is used,
##' otherwise an additive model is used by default.
##'
##' @param ylab a title for the y axis
##'
##' @param xlab a title for the x axis
##'
##' @param animate animate the plotting process?
##'
##' @param t smoothing parameter
##' @param aspect the aspect ratio of the plot; 
##'        it will be about ASPECT times wider than it is high
##'
##' @keywords timeseries
##'
##' @export
rawplot <-
  function(obj, multiplicative = FALSE, ylab = obj$currVar, xlab = "Date",
           animate = FALSE, t = 10, aspect = 3) {

    if (any(grepl("^iNZightMTS$", class(data))))
        stop("Time-Series must be univariate")

    height = 5; width = 6
    ### x and y coordinates of the time series tsObj
    tsObj = obj$tsObj
    xlist = get.x(tsObj)
    x = xlist$x
    x.units = xlist$x.units
    y = tsObj@.Data
    y.units = unit(y, "native")

    ### We want a trend line, so do a decomposition
    if (frequency(tsObj) > 1) {
        decomp = decomposition(obj, ylab = "", multiplicative = multiplicative, t = t)$decompVars
        if (multiplicative)
          smooth = exp(log(decomp$components[,"trend"]))
        else
          smooth = decomp$components[,"trend"]
    } else {
        smooth = loess(obj$data[1:length(obj$tsObj), obj$currVar] ~ x)$fitted
    }

    ### Height of the plotting viewport needs to be scale.factor times the height
    ### of the trend viewport in the decomposition plot

    value <- obj$currVar
    ts.df <- data.frame(Date = as.numeric(time(tsObj)),
                        value = as.matrix(tsObj),
                        smooth = as.matrix(smooth))
    
    headtitle <- paste("Time series plot for", obj$currVar)

    xr <- diff(range(x))
    yr <- diff(range(y))
    asp <- xr / yr / aspect

    tsplot <- ggplot(ts.df, aes(x = Date, y = value)) +
        coord_fixed(ratio = asp) +
        # theme(panel.grid.minor.y = element_blank()) +
        xlab(xlab) + ylab(ylab) # + ggtitle(headtitle)


    if (animate) {
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


    tsplot <- tsplot + geom_line() + geom_line(aes(x = Date, y = smooth), color = "red")
    dev.hold()
    print(tsplot)
    dev.flush()

    invisible(tsplot)
}
