#' Decompose a time series object
#'
#' @param obj an iNZightTS object
#' @param multiplicative fit a multiplicative time series model?
#' @param t the smoothing parameter
#' @param model.lim limits for the time series model
#' @param data.name the name of the data
#' @export
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

    xlist <- get.x(obj$tsObj)
    x <- xlist$x
    x.units <- xlist$x.units

    n <- length(obj$data)

    if (obj$freq > 1) {
        if (multiplicative)
            tsObj <- log(obj$tsObj)
        else
            tsObj <- obj$tsObj
        ### t.window is the smallest odd integer ranges from about 1.5*frequceny to 2*frequency
        ### the actual minimum value is  1.5 * frequency/(1 - 1.5/s.window)
        ### where s.window = 10* number of observation +1 by putting 'periodic'
        ### t is set to be proportion of 0.5 *frequency
        ### when t =0, the t.window takes the default value/ minimum value -the least smoothness
        ### when t = 1. the t.window takes the maximum value - the most smoothnuess
        decomp <- stl(tsObj,
            "periodic",
            t.window =
                nextodd(ceiling(
                    1.5 * frequency(data) / (1 - 1.5 / (10*n + 1)) +
                        0.5 * frequency(data) * t
                ))
            )
    } else {
        ## freq == 1, non seasonal fitted.
        if (multiplicative)  {
            ### according to internet, the span value varies from about 0.1 to 2
            ### 0.1 gives nearly no smoothness, while 2 gives nearly maximum smoothness
            ### therefore here, the span ranges from 0.1 to 2
            ### the default is 0.75
            trend.comp <-
                loess(log(obj$data[1:length(obj$tsObj), obj$currVar]) ~ x,
                    span = 0.1 + 1.9*t )$fitted + obj$tsObj * 0

            residuals.comp <- log(obj$tsObj) - trend.comp
            seasons.comp <- obj$tsObj * 0
            decomp <- list()
            decomp$time.series <-
                as.ts(data.frame(seasonal = seasons.comp,
                                 trend = trend.comp,
                                 remainder = residuals.comp))
        } else {
            trend.comp <-
                loess(obj$data[1:length(obj$tsObj), obj$currVar] ~ x)$fitted +
                    obj$tsObj * 0

            residuals.comp <- obj$tsObj - trend.comp
            seasons.comp <- obj$tsObj * 0
            decomp <- list()
            decomp$time.series <- as.ts(
                data.frame(
                    seasonal = seasons.comp,
                    trend = trend.comp,
                    remainder = residuals.comp
                )
            )
        }
    }

    decompData <- decomp$time.series    # returns matrix
    obj$decompVars <- list(
        data.name = data.name,
        raw = obj$tsObj@.Data,
        components = decomp$time.series,
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
plot.inzdecomp <- function(x, recompose.progress = c(0, 0),
                           recompose = any(recompose.progress > 0),
                           ylab = x$currVar, xlab = "Date",
                           title = NULL, xlim = c(NA, NA),
                           colour = c("black", "#45a8ff", "orangered")) {
    ## Convert to a dataframe
    td <- data.frame(
        Date = as.matrix(time(x$tsObj)),
        value = as.matrix(x$tsObj),
        trend = as.numeric(x$decompVars$components[, "trend"]),
        seasonal = as.numeric(x$decompVars$components[, "seasonal"]),
        residual = as.numeric(x$decompVars$components[, "remainder"])
    )

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

    p <- ggplot(td, aes_(~Date))
    p0 <- p +
        theme(
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
        )

    if (is.null(title)) {
        print(x$data.name)
        title <- sprintf("Decomposition%s: %s",
            ifelse(is.null(x$decompVars$data.name),
                "", paste(" of", x$decompVars$data.name)),
            x$currVar
        )
    }
    pdata <- p0 +
        geom_path(aes_(y = ~value), colour = "gray") +
        geom_path(aes_(y = ~trend), colour = colour[1]) +
        labs(title = title, y = ylab)
    if (recompose && any(recompose > 0)) {
        ri <- ifelse(recompose.progress[1] == 0, recompose.progress[2], nrow(td))
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
                colour = colour[2]
            )
        if (recompose.progress[1] == 1 && recompose.progress[2] > 0) {
            ri <- recompose.progress[2]
            rtd <- td %>%
                dplyr::mutate(
                    z = ifelse(1:nrow(td) < ri,
                        .data$value,
                        .data$trend[ri] + .data$seasonal[ri] + .data$residual
                    )
                )
            pdata <- pdata +
                geom_path(
                    aes_(y = ~z),
                    data = rtd,
                    colour = colour[3]
                )
        }
    }

    pseason <- p0 +
        geom_path(aes_(y = ~seasonal), colour = colour[2]) +
        labs(subtitle = "Seasonal Swing", y = "") +
        theme(
            # panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()
        )

    presid <- p +
        geom_path(aes_(y = ~residual), colour = colour[3]) +
        labs(subtitle = "Residuals", y = "") +
        ylim(extendrange(rrange, f = rr/2)) +
        theme(
            # panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()
        )

    dev.hold()
    on.exit(dev.flush())
    egg::ggarrange(
        pdata, pseason, presid,
        heights = ratios
    )
}

decomposition <- function(obj, ylab = "", xlab = "", trendCol = "black",
                          seasonCol = "#45a8ff",
                          randCol = seasonCol, multiplicative=FALSE, t = 0,
                          xlim = c(NA, NA), model.lim = NULL) {
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

    xlist <- get.x(obj$tsObj)
    x <- xlist$x
    x.units <- xlist$x.units

    n = length(obj$data)

    if (obj$freq > 1) {
        if (multiplicative)
            tsObj <- log(obj$tsObj)
        else
            tsObj <- obj$tsObj
        ### t.window is the smallest odd integer ranges from about 1.5*frequceny to 2*frequency
        ### the actual minimum value is  1.5 * frequency/(1 - 1.5/s.window)
        ### where s.window = 10* number of observation +1 by putting 'periodic'
        ### t is set to be proportion of 0.5 *frequency
        ### when t =0, the t.window takes the default value/ minimum value -the least smoothness
        ### when t = 1. the t.window takes the maximum value - the most smoothnuess
        decomp <- stl(tsObj,
            "periodic",
            t.window =
                nextodd(ceiling(
                    1.5 * frequency(data) / (1 - 1.5 / (10*n + 1)) +
                        0.5 * frequency(data) * t
                ))
            )
    } else {
        ## freq == 1, non seasonal fitted.
        if (multiplicative)  {
            ### according to internet, the span value varies from about 0.1 to 2
            ### 0.1 gives nearly no smoothness, while 2 gives nearly maximum smoothness
            ### therefore here, the span ranges from 0.1 to 2
            ### the default is 0.75
            trend.comp <-
                loess(log(obj$data[1:length(obj$tsObj), obj$currVar]) ~ x,
                    span = 0.1 + 1.9*t )$fitted + obj$tsObj * 0

            residuals.comp <- log(obj$tsObj) - trend.comp
            seasons.comp <- obj$tsObj * 0
            decomp <- list()
            decomp$time.series <-
                as.ts(data.frame(seasonal = seasons.comp,
                                 trend = trend.comp,
                                 remainder = residuals.comp))
        } else {
            trend.comp <-
                loess(obj$data[1:length(obj$tsObj), obj$currVar] ~ x)$fitted +
                    obj$tsObj * 0

            residuals.comp <- obj$tsObj - trend.comp
            seasons.comp <- obj$tsObj * 0
            decomp <- list()
            decomp$time.series <- as.ts(
                data.frame(
                    seasonal = seasons.comp,
                    trend = trend.comp,
                    remainder = residuals.comp
                )
            )
        }
    }

    decompData <- decomp$time.series    # returns matrix

    ## x and y coordinates
    Y <- obj$tsObj

    if (multiplicative) {
        y <- log(obj$tsObj@.Data)
        y.random <- exp(y - (decompData[,"trend"] + decompData[,"seasonal"]))  # if we not backtransform here
        y.trend <- exp(decompData[,"trend"])  # the decompositionplot will all in log scale
        y.season <- exp(decompData[,"seasonal"])
        y.season2 <- exp(decompData[,"trend"] + decompData[,"seasonal"]) - exp(decompData[,"trend"])
        y.random2 <- Y - as.numeric(exp(decompData[,"trend"] + decompData[,"seasonal"]))
        decompData[, "trend"] <- y.trend
        decompData[, "seasonal"] <- y.season
        decompData[, "remainder"] <- y.random
        y.season <- y.season2
        y.random <- y.random2
    } else {
        y <- obj$tsObj@.Data
        y.trend <- decompData[,"trend"]
        y.season <- decompData[,"seasonal"]
        y.random <- decompData[,"remainder"]
    }
    y.trend.units <- unit(y.trend, "native")
    y.season.units <- unit(y.season, "native")
    y.random.units <- unit(y.random, "native")

    Y <- obj$tsObj@.Data

    ## We want each component plotted on the same scale, so we need
    ## to find the y-axis interval for the most variable component
    minmax <- apply(decompData, 2, function(x) range(x))
    ranges <- minmax[2,] - minmax[1,]
    ranges[2] <- max(y) - min(y)

    expandBy <- (max(y) - min(y)) * 0.1

    expandBy.trend <- (max(Y) - min(Y)) * 0.1

    #if (multiplicative)  #%
    y <- obj$tsObj@.Data  #%
    ## here we edit the viewport y values to provide an extra gap at
    ## the top of each panel
    trend.vp.y <- y

    trend.vp.y[which.max(y)] <- max(y) + expandBy.trend
    trend.vp.y[which.min(y)] <- min(y) - expandBy.trend

    season.vp.y <- y.season

    max.index <- ifelse(obj$freq > 1, which.max(y.season), 1)
    season.vp.y[max.index] <- max(y.season) + expandBy

    if (obj$freq == 1)
        season.vp.y[2] <- -expandBy

    random.vp.y <- y.random
    random.vp.y[which.max(y.random)] <- max(y.random) + expandBy

    ranges <- ranges + expandBy
    which.max.range <- which.max(ranges)
    y.interval <- diff(pretty(minmax[,which.max.range]))[1]

    ## Need to find the proportion of the plot each subplot will be
    ## allocated, and create the viewports
    props <- ranges/sum(ranges)

    ## The following defines the viewport layout for the plot
    ## parent.vp holds everything - with a main central viewport
    ## and 4 viewports around it that act as margins
    vp.heights <- c(.6, 1, .6)
    vertMargins <- sum(vp.heights[-2])
    parent.vp <- viewport(
        name = "parent",
        layout =
            grid.layout(3, 3,
                heights = unit(vp.heights, c("inches", "null", "inches")),
                widths = unit(c(1.1, 1, 1), c("inches", "null", "inches"))
            )
    )
    head.vp <- viewport(layout.pos.row = 1, layout.pos.col = 1:2, name = "head")
    left.vp <- viewport(layout.pos.row = 2, layout.pos.col = 1, name = "left")
    right.vp <- viewport(layout.pos.row = 2, layout.pos.col = 3, name = "right")
    bottom.vp <- viewport(layout.pos.row = 3, layout.pos.col = 1:2, name = "bottom")
    plots.vp <- viewport(
        layout = grid.layout(3, 1, heights = props[c(2, 1, 3)]),
        name = "plots", layout.pos.row = 2, layout.pos.col = 2
    )
    trend.vp <- dataViewport(x, trend.vp.y, name = "trend", layout.pos.row = 1)
    season.vp <- dataViewport(x, season.vp.y, name = "season", layout.pos.row = 2)
    random.vp <- dataViewport(x, random.vp.y, name = "random", layout.pos.row = 3)

    plots.vptree <- vpTree(plots.vp, vpList(trend.vp, season.vp, random.vp))
    final.vptree <- vpTree(parent.vp,
        vpList(head.vp, left.vp, plots.vptree, right.vp, bottom.vp)
    )

    xlims <- season.vp$xscale
    dotted.xcoords <- c(xlims[1], x, xlims[2])
    dotted.point <- 0 #ifelse(multiplicative, 1, 0) #%
    dotted.ycoords <- rep(dotted.point, length(dotted.xcoords))

    ## The following creates a gTree which contains all of our grobs
    grobs = gList()

    grobs$trendBorder <-
        rectGrob(vp = vpPath("parent", "plots", "trend"),
            name = "trendBorder", gp = gpar(lwd = 2))
    grobs$raw.ghost <-
        linesGrob(x.units, unit(y, "native"),
            vp = vpPath("parent", "plots", "trend"),
            name = "raw.ghost",
            gp = gpar(col = "#bbbbbb"))
    grobs$trendLine <-
        linesGrob(x.units, y.trend.units,
            vp = vpPath("parent", "plots", "trend"),
            name = "trendLine",
            gp = gpar(col = trendCol, lwd = 2))
    grobs$trendYaxis <-
        yaxisGrob(main = TRUE,
            vp = vpPath("parent", "plots", "trend"),
            name = "trendYaxis",
            gp = gpar(cex = .8))
    grobs$trendYlab <-
        textGrob(ylab, x = 0, y = 0.5, rot = 90,
            vjust = -5,
            vp = vpPath("parent", "plots", "trend"),
            name = "trendYlab")

    gap <- unit(2, "mm")
    space <- unit(8, "mm")
    lineWidth <- unit(6, "mm")
    xc <- unit(2, "mm")
    grobs$trendLabel <-
        textGrob("Trend", just = c("left", "bottom"), x = xc,
            y = unit(1, "npc") - unit(1, "lines"), name = "trendLabel",
            vp = vpPath("parent", "plots", "trend"),
            gp = gpar(cex = .9, col = "black", fontface = "bold"))
    xc <- xc + stringWidth(grobs$trendLabel$label) + gap
    grobs$trendKey <-
        linesGrob(x = unit.c(xc, xc + lineWidth),
            y = unit(1, "npc") - unit(0.6, "lines"),
            vp = vpPath("parent", "plots", "trend"),
            name = "trendKey",
            gp = gpar(col = trendCol, lwd = 2))
    xc <- xc + lineWidth + space
    grobs$rawKeyText <-
        textGrob("Raw data", just = c("left", "bottom"),
            x = xc,
            y = unit(1, "npc") - unit(1, "lines"), name = "rawKeyText",
            vp = vpPath("parent", "plots", "trend"),
            gp = gpar(cex = .9, col = "#bbbbbb", fontface = "bold"))
    xc <- xc + stringWidth(grobs$rawKeyText$label) + gap
    grobs$rawKey <-
        linesGrob(unit.c(xc, xc + lineWidth),
            unit(1, "npc") - unit(0.6, "lines"),
            vp = vpPath("parent", "plots", "trend"),
            name = "rawKey",
            gp = gpar(col = "#bbbbbb"))

    grobs$seasonBorder <- rectGrob(vp = vpPath("parent", "plots", "season"),
        name = "seasonBorder", gp = gpar(lwd = 2))
    grobs$season.y0 <-
        linesGrob(unit(dotted.xcoords, "native"),
            unit(dotted.ycoords, "native"),
            vp = vpPath("parent", "plots", "season"),
            name = "season.y0",
            gp = gpar(col = "#aaaaaa", lty = "1313"))
    grobs$seasonLine <-
        linesGrob(x.units, y.season.units,
            vp = vpPath("parent", "plots", "season"),
            name = "seasonLine",
            gp = gpar(col = seasonCol))
    grobs$seasonYaxis <-
        yaxisGrob(main = FALSE,
            vp = vpPath("parent", "plots", "season"),
            name = "seasonYaxis",
            gp = gpar(cex = .8))
    grobs$seasonLabel <-
        textGrob("Seasonal Swing",
            vp = vpPath("parent", "plots", "season"),
            name = "seasonLabel",
            gp = gpar(cex = .9, col = "black", fontface = "bold"),
            x = unit(0.02, "npc"), y = unit(0.97, "npc"),
            hjust = 0, vjust = 1)

    grobs$randomBorder <-
        rectGrob(vp = vpPath("parent", "plots", "random"),
            name = "randomBorder", gp = gpar(lwd = 2))
    grobs$random.y0 <-
        linesGrob(unit(dotted.xcoords, "native"),
            unit(dotted.ycoords, "native"),
            vp = vpPath("parent", "plots", "random"),
            name = "random.y0",
            gp = gpar(col = "#aaaaaa", lty = "1313"))
    grobs$randomLine <-
        linesGrob(x.units, y.random.units,
            vp = vpPath("parent", "plots", "random"),
            name = "randomLine",
            gp = gpar(col = randCol))
    grobs$randomYaxis <-
        yaxisGrob(main = TRUE,
            vp = vpPath("parent", "plots", "random"),
            name = "randomYaxis",
            gp = gpar(cex = .8))
    grobs$Xaxis <-
        xaxisGrob(gp = gpar(cex = .8),
            vp = vpPath("parent", "plots", "random"),
            name = "Xaxis")

    grobs$XaxisLabel <-
      textGrob(xlab, x = 0.5, y = 0, vjust = 5,
            vp = vpPath("parent", "plots", "random"),
            name = "XaxisLabel")

    grobs$randomLabel <-
        textGrob("Residuals",
            vp = vpPath("parent", "plots", "random"),
            name = "randomLabel",
            gp = gpar(cex = .9, col = "black", fontface = "bold"),
            x = unit(0.02, "npc"), y = unit(0.97, "npc"),
            hjust = 0, vjust = 1)



    data.name = ifelse(ylab=="", "data", ylab)
    grobs$statusText <-
        textGrob(paste0("Decomposition of ", data.name, ":", obj$currVar),
            vp = vpPath("parent", "head"),
            name = "statusText")

    image <- gTree(name = "image", children = grobs,
        childrenvp = final.vptree)

    ## return a list with all the variables we need
    decompVars <- list(
        tree = image, ranges = ranges, props = props,
        data.name = data.name,
        raw = obj$tsObj@.Data,
        components = decompData,
        #currentName =  obj$currVar,
        #components = decomp$time.series,
        vertMargins = vertMargins,
        multiplicative = multiplicative
    )
    obj$decompVars <- decompVars
    obj
}


##' Decomposes a time series into trend, seasonal and residual components
##' using \code{loess}.
##'
##' If the frequency is greater than 1, the components are found using the
##' \code{\link{stl}} function with \code{s.window} set to \code{TRUE}
##' (effectively replacing smoothing by taking the mean).
##' If the frequency is 1, the trend component is found directly by using
##' \code{\link{loess}} and the residuals are the difference between trend
##' and actual values.
##' The trend, seasonal and residual components are plotted on the same
##' scale allowing for easy visual analysis.
##'
##' @title Plot a Time Series Decomposition
##'
##' @param obj an \code{iNZightTS} object
##'
##' @param xlab a title for the x axis
##'
##' @param t a control of smoothness of the trend of the time series with frequency bigger than 1 ranges from 0 to 1
##
##'
##'
##'
##' @param ylab a title for the y axis
##'
##' @param multiplicative logical. If \code{TRUE}, a multiplicative model is used,
##' otherwise an additive model is used by default.
##'
##' @param xlim axis limits, specified as dates
##' @param model.lim time limits to use for modelling
##'
##' @return The original \code{iNZightTS} object with an item \code{decompVars}
##' appended, containing results from the decomposition.
##'
##' @references R. B. Cleveland, W. S. Cleveland, J.E. McRae, and I. Terpenning (1990) STL: A Seasonal-Trend Decomposition Procedure Based on Loess. Journal of Official Statistics, 6, 3iV73.
##'
##' @seealso \code{\link{stl}}, \code{\link{loess}}, \code{\link{iNZightTS}}
##'
##' @examples
##' \dontrun{
##' z <- iNZightTS(ldeaths)
##' y <- decompositionplot(z)
##' }
##' @export
decompositionplot <-
    function(obj, ylab = "", xlab = "", multiplicative=FALSE, t = 0,
             xlim = c(NA, NA), model.lim = NULL) {
        vars <- decomposition(obj, ylab, xlab,
            multiplicative = multiplicative,
            t = t, xlim = xlim, model.lim = model.lim)
        # newdevice(width = 6, height = 5)
        dev.hold()
        drawImage(vars$decompVars$tree)
        dev.flush()
        vars
    }
