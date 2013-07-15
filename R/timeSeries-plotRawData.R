


tsdataplot <- function(x, animate = FALSE, e = NULL) {

}

### The plot.raw.data function plots the time series tsObj in a seperate window
### to the current decomposition window (if one is open). The plotting is done so
### that the dimensions and scale of the time series plot matches the height of
### the main plotting viewport in the decomposition plot. To do this we call the
### decomposition function.

plot.raw.data = function(vars, animate = FALSE, e = NULL) {

    height = 5; width = 6
    vars = make.ts(vars, 1)
    ### x and y coordinates of the time series tsObj
    tsObj = vars$tsObj
    xlist = get.x(tsObj)
    x = xlist$x
    x.units = xlist$x.units
    y = tsObj@.Data
    y.units = unit(y, "native")

    ### We want a trend line, so do a decomposition
    if (frequency(tsObj) > 1) {
        decomp = decomposition(vars)$decompVars
        smooth = decomp$components[,"trend"]
    } else {
        smooth = loess(vars$data[,1] ~ x)$fitted
    }

    ### Height of the plotting viewport needs to be scale.factor times the height
    ### of the trend viewport in the decomposition plot


    plotHeight = 2

    parent.vp = viewport(layout = grid.layout(3, 3,
                                              heights = unit(c(1, plotHeight, 1),
                                                             c("null", "inches", "null")),
                                              widths = unit(c(.7, 1, .2),
                                                            c("inches", "null", "inches"))),
                         name = "parent")
    head.vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2, name = "head")
    left.vp = viewport(layout.pos.row = 2, layout.pos.col = 1, name = "left")
    right.vp = viewport(layout.pos.row = 2, layout.pos.col = 3, name = "right")
    bottom.vp = viewport(layout.pos.row = 3, layout.pos.col = 1:2, name = "bottom")

    plot.vp = viewport(name = "plot", layout.pos.row = 2, layout.pos.col = 2,
                       xscale = extendrange(r = range(x)),
                       yscale = extendrange(r = range(y, smooth)))
    plot.vptree = vpTree(parent.vp, vpList(head.vp, left.vp, plot.vp, right.vp,
                         bottom.vp))

    ### The following creates a gTree which contains all of our grobs
    grobList = gList(rectGrob(vp = vpPath("parent", "plot"), name = "border"),
                     linesGrob(x.units, y.units, vp = vpPath("parent", "plot"),
                               name = "line", gp = gpar(col = "black", lwd = 2)),
                     linesGrob(x.units, unit(smooth, "native"), name = "smooth",
                               gp = gpar(col = "red"), vp = vpPath("parent", "plot")),
                     yaxisGrob(vp = vpPath("parent", "plot"), name = "yAxis",
                               gp = gpar(cex = .8)),
                     xaxisGrob(vp = vpPath("parent", "plot"), name = "xAxis",
                               gp = gpar(cex = .8)),
                     textGrob(paste("Time series plot for", vars$currentName),
                              hjust = 0.5, vjust = -1.5, y = 0,
                              name = "topLabel",
                              vp = vpPath("parent", "head")))

    image = gTree(name = "image", children = grobList, childrenvp = plot.vptree)
    newdevice(width = width, height = height)

    if (animate) {
        final.line <- getGrob(image, "line")
        final.smooth <- getGrob(image, "smooth")
        image <- removeGrob(image, "line")
        image <- removeGrob(image, "smooth")
        n.points <- length(final.line$x)

        # Drawing initial points
        p <- pointsGrob(x = final.line$x, y = final.line$y,
                        vp = vpPath("parent", "plot"), size = unit(2, "mm"),
                        pch = 19, name = "points", gp = gpar(col = "black"))
        image <- addGrob(image, p)
        pauseImage(image, 200)

        for (i in 1:n.points) {
            if (get("stopAnimation", envir = e) && i < n.points)
                next
            l <- linesGrob(x = final.line$x[1:i], y = final.line$y[1:i],
                           vp = vpPath("parent", "plot"),
                           name = "line", gp = gpar(col = "black", lwd = 2))
            image <- addGrob(image, l)
            speed <- if (i < 20) 30
                     else if (i < 60) 10
                     else 1
            pauseImage(image, speed)
        }

        if (! get("stopAnimation", envir = e)) {
            pauseImage(image, 5)
            image <- removeGrob(image, "points")
            pauseImage(image, 5)
            image <- addGrob(image, final.smooth)
        } else {
            image <- removeGrob(image, "points")
            image <- addGrob(image, final.line)
            image <- addGrob(image, final.smooth)
        }
    }
    drawImage(image)
}
