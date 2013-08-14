forecastplot <-
function(vars) {

    width = 7
    height = 4

    ## a few styles
    obs.col = "black"
    obs.lwd = 2
    fit.col = "#0e8c07"
    pred.col = "#b50000"
    int.col = "#ff7777"
    band.col = "#ffdbdb"

    #vars = make.ts(vars, 1)
    tsObj = vars$tsObj
    x.vals = get.x(tsObj)

    ## forecast 2 whole cycles ahead
    ahead = 2 * vars$freq

    hw.fit = HoltWinters(tsObj)
    pred = predict(hw.fit, n.ahead = ahead, TRUE)
    x.pred.start = tail(x.vals$x, 1)
    x.pred = seq(from = x.pred.start,
                 to = x.pred.start + ahead / vars$freq,
                 by = 1/vars$freq)
    y.pred = c(tail(hw.fit$fitted[,"xhat"], 1), pred[,"fit"])

    ylims = range(hw.fit$fitted[,"xhat"], pred, vars$tsObj)

    xlims = c(x.vals$x[1], tail(x.pred, 1))
    xlims = xlims + diff(xlims) * 0.02 * c(-1, 1)
    ylims = ylims + diff(ylims) * c(-0.02, 0.15)

    x.fit = get.x(hw.fit$fitted[,"xhat"])

    ### set up the viewports
    layout = grid.layout(3, 3,
                         widths = unit(c(1, 1, 1), c("inches", "null", "cm")),
                         heights = unit(c(1.5, 1, 2), c("cm", "null", "cm")))
    parent.vp = viewport(name = "parent", layout = layout)

    vps = vpList()
    vps$plot = viewport(name = "plot", layout.pos.row = 2, layout.pos.col = 2,
                        xscale = xlims, yscale = ylims, default.units = "native")
    vps$left = viewport(name = "left", layout.pos.row = 2, layout.pos.col = 1)
    vps$right = viewport(name = "right", layout.pos.row = 2, layout.pos.col = 3)
    vps$top = viewport(name = "top", layout.pos.row = 1, layout.pos.col = 1:3)
    vps$bottom = viewport(name = "bottom", layout.pos.row = 3, layout.pos.col = 1:3)

    vp.tree = vpTree(parent.vp, vps)

    grobs = gList()
    main = paste("Holt-Winters prediction for", vars$currVar)
    grobs$title = textGrob(main, gp = gpar(cex = 1.3, fontface = "bold"),
                           vp = vpPath("parent", "top"), name = "title")
    grobs$sep = linesGrob(tail(x.vals$x.units, 1), unit(ylims, "native"),
                          gp = gpar(col = "#555555", lty = "dashed"),
                          vp = vpPath("parent", "plot"), name = "separator")
    grobs$obsLine = linesGrob(x.vals$x.units, unit(tsObj@.Data, "native"),
                              gp = gpar(col = obs.col, lwd = obs.lwd),
                              vp = vpPath("parent", "plot"), name = "obsLine")
    grobs$fitLine = linesGrob(x.fit$x.units,
                              unit(hw.fit$fitted[,"xhat"], "native"),
                              gp = gpar(col = fit.col), name = "fitLine",
                              vp = vpPath("parent", "plot"))
    grobs$polygon = polygonGrob(unit(c(x.pred[-1], rev(x.pred[-1])), "native"),
                                unit(c(pred[,"lwr"], rev(pred[,"upr"])), "native"),
                                gp = gpar(fill = band.col, col = "transparent"),
                                name = "polygon",
                                vp = vpPath("parent", "plot"))
    grobs$predLine = linesGrob(unit(x.pred, "native"), unit(y.pred, "native"),
                               gp = gpar(col = pred.col), name = "predLine",
                               vp = vpPath("parent", "plot"))
    grobs$upperLine = linesGrob(unit(x.pred[-1], "native"), unit(pred[,"upr"], "native"),
                                gp = gpar(col = int.col, lty = "dashed"),
                                name = "upperPredLine",
                                vp = vpPath("parent", "plot"))
    grobs$lowerLine = linesGrob(unit(x.pred[-1], "native"), unit(pred[,"lwr"], "native"),
                                gp = gpar(col = int.col, lty = "dashed"),
                                name = "lowerPredLine",
                                vp = vpPath("parent", "plot"))
    grobs$box = rectGrob(vp = vpPath("parent", "plot"), name = "boxRect")
    grobs$yAxis = yaxisGrob(vp = vpPath("parent", "plot"), name = "yAxis",
	                        gp = gpar(cex = 0.9))
    grobs$xAxis = xaxisGrob(vp = vpPath("parent", "plot"), name = "xAxis",
	                        gp = gpar(cex = 0.9))
    grobs$xLabel = textGrob("Time", y = 0.3, name = "xLabel",
	                        vp = vpPath("parent", "bottom"))


    ### Legend grobs
    grobs$rawKey = linesGrob(x = unit(c(2, 8), "mm"),
                       y = unit(1, "npc") - unit(0.6, "lines"),
                       vp = vpPath("parent", "plot"),
                       name = "rawKey",
                       gp = gpar(col = obs.col, lwd = 3))
    grobs$rawKeyText = textGrob(label = "Raw data",
	                      x = unit(10, "mm"),
	                      y = unit(1, "npc") - unit(1, "lines"),
                          just = c("left", "bottom"),
                          vp = vpPath("parent", "plot"),
                          name = "rawKeyText",
                          gp = gpar(cex = .9, fontface = "bold"))

	stringw = stringWidth(grobs$rawKeyText$label)
    grobs$fitKey = linesGrob(x = unit(c(19, 25), "mm") + stringw,
                       y = unit(1, "npc") - unit(0.6, "lines"),
                       vp = vpPath("parent", "plot"),
                       name = "trendKey",
                       gp = gpar(col = fit.col, lwd = 3))
    grobs$fitKeyText = textGrob(label = "Fitted",
	                      x = unit(27, "mm") + stringw,
	                      y = unit(1, "npc") - unit(1, "lines"),
                          just = c("left", "bottom"),
                          vp = vpPath("parent", "plot"),
                          name = "trendKeyText",
                          gp = gpar(cex = .9, fontface = "bold"))

    stringw = stringw + stringWidth(grobs$fitKeyText$label)
    grobs$predKey = linesGrob(x = unit(c(36, 42), "mm") + stringw,
                        y = unit(1, "npc") - unit(0.6, "lines"),
                        vp = vpPath("parent", "plot"),
                        name = "trendSeasonKey",
                        gp = gpar(col = pred.col, lwd = 3))
    grobs$predKeyText = textGrob(label = "Prediction",
	                       x = unit(44, "mm") + stringw,
                           y = unit(1, "npc") - unit(1, "lines"),
                           just = c("left", "bottom"),
                           vp = vpPath("parent", "plot"),
                           name = "trendSeasonKeyText",
                           gp = gpar(cex = .9, fontface = "bold"))


    tree = gTree(children = grobs, childrenvp = vp.tree, name = "tree")

    dev.new(width = 7, height = 4)
    drawImage(tree)
    pred
}
