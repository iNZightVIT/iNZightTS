get.x <-
function(tsObj) {
    ### figure out the limits and step size along the x axis
    f = frequency(tsObj)
    s = start(tsObj)
    if (f == 1) {
      start.x = s[1]
      step.x = 1
      end.x = start.x + length(tsObj) - 1
    }
    else {
      step.x = 1/f
      start.x = s[1] + (s[2] - 1) * step.x
      end.x = start.x + step.x * (length(tsObj) - 1)
    }

    x = seq(start.x, end.x, by = step.x)
    x.units = unit(x, "native")
    list(x = x, x.units = x.units)
}

get.x2 <-
  function(tsObj) {
    ### figure out the limits and step size along the x axis
    f = frequency(tsObj)
    s = start(tsObj)
    if (f == 1) {
      start.x = s[1]
      step.x = 1
      end.x = start.x + length(tsObj) - 1
    }
    else {
      step.x = 1/f
      start.x = s[1] + (s[2] - 1) * step.x
      end.x = start.x + step.x * (length(tsObj) - 1)
    }

    x = seq(start.x, end.x, by = step.x)
    x = order(x)
    x = x/max(x)
    x.units = unit(x, "native")
    list(x = x, x.units = x.units)
  }

get.line.coords <-
function(vars.decomp, vpName, lineGrobName) {
    decomp = vars.decomp$decompVars
    seekViewport(vpName)
    line = getGrob(decomp$tree, lineGrobName)
    line.y = convertUnit(line$y, attr(line$y[1], "unit"), valueOnly = TRUE)
    line.vp.yrange = current.viewport()$yscale
    line.y.npc = (line.y - line.vp.yrange[1]) / diff(line.vp.yrange)
    line.y.parent = switch(vpName,
                           season = decomp$props["remainder"] +
                                      line.y.npc * decomp$props["seasonal"],
                           random = line.y.npc * decomp$props["remainder"],
                            trend = line.y.npc * decomp$props["trend"] +
                                      decomp$props["seasonal"] +
                                      decomp$props["remainder"])
    line.x = convertUnit(line$x, "native", valueOnly = TRUE)
    line.vp.xrange = current.viewport()$xscale
    line.x.npc = (line.x - line.vp.xrange[1]) / diff(line.vp.xrange)
    x.parent = line.x.npc

    list(line.y = line.y, line.vp.yrange = line.vp.yrange,
         line.y.npc = line.y.npc, line.y.parent = line.y.parent,
         line.x = line.x, line.vp.xrange = line.vp.xrange,
         line.x.npc = line.x.npc, x.parent = x.parent,
         line.col = line$gp$col)
}



add.line.plots.vp <-
function(vars.decomp, vpName, lineCol = "red",
                             name = paste(vpName, "copy", sep = ".")) {
    z = get.line.coords(vars.decomp, vpName, paste(vpName, "Line", sep = ""))
    lineCopy = linesGrob(unit(z$x.parent, "npc"),
                         unit(z$line.y.parent, "npc"),
                         name = name,
                         vp = vpPath("parent", "plots"),
                         gp = gpar(col = lineCol))
    updated.tree = addGrob(vars.decomp$decompVars$tree, lineCopy)
    vars.decomp$decompVars$tree = updated.tree
    vars.decomp
}


newdevice <-
function(...) {
    warning("Depreciated. Use iNZightTools::newdevice() instead.")

    ## if ("package:shiny" %in% search()) {
    ##     ## we should let shiny to set their default graphics device
    ##     ## setting any width and height here force shiny popup a new window to you
    ##     return()
    ## }

    ## if (requireNamespace("iNZightTools", quietly = TRUE)) {
    ##     iNZightTools::newdevice(width = width, height = height, ...)
    ## } else {
    ##     dev.new(width = width, height = height)
    ##     warning("If you experience graphical issues, we suggest installing iNZightTools.")
    ## }
}



drawImage <-
function(image) {
  ## if ("Acinonyx" %in% rownames(installed.packages()))
  ##   plot.new()

  # Draws current image in device.
  dev.hold()
  grid.newpage()
  grid.draw(image)
  dev.flush()

  # On some devices (notably on Mac) we end up being unable to
  # see anything besides a single frame due to buffering.
  # dev.flush() will force the device to show what it has
  # currently buffered.
  if (exists("dev.flush"))
    dev.flush(1)
}



pauseImage <-
function(image, pause = 1) {
  for (i in 1:pause) {
      dev.hold()
      drawImage(image)
      dev.flush()
  }
}



rmGrobs <-
function(image, grobs) {
  for (i in grobs) {
    if (i %in% childNames(image)) {
      image <- removeGrob(image, gPath(i))
    }
  }
  image
}


### a function to choose next odd integer
### this function is used for input of the contro of smoothness of trend


nextodd <- function(x) {
  x <- round(x)
  if (x%%2 == 0)
    x <- x + 1
  as.integer(x)
}
