### make.ts returns a ts object. which.var is the column number/name of
### the data frame called 'data', inside the list called vars. Returns a
### modified list vars with a time series object in it, and the name of the
### corresponding variable.

makeTS = function(x, start, freq) {
    vars = list(data = x, start = start, freq = freq)
    vars$tsObj = ts(x, start = start, frequency = freq)
    vars$currentName = names(x)
    class(vars) = "iNZightTS"
    vars
}

makeWithTimeVar <- function(x, timeVar) {
    timeInfo <- get.ts.structure(timeVar)
    if (any(c(is.na(timeInfo$start), is.na(timeInfo$frequency))))
        stop("There is an error in your time series, a hole in the series perhaps?")
    makeTS(x, timeInfo$start, timeInfo$frequency)
}

get.ts.structure <- function(vardata) {
  if (is.factor(vardata))
    vardata <- as.character(vardata)

  if (any(is.na(vardata))) {
    return(list(start = NA, freq = NA))
  }

  if (is.numeric(vardata)) {
    if (any(vardata != round(vardata)))
      return(list(start = NA, frequency = NA))
    vardata <- as.character(vardata)
    if (any(nchar(vardata) > 4))
      return(list(start = NA, frequency = NA))
  }

  ## What frequency is the data at (monthly, quarterly, yearly)
  firstval <- vardata[1]
  if (nchar(firstval) > 7)
    return(list(start = NA, frequency = NA))
  if (nchar(firstval) > 4) {
    interval <- substring(firstval, 5, 5)
    freq <- ifelse(interval == "Q", 4, 12)
  } else {
    interval <- "A"
    freq <- 1
  }

  # form the start value (used when we create ts objects)
  start <- as.integer(substring(firstval, 1 ,4))
  if (interval != "A")
    start <- c(start, as.integer(substring(firstval, 6)))

  # Checking that we have no holes in the time variable itself
  n <- length(vardata)
  lastyear <- if (is.character(vardata))
               as.integer(substring(tail(vardata, 1), 1, 4))
             else
               tail(vardata, 1)
  if (interval == "A") {
    valid.time <- all(as.character(start:(start + (n - 1))) == vardata)
    if (! valid.time) {
      return(list(start = NA, frequency = NA))
    }
  }

  if (interval == "Q") {
    n.in.first.year <- freq - start[2] + 1
    n.rest <- n - n.in.first.year
    expected.years <- c(rep(start[1], n.in.first.year),
                        rep((start[1] + 1):lastyear, each = freq, length.out = n.rest))
    expected.quarters <- c(start[2]:(start[2] + (n.in.first.year - 1)),
                           rep(1:4, length.out = n.rest))
    expected.times <- paste(expected.years, "Q", expected.quarters, sep = "")
    if (length(expected.times) != length(vardata) || ! all(vardata == expected.times)) {
      return(list(start = NA, frequency = NA))
    }
  }

  if (interval == "M") {
    n.in.first.year <- freq - start[2] + 1
    n.rest <- n - n.in.first.year
    expected.years <- c(rep(start[1], n.in.first.year),
                        rep((start[1] + 1):lastyear, each = freq, length.out = n.rest))
    expected.months <- c(start[2]:(start[2] + (n.in.first.year - 1)),
                         rep(1:12, length.out = n.rest))
    expected.times <- paste(expected.years, "M", sprintf("%02d", expected.months), sep = "")
    if (length(expected.times) != length(vardata) || ! all(vardata == expected.times)) {
      return(list(start = NA, frequency = NA))
    }
  }

  list(start = start, frequency = freq)
}

tsStructureGen <- function() {
    ts.structure <- NULL
    function(start = NULL, freq = NULL) {
        # If both are null, assume that we just want the stored data
        if (is.null(start) && is.null(freq)) {
            if (is.null(ts.structure))
                stop("TS information must first be provided to tsStructure() before retrieving")
            else
                ts.structure
        }
        # If only one is null, assume we've missed some required arg
        if (is.null(start) || is.null(freq))
            stop("One or more arguments are missing or invalid.")
        else {
            ts.structure <<- list(start = start, freq = freq)
            invisible(ts.structure)
        }
    }
}

tsStructure <- tsStructureGen()

print.iNZightTS <- function(x, ...) {
    cat("Time Series Object for", sQuote(x$currentName), "\n")
    cat("\n")
    cat("Data:", paste(head(x$data), collapse = ", "), "...\n")
    cat("Start:", paste(x$start, collapse = ", "), "\n")
    cat("Frequency:", paste(x$freq, collapse = ", "), "\n")
}

### The function get.x computes the 'x values' of the time series
### object inside vars. Returns a list, one element is a numeric
### vector and the other is a native units vector

get.x = function(tsObj) {
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


### The get.line.coords function returns a list of various coordinates
### for a lineGrob. This is necessary because we draw the line copies
### in the plots.vp viewport - the original lines are drawn within the 3
### children viewports of plot.vp.

get.line.coords = function(vars.decomp, vpName, lineGrobName) {
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


### The function add.line.plots.vp adds a copy of a lineGrob to the decomposition
### plot. The original is positioned within the child viewports of plots.vp, the copy
### is drawn within plots.vp so that we can shift the lines upwards (later) more easily

add.line.plots.vp = function(vars.decomp, vpName, lineCol = "red",
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

# Borrowed from VIT, use it for TS decomp & recomp
# because otherwise we end up with "flashy" animation
newdevice <- function(width, height, ...) {
    # The windows device works fine (for now), only attempt to speed up
    # any other devices that we're going to be using.
    # We speed them up by getting rid of bufferring.
    if ("Acinonyx" %in% rownames(installed.packages())) {
        # Acinonyx uses pixels rather than inches, convert inches to
        # pixels to determine dims. Assume 90 dpi.
        width.in <- round(width * 90)
        height.in <- round(height * 90)
        Acinonyx::idev(width = width.in, height = height.in)
    } else {
        if (.Platform$OS.type != "windows" && Sys.info()["sysname"] != "Darwin")
            dev.new(width = width, height = height, type = "nbcairo", ...)
        else
            dev.new(width = width, height = height, ...)
    }
}

drawImage = function(image) {
  if ("Acinonyx" %in% rownames(installed.packages()))
    plot.new()

  # Hold syncing of drawing until we flush, potentially reduces the amount
  # of drawing to the device by a large amount
  if (exists("dev.hold"))
      dev.hold()

  # Draws current image in device.
  grid.newpage()
  grid.draw(image)

  # On some devices (notably on Mac) we end up being unable to
  # see anything besides a single frame due to buffering.
  # dev.flush() will force the device to show what it has
  # currently buffered.
  if (exists("dev.flush"))
    dev.flush()
}

pauseImage = function(image, pause = 1) {
  for (i in 1:pause)
    drawImage(image)
}

rmGrobs = function(image, grobs) {
  for (i in grobs) {
    if (i %in% childNames(image)) {
      image <- removeGrob(image, gPath(i))
    }
  }
  image
}
