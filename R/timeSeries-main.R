# This is a module used for graphical time series analysis.
e$timeSeries <- function() {
  e$tsStructure <- list(start = NA, frequency = NA)
  fully.loaded <- FALSE
  tswin <- gwindow(title = "Time Series", expand = FALSE)
  main.group <- ggroup(horizontal = FALSE, expand = TRUE, container = tswin)
  main.layout <- glayout(container = main.group)
  main.layout[1:2, 1] <- (ts.select <- gradio(c("Select *TIME* Variable", "Provide Time Information"),
                                              handler = function(h, ...) {
                                                if (svalue(ts.select, index = TRUE) == 1) {
                                                  enabled(e$createTimeVar) <- FALSE
                                                  enabled(e$tsTimeVar) <- TRUE
                                                } else {
                                                  enabled(e$tsTimeVar) <- FALSE
                                                  enabled(e$createTimeVar) <- TRUE
                                                }
                                              }))

  main.layout[1, 2, expand = TRUE] <- (e$tsTimeVar <- gcombobox(names(tag(e$obj, "dataSet")),
                                                                handler = function(h, ...) {
                                                                  loadTimeInfo()
                                                                }))

  loadTimeInfo <- function() {
    vardata <- tag(e$obj, "dataSet")[, svalue(e$tsTimeVar)]
    ts.info <- get.ts.structure(vardata)
    e$tsStructure <- ts.info
    if (any(is.na(ts.info$start))) {
      if (! fully.loaded) {
        fully.loaded <- TRUE
      } else {
        gmessage("Invalid date", title = "Error", icon = "error")
      }
      return()
    }
  }

  # Just run once
  loadTimeInfo()

  main.layout[2, 2, expand = TRUE] <- (e$createTimeVar <- gbutton("Create",
                      handler = function(h, ...) {
                        e$createTSInfo()
                      }))

  enabled(e$createTimeVar) <- FALSE
  allow.recompose <- FALSE

  # Need to create an environment, within which we are able to
  # allow for a somewhat "global" variable
  tsenv <- new.env()
  assign("stopAnimation", FALSE, envir = tsenv)

  main.layout[3, 1:2] <- glabel("Select variable(s)\n(Use Ctrl for multiple selection)")
  main.layout[4, 1:2] <- (e$tsVarselect <- gtable(names(tag(e$obj, "dataSet")), multiple = TRUE))
  size(e$tsVarselect) <- c(300, 200)
  main.layout[5, 1:2] <- (single.label <- glabel("Single Series"))
  main.layout[6, 1] <- (single.tsplot.animated <- gbutton("Time Series Plot - Animate",
                                                          handler = function(h, ...) {
                                                            if (svalue(single.tsplot.animated) == "Time Series Plot - Animate") {
                                                              assign("stopAnimation", FALSE, envir = tsenv)
                                                              ts.info <- e$tsStructure
                                                              valid.ts <- valid.ts.info(ts.info)
                                                              # We always know that there will be at least one selected value
                                                              # otherwise we would not be able to run this handler
                                                              var.df <- tag(e$obj, "dataSet")[, svalue(e$tsVarselect), drop = FALSE]
                                                              valid.var <- valid.vars(var.df)
                                                              allow.recompose <- FALSE
                                                              enabled(single.recompose) <- allow.recompose
                                                              enabled(single.recomp.result) <- allow.recompose
                                                              if (valid.ts && valid.var) {
                                                                enabled(single.label) <- FALSE
                                                                enabled(single.tsplot.animated) <- TRUE
                                                                svalue(single.tsplot.animated) <- "Skip Animation"
                                                                enabled(single.tsplot) <- FALSE
                                                                enabled(single.decompose) <- FALSE
                                                                enabled(single.seasonal) <- FALSE
                                                                enabled(single.forecast) <- FALSE
                                                                enabled(single.recompose) <- FALSE
                                                                enabled(single.recomp.result) <- FALSE
                                                                e$tsPlot(var.df = var.df, start = ts.info$start,
                                                                         frequency = ts.info$frequency, animate = TRUE, env = tsenv)
                                                                enabled(single.label) <- TRUE
                                                                enabled(single.tsplot.animated) <- TRUE
                                                                svalue(single.tsplot.animated) <- "Time Series Plot - Animate"
                                                                enabled(single.tsplot) <- TRUE
                                                                enabled(single.decompose) <- TRUE
                                                                enabled(single.recompose) <- FALSE
                                                                enabled(single.recomp.result) <- FALSE
                                                                enabled(single.seasonal) <- length(ts.info$start) > 1
                                                                enabled(single.forecast) <- length(ts.info$start) > 1
                                                              }
                                                            } else if (svalue(single.tsplot.animated) == "Skip Animation") {
                                                              ts.info <- e$tsStructure
                                                              assign("stopAnimation", TRUE, envir = tsenv)
                                                              enabled(single.label) <- TRUE
                                                              enabled(single.tsplot.animated) <- TRUE
                                                              svalue(single.tsplot.animated) <- "Time Series Plot - Animate"
                                                              enabled(single.tsplot) <- TRUE
                                                              enabled(single.decompose) <- TRUE
                                                              enabled(single.seasonal) <- length(ts.info$start) > 1
                                                              enabled(single.forecast) <- length(ts.info$start) > 1
                                                              enabled(single.recompose) <- FALSE
                                                              enabled(single.recomp.result) <- FALSE
                                                            }
                                                          }))
  main.layout[6, 2] <- (single.tsplot <- gbutton("Time Series Plot",
                                                 handler = function(h, ...) {
                                                   ts.info <- e$tsStructure
                                                   valid.ts <- valid.ts.info(ts.info)
                                                   # We always know that there will be at least one selected value
                                                   # otherwise we would not be able to run this handler
                                                   var.df <- tag(e$obj, "dataSet")[, svalue(e$tsVarselect), drop = FALSE]
                                                   valid.var <- valid.vars(var.df)
                                                   allow.recompose <- FALSE
                                                   enabled(single.recompose) <- allow.recompose
                                                   enabled(single.recomp.result) <- allow.recompose
                                                   if (valid.ts && valid.var) {
                                                     assign("stopAnimation", FALSE, envir = tsenv)
                                                     e$tsPlot(var.df = var.df, start = ts.info$start,
                                                              frequency = ts.info$frequency, animate = FALSE, env = tsenv)
                                                   }
                                                 }))
  main.layout[7, 1:2] <- (single.decompose <- gbutton("Decompose",
                                                      handler = function(h, ...) {
                                                        ts.info <- e$tsStructure
                                                        valid.ts <- valid.ts.info(ts.info)
                                                        var.df <- tag(e$obj, "dataSet")[, svalue(e$tsVarselect), drop = FALSE]
                                                        valid.var <- valid.vars(var.df)
                                                        if (valid.ts && valid.var) {
                                                          allow.recompose <- TRUE
                                                          enabled(single.recompose) <- allow.recompose
                                                          enabled(single.recomp.result) <- allow.recompose
                                                          svalue(single.recompose) <- "Recompose - Animate"
                                                          e$tsDecompose(var.df = var.df, start = ts.info$start, frequency = ts.info$frequency)
                                                        }
                                                      }))
  main.layout[8, 1] <- (single.recompose <- gbutton("Recompose - Animate",
                                                      handler = function(h, ...) {
                                                        if (svalue(single.recompose) == "Recompose - Animate") {
                                                          ts.info <- e$tsStructure
                                                          valid.ts <- valid.ts.info(ts.info)
                                                          var.df <- tag(e$obj, "dataSet")[, svalue(e$tsVarselect), drop = FALSE]
                                                          valid.var <- valid.vars(var.df)
                                                          allow.recompose <- FALSE
                                                          enabled(single.recompose) <- allow.recompose
                                                          enabled(single.recomp.result) <- allow.recompose
                                                          if (valid.ts && valid.var) {
                                                            enabled(single.label) <- FALSE
                                                            enabled(single.tsplot.animated) <- FALSE
                                                            enabled(single.tsplot) <- FALSE
                                                            enabled(single.decompose) <- FALSE
                                                            enabled(single.seasonal) <- FALSE
                                                            enabled(single.forecast) <- FALSE
                                                            enabled(single.recompose) <- TRUE
                                                            svalue(single.recompose) <- "Skip Animation"
                                                            assign("stopAnimation", FALSE, envir = tsenv)
                                                            e$tsRecompose(var.df = var.df, start = ts.info$start,
                                                                          frequency = ts.info$frequency, env = tsenv)
                                                            enabled(single.label) <- TRUE
                                                            enabled(single.tsplot.animated) <- TRUE
                                                            enabled(single.tsplot) <- TRUE
                                                            enabled(single.decompose) <- TRUE
                                                            enabled(single.recompose) <- FALSE
                                                            svalue(single.recompose) <- "Recompose - Animate"
                                                            enabled(single.seasonal) <- length(ts.info$start) > 1
                                                            enabled(single.forecast) <- length(ts.info$start) > 1
                                                          }
                                                        } else if (svalue(single.recompose) == "Skip Animation") {
                                                          ts.info <- e$tsStructure
                                                          enabled(single.label) <- TRUE
                                                          enabled(single.tsplot.animated) <- TRUE
                                                          enabled(single.tsplot) <- TRUE
                                                          enabled(single.decompose) <- TRUE
                                                          enabled(single.seasonal) <- length(ts.info$start) > 1
                                                          enabled(single.forecast) <- length(ts.info$start) > 1
                                                          enabled(single.recompose) <- FALSE
                                                          svalue(single.recompose) <- "Recompose - Animate"
                                                          enabled(single.recomp.result) <- FALSE
                                                          assign("stopAnimation", TRUE, envir = tsenv)
                                                        }
                                                      }))
  main.layout[8, 2] <- (single.recomp.result <- gbutton("Recompose - Result",
                                                        handler = function(h, ...) {
                                                          ts.info <- e$tsStructure
                                                          valid.ts <- valid.ts.info(ts.info)
                                                          var.df <- tag(e$obj, "dataSet")[, svalue(e$tsVarselect), drop = FALSE]
                                                          valid.var <- valid.vars(var.df)
                                                          allow.recompose <- FALSE
                                                          enabled(single.recompose) <- allow.recompose
                                                          enabled(single.recomp.result) <- allow.recompose
                                                          if (valid.ts && valid.var) {
                                                            assign("stopAnimation", FALSE, envir = tsenv)
                                                            e$tsRecompResult(var.df = var.df, start = ts.info$start,
                                                                             frequency = ts.info$frequency, env = tsenv)
                                                          }
                                                        }))
  main.layout[9, 1:2] <- (single.seasonal <- gbutton("Seasonal effect",
                                                     handler = function(h, ...) {
                                                       ts.info <- e$tsStructure
                                                       valid.ts <- valid.ts.info(ts.info)
                                                       var.df <- tag(e$obj, "dataSet")[, svalue(e$tsVarselect), drop = FALSE]
                                                       valid.var <- valid.vars(var.df)
                                                       allow.recompose <- FALSE
                                                       enabled(single.recompose) <- allow.recompose
                                                       enabled(single.recomp.result) <- allow.recompose
                                                       if (valid.ts && valid.var)
                                                         e$tsSeasonal(var.df = var.df, start = ts.info$start, frequency = ts.info$frequency)
                                                     }))
  main.layout[10, 1:2] <- (single.forecast <- gbutton("Predict",
                                                      handler = function(h, ...) {
                                                        ts.info <- e$tsStructure
                                                        valid.ts <- valid.ts.info(ts.info)
                                                        var.df <- tag(e$obj, "dataSet")[, svalue(e$tsVarselect), drop = FALSE]
                                                        valid.var <- valid.vars(var.df)
                                                        allow.recompose <- FALSE
                                                        enabled(single.recompose) <- allow.recompose
                                                        enabled(single.recomp.result) <- allow.recompose
                                                        if (valid.ts && valid.var) {
                                                          hwPredictionWindow(var.df, start = ts.info$start, frequency = ts.info$frequency)
                                                          e$tsForecast(var.df = var.df, start = ts.info$start, frequency = ts.info$frequency)
                                                        }
                                                      }))
  main.layout[11, 1:2] <- (several.label <- glabel("Several Series"))
  main.layout[12, 1:2] <- (several.compare.series <- gbutton("Compare Series", container = main.group,
                                                             handler = function(h, ...) {
                                                               ts.info <- e$tsStructure
                                                               valid.ts <- valid.ts.info(ts.info)
                                                               var.df <- tag(e$obj, "dataSet")[, svalue(e$tsVarselect), drop = FALSE]
                                                               valid.var <- valid.vars(var.df)
                                                               allow.recompose <- FALSE
                                                               enabled(single.recompose) <- allow.recompose
                                                               enabled(single.recomp.result) <- allow.recompose
                                                               if (valid.ts && valid.var)
                                                                 e$compareSeries(var.df = var.df, start = ts.info$start, frequency = ts.info$frequency)
                                                             }))

  multiple.select.handler <- function(...) {
    ts.info <- e$tsStructure
    invalid.time <- any(is.na(ts.info$start))
    if (! invalid.time) {
      show.annual <- length(ts.info$start) > 1
      n <- length(svalue(e$tsVarselect))
    } else {
      n <- 0
    }
    if (n > 1) {
      enabled(single.label) <- FALSE
      enabled(single.tsplot.animated) <- FALSE
      enabled(single.tsplot) <- FALSE
      enabled(single.decompose) <- FALSE
      enabled(single.recompose) <- FALSE
      enabled(single.recomp.result) <- FALSE
      enabled(single.seasonal) <- FALSE
      enabled(single.forecast) <- FALSE
      enabled(several.label) <- TRUE
      enabled(several.compare.series) <- TRUE
    } else if (n == 1) {
      enabled(single.label) <- TRUE
      enabled(single.tsplot.animated) <- TRUE
      enabled(single.tsplot) <- TRUE
      enabled(single.decompose) <- TRUE
      enabled(single.recompose) <- allow.recompose
      enabled(single.recomp.result) <- allow.recompose
      enabled(single.seasonal) <- show.annual
      enabled(single.forecast) <- show.annual
      enabled(several.label) <- FALSE
      enabled(several.compare.series) <- FALSE
    } else {
      enabled(single.label) <- FALSE
      enabled(single.tsplot.animated) <- FALSE
      enabled(single.tsplot) <- FALSE
      enabled(single.decompose) <- FALSE
      enabled(single.recompose) <- FALSE
      enabled(single.recomp.result) <- FALSE
      enabled(single.seasonal) <- FALSE
      enabled(single.forecast) <- FALSE
      enabled(several.label) <- FALSE
      enabled(several.compare.series) <- FALSE
    }
  }

  # Each time that the gtable is clicked, check how many
  # of the elements are selected so that we can enable the
  # correct buttons
  addHandlerClicked(e$tsVarselect, handler = multiple.select.handler)
  multiple.select.handler()
}

e$tsPlot <- function(var.df, start, frequency, animate, env) {
  vars <- list(data = var.df, start = start, freq = frequency)
  plot.raw.data(vars, animate = animate, e = env)
}

e$tsDecompose <- function(var.df, start, frequency) {
  vars <- list(data = var.df, start = start, freq = frequency)
  vars <- decomposition(vars)
  plot.decomposition(vars)
}

e$tsRecompose <- function(var.df, start, frequency, env) {
  vars <- list(data = var.df, start = start, freq = frequency)
  vars <- decomposition(vars)
  recompose(vars, animate = TRUE, e = env)
}

e$tsRecompResult <- function(var.df, start, frequency, env) {
  vars <- list(data = var.df, start = start, freq = frequency)
  vars <- decomposition(vars)
  recompose(vars, animate = FALSE, e = env)
}

e$tsSeasonal <- function(var.df, start, frequency) {
  vars <- list(data = var.df, start = start, freq = frequency)
  make.seasonplot(vars)
}

hwPredictionWindow <- function(var.df, start, frequency) {
  vars <- list(data = var.df, start = start, freq = frequency)
  pw <- gwindow(title = "Prediction Output", width = 600, height = 400)
  pg <- ggroup(horizontal = FALSE, use.scrollwindow = TRUE, container = pw)
  predtext <- gtext("", font.attr = c(family = "monospace"), wrap = FALSE,
                    expand = TRUE, container = pg)

  # Get output from HW fitting
  vars = tsStructure()
  tsObj = vars$tsObj
  x.vals = get.x(tsObj)

  # forecast 2 whole cycles ahead
  ahead = 2 * vars$freq

  hw.fit = HoltWinters(tsObj)
  pred = predict(hw.fit, n.ahead = ahead, TRUE)

  colnames(pred) <- c("fitted", "upper 95% bound", "lower 95% bound")
  pred <- pred[, c(1, 3, 2)] # Reorder cols
  printed.text <- capture.output(print(pred))
  insert(predtext, printed.text)
}

e$tsForecast <- function(var.df, start, frequency) {
  vars <- list(data = var.df, start = start, freq = frequency)
  hw.forecast(vars)
}

e$compareSeries <- function(var.df, start, frequency) {
  vars <- list(data = var.df, start = start, freq = frequency)
  # Dealing with multiple values
  multiseries(vars)#, start, frequency)
}

get.ts.structure <- function(vardata) {
  if (is.factor(vardata))
    vardata <- as.character(vardata)

  if (any(is.na(vardata))) {
    gmessage("There is a hole in your time series", icon = "error")
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
      start <- NA
      freq <- NA
      gmessage("There is a hole in your time series", icon = "error")
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
      start <- NA
      freq <- NA
      gmessage("There is a hole in your time series", icon = "error")
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
      start <- NA
      freq <- NA
      gmessage("There is a hole in your time series", icon = "error")
    }
  }

  list(start = start, frequency = freq)
}

valid.ts.info <- function(ts.info) {
  valid <- TRUE

  # If we haven't set a time variable, inform the user
  if (any(is.na(ts.info$start))) {
    valid <- FALSE
    gmessage("Please set a valid time variable", title = "Error", icon = "error")
  }

  valid
}

valid.vars <- function(var.df) {
  var.classes <- lapply(var.df, class)
  # Only allow numeric values
  valid <- all(var.classes %in% c("numeric", "integer", "double"))
  if (! valid) {
    gmessage("A data variable is not numeric, no categorical variables are allowed.", title = "Error", icon = "error")
  }
  valid
}
