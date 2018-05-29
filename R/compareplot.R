##' Plot a multiple time series object to compare several series
##'
##' @title Plot multiple time series
##' 
##' @param x Multiple time series object
##' @param compare logical, if \code{true}, the series will be graphed in a single plot; 
##'        otherwise graphed in individual rows
##' @param multiplicative logical, if TRUE multiplcative series will be used; otherwise additive
##' @param ylab y axis label
##' @param xlab x axis label
##' @param title the title for the plot
##' @param t smoothing parameter
##' @param aspect aspect ratio (width:height) for the time series
##' @param ... additional arguments
##'
##' @return NULL
##' @author Tom Elliott
##' @export
plot.iNZightMTS <- function(x, compare = TRUE, multiplicative = FALSE, 
                            ylab = 'Value', xlab = "Date", title = "%var",
                            t = 10, aspect = 2, ...) {
  if (compare) {
    ## fetch the main time series plot
    p1 <- NextMethod(x, multiplicative = multiplicative, ylab = ylab, 
                     xlab = xlab, title = title, t = t, aspect = aspect,
                     plot = FALSE, ...)

    if (x$freq > 1) {
      ## for time series with freq > 1, show the seasonal effects
      p1 <- p1 + theme(legend.position = 'none')
      p2 <- compareseasons(x, multiplicative = multiplicative, t = 0)
      
      ## extract legend
      tmp <- ggplot_gtable(ggplot_build(p2))
      legend <- tmp$grobs[[which(sapply(tmp$grobs, function(x) x$name) == "guide-box")]]
      p2 <- p2 + theme(legend.position = 'none')

      ## ensure LHS axes are the same widths
      p1 <- ggplot_gtable(ggplot_build(p1))

      # if (!multiplicative) {
      #   ## first, make scales the same ... 
      #   yr <- range(x$tsObj)
      #   yr <- yr - mean(yr)
      #   p2 <- p2 + ylim(yr)
      # }

      p2 <- ggplot_gtable(ggplot_build(p2))
      max.width <- unit.pmax(p1$widths[2:3], p2$widths[2:3])
      p1$widths[2:3] <- max.width
      p2$widths[2:3] <- max.width
      

      dev.hold()
      gridExtra::grid.arrange(
        p1, p2, legend,
        layout_matrix = rbind(c(1, 1), c(2, 3)),
        heights = c(6, 4), widths = c(6, 4)
      )
      dev.flush()
    } else {
      ## don't show the seasonal effects (because there aren't any!)
      p1 <- p1 + theme(legend.position = 'bottom')
      dev.hold()
      print(p1)
      dev.flush()
    }
  } else {
    ## each series in its own row
    Np <- length(x$currVar)
    plist <- vector('list', Np)
    if (x$freq > 1) slist <- plist
    for (i in 1:Np) {
      subts <- x
      subts$tsObj <- x$tsObj[, x$currVar[i]]
      subts$currVar <- x$currVar[i]
      class(subts) <- "iNZightTS"

      plist[[i]] <- plot(subts, multiplicative = multiplicative, ylab = ylab, 
                         xlab = xlab, title = title, t = t,
                         col = "blue", aspect = NULL, plot = FALSE)
      if (i < Np) plist[[i]] <- plist[[i]] + xlab("")

      if (x$freq > 1) {
        slist[[i]] <- compareseasons(subts, multiplicative = multiplicative, t = t) +
          theme(legend.position = 'none') + ggtitle('')

        if (!multiplicative) {
          ## figure out what the XLIM should be ...
          yl <- range(subts$tsObj)
          yl <- yl - mean(yl)
          slist[[i]] <- slist[[i]] + ylim(yl)
        }
        if (i < Np) slist[[i]] <- slist[[i]] + xlab("")

        ## make sure they have the same heights ...
        plist[[i]] <- ggplot_gtable(ggplot_build(plist[[i]]))
        slist[[i]] <- ggplot_gtable(ggplot_build(slist[[i]]))
        max.height <- unit.pmax(plist[[i]]$heights, slist[[i]]$heights)
        plist[[i]]$heights <- max.height
        slist[[i]]$heights <- max.height
      }
    }
    if (x$freq > 1) {
      plist$layout_matrix <- 
         plist$layout_matrix <- cbind(1:Np, 1:Np + Np, rep(NA, Np))
      plist$widths <- unit.c(unit(6, 'null'), unit(2, 'null'), unit(10, 'mm'))
      plist <- c(plist, slist)
    } else {
      plist$layout_matrix <- cbind(1:Np)
    }

    dev.hold()
    do.call(gridExtra::grid.arrange, plist)
    dev.flush()
  }
  invisible(NULL)
}



compareseasons <- function(x, multiplicative = FALSE, t = 0) {
  varNums <- seq_along(x$currVar)
  trendCol <- "black"
  trendSeasonCol <- "#0e8c07"
  rawCol <- "black"
  seasonCol <- "red"
  groupCol <-  hcl(h = seq(30, 300, by = 270 / (length(x$currVar) - 1)), 
                   c = 50, l = 70)
  groupCol.text <- hcl(h = seq(30, 300, by = 270 / (length(x$currVar) - 1)), 
                       c = 50, l = 40)

  ### put all the necessary "x" variables into a list
  listVars <- vector("list")
  varNames <- character(0)
  for (i in x$currVar) {
    # add the time and the data for the ts
    vardata <- cbind(x$data[, 1, drop = FALSE], x$data[, i, drop = FALSE])
    curr.vars <- x
    curr.vars$data <- vardata
    curr.vars$tsObj <- ts(x$data[, i], x$start, x$end, x$freq)
    curr.vars$currVar <- i
    curr.vars <- decomposition(curr.vars, ylab = "", multiplicative = multiplicative, t = t)

    curr.vars

    name <- gsub("[[:space:]]+", "_", curr.vars$currVar)
    listVars[[name]] <- curr.vars
  }



  n <- length(varNums)
  x.vals <- get.x2(listVars[[1]]$tsObj)
  freq <- listVars[[1]]$freq
  startSeason <- listVars[[1]]$start[2]
  subset <- 1:freq
  if (startSeason > 1) {
    subset <- c(startSeason:freq, 1:(startSeason-1))
  } else {
    subset <- 1:freq
  }

  seasonData = matrix(ncol = 3, nrow = 0)
  ## for multiplicative, divide by trend to get seasonal effect; otherwise subtract
  detrend <- if (multiplicative) `/` else `-`

  compare <- n == 1
  if (compare)
    timeSeasonData <- data.frame()
  for (i in varNums) {
      # raw.y.vals <- listVars[[i]]$decompVars$raw
      # trend.y.vals[,i] <- listVars[[i]]$decompVars$components[,"trend"]@.Data

      season.y.vals <- listVars[[i]]$decompVars$components[, "seasonal"]@.Data
      ordered.vals = numeric(freq)
      ordered.vals[subset] = season.y.vals[1:freq]
      seasonData <- rbind(seasonData, 
                          cbind(group = i, season = 1:freq, value = ordered.vals))

      if (compare)
        timeSeasonData <- 
            data.frame(cycle = as.numeric(floor(time(listVars[[i]]$decompVars$components))),
                       season = rep(subset, length = nrow(listVars[[i]]$decompVars$components)),
                       value = detrend(listVars[[i]]$decompVars$raw, 
                         listVars[[i]]$decompVars$components[,"trend"]@.Data))
  }
  seasonData <- as.data.frame(seasonData)
  seasonData$group <- factor(seasonData$group, levels = seq_along(x$currVar), labels = x$currVar)

  effects <- ifelse(multiplicative, "Multiplicative Seasonal effects", "Additive Seasonal effects")
  labs <- 1:freq
  xlab = "Season"

  if (freq == 12) {
    labs = substring(month.abb, 1, 1)
    xlab = "Month"
  }
  if (freq == 4) {
    labs = paste(month.abb[c(1, 4, 7, 10)],
                 month.abb[c(3, 6, 9, 12)],
                 sep = " - ")
    xlab = "Quarter"
  }
  if (freq == 7) {
    labs = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
    xlab = "Day"
  }

  p <- ggplot(seasonData, aes_(x = ~season, y = ~value,
                               group = ~group, color = ~group, shape = ~group))

  if (compare)
    p <- p +
      geom_path(aes_(x = ~season, y = ~value, group = ~cycle, colour = NULL, shape = NULL),
          data = timeSeasonData, colour = "#bbbbbb")
  
  p <- p +
    geom_hline(yintercept = as.numeric(multiplicative), linetype = 2) +
    geom_line(lwd = 1) +
    geom_point(size = 2, stroke = 2, fill = "white") +
    ggtitle(effects) + ylab("") + xlab(xlab) + labs(color = "", shape = "")

  if (is.character(labs)) {
    p <- p + scale_x_continuous(breaks = 1:freq, labels = labs)
  }

  p
}


##' Comparison plot - depreciated
##' @param x an iNZightTS object
##' @param ... additional arguments passed to `plot()`
##' @export
compareplot <- function(x, ...) {
  cat("Depreciated: use `plot()` instead.\n")
  if (!any(grepl("^iNZightMTS$", class(x))))
      stop("x is not an iNZightMTS object")
  if (x$freq > 1) {
    plot(x, ...)
  } else {
    plot(x, ...)
  }
}
