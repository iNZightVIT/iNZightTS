#' Plot a multiple time series object to compare several series
#'
#' @title Plot multiple time series
#'
#' @param x Multiple time series object
#' @param compare logical, if \code{true}, the series will be graphed in a single plot;
#'        otherwise graphed in individual rows
#' @param multiplicative logical, if TRUE multiplicative series will be used; otherwise additive
#' @param ylab y axis label
#' @param xlab x axis label
#' @param title the title for the plot
#' @param t smoothing parameter
#' @param smoother logical, if \code{TRUE} the smoother will be drawn
#' @param aspect aspect ratio (width:height) for the time series
#' @param xlim limits to control how much of series is shown
#' @param model.lim time limits to use for modelling
#' @param ... additional arguments
#'
#' @examples
#' tm <- iNZightTS(visitorsQ, var = 2:5)
#' plot(tm)
#' plot(tm, compare = FALSE)
#'
#' @return No return value, called for the side effect of drawing a plot.
#' @author Tom Elliott
#' @export
plot.iNZightMTS <- function(x, compare = TRUE, multiplicative = FALSE,
                            ylab = "Value", xlab = "Date", title = "%var",
                            t = 10, smoother = TRUE, aspect = 2,
                            xlim = c(NA, NA), model.lim = NULL, ...) {

    multiplicative <- is_multiplicative(x$tsObj, multiplicative)

    on.exit(dev.flush())
    if (compare) {
        ## fetch the main time series plot
        p1 <- NextMethod(x,
            multiplicative = multiplicative, ylab = ylab,
            xlab = xlab, title = title, t = t, smoother = smoother,
            aspect = aspect,
            plot = FALSE, xlim = xlim, model.lim = model.lim, ...
        )

        if (x$freq > 1) {
            ## for time series with freq > 1, show the seasonal effects
            p1 <- p1 + theme(legend.position = "none")
            p2 <- compareseasons(x,
                multiplicative = multiplicative,
                t = t,
                model.lim = model.lim
            )

            yratio <- attr(p2, "yratio")
            if (multiplicative || is.null(yratio)) yratio <- 4/6

            ## extract legend
            tmp <- ggplot_gtable(ggplot_build(p2))
            legend <- tmp$grobs[[
                which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
            ]]
            p2 <- p2 + theme(legend.position = "none")

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
            on.exit(dev.flush())
            gridExtra::grid.arrange(
                p1, p2, legend,
                layout_matrix = rbind(c(1, 1), c(2, 3)),
                heights = c(1, yratio), widths = c(6, 4)
            )
        } else {
            ## don't show the seasonal effects (because there aren't any!)
            p1 <- p1 + theme(legend.position = "bottom")
            dev.hold()
            p1
            # dev.flush()
        }
    } else {
        ## each series in its own row
        Np <- length(x$currVar)
        plist <- vector("list", Np)
        if (x$freq > 1) slist <- plist
        for (i in 1:Np) {
            subts <- x
            subts$tsObj <- x$tsObj[, x$currVar[i]]
            subts$currVar <- x$currVar[i]
            class(subts) <- "iNZightTS"

            plist[[i]] <- plot(subts,
                multiplicative = multiplicative,
                ylab = ylab,
                xlab = xlab,
                title = title,
                t = t,
                smoother = smoother,
                col = "blue",
                aspect = NULL,
                plot = FALSE,
                xlim = xlim,
                model.lim = model.lim
            )
            if (i < Np) plist[[i]] <- plist[[i]] + xlab("")

            if (x$freq > 1) {
                slist[[i]] <- compareseasons(subts,
                    multiplicative = multiplicative,
                    t = t,
                    model.lim = model.lim
                ) +
                    theme(legend.position = "none") +
                    ggtitle("")

                if (!multiplicative) {
                    ## figure out what the YLIM should be ...
                    yl <- range(subts$tsObj)
                    yl <- yl - mean(yl)
                    slist[[i]] <- slist[[i]] + ylim(yl)
                }
                if (i < Np) slist[[i]] <- slist[[i]] + xlab("")

                ## make sure they have the same heights ...
                plist[[i]] <- ggplot_gtable(ggplot_build(plist[[i]]))
                slist[[i]] <- ggplot_gtable(ggplot_build(slist[[i]]))
                max.height <- unit.pmax(
                    plist[[i]]$heights,
                    slist[[i]]$heights
                )
                plist[[i]]$heights <- max.height
                slist[[i]]$heights <- max.height
            }
        }
        if (x$freq > 1) {
            plist$layout_matrix <-
                plist$layout_matrix <- cbind(1:Np, 1:Np + Np, rep(NA, Np))
            plist$widths <- unit.c(
                unit(6, "null"),
                unit(2, "null"),
                unit(10, "mm")
            )
            plist <- c(plist, slist)
        } else {
            plist$layout_matrix <- cbind(1:Np)
        }

        dev.hold()
        do.call(gridExtra::grid.arrange, plist)
        # dev.flush()
    }
}



compareseasons <- function(x, multiplicative = FALSE, t = 0,
                           model.lim = NULL) {
    varNums <- seq_along(x$currVar)
    trendCol <- "black"
    trendSeasonCol <- "#0e8c07"
    rawCol <- "black"
    seasonCol <- "red"
    groupCol <- hcl(
        h = seq(30, 300, by = 270 / (length(x$currVar) - 1)),
        c = 50, l = 70
    )
    groupCol.text <- hcl(
        h = seq(30, 300, by = 270 / (length(x$currVar) - 1)),
        c = 50, l = 40
    )

    ### put all the necessary "x" variables into a list
    listVars <- vector("list")
    varNames <- character(0)
    for (i in x$currVar) {
        # add the time and the data for the ts
        vardata <- cbind(
            x$data[, 1, drop = FALSE],
            x$data[, i, drop = FALSE]
        )
        curr.vars <- x
        curr.vars$data <- vardata
        curr.vars$tsObj <- ts(x$data[, i], x$start, x$end, x$freq)
        curr.vars$currVar <- i
        curr.vars <- decompose(curr.vars,
            ylab = "",
            multiplicative = multiplicative,
            t = t,
            model.lim = model.lim
        )

        curr.vars

        name <- gsub("[[:space:]]+", "_", curr.vars$currVar)
        listVars[[name]] <- curr.vars
    }

    n <- length(varNums)
    x.vals <- get.x2(listVars[[1]]$tsObj)
    freq <- listVars[[1]]$freq
    startSeason <- listVars[[1]]$start[2]
    if (!is.null(model.lim)) {
        tt <- time(listVars[[1]]$decompVars$components)
        startSeason <- (tt[1] - floor(tt[1])) * freq + 1
    }
    subset <- 1:freq
    if (startSeason > 1) {
        subset <- c(startSeason:freq, 1:(startSeason - 1))
    } else {
        subset <- 1:freq
    }

    seasonData <- matrix(ncol = 3, nrow = 0)
    ## for multiplicative, divide by trend to get seasonal effect;
    ## otherwise subtract
    detrend <- if (multiplicative) `/` else `-`

    compare <- n == 1
    if (compare) {
        timeSeasonData <- data.frame(stringsAsFactors = TRUE)
    }
    for (i in varNums) {
        season.y.vals <-
            listVars[[i]]$decompVars$components[, "seasonal"]@.Data
        ordered.vals <- numeric(freq)
        ordered.vals[subset] <- season.y.vals[1:freq]
        seasonData <- rbind(
            seasonData,
            cbind(group = i, season = 1:freq, value = ordered.vals)
        )

        if (compare) {
            timeSeasonData <-
                data.frame(
                    cycle = as.numeric(
                        floor(
                            time(listVars[[i]]$decompVars$components)
                        )
                    ),
                    season = rep(subset,
                        length = nrow(listVars[[i]]$decompVars$components)
                    ),
                    value = detrend(
                        listVars[[i]]$decompVars$raw,
                        listVars[[i]]$decompVars$components[, "trend"]@.Data
                    ),
                    stringsAsFactors = TRUE
                )
          }
    }
    seasonData <- as.data.frame(seasonData, stringsAsFactors = TRUE)
    seasonData$group <- factor(seasonData$group,
        levels = seq_along(x$currVar),
        labels = x$currVar
    )

    effects <- ifelse(
        multiplicative,
        "Multiplicative Seasonal effects",
        "Additive Seasonal effects"
    )
    labs <- 1:freq
    xlab <- "Season"

    if (freq == 12) {
        labs <- substring(month.abb, 1, 1)
        xlab <- "Month"
    }
    if (freq == 4) {
        labs <- paste(month.abb[c(1, 4, 7, 10)],
            month.abb[c(3, 6, 9, 12)],
            sep = " - "
        )
        xlab <- "Quarter"
    }
    if (freq == 7) {
        labs <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
        xlab <- "Day"
    }

    p <- ggplot(seasonData,
        aes_(
            x = ~season,
            y = ~value,
            group = ~group,
            color = ~group,
            shape = ~group
        )
    )

    if (compare) {
        p <- p +
            geom_path(
                aes_(
                    x = ~season,
                    y = ~value,
                    group = ~cycle,
                    colour = NULL,
                    shape = NULL
                ),
                data = timeSeasonData,
                colour = "#bbbbbb",
                na.rm = TRUE
            )
    }

    p <- p +
        geom_hline(
            yintercept = as.numeric(multiplicative),
            linetype = 2
        ) +
        geom_line(lwd = 1, na.rm = TRUE) +
        geom_point(size = 2, stroke = 2, fill = "white") +
        ggtitle(effects) +
        ylab("") +
        xlab(xlab) +
        labs(color = "", shape = "")

    if (is.character(labs)) {
        p <- p + scale_x_continuous(breaks = 1:freq, labels = labs)
    }

    ## To get the y-axis ranges relative, compute relative ratio:
    dat_yr <- diff(range(x$tsObj))
    eff_yr <- diff(range(seasonData$value))
    yratio <- eff_yr / dat_yr

    attr(p, "yratio") <- yratio
    p
}


#' Comparison plot - depreciated
#' @param x an iNZightTS object
#' @param ... additional arguments passed to `plot()`
#' @export
#' @return No return value, called for the side effect of drawing a plot.
compareplot <- function(x, ...) {
    warning("Depreciated: use `plot()` instead.\n")
    if (!any(grepl("^iNZightMTS$", class(x)))) {
          stop("x is not an iNZightMTS object")
      }
    if (x$freq > 1) {
        plot(x, ...)
    } else {
        plot(x, ...)
    }
}
