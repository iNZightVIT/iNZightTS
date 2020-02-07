get.x <- function(tsObj) {
    ## figure out the limits and step size along the x axis
    f <- frequency(tsObj)
    s <- start(tsObj)
    if (f == 1) {
        start.x <- s[1]
        step.x <- 1
        end.x <- start.x + length(tsObj) - 1
    } else {
        step.x <- 1/f
        start.x <- s[1] + (s[2] - 1) * step.x
        end.x <- start.x + step.x * (length(tsObj) - 1)
    }

    x <- seq(start.x, end.x, by = step.x)
    x.units <- unit(x, "native")
    list(x = x, x.units = x.units)
}

get.x2 <- function(tsObj) {
    ## figure out the limits and step size along the x axis
    f <- frequency(tsObj)
    s <- start(tsObj)
    if (f == 1) {
        start.x <- s[1]
        step.x <- 1
        end.x <- start.x + length(tsObj) - 1
    } else {
        step.x <- 1/f
        start.x <- s[1] + (s[2] - 1) * step.x
        end.x <- start.x + step.x * (length(tsObj) - 1)
    }

    x <- seq(start.x, end.x, by = step.x)
    x <- order(x)
    x <- x/max(x)
    x.units <- unit(x, "native")
    list(x = x, x.units = x.units)
}


newdevice <- function(...) {
    warning("Depreciated. Use iNZightTools::newdevice() instead.")
}


### a function to choose next odd integer
### this function is used for input of the contro of smoothness of trend
nextodd <- function(x) {
    x <- round(x)
    if (x %% 2 == 0)
        x <- x + 1
    as.integer(x)
}

is_multiplicative <- function(x, multiplicative) {
    res <- all(x > 0) && multiplicative
    if (res != multiplicative)
        warning("Cannot use multiplicative model with negative values, defaulting to additive.")
    res
}
