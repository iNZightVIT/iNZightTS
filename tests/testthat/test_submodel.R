context("Use subseries to model the time series")
data(visitorsQ)

t <- iNZightTS(visitorsQ)
tm <- iNZightTS(visitorsQ, var = 2:5)

## single series
test_that("Decomposition uses sub-portion of the plot", {
    expect_silent(d <- decomposition(t, model.lim = c(2005, 2009.75)))
    expect_is(d, "iNZightTS")
    expect_equal(attr(d$decompVars$components, "tsp"), c(2005, 2009.75, 4))
})

test_that("Decomposition doesn't subset what's actually drawn", {
    expect_silent(p <- plot(t, model.lim = c(2000, 2010)))
    expect_equal(range(p$data$Date), c(1998.75, 2012))
})

test_that("Forecasts uses specified region", {
    f1 <- forecastplot(t)
    f2 <- forecastplot(t, model.lim = c(2008, 2009.75))
})


## multiseries
test_that("Decomposition uses sub-portion of the plots (multi)", {
    expect_silent(plot(tm, model.lim = c(2005, 2009.75)))
    expect_warning(
        plot(tm, compare = FALSE, model.lim = c(2005, 2009.75)),
        "Removed \\d+ rows containing missing values"
    )

})

unlink("Rplots.pdf")
