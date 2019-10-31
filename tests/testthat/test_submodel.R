context("Use subseries to model the time series")
data(visitorsQ)

t <- iNZightTS(visitorsQ)
tm <- iNZightTS(visitorsQ, var = 2:5)

## single series
test_that("Decomposition uses sub-portion of the plot", {
    expect_silent(d <- decompose(t, model.lim = c(2005, 2009.75)))
    expect_is(d, "iNZightTS")
    expect_equal(attr(d$decompVars$components, "tsp"), c(2005, 2009.75, 4))
})

test_that("Decomposition doesn't subset what's actually drawn", {
    expect_silent(p <- plot(t, model.lim = c(2000, 2010)))
    expect_equal(range(p$data$Date), c(1998.75, 2012))
})

test_that("Season plots use sub-set of data", {
    expect_silent(seasonplot(t))
    expect_silent(seasonplot(t, model.lim = c(2000, 2010)))
    expect_silent(seasonplot(t, model.lim = c(2005, 2011)))
})

test_that("Forecasts uses specified region", {
    f1 <- plot(t, forecast = 8)
    f2 <- plot(t, forecast = 8, model.lim = c(2008, 2009.75))
    expect_is(f1, "ggplot")
    expect_is(f2, "ggplot")
    expect_is(pred(f1), "mts")
    expect_is(pred(f2), "mts")
    expect_equal(range(time(pred(f1))), c(2012.25, 2014))
    expect_equal(range(time(pred(f2))), c(2010.00, 2011.75))
})


## multiseries
test_that("Decomposition uses sub-portion of the plots (multi)", {
    expect_silent(plot(tm))
    expect_silent(plot(tm, model.lim = c(2005, 2009.75)))
    expect_warning(
        plot(tm, compare = FALSE, model.lim = c(2005, 2009.75)),
        "Removed \\d+ rows containing missing values"
    )

})

unlink("Rplots.pdf")
