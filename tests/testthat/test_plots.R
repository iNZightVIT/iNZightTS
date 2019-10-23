context("Basic time series graphs")
# data(visitorsQ)
# data(visitorsM2)

## single series
t <- iNZightTS(visitorsQ)
test_that("Basic ts graph works", {
    expect_is(plot(t), "ggplot")
})

test_that("Smoother can be disabled", {
    expect_is(plot(t, smoother = FALSE), "ggplot")
})

test_that("Decomposition and recomposition plots work", {
    expect_is(decompositionplot(t), "iNZightTS")
    expect_is(recompose(decompositionplot(t), animate = FALSE), "iNZightTS")
    # expect_is(plot(t, show = "decomp"), "ggplot")
    # expect_is(plot(t, show = "recomp", animated = FALSE), "ggplot")
})

test_that("Season plot is OK", {
    # expect_null(seasonplot(t))
    expect_is(seasonplot(t), "gtable")

    expect_is(seasonplot(iNZightTS(visitorsM2, var = 4)), "gtable")
})

test_that("Forecast is fine", {
    expect_is(plot(t, forecast = 8), "ggplot")
    expect_is(plot(t, forecast = 4*2, model.lim = c(2000, 2010)), "ggplot")
    expect_is(pred(plot(t, forecast = 8)), "mts")
})

## multi series
tm <- iNZightTS(visitorsQ, var = 2:5)
test_that("Multi series graph works", {
    expect_is(plot(tm), "gtable")
    expect_is(plot(tm, smoother = FALSE), "gtable")
    expect_is(suppressWarnings(plot(tm, compare = FALSE)), "gtable")
    expect_is(suppressWarnings(plot(tm, smoother = FALSE, compare = FALSE)), "gtable")
})

test_that("Unsupported plots error", {
    expect_error(decompositionplot(tm))
    expect_error(recompose(decompositionplot(tm)))
    expect_error(seasonplot(tm))
    expect_warning(capture.output(forecastplot(tm)))
})

## clean up
unlink("Rplot.pdf")
