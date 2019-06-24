context("Basic time series graphs")
data(visitorsQ)

## single series
t <- iNZightTS(visitorsQ)
test_that("Basic ts graph works", {
    expect_is(plot(t), "ggplot")
})

test_that("Decomposition and recomposition plots work", {
    expect_is(decompositionplot(t), "iNZightTS")
    expect_is(recompose(decompositionplot(t), animate = FALSE), "iNZightTS")
    # expect_is(plot(t, show = "decomp"), "ggplot")
    # expect_is(plot(t, show = "recomp", animated = FALSE), "ggplot")
})

test_that("Season plot is OK", {
    expect_equal(seasonplot(t), 0)
    # expect_is(plot(t, show = "seasons"), "ggplot")
})

test_that("Forecast is fine", {
    expect_is(forecastplot(t), "mts")
})

## multi series
tm <- iNZightTS(visitorsQ, var = 2:5)
test_that("Multi series graph works", {
    expect_is(plot(tm), "gtable")
    expect_is(suppressWarnings(plot(tm, compare = FALSE)), "gtable")
})

test_that("Unsupported plots error", {
    expect_error(decompositionplot(tm))
    expect_error(recompose(decompositionplot(tm)))
    expect_error(seasonplot(tm))
    expect_error(forecaseplot(tm))
})

## clean up
unlink("Rplot.pdf")
