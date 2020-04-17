context("Basic time series graphs")
# data(visitorsQ)
# data(visitorsM2)

## single series
t <- iNZightTS(visitorsQ)
test_that("Basic ts graph works", {
    expect_is(plot(t), "ggplot")
    expect_is(plot(t, multiplicative = TRUE), "ggplot")
})

test_that("Smoother can be disabled", {
    expect_is(plot(t, smoother = FALSE), "ggplot")
})

test_that("Decomposition and recomposition plots work", {
    expect_silent(d <- decompose(t, data.name = "Visitors"))
    expect_is(plot(d), "inzdecomp")
    expect_is(plot(d, recompose.progress = c(0, 20)), "inzdecomp")
    expect_is(
        plot(d, recompose.progress = c(1, 20)),
        "inzdecomp"
    )
    expect_is(
        plot(decompose(t, multiplicative = TRUE)),
        "inzdecomp"
    )
})

test_that("Season plot is OK", {
    expect_is(seasonplot(t), "gtable")
    expect_is(seasonplot(t, multiplicative = TRUE), "gtable")
    expect_is(
        seasonplot(iNZightTS(visitorsM2, var = 4), multiplicative = TRUE),
        "gtable"
    )
})

test_that("Forecast is fine", {
    expect_is(plot(t, forecast = 8), "ggplot")
    expect_is(plot(t, forecast = 4*2, model.lim = c(2000, 2010)), "ggplot")
    expect_is(pred(plot(t, forecast = 8)), "mts")
    expect_is(plot(t, forecast = 8, multiplicative = TRUE), "ggplot")
})

## multi series
tm <- iNZightTS(visitorsQ, var = 2:5)
test_that("Multi series graph works", {
    expect_is(plot(tm), "gtable")
    expect_is(plot(tm, multiplicative = TRUE), "gtable")
    expect_is(plot(tm, smoother = FALSE), "gtable")
    expect_is(suppressWarnings(plot(tm, compare = FALSE)), "gtable")
    expect_is(
        suppressWarnings(plot(tm, compare = FALSE, multiplicative = TRUE)),
        "gtable"
    )
    expect_is(
        suppressWarnings(plot(tm, smoother = FALSE, compare = FALSE)),
        "gtable"
    )
})

test_that("Unsupported plots error", {
    expect_warning(decompositionplot(t))
    expect_warning(recompose(decompositionplot(t)))
    expect_warning(decompositionplot(tm))
    expect_warning(recompose(decompositionplot(tm)))
    expect_error(seasonplot(tm))
    expect_warning(capture.output(forecastplot(tm)))
})

## clean up
unlink("Rplot.pdf")




if (FALSE) {
    d <- decompose(t, data.name = "Visitors")
    ## demo playthrough
    for (i in 0:1)
        for (j in 1:nrow(d$data))
            plot(d, recompose.progress = c(i, j))
}
