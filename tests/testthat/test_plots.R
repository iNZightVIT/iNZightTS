context("Basic time series graphs")
# data(visitorsQ)
# data(visitorsM2)

## single series
t <- inzightts(visitorsQ)
test_that("Basic ts graph works", {
    expect_is(plot(t), "ggplot")
    expect_is(plot(t, mult_fit = TRUE), "ggplot")
})

test_that("Smoother can be disabled", {
    expect_is(plot(t, smoother = FALSE), "ggplot")
})

test_that("Decomposition and recomposition plots work", {
    expect_silent(d <- decomp(t, var = "Australia"))
    expect_is(plot(d), "inz_dcmp")
    expect_is(plot(d, recompose.progress = c(0, 20)), "inz_dcmp")
    expect_is(plot(d, recompose.progress = c(1, 20)), "inz_dcmp")
    expect_is(plot(decomp(t, mult_fit = TRUE)), "inz_dcmp")
    plot(d, recompose = TRUE)
})

test_that("Season plot is OK", {
    expect_is(seasonplot(t), "patchwork")
    expect_is(seasonplot(t, mult_fit = TRUE), "patchwork")
    expect_is(
        seasonplot(inzightts(visitorsM2, var = 4), mult_fit = TRUE),
        "patchwork"
    )
})

test_that("Forecast is fine", {
    expect_is(plot(predict(t, h = 8)), "ggplot")
    expect_is(plot(predict(t, h = "2 years")), "ggplot")
    expect_is(plot(predict(t, h = 8, mult_fit = TRUE)), "ggplot")
})

## multi series
tm <- inzightts(visitorsQ, var = 2:5)
var_name <- names(tm)[-1]
test_that("Multi series graph works", {
    expect_is(plot(tm, var_name), "patchwork")
    expect_is(plot(tm, var_name, mult_fit = TRUE), "patchwork")
    expect_is(plot(tm, var_name, smoother = FALSE), "patchwork")
    # expect_is(suppressWarnings(plot(tm, compare = FALSE)), "gtable")
    # expect_is(
    #     suppressWarnings(plot(tm, compare = FALSE, multiplicative = TRUE)),
    #     "gtable"
    # )
    # expect_is(
    #     suppressWarnings(plot(tm, smoother = FALSE, compare = FALSE)),
    #     "gtable"
    # )
})

test_that("Annual data forecast is fine", {
    t <- inzightts(visitorsA2)
    tm <- inzightts(visitorsA2, var = 2:5)
    expect_is(plot(predict(t)), "ggplot")
    expect_is(plot(predict(tm)), "ggplot")
    # expect_is(plot(tm, compare = FALSE), "gtable")
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
