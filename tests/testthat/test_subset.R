context("Plotting subset of series")
data(visitorsQ)

## single series
t <- iNZightTS(visitorsQ)
test_that("Subset of time series can be viewed", {
    # p <- plot(t, xlim = as.Date(c("2000-01-01", "2011-01-01")))
    expect_silent(p <- plot(t, xlim = c(2000, 2011)))
    expect_silent(p1 <- plot(t, xlim = c(2000, NA)))
    expect_silent(p2 <- plot(t, xlim = c(NA, 2005)))
    expect_is(p, "ggplot")
    expect_is(p1, "ggplot")
    expect_is(p2, "ggplot")
    # expect_equal(p$scales$scales[[2]]$limits, c(2000, 2011))
    # expect_equal(p1$scales$scales[[2]]$limits, c(2000, NA))
    # expect_equal(p2$scales$scales[[2]]$limits, c(NA, 2005))
})

test_that("Subset of decomposition plot can be shown", {
    d <- plot(decompose(t), xlim = c(2000, 2011))
    d1 <- plot(decompose(t), xlim = c(2000, NA))
    d2 <- plot(decompose(t), xlim = c(NA, 2005))
    expect_is(d, "inzdecomp")
    expect_is(d1, "inzdecomp")
    expect_is(d2, "inzdecomp")

    r <- plot(d, recompose.progress = c(0, 20))
    r1 <- plot(d1, recompose.progress = c(0, 20))
    r2 <- plot(d2, recompose.progress = c(0, 20))
    expect_is(r, "inzdecomp")
    expect_is(r1, "inzdecomp")
    expect_is(r2, "inzdecomp")
})

test_that("Subset of season plot", {
    s <- seasonplot(t, model.lim = c(2000, 2011))
    expect_is(seasonplot(t), "gtable")
})

test_that("Subset of forecast plot", {
    expect_is(plot(t, forecast = 4*2, xlim = c(2000, 2010)), "ggplot")
    expect_is(pred(plot(t, forecast = 4*2, xlim = c(2000, 2010))), "mts")
    expect_warning(
        plot(t,
            forecast = 4,
            xlim = c(2000, 2010),
            model.lim = c(2000, 20011)
        ),
        "Upper modelling limit cannot be greater than upper x limit"
    )
})

## multi series
tm <- iNZightTS(visitorsQ, var = 2:5)
test_that("Subset of multi series graph works", {
    p <- plot(tm, xlim = c(2000, 2011))
    expect_is(p, "patchwork")


    ## non-compare version
    expect_silent(p <- plot(tm, compare = FALSE, xlim = c(2000, 2011)))
    expect_is(p, "gtable")
})

## covid data
test_that("Modelling limits are OK", {
    ct <- iNZightTS(covid, time.var = 6, var = 5)

    expect_is(plot(ct, xlim = c(10, 20)), "ggplot")
    expect_is(plot(ct), "ggplot")
})

unlink("Rplots.pdf")
