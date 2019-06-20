context("Plotting subset of series")
data(visitorsQ)

## single series
t <- iNZightTS(visitorsQ)
test_that("Subset of time series can be viewed", {
    # p <- plot(t, xlim = as.Date(c("2000-01-01", "2011-01-01")))
    expect_warning(
        p <- plot(t, xlim = c(2000, 2011)),
        "Removed \\d+ rows containing missing values"
    )
    expect_warning(
        p1 <- plot(t, xlim = c(2000, NA)),
        "Removed \\d+ rows containing missing values"
    )
    expect_warning(
        p2 <- plot(t, xlim = c(NA, 2005)),
        "Removed \\d+ rows containing missing values"
    )
    expect_is(p, "ggplot")
    expect_is(p1, "ggplot")
    expect_is(p2, "ggplot")
    expect_equal(p$scales$scales[[2]]$limits, c(2000, 2011))
    expect_equal(p1$scales$scales[[2]]$limits, c(2000, NA))
    expect_equal(p2$scales$scales[[2]]$limits, c(NA, 2005))
})

test_that("Subset of decomposition plot can be shown", {
    d <- decompositionplot(t, xlim = c(2000, 2011))
    d1 <- decompositionplot(t, xlim = c(2000, NA))
    d2 <- decompositionplot(t, xlim = c(NA, 2005))
    expect_is(d, "iNZightTS")
    expect_is(d1, "iNZightTS")
    expect_is(d2, "iNZightTS")

    r <- recompose(d, animate = FALSE)
    r1 <- recompose(d1, animate = FALSE)
    r2 <- recompose(d2, animate = FALSE)
    expect_is(r, "iNZightTS")
    expect_is(r1, "iNZightTS")
    expect_is(r2, "iNZightTS")


    # expect_is(plot(t, show = "decomp"), "ggplot")
    # expect_is(plot(t, show = "recomp", animated = FALSE), "ggplot")
})

test_that("Subset of season plot", {
    # s <- seasonplot(t, xlim = c(2000, 2011))
    # expect_equal(x, 0)
    # expect_is(plot(t, show = "seasons"), "ggplot")
})

## multi series
tm <- iNZightTS(visitorsQ, var = 2:5)
test_that("Subset of multi series graph works", {
    expect_warning(
        p <- plot(tm, xlim = c(2000, 2011)),
        "Removed \\d+ rows containing missing values"
    )
    expect_is(p, "gtable")


    ## non-compare version
    expect_warning(
        p <- plot(tm, compare = FALSE, xlim = c(2000, 2011)),
        "Removed \\d+ rows containing missing values"
    )
    expect_is(p, "gtable")
})

unlink("Rplots.pdf")
