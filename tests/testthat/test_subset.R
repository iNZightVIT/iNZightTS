data(visitorsQ)

## single series
t <- inzightts(visitorsQ)
test_that("Subset of time series can be viewed", {
    expect_message(p <- plot(t, xlim = as.Date(c("2000-01-01", "2011-01-01"))))
    expect_silent(p <- plot(t, "Australia", xlim = c(2000, 2011)))
    expect_silent(p1 <- plot(t, "Australia", xlim = c(2000, NA)))
    expect_silent(p2 <- plot(t, "Australia", xlim = c(NA, 2005)))
    expect_s3_class(p, "ggplot")
    expect_s3_class(p1, "ggplot")
    expect_s3_class(p2, "ggplot")
})

test_that("Subset of decomposition plot can be shown", {
    d <- plot(decomp(t), xlim = c(2000, 2011))
    d1 <- plot(decomp(t), xlim = c(2000, NA))
    d2 <- plot(decomp(t), xlim = c(NA, 2005))
    expect_s3_class(d, "ggplot")
    expect_s3_class(d1, "ggplot")
    expect_s3_class(d2, "ggplot")

    r <- plot(d, recompose.progress = c(0, 20))
    r1 <- plot(d1, recompose.progress = c(0, 20))
    r2 <- plot(d2, recompose.progress = c(0, 20))
    expect_s3_class(r, "ggplot")
    expect_s3_class(r1, "ggplot")
    expect_s3_class(r2, "ggplot")
})

test_that("Subset of season plot", {
    s <- seasonplot(t, model_range = c(2000, 2011))
    expect_s3_class(seasonplot(t), "patchwork")
})

test_that("Subset of forecast plot", {
    expect_s3_class(plot(predict(t, h = 8, xlim = c(2000, 2010))), "ggplot")
})

## multi series
tm <- inzightts(visitorsQ, var = 2:5)
test_that("Subset of multi series graph works", {
    p <- plot(tm, names(tm)[-1], xlim = c(2000, 2011))
    expect_s3_class(p, "patchwork")
    # ## non-compare version
    # expect_silent(p <- plot(tm, compare = FALSE, xlim = c(2000, 2011)))
    # expect_s3_class(p, "gtable")
})

## covid data
test_that("Modelling limits are OK", {
    ct <- inzightts(covid, var = 5, index = 6)

    # expect_s3_class(plot(ct, xlim = c(10, 20)), "ggplot")
    expect_s3_class(plot(ct), "ggplot")
})

unlink("Rplots.pdf")
