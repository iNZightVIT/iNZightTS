data(visitorsQ)

x <- inzightts(visitorsQ, var = 2:5)

## single series
test_that("Decomposition uses sub-portion of the plot", {
    expect_silent(d <- decomp(x, "Japan", model_range = c(2005, 2009)))
    expect_s3_class(d, "inz_dcmp")
})

test_that("Season plots use sub-set of data", {
    expect_silent(seasonplot(x, "Japan"))
    expect_silent(seasonplot(x, "Japan", model_range = c(2000, 2010)))
    expect_silent(seasonplot(x, "Japan", model_range = c(2005, 2011)))
})

test_that("Forecasts uses specified region", {
    f1 <- plot(predict(x, h = "2 years"))
    f2 <- plot(predict(x, h = 8, model_range = lubridate::ymd(c(20050101, 20090930))))
    expect_s3_class(f1, "ggplot")
    expect_s3_class(f2, "ggplot")
    expect_s3_class(predict(x), "inz_frct")
})

## multiseries
test_that("Decomposition uses sub-portion of the plots (multi)", {
    expect_silent(plot(predict(x, names(x[-1]))))
    expect_silent(plot(predict(x, names(x[-1])), model_range = c(2005, 2009)))
})

unlink("Rplots.pdf")
