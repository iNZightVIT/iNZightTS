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

tm <- inzightts(visitorsQ, var = 2:5)
var_name <- names(tm)[-1]
test_that("Multi series graph works", {
    expect_is(plot(tm, var_name), "patchwork")
    expect_is(plot(tm, var_name, mult_fit = TRUE), "patchwork")
    expect_is(plot(tm, var_name, smoother = FALSE), "patchwork")
})

test_that("Annual data forecast is fine", {
    t <- inzightts(visitorsA2)
    tm <- inzightts(visitorsA2, var = 2:5)
    expect_is(plot(predict(t)), "ggplot")
    expect_is(plot(predict(tm)), "ggplot")
})

t_neg <- t
t_neg$Australia[1] <- -1
y <- visitorsQ %>%
    dplyr::mutate(key = rep(c("A", "B", "C"), each = 18)) %>%
    inzightts(key = "key")

test_that("Check raw-plot configuration", {
    expect_is(plot(t, xlim = c(NA, NA)), "ggplot")
    expect_warning(plot(t_neg, mult_fit = TRUE))
    expect_is(plot(t, xlim = c(2005, 2010)), "ggplot")
    expect_warning(plot(t, xlim = lubridate::ymd(c(20050101, 20101231))))
    x <- inzightts(visitorsQ, var = 2:5)
    expect_error(plot(x, c("Australia", "Japan"), ylab = "AUS"))
    expect_warning(plot(x, names(x)[-1], aspect = 4))
    expect_is(plot(t, aspect = 1), "ggplot")
    expect_equal(guess_plot_var(t, "test"), c("", "test"))
    expect_is(guess_plot_var(t, NULL), "name")
    expect_is(plot(y), "ggplot")
    expect_is(plot(y, compare = FALSE), "ggplot")
    expect_is(plot(y, emphasise = 2), "ggplot")
    expect_is(plot(y, emphasise = 2, non_emph_opacity = 1e-6), "ggplot")
    expect_is(plot(y, emphasise = 2, non_emph_opacity = 0), "ggplot")
})

test_that("Check decomposition plot configuration", {
    expect_error(decomp(t, sm_model = "null_model"))
    expect_is(iNZightTS:::use_decomp_method("stl"), "use_stl")
    expect_is(iNZightTS:::use_decomp_method("test"), "use_test")
    expect_equal(iNZightTS:::as_year.character("2010"), 2010)
    expect_equal(iNZightTS:::as_year.vctrs_vctr(tsibble::yearquarter("2010Q1")), 2010)
    expect_equal(iNZightTS:::as_year.numeric(2010), 2010)
    expect_true(fabletools::is_dable(
        .decomp(
            iNZightTS:::use_decomp_method("stl"),
            t, "Australia", mult_fit = TRUE
        )
    ))
    t_gap <- t
    t_gap$Australia[3] <- NA
    expect_warning(decomp(t_gap, "Australia"))
})

test_that("Check season plot configuration", {
    expect_is(seasonplot(t, labels = "none"), "patchwork")
    expect_error(seasonplot(t, model_range = 2001))
    expect_warning(seasonplot(t, model_range = lubridate::ymd(c(20050101, 20101231))))
    expect_is(seasonplot(tm, names(tm)[-1]), "patchwork")
    t_key_gap <- y
    t_key_gap$Australia[3] <- NA
    expect_warning(seasonplot(t_key_gap, "Australia"))
    expect_is(suppressWarnings(seasonplot(t_key_gap)), "patchwork")
})

test_that("Check forecasting configuration", {
    expect_warning(predict(t_neg, mult_fit = TRUE))
    expect_error(predict(t, t_range = c(2001, 2002), model_range = lubridate::ymd(c(20010101, 20021231))))
    expect_error(predict(t, t_range = 2001))
    expect_error(predict(t, model_range = 2001))
    expect_is(predict(t, t_range = c(NA, 2008), model_range = c(2002, NA)), "inz_frct")
    expect_warning(predict(t,
        t_range = lubridate::ymd(c(20010101, 20101231)),
        model_range = lubridate::ymd(c(20010101, 20051231))
    ))
    expect_error(plot(predict(tm, names(tm)[-1]), ylab = "A"))
    pred_arima <- predict(t)
    expect_is(summary(pred_arima), "summary_inz_frct")
    expect_message(summary(pred_arima))
    expect_warning(summary(pred_arima, c("Australia", "Japan")))
    expect_output(print(summary(pred_arima, "Australia"), show_details = TRUE))
    expect_output(print(summary(pred_arima, "Australia")))
    y2 <- visitorsM2 %>%
        pivot_longer(!Time, names_to = "Country", values_to = "Visitors") %>%
        inzightts(key = "Country")
    expect_is(plot(predict(y2, pred_model = fable::ARIMA)), "ggplot")
    expect_is(plot(predict(y2, pred_model = "arima")), "ggplot")
    expect_is(plot(predict(y2, h = "2 years")), "ggplot")
})

## clean up
unlink("Rplot.pdf")
