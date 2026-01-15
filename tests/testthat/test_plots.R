# data(visitorsQ)
# data(visitorsM2)

## single series
t <- inzightts(visitorsQ)
test_that("Basic ts graph works", {
    expect_s3_class(plot(t), "ggplot")
    expect_s3_class(plot(t, mult_fit = TRUE), "ggplot")
    expect_true(ggplotable(plot(t)))
})

test_that("Smoother can be disabled", {
    expect_s3_class(plot(t, smoother = FALSE), "ggplot")
})

test_that("Data can be seasonally adjusted", {
    expect_s3_class(plot(t, seasonal_adjustment = TRUE), "ggplot")
})

test_that("Decomposition and recomposition plots work", {
    expect_silent(d <- decomp(t, var = "Australia"))
    expect_s3_class(plot(d), "ggplot")
    expect_s3_class(plot(d, recompose.progress = c(0, 20)), "ggplot")
    expect_s3_class(plot(d, recompose.progress = c(1, 20)), "ggplot")
    expect_s3_class(plot(decomp(t, mult_fit = TRUE)), "ggplot")
    plot(d, recompose = TRUE)
})

test_that("Season plot is OK", {
    expect_s3_class(seasonplot(t), "patchwork")
    expect_s3_class(seasonplot(t, mult_fit = TRUE), "patchwork")
    expect_s3_class(
        seasonplot(inzightts(visitorsM2, var = 4), mult_fit = TRUE),
        "patchwork"
    )
    expect_false(ggplotable(seasonplot(t)))
})

test_that("Forecast is fine", {
    expect_s3_class(plot(predict(t, h = 8)), "ggplot")
    expect_s3_class(plot(predict(t, h = "2 years")), "ggplot")
    expect_s3_class(plot(predict(t, h = 8, mult_fit = TRUE)), "ggplot")
    expect_true(ggplotable(plot(predict(t))))
})

tm <- inzightts(visitorsQ, var = 2:5)
var_name <- names(tm)[-1]
test_that("Multi series graph works", {
    expect_s3_class(plot(tm, var_name), "patchwork")
    expect_s3_class(plot(tm, var_name, mult_fit = TRUE), "patchwork")
    expect_s3_class(plot(tm, var_name, smoother = FALSE), "patchwork")
    expect_false(ggplotable(plot(tm, var_name)))
})

test_that("Annual data forecast is fine", {
    t <- inzightts(visitorsA2)
    tm <- inzightts(visitorsA2, var = 2:5)
    expect_s3_class(plot(predict(t)), "ggplot")
    expect_s3_class(plot(predict(tm)), "ggplot")
})

t_neg <- t
t_neg$Australia[1] <- -1
y <- visitorsQ |>
    dplyr::mutate(key = rep(c("A", "B", "C"), each = 18)) |>
    inzightts(key = "key")

test_that("Check raw-plot configuration", {
    expect_s3_class(plot(t, xlim = c(NA, NA)), "ggplot")
    expect_warning(plot(t_neg, mult_fit = TRUE))
    expect_s3_class(plot(t, xlim = c(2005, 2010)), "ggplot")
    # Note: the below no longer produces a warning from dplyr 1.1.0
    # expect_warning(plot(t, xlim = lubridate::ymd(c(20050101, 20101231))))
    x <- inzightts(visitorsQ, var = 2:5)
    expect_error(plot(x, c("Australia", "Japan"), ylab = "AUS"))
    expect_warning(plot(x, names(x)[-1], aspect = 4))
    expect_s3_class(plot(t, aspect = 1), "ggplot")
    expect_equal(guess_plot_var(t, "test"), c("", "test"))
    expect_type(guess_plot_var(t, NULL), "symbol")
    expect_s3_class(plot(y), "ggplot")
    expect_s3_class(plot(y, compare = FALSE), "ggplot")
    expect_s3_class(plot(y, emphasise = 2), "ggplot")
    expect_s3_class(plot(y, emphasise = 2, non_emph_opacity = 1e-6), "ggplot")
    expect_s3_class(plot(y, emphasise = 2, non_emph_opacity = 0), "ggplot")
    x <- visitorsM2 |>
        dplyr::mutate(y = ggplot2::cut_number(Japan, 4)) |>
        inzightts()
    y <- visitorsA2 |>
        tidyr::pivot_longer(!Time, names_to = "Country", values_to = "Visitors") |>
        dplyr::mutate(y = ggplot2::cut_number(Visitors, 4)) |>
        inzightts(key = "Country")
    expect_s3_class(plot(x, "y"), "ggplot")
    expect_s3_class(suppressWarnings(plot(y, "y")), "ggplot")
    expect_s3_class(suppressWarnings(plot(y, "y")), "ggplot")
    expect_s3_class(plot(x, "y", pal = 1:4), "ggplot")
    expect_s3_class(plot(x, "y", pal = 1:5), "ggplot")
    expect_s3_class(suppressWarnings(plot(y, "y", pal = 1:5)), "ggplot")
    expect_error(plot(y, c("y", "Visitors")))
    expect_warning(expect_warning(plot(y, c("y", "y"))))
    expect_warning(plot(x, "y", aspect = 1))
    x$Australia[c(23, 25)] <- NA
    expect_s3_class(suppressWarnings(plot(x)), "ggplot")
})

test_that("Check decomposition plot configuration", {
    expect_error(decomp(t, sm_model = "null_model"))
    expect_s3_class(iNZightTS:::use_decomp_method("stl"), "use_stl")
    expect_s3_class(iNZightTS:::use_decomp_method("test"), "use_test")
    expect_equal(iNZightTS:::as_year.character("2010"), 2010)
    expect_equal(iNZightTS:::as_year.vctrs_vctr(tsibble::yearquarter("2010Q1")), 2010)
    expect_equal(iNZightTS:::as_year.numeric(2010), 2010)
    expect_true(fabletools::is_dable(
        .decomp(
            iNZightTS:::use_decomp_method("stl"),
            t, "Australia",
            mult_fit = TRUE
        )
    ))
    t_gap <- t
    t_gap$Australia[3] <- NA
    expect_warning(decomp(t_gap, "Australia"))
})

test_that("Check season plot configuration", {
    expect_s3_class(seasonplot(t, labels = "none"), "patchwork")
    expect_error(seasonplot(t, model_range = 2001))
    # Note: the below no longer produces a warning from dplyr 1.1.0
    # expect_warning(seasonplot(t, model_range = lubridate::ymd(c(20050101, 20101231))))
    expect_s3_class(seasonplot(tm, names(tm)[-1]), "patchwork")
    t_key_gap <- y
    t_key_gap$Australia[3] <- NA
    # TODO: gives ERROR instead of WARNING  but not reproducible under some unknown conditions
    expect_warning(seasonplot(t_key_gap, "Australia"))
    # TODO: gives ERROR but not reproducible under some unknown conditions
    #   Error in `dplyr::mutate()`:
    #   <U+2139> In argument: `season_effect = dplyr::case_when(mult_fit ~ `NA` * remainder, TRUE ~ `NA` +
    #     remainder)`.
    #   Caused by error in `dplyr::case_when()`:
    #   ! Failed to evaluate the right-hand side of formula 1.
    #   Caused by error:
    #   ! object 'NA' not found
    expect_s3_class(suppressWarnings(seasonplot(t_key_gap)), "patchwork")
})

test_that("Check forecasting configuration", {
    expect_warning(predict(t_neg, mult_fit = TRUE))
    expect_error(plot(predict(t, model_range = c(2001, 2004)), t_range = c(NA, 2003)))
    expect_error(plot(predict(t), t_range = 2001))
    expect_error(predict(t, model_range = 2001))
    expect_s3_class(predict(t, model_range = c(2002, NA)), "inz_frct")
    expect_error(plot(predict(tm, names(tm)[-1]), ylab = "A"))
    pred_arima <- predict(t)
    expect_s3_class(summary(pred_arima), "summary_inz_frct")
    expect_message(summary(pred_arima))
    expect_warning(summary(pred_arima, c("Australia", "Japan")))
    expect_output(print(summary(pred_arima, "Australia"), show_details = TRUE))
    y2 <- visitorsM2 |>
        tidyr::pivot_longer(!Time, names_to = "Country", values_to = "Visitors") |>
        inzightts(key = "Country")
    expect_s3_class(plot(predict(y2, pred_model = fable::ARIMA)), "ggplot")
    expect_s3_class(plot(predict(y2, pred_model = "arima")), "ggplot")
    expect_s3_class(plot(predict(y2, h = "2 years")), "ggplot")
    expect_s3_class(summary(predict(y2)), "summary_inz_frct")
})

## clean up
unlink("Rplot.pdf")
