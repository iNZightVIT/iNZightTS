data(visitorsQ)

x <- inzightts(visitorsQ)
y <- visitorsQ |>
    tidyr::pivot_longer(!Date, names_to = "Country", values_to = "Visitors") |>
    inzightts(key = "Country")

test_that("Seasonal subseries plots works for inzightts", {
    expect_s3_class(subseries(x, names(x)[-1]), "ggplot")
    expect_s3_class(subseries(y), "ggplot")
    expect_s3_class(subseries(y, show_mean = FALSE), "ggplot")
    expect_error(subseries(x, names(x)[-1], ylab = "y"))
    expect_false(ggplotable(subseries(x, names(x)[-1])))
    expect_true(ggplotable(subseries(x)))
})
