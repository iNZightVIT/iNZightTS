context("Loading and initialising data sets")
data(visitorsQ)

test_that("Various index formats can be recognised", {
    expect_equal(format_index(2022), 2022)
    expect_equal(format_index("2022"), 2022)
    expect_equal(format_index("2022"), 2022)
    expect_equal(format_index("Y2022M01"), tsibble::yearmonth("2022M01"))
    expect_equal(format_index("Y2022Q01"), tsibble::yearquarter("2022Q1"))
    expect_equal(format_index("Y2022W01"), tsibble::yearweek("2022W01"))
    expect_equal(format_index("Y2022D31"), lubridate::ymd(20220131))
    expect_s3_class(format_index("D01H01"), "POSIXct")
    expect_error(format_index("2022 Jan 01"))
})

test_that("Various data classes can be loaded", {
    expect_s3_class(inzightts(ts(1:10)), "inz_ts")
    expect_s3_class(inzightts(visitorsQ), "inz_ts")
    expect_s3_class(inzightts(visitorsQ, index = 1, var = 2), "inz_ts")
    expect_s3_class(inzightts(visitorsQ, start = c(1998, 4), freq = 4), "inz_ts")
    expect_s3_class(inzightts(visitorsQ, start = 1, end = nrow(visitorsQ)), "inz_ts")
    expect_s3_class(inzightts(visitorsQ, freq = 4, end = c(2012, 1)), "inz_ts")
    data_with_key <- visitorsQ |>
        dplyr::mutate(key = sample(LETTERS[1:3], nrow(visitorsQ), TRUE))
    expect_s3_class(inzightts(data_with_key, key = "key"), "inz_ts")
    expect_error(inzightts(rbind(visitorsQ, visitorsQ)))
    x <- dplyr::mutate(visitorsQ, Date = tsibble::yearquarter(as.character(Date)))
    expect_s3_class(inzightts(x), "inz_ts")
})
