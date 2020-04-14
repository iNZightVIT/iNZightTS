context("Get time series structure")

test_that("Yearly data", {
    expect_equal(
        get.ts.structure(as.character(1900:1950)),
        list(start = 1900, frequency = 1)
    )
})

test_that("Monthly data", {
    vals <- with(expand.grid(M = 1:12, Y = 2000:2005),
        paste0(Y, "M", M)
    )
    expect_equal(get.ts.structure(vals), list(start = c(2000, 1), frequency = 12))
})

test_that("Quarterly data", {
    vals <- with(expand.grid(Q = 1:4, Y = 2000:2005),
        paste0(Y, "Q", Q)
    )
    expect_equal(get.ts.structure(vals), list(start = c(2000, 1), frequency = 4))
})

test_that("Year/week data", {
    vals <- with(expand.grid(W = 1:52, Y = 2000:2005)[-(1:50),],
        paste0(Y, "W", W)
    )
    expect_equal(get.ts.structure(vals), list(start = c(2000, 51), frequency = 52))
})

test_that("Year/day data", {
    vals <- with(expand.grid(D = 1:365, Y = 2001:2004)[1:(365*3) + 30,],
        paste0(Y, "D", D)
    )
    expect_equal(
        get.ts.structure(vals),
        list(
            start = c(2001L, 31L),
            frequency = 365.25
        )
    )
})

test_that("Seven day week", {
    vals <- with(expand.grid(D = 1:7, W = 1:5)[-(1:3),],
        paste0("W", W, "D", D)
    )
    expect_equal(get.ts.structure(vals), list(start = c(1, 4), frequency = 7))
})

test_that("Five day week", {
    vals <- with(expand.grid(D = 1:5, W = 1:5)[-(1:3),],
        paste0("W", W, "D", D)
    )
    expect_equal(get.ts.structure(vals), list(start = c(1, 4), frequency = 5))
})

test_that("Hourly observations", {
    vals <- with(expand.grid(H = 0:23, D = 1:5)[-(1:10),],
        paste0("D", D, "H", H)
    )
    expect_equal(get.ts.structure(vals), list(start = c(1, 10), frequency = 24))
})


test_that("iNZightTS object can be created from a base ts object", {
    t <- ts(visitorsQ$Australia,
        start = c(1998, 4),
        frequency = 4
    )
    expect_equal(
        iNZightTS(t)$tsObj,
        iNZightTS(visitorsQ)$tsObj
    )
})
