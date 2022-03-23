context("Utility functions")

test_that("Leap year function returns the correct value", {
    expect_true(is.leapyear(2020))
    expect_false(is.leapyear(2019))
})
