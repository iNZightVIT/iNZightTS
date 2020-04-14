context("Utility functions")

test_that("Multiplicative series only if x are all positive", {
    expect_true(is_multiplicative(1:10, TRUE))
    expect_false(is_multiplicative(1:10, FALSE))
    expect_false(is_multiplicative(-100:-50, TRUE))
    expect_false(is_multiplicative(-50:50, TRUE))
})
