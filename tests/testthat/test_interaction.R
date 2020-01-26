context("Plot interactivity helper functions")

skip_on_cran()
skip_if(
    length(find.package("iNZightPlots2", quiet = TRUE)) == 0,
    message = "iNZightPlots package not available."
)
can.interact <- eval(parse(text = "iNZightPlots::can.interact"))

data(visitorsQ)
t <- iNZightTS(visitorsQ)
tm <- iNZightTS(visitorsQ, var = 2:5)

test_that("Interactivity helper returns correct result", {
    expect_true(can.interact(plot(t)))
    expect_false(can.interact(seasonplot(t)))
    expect_false(can.interact(plot(decompose(t))))
    expect_true(can.interact(plot(t, forecast = 8)))

    expect_false(can.interact(plot(tm)))
    expect_false(can.interact(plot(tm, compare = FALSE)))
})

unlink("Rplots.pdf)")
