context("Plot interactivity helper functions")

data(visitorsQ)
t <- iNZightTS(visitorsQ)
tm <- iNZightTS(visitorsQ, var = 2:5)

test_that("Interactivity helper returns correct result", {
    expect_true(iNZightPlots::can.interact(plot(t)))
    expect_false(iNZightPlots::can.interact(seasonplot(t)))
    expect_false(iNZightPlots::can.interact(decompositionplot(t)))
    expect_true(iNZightPlots::can.interact(plot(t, forecast = 8)))
})

unlink("Rplot.pdf)")
