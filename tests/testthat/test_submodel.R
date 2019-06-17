context("Use subseries to model the time series")
data(visitorsQ)

## single series
t <- iNZightTS(visitorsQ)
expect_warning(
    p <- plot(t, model.lim = c("2000Q1", "2010Q4")),
    "Removed \\d+ rows containing missing values"
)
