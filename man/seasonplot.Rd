\name{seasonplot}
\alias{seasonplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot Seasonal Subseries from a Time Series
}
\description{
  This function plots the seasonal components of a time series together
  with the estimated seasonal effects of that series.
}
\usage{
seasonplot(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
    an \code{iNZightTS} object
}
}
\details{
  The resulting window will contain two plots. On the left, every
  seasonal subseries of the time series is plotted. On the right will be
  the average seasonal effect of the series.
}

\seealso{
\code{\link{iNZightTS}}
}
\examples{
z <- iNZightTS(data.frame(Return = rnorm(100)), freq=12, var="Return")
seasonplot(z)
}

