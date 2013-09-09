\name{seasonplot}
\alias{seasonplot}
\title{
  Plot Seasonal Subseries from a Time Series
}
\description{
  This function plots the seasonal components of a time series together
  with the estimated seasonal effects of that series.
}
\usage{
seasonplot(obj, ...)
}
\arguments{
  \item{obj}{
    an \code{iNZightTS} object
}
  \item{\dots}{
    Further arguments to be passed onto specific methods.
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
x <- iNZightTS(UKgas)
seasonplot(x)
}

