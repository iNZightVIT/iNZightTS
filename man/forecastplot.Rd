\name{forecastplot}
\alias{forecastplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Forecast plot
}
\description{
  Plot a raw time series together with it's fitted curve and add
  forecasts and prediction intervals to the end.
}
\usage{
forecastplot(vars)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vars}{
    \code{iNZightTS} object
}
}
\details{
  The predictions and prediction intervals are the result of models
  fitted by the Holt-Winters method. The amount of predicted
  observations is calculated by 2 * \code{freq}, where \code{freq} is
  the frequency of the time series object.
}
\value{
  A multiple time series of the predicted values with columns fit,
  lwr and upr for the predicted values and the lower and upper bounds
  respectively.
}
\references{
  C.C Holt (1957)
  Forecasting seasonals and trends by exponentially weighted
  moving averages,
  ONR Research Memorandum, Carnigie Institute 52.

  P.R Winters (1960)
  Forecasting sales by exponentially weighted moving averages,
  \emph{Management Science} \bold{6}, 324--342.
}

\seealso{
  \code{\link{iNZightTS}},
  \code{\link{HoltWinters}}
}
\examples{
z <- iNZightTS(ldeaths)
forecastplot(z)
}

