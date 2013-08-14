\name{decompositionplot}
\alias{decompositionplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a Time Series Decomposition
}
\description{
Decomposes a time series into trend, seasonal and residual components
using \code{loess}.
}
\usage{
decompositionplot(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
    an \code{iNZightTS} object
}
}
\details{
  If the frequency is greater than 1, the components are found using the
  \code{\link{stl}} function with \code{s.window} set to \code{TRUE}
  (effectively replacing smoothing by taking the mean).
  If the frequency is 1, the trend component is found directly by using
  \code{\link{loess}} and the residuals are the difference between trend
  and actual values.
  The trend, seasonal and residual components are plotted on the same
  scale allowing for easy visual analysis.
}
\value{
  The original \code{iNZightTS} object with an item \code{decompVars}
  appended, containing results from the decomposition.
}
\references{
R. B. Cleveland, W. S. Cleveland, J.E. McRae, and I. Terpenning (1990) STL: A Seasonal-Trend Decomposition Procedure Based on Loess. Journal of Official Statistics, 6, 3¡V73.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{stl}}, \code{\link{loess}}, \code{\link{iNZightTS}}
}
\examples{
z <- iNZightTS(data.frame(Return = rnorm(100)), freq = 12, var = 'Return')
y <- decompositionplot(z)
}