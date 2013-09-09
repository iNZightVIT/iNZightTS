\name{rawplot}
\alias{rawplot}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Draw a simple time series plot
}

\description{
Draws a plot of a given \code{iNZightTS} object with the trend superimposed.
}

\usage{
rawplot(obj, animate = FALSE)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
    iNZightTS object
}
  \item{animate}{
    animate the plotting process?
}
}

\details{
  If animate is set to \code{TRUE}, a scatterplot of all points in the
  time series will appear followed by slowly drawn lines connecting the
  points, simulating the drawing of a time series by hand.
}


\seealso{
\code{\link{iNZightTS}}
}

\examples{
z <- data.frame(Return = rnorm(100))
x <- iNZightTS(as.ts(z), var="Return")
rawplot(x)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{timeseries}
