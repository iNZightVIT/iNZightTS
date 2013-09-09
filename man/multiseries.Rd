\name{multiseries}
\alias{multiseries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Compare multiple timeseries
}

\description{
  Draw a plot to compare 2 or more timeseries with each
  other. The resulting plot contains the original timeseries as well as
  plots showing the season effects of each timeseries, if the frequency
  is greater than 1.
}

\usage{
multiseries(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{iNZightMTS object containing data}
}

\details{
  \code{x} is an \code{\link{iNZightMTS}} object containing some
  data for which timeseries objects can be created. The data columns
  used for the plotting are those that are used in the \code{\link{iNZightMTS}}
  object.

  The frequency used for the plotting is also stored in \code{x}.
}

\seealso{
\code{\link{iNZightTS}}
}

\examples{
# Frequency > 1
y <- iNZightTS(Seatbelts)
multiseries(y)

# Frequency = 1
# Casulties for Jan of each year
X <- Seatbelts[12*(1:15), -8]
X <- as.ts(X)
z <- iNZightTS(X)
multiseries(z)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{timeseries}