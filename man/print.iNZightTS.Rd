\name{print.iNZightTS}
\alias{print.iNZightTS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Print an iNZightTS object
}
\description{
 Print method for iNZightTS (time series) objects.
}
\usage{
## S3 method for class 'iNZightTS'
print.iNZightTS(obj, full = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
    the \code{iNZightTS} object to be printed
}
  \item{full}{
    whether to print all the underlying data
}
}
\details{
The \code{full} argument controls whether to print all the data from
which the \code{iNZightTS} object has been created. The default is set
to \code{FALSE} and only the \code{head()} of the data will be printed.
}

\seealso{
  \code{\link{print}}, \code{\link{iNZightTS}}
}

\examples{
print(iNZightTS(data.frame(Return = rnorm(20)), var = "Return"), full = TRUE)
}

