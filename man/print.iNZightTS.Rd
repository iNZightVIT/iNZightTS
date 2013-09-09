\name{print.iNZightTS}
\alias{print.iNZightTS}
\title{
  Print an iNZightTS object
}
\description{
 Print method for iNZightTS (time series) objects.
}
\usage{
\method{print}{iNZightTS}(x, full = FALSE, \dots)
}
\arguments{
  \item{x}{
    the \code{iNZightTS} object to be printed
}
  \item{full}{
    whether to print all the underlying data
}
  \item{\dots}{
    Unused arguments. Only here for consistency with the base S3 method.
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
print(iNZightTS(UKgas))
}

