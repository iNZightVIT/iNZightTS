% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recompose.R
\name{recompose}
\alias{recompose}
\title{Recompose a decomposed time series}
\usage{
recompose(vars.decomp, animate = TRUE, e = NULL)
}
\arguments{
\item{vars.decomp}{a decomposed time series object}

\item{animate}{logical, if \code{TRUE} the plot will be animated}

\item{e}{environment to use (for controlling animation timings)}
}
\value{
the recomposed series
}
\description{
Recompose a time series object, with optional animation.
}
\author{
iNZight
}