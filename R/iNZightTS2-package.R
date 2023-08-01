#' The iNZightTS package provides some simple analysis tools for
#' exploring time series data. It is used in the iNZight software.
#'
#' @title Time Series Data Analysis
#'
#' @author Tom Elliott (previously: Marco Kuper, Simon Potter, and David Banks)
#'
#' @docType package
#'
#' @keywords iNZight
#'
#' @seealso \code{\link[iNZightTS]{inzightts}}
#'
#' @import grid grDevices graphics stats utils
#' @importFrom rlang .data
#' @importFrom rlang ':='
#'
#' @name iNZightTS-package
NULL


utils::globalVariables(c(
    "trend", "remainder", "seasonal", "residual", "Date", "value", ".key", ".x",
    "Prediction", "index", ".var", ".model", ".mean", ".lower", ".upper", ".y",
    ".fitted", ".", "P", "Q", "constant", "d", "p", ".rows", "id", ".yint", "z"
))
