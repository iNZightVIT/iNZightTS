format_index <- function(x) {
    x <- as.character(x)

    if (all(grepl("^[Y]?[0-9]+$", x, TRUE))) {
        x <- as.numeric(gsub("^[Y]?", "", x, TRUE))
    } else if (all(grepl("^[Y]?[0-9]+[M][0-9]+$", x, TRUE))) {
        x <- tsibble::yearmonth(gsub("^[Y]?", "", x, TRUE))
    } else if (all(grepl("^[Y]?[0-9]+[Q][0-9]+$", x, TRUE))) {
        x <- gsub("^[Y]?([0-9]+[Q])[0-9]?([0-9]{1})$", "\\1\\2", x, TRUE) %>%
            tsibble::yearquarter()
    } else if (all(grepl("^[Y]?[0-9]+[W][0-9]+$", x, TRUE))) {
        x <- tsibble::yearweek(gsub("^[Y]?", "", x, TRUE))
    } else if (all(grepl("^[Y]?[0-9]+[D][0-9]+$", x, TRUE))) {
        y <- (gsub("^[Y]?([0-9]+)[D][0-9]+$", "\\1", x, TRUE) %>%
            paste0("0101") %>%
            as.numeric() %>%
            lubridate::ymd())
        d <- (gsub("^[Y]?[0-9]+[D]([0-9]+)$", "\\1", x, TRUE) %>%
            as.numeric() - 1) %>%
            lubridate::days()
        x <- y + d
    } else if (all(grepl("^[W][0-9]+[D][0-9]+$", x, TRUE))) {
        y <- lubridate::ymd("00010101")
        w <- (gsub("^[W]([0-9]+)[D][0-9]+$", "\\1", x, TRUE) %>%
            as.numeric() - 1) %>%
            lubridate::weeks()
        d <- (gsub("^[W][0-9]+[D]([0-9]+)$", "\\1", x, TRUE) %>%
            as.numeric() - 1) %>%
            lubridate::days()
        x <- y + w + d
    } else if (all(grepl("^[D][0-9]+[H][0-9]+$", x, TRUE))) {
        y <- lubridate::ymd("00010101")
        d <- (gsub("^[D]([0-9]+)[H][0-9]+$", "\\1", x, TRUE) %>%
            as.numeric() - 1) %>%
            lubridate::days()
        h <- gsub("^[D][0-9]+[H]([0-9]+)$", "\\1", x, TRUE) %>%
            as.numeric() %>%
            lubridate::hours()
        x <- y + d + h
    } else {
        rlang::abort("Invalid index format")
    }

    x
}


#' The function \code{inzightts} is used to create temporal data frames
#' used in iNZight.
#'
#' The function \code{inzightts} is used to create temporal data
#' frames. Unlike \code{ts} objects, these are tsibble objects
#' that enables temporal data wrangling adapting to the tidy
#' data principles, which are both data- and model-oriented.
#'
#' If a \code{ts} object is used to create the inzightts object,
#' all the domain information is extracted from that object.
#'
#' \code{index} should be a \code{character}, \code{Date},
#' \code{\link[tsibble]{yearweek}}, \code{\link[tsibble]{yearmonth}} or
#' \code{\link[tsibble]{yearquarter}} object.
#'
#' If \code{index} is a \code{character}, the function recognises the
#' following time variable formatS without case sensitive:
#'  \itemize{
#'   \item "(Y)yyyy" annually data e.g."(Y)1991"
#'   \item "(Y)yyyyMmm" monthly data e.g."(Y)1991M01"
#'   \item "(Y)yyyyQqq" quarterly data e.g."(Y)1991Q01"
#'   \item "(Y)yyyyWww" weekly data with yearly seasonality e.g."(Y)1991W01"
#'   \item "(Y)yyyyDdd" daily data with yearly seasonality e.g."(Y)1991D01"
#'   \item "WwwDdd"  daily data with weekly seasonality e.g. "W01D01"
#'   \item "DddHhh" hourly data with daily seasonality e.g. "D01H01"
#' }
#' The length of digits of each time unit could be flexible and allowing space
#' between the time unit
#'
#' In case of \code{data} being a data.frame or path to a \code{.csv}
#' file and \code{start} being omitted, the starting date and the
#' \code{freq} is extracted from the column that includes the time
#' information. This column is either named \code{"Time"} or is the first
#' column. If \code{end} is omitted, all of the data will be used for the
#' time-series.
#'
#'
#' @title Coerce to an inzightts (Time-Series) Objects
#'
#' @param x a \code{data.frame}, \code{ts}, tsibble, or path
#' @param ... additional arguments to be passed to or from methods
#'
#' @return an inzightts (\code{inz_ts}) object, a sub-class of tsibble
#'         which includes the index variable, temporal variable and,
#'         if applicable, relevant keys.
#'
#' @rdname inzightts
#'
#' @seealso \code{\link[tsibble]{tsibble}}, \code{\link[tsibble]{as_tsibble}}
#'          and \code{\link[tsibble]{new_tsibble}}
#'
#' @examples
#' # create from a ts object
#' z <- inzightts(UKgas)
#' plot(z)
#'
#' # create from a data.frame
#' x <- inzightts(
#'     data.frame(Return = rnorm(100), Time = 1900:1999),
#'     var = "Return"
#' )
#' # or specify a time column
#' x <- inzightts(
#'     data.frame(Return = rnorm(100), Year = 1900:1999),
#'     var = "Return", index = "Year"
#' )
#'
#' # create from a data.frame with modified time frame
#' y <- inzightts(
#'     data.frame(Return = rnorm(100)),
#'     start = c(1990, 1), end = c(1993, 5), freq = 12, var = 1
#' )
#' plot(y)
#'
#' @export
inzightts <- function(x, ...) {
    UseMethod("inzightts")
}


#' @param stringsAsFactors see \code{\link[utils]{read.csv}}
#' @param as.is see \code{\link[utils]{read.csv}}
#'
#' @rdname inzightts
#'
#' @export
inzightts.character <- function(x, stringsAsFactors = TRUE, as.is = TRUE, ...) {
    inzightts(read.csv(x, stringsAsFactors = stringsAsFactors, as.is = as.is, ...))
}


#' @param var the column number or name for the observations used
#'        from \code{data} in the actual time series
#' @param index which column contains the time variable
#' @param key Variable(s) that uniquely determine time indices
#' @param start the time of the first observation.
#'        Either a single number or a vector
#'        of two integers, which specify a natural time unit
#'        and a (1-based) number of samples into the time unit
#' @param end the time of the last observation, specified in the
#'        same way as \code{start}
#' @param freq the number of observations per unit of time
#'
#' @rdname inzightts
#'
#' @export
inzightts.data.frame <- function(x, var = NULL, index = NULL, key = NULL,
                                 start = NULL, end = NULL, freq = NULL, ...) {
    if (is.null(index) & sum(is.null(start), is.null(end), is.null(freq)) > 1) {
        index <- grep("time|date|index", names(x), ignore.case = TRUE)[1]
    }

    if (is.numeric(index)) index <- names(x)[index]
    if (is.numeric(key)) key <- names(x)[key]

    if (is.null(var)) {
        var <- seq_len(ncol(x))
    }
    if (is.numeric(var)) var <- names(x)[var]

    x <- dplyr::select(x, !!unique(na.omit(c(index, var))))

    if (is.null(index) | isTRUE(is.na(index))) {
        if (sum(is.null(start), is.null(end), is.null(freq)) > 1) {
            rlang::abort("Unable to automatically identify the index column.")
        }
        ts_spec <- list()
        if (!is.null(start)) ts_spec$start <- start
        if (!is.null(end)) ts_spec$end <- end
        if (!is.null(freq)) ts_spec$freq <- freq
        inzightts <- expr(ts(x[, var], !!!ts_spec)) %>%
            rlang::new_quosure() %>%
            rlang::eval_tidy()
    } else if (inherits(x[[index]], "Date") | inherits(x[[index]], "vctrs_vctr")) {
        inzightts <- try(
            tsibble::as_tsibble(x, index = !!index, key = !!key),
            silent = TRUE
        )
    } else if (
        is.character(x[[index]]) | is.factor(x[[index]]) | is.numeric(x[[index]])
    ) {
        inzightts <- try(
            dplyr::mutate(x, !!index := format_index(!!sym(index))) %>%
                tsibble::as_tsibble(index = !!index, key = !!key),
            silent = TRUE
        )
    }

    if (inherits(inzightts, "try-error")) {
        rlang::abort("Invalid data. Maybe you forgot to specify `key`?")
    } else {
        inzightts(inzightts, var_name = var)
    }
}


#' @param var_name rename the variable column of the univariate time series,
#'        applicable only if \code{x} is not an \code{mts} object.
#' @param pivot_longer logical, \code{TRUE} gives a "longer" form of the data,
#'        otherwise as is, applicable only if \code{x} is an \code{mts} object.
#'
#' @rdname inzightts
#'
#' @export
inzightts.ts <- function(x, var_name = NULL, pivot_longer = FALSE, ...) {
    if (is.mts(x)) {
        var_name <- NULL
        inzightts <- tsibble::as_tsibble(x, pivot_longer = pivot_longer, ...)
    } else {
        if (is.null(var_name)) var_name <- "value"
        pivot_longer <- NULL
        inzightts <- x %>%
            tsibble::as_tsibble(...) %>%
            dplyr::rename(!!sym(var_name) := value)
    }

    inzightts(inzightts)
}


#' @rdname inzightts
#'
#' @export
inzightts.tbl_ts <- function(x, ...) {
    x %>%
        dplyr::rename(index = !!tsibble::index(x)) %>%
        tsibble::fill_gaps() %>%
        tsibble::new_tsibble(..., class = "inz_ts")
}
