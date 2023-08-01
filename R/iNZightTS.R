format_index <- function(x) {
    x <- as.character(x)
    if (all(grepl("^[Y]?[0-9]+$", x, TRUE))) {
        x <- as.numeric(gsub("^[Y]?", "", x, TRUE))
    } else if (all(grepl("^[Y]?[0-9]+\\s?[M][0-9]+$", x, TRUE))) {
        x <- tsibble::yearmonth(gsub("^[Y]?", "", x, TRUE))
    } else if (all(grepl("^[Y]?[0-9]+\\s?[Q][0-9]+$", x, TRUE))) {
        x <- gsub("^[Y]?([0-9]+\\s?[Q])[0-9]?([0-9]{1})$", "\\1\\2", x, TRUE) |>
            tsibble::yearquarter()
    } else if (all(grepl("^[Y]?[0-9]+\\s?[W][0-9]+$", x, TRUE))) {
        x <- tsibble::yearweek(gsub("^[Y]?", "", x, TRUE))
    } else if (all(grepl("^[Y]?[0-9]+\\s?[D][0-9]+$", x, TRUE))) {
        ## Two-digit decimal year will be assumed as "20XX"
        y <- gsub("^[Y]?([0-9]+\\s?)[D][0-9]+$", "\\1", x, TRUE) |>
            paste0("0101") |>
            as.numeric() |>
            lubridate::ymd()
        d <- (gsub("^[Y]?[0-9]+\\s?[D]([0-9]+)$", "\\1", x, TRUE) |>
            as.numeric() - 1) |>
            lubridate::days()
        x <- y + d
    } else if (all(grepl("^[W][0-9]+[D][0-9]+$", x, TRUE))) {
        y <- lubridate::ymd("00010101")
        w <- (gsub("^[W]([0-9]+)[D][0-9]+$", "\\1", x, TRUE) |>
            as.numeric() - 1) |>
            lubridate::weeks()
        d <- (gsub("^[W][0-9]+[D]([0-9]+)$", "\\1", x, TRUE) |>
            as.numeric() - 1) |>
            lubridate::days()
        x <- y + w + d
    } else if (all(grepl("^[D][0-9]+[H][0-9]+$", x, TRUE))) {
        y <- lubridate::ymd("00010101")
        d <- (gsub("^[D]([0-9]+)[H][0-9]+$", "\\1", x, TRUE) |>
            as.numeric() - 1) |>
            lubridate::days()
        h <- gsub("^[D][0-9]+[H]([0-9]+)$", "\\1", x, TRUE) |>
            as.numeric() |>
            lubridate::hours()
        x <- y + d + h
    } else {
        rlang::abort("Invalid index format")
    }
    x
}


#' Coerce data to an inzightts (time-series) object
#'
#' The function `inzightts` creates temporal data frames for use in iNZight.
#' Unlike `ts` objects, these are tsibble objects that enable temporal data
#' wrangling, adapting to tidy data principles, which are both data- and
#' model-oriented.
#'
#' If a `ts` object is used to create the inzightts object, all the domain
#' information is extracted from that object.
#'
#' The `index` parameter should be a `character`, `Date`,
#' `yearweek`, `yearmonth`, or `yearquarter` object.
#'
#' If `index` is a `character`, the function recognizes the following
#' time variable formats without case sensitivity:
#'   - "(Y)yyyy": annually data, e.g., "(Y)1991"
#'   - "(Y)yyyyMmm": monthly data, e.g., "(Y)1991M01"
#'   - "(Y)yyyyQqq": quarterly data, e.g., "(Y)1991Q01"
#'   - "(Y)yyyyWww": weekly data with yearly seasonality, e.g., "(Y)1991W01"
#'   - "(Y)yyyyDdd": daily data with yearly seasonality, e.g., "(Y)1991D01"
#'   - "WwwDdd": daily data with weekly seasonality, e.g., "W01D01"
#'   - "DddHhh": hourly data with daily seasonality, e.g., "D01H01"
#'
#' The length of digits of each time unit could be flexible, and spaces between
#' the time unit are allowed.
#'
#' In case `data` is a data.frame or path to a `.csv` file, and
#' `start` is omitted, the starting date and the `freq` are extracted
#' from the column that includes the time information. This column is either
#' named `"Time"` or is the first column. If `end` is omitted, all of
#' the data will be used for the time-series.
#'
#' @param x A `data.frame`, `ts`, tsibble, or path.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @return An `inzightts` (`inz_ts`) object, a sub-class of tsibble,
#'         which includes the index variable, temporal variable, and, if
#'         applicable, relevant keys.
#'
#' @seealso \code{\link[tsibble]{tsibble}}, \code{\link[tsibble]{as_tsibble}}
#'          and \code{\link[tsibble]{new_tsibble}}
#'
#' @examples
#' # create from a ts object
#' z <- inzightts(UKgas)
#' \dontrun{
#' plot(z)
#' }
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
#' \dontrun{
#' plot(y)
#' }
#'
#' @export
#' @md
inzightts <- function(x, ...) {
    UseMethod("inzightts")
}


#' @param stringsAsFactors See \code{\link[utils]{read.csv}}
#' @param as.is See \code{\link[utils]{read.csv}}
#'
#' @rdname inzightts
#'
#' @export
inzightts.character <- function(x, stringsAsFactors = TRUE, as.is = TRUE, ...) {
    inzightts(read.csv(x, stringsAsFactors = stringsAsFactors, as.is = as.is, ...))
}


#' @param var The column number or name in `data` representing the observations
#'            used in the actual time series.
#' @param index The column number or name in `data` containing the time
#'        variable.
#' @param key The variable(s) that uniquely determine time indices.
#' @param start The time of the first observation. It can be a single number or
#'             a vector of two integers representing a natural time unit and
#'             a (1-based) number of samples into the time unit.
#' @param end The time of the last observation, specified in the same way as
#'            `start`.
#' @param freq The number of observations per unit of time.
#'
#' @rdname inzightts
#'
#' @export
#' @md
inzightts.data.frame <- function(x, var = NULL, index = NULL, key = NULL,
                                 start = NULL, end = NULL, freq = NULL, ...) {
    if (is.null(index) && sum(is.null(start), is.null(end), is.null(freq)) > 1) {
        index <- grep("time|date|index", names(x), ignore.case = TRUE)[1]
    }
    if (is.numeric(index)) index <- names(x)[index]
    if (is.numeric(key)) key <- names(x)[key]
    if (is.null(var)) var <- seq_len(ncol(x))
    if (is.numeric(var)) var <- names(x)[var]
    x <- dplyr::select(x, !!unique(na.omit(c(index, var))))
    if (is.null(index) || isTRUE(is.na(index))) {
        if (sum(is.null(start), is.null(end), is.null(freq)) > 1) {
            rlang::abort("Unable to automatically identify the index column.")
        }
        ts_spec <- list()
        if (!is.null(start)) ts_spec$start <- start
        if (!is.null(end)) ts_spec$end <- end
        if (!is.null(freq)) ts_spec$freq <- freq
        inzightts <- rlang::inject(ts(x[, var], !!!ts_spec))
    } else if (inherits(x[[index]], "Date") || inherits(x[[index]], "vctrs_vctr")) {
        inzightts <- try(
            tsibble::as_tsibble(x, index = !!index, key = !!key),
            silent = TRUE
        )
    } else if (
        is.character(x[[index]]) || is.factor(x[[index]]) || is.numeric(x[[index]])
    ) {
        inzightts <- try(
            dplyr::mutate(x, !!index := format_index(!!sym(index))) |>
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


#' @param var_name The new name for the variable column of the univariate time
#'        series, applicable only if `x` is not an `mts` object.
#' @param pivot_longer Logical; set to `TRUE` to transform data to a "longer"
#'        form, otherwise keep the current form. Applicable only if `x` is an
#'        `mts` object.
#'
#' @rdname inzightts
#'
#' @export
#' @md
inzightts.ts <- function(x, var_name = NULL, pivot_longer = FALSE, ...) {
    if (is.mts(x)) {
        var_name <- NULL
        inzightts <- tsibble::as_tsibble(x, pivot_longer = pivot_longer, ...)
    } else {
        if (is.null(var_name)) var_name <- "value"
        pivot_longer <- NULL
        inzightts <- x |>
            tsibble::as_tsibble(...) |>
            dplyr::rename(!!sym(var_name) := value)
    }
    inzightts(inzightts)
}


#' @rdname inzightts
#'
#' @export
inzightts.tbl_ts <- function(x, ...) {
    x |>
        dplyr::rename(index = !!tsibble::index(x)) |>
        tsibble::fill_gaps() |>
        tsibble::new_tsibble(..., class = "inz_ts")
}
