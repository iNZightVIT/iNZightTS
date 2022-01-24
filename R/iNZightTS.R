#' The function \code{iNZightTS} is used to create time-series objects used
#' in iNZight.
#'
#' The function \code{iNZgithTS} is used to create time-series
#' objects. Unlike \code{ts} objects, these are lists containing
#' information about the time-series as well as the data and the
#' time-series (\code{ts} object) itself.
#'
#' If a \code{ts} object is used to create the \code{iNZightTS} object,
#' all the domain information is extracted from that object.
#'
#' The function recognises the following time variable formatS without case sensitive:
#'  \itemize{
#'   \item "(Y)yyyy" annually data e.g."(Y)1991"
#'   \item "(Y)yyyyMmm" monthly data e.g."(Y)1991M01"
#'   \item "(Y)yyyyQqq" quarterly data e.g."(Y)1991Q01"
#'   \item "(Y)yyyyWww" weekly data with yearly seasonality e.g."(Y)1991W01"
#'   \item "(Y)yyyyDdd" daily data with yearly seasonality e.g."(Y)1991D01"
#'   \item "WwwDdd"  daily data with weekly seasonality e.g. "W01D01"
#'   \item "DddHhh" hourly data with daily seasonality e.g. "D01H01"
#' }
#' The length of digits of each time unit could be flexible and allowing space between the
#' time unit
#'
#' In case of \code{data} being a data.frame or path to a \code{.csv}
#' file and \code{start} being omitted, the starting date and the
#' \code{freq} is extracted from the column that includes the time
#' information. This column is either named \code{"Time"} or is the first
#' column. If \code{end} is omitted, all of the data will be used for the
#' time-series.
#'
#'
#' @title iNZightTS (Time-Series) Objects
#'
#' @aliases iNZightMTS
#'
#' @param data a \code{data.frame} containing time information and
#'             observation or a path to a \code{.csv} file with
#'             such information or a \code{ts} object
#'
#' @param start the time of the first observation.
#'              Either a single number or a vector
#'              of two integers, which specify a natural time unit
#'              and a (1-based) number of samples into the time unit
#'
#' @param end the time of the last observation, specified in the
#'            same way as \code{start}
#'
#' @param freq the number of observations per unit of time
#'
#' @param var the column number or name for the observations used
#'            from \code{data} in the actual time series
#'
#' @param time.col which column contains the time variable
#' @param ignore.case logical, ignore the case?
#' @param ... additional information passed to \code{read.csv()} and used when
#' \code{data} is a path
#'
#' @return a \code{iNZightTS} object. If multiple variables are requested,
#'         the \code{iNZightMTS} class is added to the result. The result
#'         object contains the original data as a time series object,
#'         as well as information on the series start, end, and frequency.
#'
#' @seealso \code{\link{ts}}, \code{\link{print.iNZightTS}},
#'
#' @examples
#' # create from a ts object
#' z <- iNZightTS(UKgas)
#' plot(z)
#'
#' # create from a data.frame
#' x <- iNZightTS(data.frame(Return = rnorm(100), Time = 1900:1999),
#'     var = "Return")
#' # or specify a time column
#' x <- iNZightTS(data.frame(Return = rnorm(100), Year = 1900:1999),
#'     var = "Return", time.col = "Year")
#'
#' # create from a data.frame with modified time frame
#' y <- iNZightTS(data.frame(Return = rnorm(100)),
#'     start = c(1990, 1), end = c(1993, 5), freq = 12, var = 1)
#' plot(y)
#'
#' @export
iNZightTS <- function(data, index = guess_index_var(data),
                      key = guess_key_var(data), fill_time_gaps = TRUE, ...) {
  data[[index]] <- clean_index_format(data[[index]])

  inzightts <- as_tsibble(data, index = index, key = key, ...)

  if (fill_time_gaps) inzightts <- tsibble::fill_gaps(inzightts)

  # class(inzightts) <- c("iNZightTS", class(inzightts))
  # if (ncol(inzightts) - length(tsibble::key_vars(inzightts)) > 2) {
  #   class(inzightts) <- c("iNZightMTS", class(inzightts))
  # }

  inzightts
}


guess_index_var <- function(data) { ## To be completed: more criteria
  index_candidate <- purrr::map_chr(colnames(data), function(x) {
    index_var_name <- NA

    ## If variable name contains "date" or "time"
    if (grepl("date|time", tolower(x))) {
      index_var_name <- x
    }

    ## If exists month or day of week labels
    patt <- as.character(lubridate::month(seq_len(12), label = TRUE)) %>%
      c(as.character(lubridate::wday(seq_len(7), label = TRUE))) %>%
      paste(collapse = "|") %>%
      tolower()
    if (any(grepl(patt, tolower(data[[x]])))) {
      index_var_name <- x
    }

    ## If exists leading or trailing 4-digit numbers
    if (all(grepl("^[0-9]{4}|[0-9]{4}$", data[[x]]))) {
      index_var_name <- x
    }

    index_var_name
  })

  ## Assume index variable likely to be one of the firsts
  if (all(is.na(index_candidate))) {
    colnames(data)[1]
  } else {
    na.omit(index_candidate)[1]
  }
}


guess_key_var <- function(data) { # TODO
  NULL
}


clean_index_format <- function(x) { ## To be completed: more formats
  ## If index is in year-month format
  if (all(grepl("[0-9]{4}M[0-9]{2}", x))) {
    x <- tsibble::yearmonth(as.character(x))
  }

  ## If index is in year-quarter format
  if (all(grepl("[0-9]{4}Q[0-9]{1}", x))) {
    x <- tsibble::yearquarter(as.character(x))
  }

  ## If index is in year-week format
  if (all(grepl("[0-9]{4}W[0-9]{2}", x))) {
    x <- tsibble::yearweek(as.character(x))
  }

  x
}





is.leapyear <- function(year){
    ( (year %% 4 == 0) & (year %% 100 != 0) ) |
        (year %% 400 == 0)
}



get.ts.structure <- function(vardata) {
    ## check for factor and converting if it is factor
    if (is.factor(vardata))
        vardata <- as.character(vardata)

    ## check for NA return NA if there is
    if (any(is.na(vardata))) {
        return(list(start = NA, frequency = NA))
    }

    ## check for numeric value with decimal place
    if (is.numeric(vardata)) {
        if (any(vardata != round(vardata)))
            return(list(start = NA, frequency = NA))
        vardata <- as.character(vardata)
        ## if the variable is numeric, it can only be yearly data with only 4 digits
        if (any(nchar(vardata) > 4))
            return(list(start = NA, frequency = NA))
    }

    ## extract the first and last value
    firstval <- vardata[1]

    lastval <- vardata[length(vardata)]



    ### extract the first and the last year by fingding the first few digits if the
    ### time variable starts with Y or just digits
    if (all(grepl("^[Y]?[0-9]+", vardata,ignore.case = TRUE))) {

        ## find the location of the year
        first.year.loc <- regexpr("[Y]?[0-9]+", firstval, ignore.case = TRUE)

        last.year.loc <- regexpr("[Y]?[0-9]+", lastval, ignore.case = TRUE)

        ##check if there is a Y in the year and substring the year
        if (attr(first.year.loc, "match.length") == 4){
            first.year <- as.numeric(
                substr(firstval,first.year.loc,
                    attr(first.year.loc, "match.length") +
                        first.year.loc - 1
                )
            )

            last.year <-  as.numeric(
                substr(lastval, last.year.loc,
                    attr(last.year.loc, "match.length") +
                        last.year.loc - 1
                )
            )
        } else  {
            first.year <- as.numeric(
                substr(firstval,first.year.loc + 1,
                    attr(first.year.loc, "match.length") +
                        first.year.loc - 1
                )
            )

            last.year <-  as.numeric(
                substr(lastval, last.year.loc + 1,
                    attr(last.year.loc, "match.length") +
                        last.year.loc - 1
                )
            )
        }
    }
    ### if the frequency is yearly and check for holes in the series by
    ### comparing the numerber of observation with the number of year
    ### calculated by the first year and last year
    if (all(grepl("^[Y]?[0-9]+$", vardata,ignore.case = TRUE))) {
        if (last.year - first.year +1 == length(vardata)) {
            return(list(start = c(first.year), frequency = 1))
        } else {
            ## if the number of observations does not match
            ## return NAs
            return(list(start = NA, frequency = NA))
        }
    }

    ### check for monthly with yearly seasonality "1886M02"

    if (all(grepl("^[Y]?[0-9]+[M][0-9]+$", vardata,ignore.case = TRUE))) {

        ## extract the first month and last month
        first.month.loc <- regexpr("[M][0-9]+", firstval,ignore.case = TRUE)

        last.month.loc <- regexpr("[M][0-9]+", lastval,ignore.case = TRUE)

        first.month <- as.numeric(
            substr(firstval, first.month.loc + 1,
                attr(first.month.loc, "match.length") + first.month.loc - 1
            )
        )

        last.month <- as.numeric(
            substr(lastval, last.month.loc + 1,
                attr(last.month.loc, "match.length") + last.month.loc - 1
            )
        )

        ## check for holes in the obervation by comparing the number of
        ## observations and the number of month calculated by the using the
        ## number of years and month
        ## it works if the observation is in same year
        hole_check <- 12 - first.month + 1 + last.month +
            (last.year - first.year - 1) * 12
        if (hole_check  == length(vardata)) {
            return(
                list(
                    start = c(first.year, first.month),
                    frequency = 12
                )
            )
        } else {
            ## if the numbers do not match, return NAs
            return(list(start = NA, frequency = NA))
        }
    }

    ### check for quarterly with yearly seasonality "1994Q03"
    if (all(grepl("^[Y]?[0-9]+[Q][0-9]+$", vardata, ignore.case = TRUE))) {
        ## extract the quarters
        first.quarter.loc <- regexpr("[Q][0-9]+", firstval, ignore.case = TRUE)

        last.quarter.loc <- regexpr("[Q][0-9]+", lastval, ignore.case = TRUE)

        first.quarter <- as.numeric(
            substr(firstval, first.quarter.loc + 1,
                attr(first.quarter.loc, "match.length") +
                    first.quarter.loc - 1
            )
        )

        last.quarter <- as.numeric(
            substr(lastval, last.quarter.loc + 1,
                attr(last.quarter.loc, "match.length") +
                    last.quarter.loc - 1
            )
        )

        ## check for holes in the observation by comparing the
        ## number of observations and the number of quaters calculated by
        ## the number of years and the number of quaters
        hole_check <- 4 - first.quarter + 1 + last.quarter +
            (last.year - first.year -1) * 4
        if (hole_check == length(vardata)) {
            return(
                list(
                    start = c(first.year, first.quarter),
                    frequency = 4
                )
            )
        } else {
            ## if the numbers do not match, return NAs
            return(list(start = NA, frequency = NA))
        }
    }

    ### check for weekly with yearly seasonality "1990W01"
    if (all(grepl("^[Y]?[0-9]+[W][0-9]+$", vardata, ignore.case = TRUE))) {
        ## extract the first and last week
        first.week.loc <- regexpr("[W][0-9]+", firstval,ignore.case = TRUE)

        last.week.loc <- regexpr("[W][0-9]+", lastval,ignore.case = TRUE)

        first.week <- as.numeric(
            substr(firstval, first.week.loc + 1,
                attr(first.week.loc, "match.length") +
                    first.week.loc - 1
            )
        )

        last.week <- as.numeric(
            substr(lastval, last.week.loc + 1,
                attr(last.week.loc, "match.length") + last.week.loc - 1
            )
        )

        ## check for holes by comparing the number of observations and the number
        ## of the weeks calculated by years and weeks
        hole_check <- 52 - first.week + 1 + last.week +
            (last.year - first.year -1) * 52
        if (hole_check == length(vardata)) {
            return(
                list(
                    start = c(first.year, first.week),
                    frequency = 52
                )
            )
        } else {
            ## if numbers do not match, return NAs
            return(list(start = NA, frequency = NA))
        }
    }

    ### check for daily with yearly seasonality "1991D1"
    if (all(grepl("^[Y]?[0-9]+[D][0-9]+$", vardata, ignore.case = TRUE))) {
        ## extract the days in first and last value
        first.day.loc <- regexpr("[D][0-9]+", firstval, ignore.case = TRUE)

        last.day.loc <- regexpr("[D][0-9]+", lastval, ignore.case = TRUE)

        first.day <- as.numeric(
            substr(firstval, first.day.loc + 1,
                attr(first.day.loc, "match.length") + first.day.loc - 1
            )
        )

        last.day <- as.numeric(
            substr(lastval, last.day.loc + 1,
                attr(last.day.loc, "match.length") + last.day.loc - 1
            )
        )

        ## generate a sequence of year without the first and last year
        if (last.year - first.year > 2) {
            years <- seq(first.year + 1, last.year - 1)
        } else {
            years <- -1
        }

        hole_check <- 365 - first.day + 1 + last.day +
            365 * (last.year - first.year - 1) + sum(is.leapyear(years)) +
            ( is.leapyear(first.year) & (first.day <= 59) )
        if (hole_check == length(vardata)) {
            return(
                list(
                    start = c(first.year, first.day),
                    frequency = 365.25
                )
            )
        } else {
            return(list(start = NA, frequency = NA))
        }
    }

    ### check for daily data in weekly seasonality "W01D01"
    if (all(grepl("^[w][0-9]+[D][0-9]+$", vardata, ignore.case = TRUE))) {
        ## extract the week and days from the first and last value
        first.week.loc <- regexpr("[w][0-9]+", firstval, ignore.case = TRUE)

        last.week.loc <- regexpr("[w][0-9]+", lastval, ignore.case = TRUE)

        first.week <- as.numeric(
            substr(firstval, first.week.loc + 1,
                attr(first.week.loc, "match.length") + first.week.loc - 1
            )
        )

        last.week <- as.numeric(
            substr(lastval, last.week.loc + 1,
                attr(last.week.loc, "match.length") + last.week.loc - 1
            )
        )

        first.day.loc <- regexpr("[D][0-9]+", firstval, ignore.case = TRUE)

        last.day.loc <- regexpr("[D][0-9]+", lastval, ignore.case = TRUE)

        first.day <- as.numeric(
            substr(firstval, first.day.loc + 1,
                attr(first.day.loc, "match.length") + first.day.loc - 1
            )
        )

        last.day <- as.numeric(
            substr(lastval, first.day.loc + 1,
                attr(last.day.loc, "match.length") + last.day.loc - 1
            )
        )

        # determine frequency as 5 or 7 day week
        freq <- ifelse(
            all(grepl("[d][0-5]+$", vardata, ignore.case = TRUE)),
            5,
            7
        )

        ## check holes by comparin the number of observations and the number of days obtained
        ## by calculation using the weeks and the days
        hole_check <- freq - first.day + 1 + last.day +
            (last.week - first.week - 1) * freq
        if (hole_check == length(vardata)) {
            return(
                list(
                    start = c(first.week, first.day),
                    frequency = freq
                )
            )
        } else {
            return(list(start = NA, frequency = NA))
        }
    }

    ## hourly in daily seasonality "D01H01"
    if (all(grepl("^[D][0-9]+[H][0-9]+$", vardata, ignore.case = TRUE))) {
        ## extract the days and the hours from the first and last value
        first.day.loc <- regexpr("[d][0-9]+", firstval, ignore.case = TRUE)
        last.day.loc <- regexpr("[D][0-9]+", lastval, ignore.case = TRUE)
        first.day <- as.numeric(
            substr(firstval, first.day.loc + 1,
                attr(first.day.loc, "match.length") + first.day.loc - 1
            )
        )
        last.day <- as.numeric(
            substr(lastval, last.day.loc + 1,
                attr(last.day.loc, "match.length") + last.day.loc - 1
            )
        )

        first.hour.loc <- regexpr("[h][0-9]+", firstval, ignore.case = TRUE)
        last.hour.loc <- regexpr("[h][0-9]+", lastval, ignore.case = TRUE)
        first.hour <- as.numeric(
            substr(firstval, first.hour.loc + 1,
                attr(first.hour.loc, "match.length") + first.hour.loc - 1
            )
        )

        last.hour <- as.numeric(
            substr(lastval, last.hour.loc + 1,
                attr(last.hour.loc, "match.length") + last.hour.loc - 1
            )
        )

        ## check for holes by comparing the number of the observation and the
        ## number obtained by calculation using the days and hours obtained above
        hole_check <- 24 - first.hour + 1 + last.hour +
            (last.day - first.day - 1) * 24
        if (hole_check == length(vardata)) {
            return(
                list(
                    start = c(first.day, first.hour),
                    frequency  = 24
                )
            )
        } else {
            return(list(start = NA, frequency = NA))
        }
    } else {
        ## if  the varable does not match all the format above
        ## return  NA
        return(list(start = NA, frequency = NA))
    }
}
