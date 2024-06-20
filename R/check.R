#' Check model assumptions
#' @param x An object of class "inzts_fit"
#' @param ... Additional arguments
#' @return The object with the checked and transformed fit
#' @export
#' @md
check <- function(x, ...) {
    UseMethod("check", x)
}

#' @include check_input.R
ts_checks <- list(
    independence = list(
        name = "Independence of residuals",
        # test = surrogate.test,
        args = c(
            lag = chk_number(min = 1, integer = TRUE, default = 4),
            R = chk_number(min = 100, max = 1e6, integer = TRUE, default = 1000),
            test.stat = chk_enum(c("ljung-box", "box-pierce"))
        )
    )
)

#' @export
check.inz_ts <- function(x, ...) {
    if (attr(x, "checked")) {
        return(x)
    }

    attr(x, "checked") <- TRUE
    class(x) <- c("inzts_chk_ts", class(x))
    x
}

#' @export
check.inzts_fit <- function(x, ...) {
    if (attr(x, "checked")) {
        return(x)
    }

    attr(x, "checked") <- TRUE
    class(x) <- c("inzts_chk_fit", class(x))
    x
}

#' @export
check.inzts_chk_fit <- function(x, ...) {
    # re-check a model
}
