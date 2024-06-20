chk_input <- function(type, constraints, fun) {
    obj <- list(
        type = type,
        constraints = constraints,
        fun = fun
    )
    class(obj) <- "chk_input"
    obj
}

#' @export
print.chk_input <- function(x, ...) {
    cat(x$type)
}

chk_number <- function(min = NULL, max = NULL, integer = TRUE, default = NA) {
    constraints <- list()
    if (!is.null(min)) constraints$min <- c("Minimum value" = min)
    if (!is.null(max)) constraints$max <- c("Maximum value" = max)
    if (integer) constraints$integer <- c("Must be an integer" = TRUE)
    chk_input(
        "number", constraints,
        function(x) {
            if (!is.na(default)) if (missing(x)) {
                return(default)
            }
            if (!is.null(min)) if (x < min) stop("Value must be", min, "or greater")
            if (!is.null(max)) if (x > max) stop("Value must be", max, "or greater")
            if (integer) if (!is.integer(x)) stop("Value must be an integer")
            x
        }
    )
}

chk_enum <- function(options) {
    chk_input(
        "enum",
        list(
            "values" = options
        ),
        function(x) match.arg(x, options)
    )
}
