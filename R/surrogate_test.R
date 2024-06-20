#' Surrogate test for independence
#'
#' @param data A tsibble or numeric vector object.
#' @param lag Number of lags in portmanteau test statistic.
#' @param R Number of permutations to perform.
#' @param test.stat Either "ljung-box" or "box-pierce".
#' @param interactive Whether to prompt the user for input or just show warnings.
#'
#' @return An object of class "surrogate" containing the following components:'
#' \itemize{
#'  \item{Q.null}{A numeric vector of test statistics from the null distribution.}
#' \item{Q.obs}{The observed test statistic.}
#' \item{test.stat}{The type of test statistic used.}
#' \item{p.value}{The p-value of the test.}
#' }
#'
#' @examples
#' data <- arima.sim(n = 256, model = list(ar = 0.35))
#' o <- par(mfrow = c(1, 2))
#' plot(data, type = "l")
#' acf(data)
#' par(o)
#' (s <- surrogate_independence(data, lag = 10, interactive = FALSE))
#' plot(s)
#'
#' data <- tsibble::tsibble(value = rnorm(256), Time = 1:256, index = Time)
#' o <- par(mfrow = c(1, 2))
#' plot(data$value, type = "l")
#' acf(data$value)
#' par(o)
#' (s <- surrogate_independence(data, lag = 8, R = 5000, test.stat = "box-pierce"))
#' plot(s)
#'
#' @export
surrogate_independence <- function(
    data,
    lag = 10,
    R = 1000,
    test.stat = c("ljung-box", "box-pierce"),
    interactive = base::interactive()) {
  test.stat <- match.arg(test.stat)

  if (tsibble::is_tsibble(data)) {
    if (length(tsibble::measures(data)) != 1) {
      stop("data must be a tsibble with one measurement variable")
    }
    # Extract time series
    data <- data |>
      dplyr::pull(as.character(tsibble::measures(data)[[1]]))
  }

  n <- length(data)
  surrogates <- replicate(R, sample(data, n)) # n x R matrix

  Robs <- function(x, l) acf(x, plot = FALSE)$acf[2:(l + 1)]
  r.obs <- Robs(data, lag)
  r.null <- apply(surrogates, 2L, Robs, l = lag)
  if (lag == 1L) r.null <- t(r.null)

  Qobs <- function(r, n, l) {
    switch(test.stat,
      "ljung-box" = n * (n + 2) * sum(r^2 / (n - 1:l)),
      "box-pierce" = n * sum(r^2)
    )
  }
  Q.obs <- Qobs(r.obs, n, lag)
  Q.null <- apply(r.null, 2L, Qobs, n = n, l = lag)

  p.value <- mean(Q.null >= Q.obs)
  ntail <- sum(Q.null >= Q.obs)

  if (ntail < 10) {
    if (!interactive) {
      cli::cli_alert_warning("Fewer than 10 permutations in the tail")
    } else {
      res <- utils::menu(
        c(
          "Continue",
          sprintf("Repeat with %d permutations", R * 10),
          "Let me choose the number of permutations"
        ),
        title = "Fewer than 10 permutations in the tail"
      )
      if (res > 1) {
        if (res == 2) {
          R <- R * 10
        } else {
          R <- as.integer(readline(sprintf(
            "Enter a new number of permutations: (current = %d) ",
            R
          )))
        }
        return(
          surrogate_independence(data,
            lag = lag,
            R = R,
            test.stat = test.stat,
            interactive = interactive
          )
        )
      }
    }
  }

  # Output
  output <- list(
    Q.null = Q.null,
    Q.obs = Q.obs,
    test.stat = test.stat,
    p.value = p.value
  )

  class(output) <- "srgt_indep"
  output
}

#' @export
print.srgt_indep <- function(x, ...) {
  cat("Surrogate test for independence\n")
  cat(x$test.stat, "test statistic: ", x$Q.obs, "\n")
  cat("p-value: ", x$p.value, "(based on", length(x$Q.null), "permutations)\n")
}


#' Plot surrogate null distribution and observed test statistic
#' @param x Object of class "surrogate".
#' @param ... Additional arguments to pass to the plot function.
#' @describeIn surrogate_independence Plot method
#' @export
plot.srgt_indep <- function(x, ...) {
  # x: Object of class "surrogate"

  ggplot2::ggplot(
    data = data.frame(Q = x$Q.null),
    mapping = ggplot2::aes(x = Q)
  ) +
    ggplot2::geom_histogram(
      bins = 30
    ) +
    ggplot2::geom_vline(
      xintercept = x$Q.obs,
      linetype = "dashed"
    ) +
    ggplot2::labs(
      x = "Test statistic",
      y = "Count"
    )
}

surrogate_stationary <- function(data, ...) {

}
