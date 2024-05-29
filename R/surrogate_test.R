#' Surrogate test for independence
#'
#' @param data A tsibble or numeric vector object.
#' @param lag Number of lags in portmanteau test statistic.
#' @param R Number of permutations to perform.
#' @param test.stat Either "ljung-box" or "box-pierce".
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
#' (s <- surrogate.test(data, lag = 10))
#' plot(s)
#'
#' data <- tsibble::tsibble(value = rnorm(256), Time = 1:256, index = Time)
#' o <- par(mfrow = c(1, 2))
#' plot(data$value, type = "l")
#' acf(data$value)
#' par(o)
#' (s <- surrogate.test(data, lag = 8, R = 5000, test.stat = "box-pierce"))
#' plot(s)
#'
#' @export
surrogate.test <- function(
    data,
    lag = 4,
    R = 1000,
    test.stat = c("ljung-box", "box-pierce")) {
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

  # Output
  output <- list(
    Q.null = Q.null,
    Q.obs = Q.obs,
    test.stat = test.stat,
    p.value = mean(Q.null >= Q.obs)
  )

  class(output) <- "surrogate"
  output
}

#' @export
print.surrogate <- function(x, ...) {
  cat("Surrogate test for independence\n")
  cat(x$test.stat, "test statistic: ", x$Q.obs, "\n")
  cat("p-value: ", x$p.value, "(based on", length(x$Q.null), "permutations)\n")
}


#' Plot surrogate null distribution and observed test statistic
#' @param x Object of class "surrogate".
#' @describeIn surrogate.test Plot method
#' @export
plot.surrogate <- function(x, ...) {
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
