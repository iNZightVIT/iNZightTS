#' Surrogate test for independence
#' @export
surrogate.test <- function(data, lag, N = 1000, test.stat = "ljung-box") {
  # data: a tsibble or numeric vector object
  # lag: number of lags in portmanteau test statistic
  # N: number of permutations to perform
  # test.stat: either "ljung-box" or "box-pierce"

  if (tsibble::is_tsibble(data)) {
    if (length(tsibble::measures(data)) != 1) {
      stop("data must be a tsibble with one measurement variable")
    }
    # Extract time series
    data <- data %>%
      dplyr::pull(as.character(tsibble::measures(data)[[1]]))
  }

  n <- length(data)

  Q.null <- rep(NA, N) # Open test statistic vectors

  if (test.stat == "ljung-box") {
    # Observed test statistic
    r.obs <- acf(data, plot = FALSE)$acf[2:(lag + 1)]
    Q.obs <- n * (n + 2) * sum(r.obs^2 / (n - 1:lag))

    # Null distribution
    for (i in 1:N) {
      surrogate <- sample(data, n) # Permute data (kill autocorrelation, maintain amplitude)
      r <- acf(surrogate, plot = FALSE)$acf[2:(lag + 1)] # Estimate autocorrelation
      Q.null[i] <- n * (n + 2) * sum(r^2 / (n - 1:lag)) # Ljung-Box test statistic
    }
  }

  if (test.stat == "box-pierce") {
    # Observed test statistic
    r.obs <- acf(data, plot = FALSE)$acf[2:(lag + 1)]
    Q.obs <- n * sum(r.obs^2)

    # Null distribution
    for (i in 1:N) {
      surrogate <- sample(data, n) # Permute data (kill autocorrelation, maintain amplitude)
      r <- acf(surrogate, plot = FALSE)$acf[2:(lag + 1)] # Estimate autocorrelation
      Q.null[i] <- n * sum(r^2) # Box-Pierce test statistic
    }
  }

  # Compute p-value
  p.value <- mean(Q.null >= Q.obs) # p-value

  # Output
  output <- list(
    Q.null = Q.null,
    Q.obs = Q.obs,
    test.stat = test.stat,
    p.value = p.value
  )

  class(output) <- "surrogate"

  return(output)
}


# Function to plot surrogate null distribution and observed test statistic
#' @export
plot.surrogate <- function(x, ...) {
  # x: Object of class "surrogate"

  ggplot2::ggplot(
    data = data.frame(Q = x$Q.null),
    mapping = ggplot2::aes(x = Q)
  ) +
    ggplot2::geom_histogram(fill = "navy", colour = "black") +
    ggplot2::geom_vline(
      xintercept = x$Q.obs,
      linetype = "dashed"
    ) +
    ggplot2::labs(
      x = "Test statistic",
      y = "Count"
    )
}
