# 'prais_winsten' reports the history of the iterations. This helper suppresses
# those messages to keep the test results readable. Arguments that are not
# evaluated in the usual way, such as 'subset' and 'weights', cannot be passed
# through the dots of a wrapper and are tested with direct calls.
fit_quietly <- function(...) {
  suppressMessages(prais_winsten(...))
}

# Generates a time series sample with AR(1) errors. Only 'rnorm' is used, so that
# the samples are reproducible across R versions.
ar1_sample <- function(n = 100, rho = .5, seed = 1234567) {
  set.seed(seed)
  x <- rnorm(n, 30, 5)
  z <- rnorm(n)
  u <- rnorm(n, 0, 5)
  for (i in 2:n) {
    u[i] <- u[i] + rho * u[i - 1]
  }
  data.frame(x = x, z = z, g = factor(rep_len(c("a", "b", "c"), n)),
             y = 10 + 1.5 * x - .5 * z + u, time = 1:n)
}

# Generates a balanced panel sample with AR(1) errors.
ar1_panel <- function(n_group = 5, n_time = 20, rho = .5, seed = 1234567) {
  set.seed(seed)
  result <- NULL
  for (i in 1:n_group) {
    x <- rnorm(n_time, 30, 5)
    u <- rnorm(n_time, 0, 5)
    for (j in 2:n_time) {
      u[j] <- u[j] + rho * u[j - 1]
    }
    result <- rbind(result, data.frame(id = i, time = 1:n_time, x = x,
                                       g = factor(rep_len(c("a", "b"), n_time)),
                                       y = 10 + 1.5 * x + u))
  }
  result
}

# Inverse of the covariance matrix of an AR(1) process with coefficient 'rho'
ar1_omega_inv <- function(rho, n) {
  solve(rho^abs(outer(1:n, 1:n, "-")) / (1 - rho^2))
}

# Applies the Prais-Winsten transformation to a model matrix of a time series,
# independently of the implementation of the package
pw_transform_series <- function(x, rho, intercept = TRUE) {
  n <- nrow(x)
  result <- rbind(sqrt(1 - rho^2) * x[1, ], x[-1, , drop = FALSE] - rho * x[-n, , drop = FALSE])
  if (intercept) {
    result[1, 1] <- sqrt(1 - rho^2)
    result[-1, 1] <- 1 - rho
  }
  result
}
