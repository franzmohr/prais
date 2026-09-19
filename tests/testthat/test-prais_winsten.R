test_that("the estimation output has the documented structure", {
  data <- ar1_sample()
  pw <- fit_quietly(y ~ x, data = data, index = "time")

  expect_s3_class(pw, "prais")
  expect_true(all(c("coefficients", "rho", "residuals", "fitted.values", "rank",
                    "df.residual", "call", "terms", "model") %in% names(pw)))
  expect_identical(names(pw$coefficients), c("(Intercept)", "x"))
  expect_identical(pw$rank, 2L)
  expect_identical(pw$df.residual, nrow(data) - 2L)
  expect_length(pw$fitted.values, nrow(data))
  expect_length(pw$residuals, nrow(data))
  expect_equal(pw$residuals, data$y - pw$fitted.values, ignore_attr = TRUE)
})

test_that("the estimator reproduces the exact GLS solution", {
  data <- ar1_sample(n = 120)
  pw <- fit_quietly(y ~ x + z, data = data, index = "time",
                    tol = 1e-12, max_iter = 200)

  n <- nrow(data)
  x <- stats::model.matrix(~ x + z, data)
  y <- data$y
  omega_inv <- ar1_omega_inv(pw$rho[NROW(pw$rho), "rho"], n)
  xoi <- crossprod(x, omega_inv)
  beta <- solve(xoi %*% x, xoi %*% y)

  expect_equal(unname(pw$coefficients), unname(drop(beta)))

  # The standard errors of the summary must be the GLS standard errors
  u <- y - x %*% beta
  sigma_sq <- drop(crossprod(u, omega_inv) %*% u) / (n - ncol(x))
  expect_equal(unname(summary(pw)$coefficients[, "Std. Error"]),
               unname(sqrt(diag(solve(xoi %*% x)) * sigma_sq)))
})

test_that("the data are ordered by the variables of argument 'index'", {
  data <- ar1_sample(n = 40)
  shuffled <- data[c(20:40, 1:19), ]
  pw <- fit_quietly(y ~ x, data = shuffled, index = "time")

  # 'data' is already ordered by the time variable
  expect_equal(pw$model$x, data$x)
  expect_equal(pw$coefficients,
               fit_quietly(y ~ x, data = data, index = "time")$coefficients)
})

test_that("panel models contain the ID and time variables in the model frame", {
  data <- ar1_panel(n_group = 3, n_time = 10)
  shuffled <- data[c(11:30, 1:10), ]
  pw <- fit_quietly(y ~ x, data = shuffled, index = c("id", "time"))

  ordered <- data[order(data$id), ]
  ordered <- ordered[order(ordered$time), ]
  expect_identical(pw$model$id, ordered$id)
  expect_identical(pw$model$time, ordered$time)
})

test_that("the model frame of a panel model keeps its terms attribute", {
  data <- ar1_panel(n_group = 3, n_time = 10)
  pw <- fit_quietly(y ~ x, data = data, index = c("id", "time"))

  # Appending the index variables must not turn the model frame into a plain
  # data frame, because 'model.matrix' would then evaluate the variables of the
  # formula against it again instead of taking the columns it already holds
  expect_s3_class(attr(pw$model, "terms"), "terms")
  expect_identical(attr(pw$model, "terms"), pw$terms)
})

test_that("panel models support terms whose source column is not in the frame", {
  data <- ar1_panel(n_group = 3, n_time = 10)
  data$log_x <- log(data$x)
  pw <- fit_quietly(y ~ log(x), data = data, index = c("id", "time"))
  # The same model with the transformation applied beforehand, which does not
  # depend on the model frame being recognised as one
  plain <- fit_quietly(y ~ log_x, data = data, index = c("id", "time"))

  expect_identical(colnames(stats::model.matrix(pw$terms, pw$model)),
                   c("(Intercept)", "log(x)"))
  expect_equal(unname(pw$coefficients), unname(plain$coefficients))
  expect_equal(unname(summary(pw)$coefficients),
               unname(summary(plain)$coefficients))
  expect_equal(unname(vcovHC(pw)), unname(vcovHC(plain)))
  expect_equal(unname(vcovPC(pw)), unname(vcovPC(plain)))
  expect_equal(unname(confint(pw)), unname(confint(plain)))
})

test_that("argument 'index' is validated", {
  data <- ar1_sample(n = 40)
  expect_error(fit_quietly(y ~ x, data = data, index = c("id", "time", "x")),
               "can only have up to 2 elements")
})

test_that("argument 'twostep' stops the estimation after one iteration", {
  data <- ar1_sample()
  pw <- fit_quietly(y ~ x, data = data, index = "time", twostep = TRUE)

  # The history contains the starting value and one iteration
  expect_identical(NROW(pw$rho), 2L)
  expect_identical(pw$rho[1, "rho"], 0)
})

test_that("rho is bounded to the interval [-1, 1]", {
  data <- ar1_sample(n = 60, rho = .99)
  pw <- fit_quietly(y ~ x, data = data, index = "time")

  expect_true(all(abs(pw$rho) <= 1))
})

test_that("panel models are estimated for every specification of rho", {
  data <- ar1_panel()
  pooled <- fit_quietly(y ~ x, data = data, index = c("id", "time"))
  expect_identical(NCOL(pooled$rho), 1L)
  expect_identical(pooled$index, c("id", "time"))
  expect_true(all(c("id", "time") %in% names(pooled$model)))

  panelwise <- fit_quietly(y ~ x, data = data, index = c("id", "time"),
                           panelwise = TRUE)
  expect_identical(NCOL(panelwise$rho), 5L)
  expect_identical(dimnames(panelwise$rho)[[2]], as.character(1:5))

  for (weight in c("T", "T1")) {
    weighted <- fit_quietly(y ~ x, data = data, index = c("id", "time"),
                            panelwise = TRUE, rhoweight = weight)
    # A weighted mean of the panel specific estimates is a single coefficient
    expect_identical(NCOL(weighted$rho), 1L)
    expect_true(abs(weighted$rho[NROW(weighted$rho), 1]) <= 1)
  }
})

test_that("panel specific estimates of rho are used for the transformation", {
  data <- ar1_panel()
  pooled <- fit_quietly(y ~ x, data = data, index = c("id", "time"))
  panelwise <- fit_quietly(y ~ x, data = data, index = c("id", "time"),
                           panelwise = TRUE)

  expect_false(isTRUE(all.equal(unname(pooled$coefficients),
                                unname(panelwise$coefficients))))
})

test_that("panel estimates reproduce the block diagonal GLS solution", {
  # Every panel follows an AR(1) process, so the covariance matrix of the errors
  # is block diagonal with one AR(1) block per panel
  data <- ar1_panel(n_group = 8, n_time = 25, rho = .7)
  pw <- fit_quietly(y ~ x, data = data, index = c("id", "time"),
                    tol = 1e-12, max_iter = 500)
  rho <- pw$rho[NROW(pw$rho), "rho"]
  expect_gt(abs(rho), 0.4)

  frame <- pw$model
  x <- stats::model.matrix(pw$terms, frame)
  y <- frame[, "y"]
  omega_inv <- matrix(0, nrow(x), nrow(x))
  for (i in unique(frame$id)) {
    pos <- which(frame$id == i)
    pos <- pos[order(frame$time[pos])]
    omega_inv[pos, pos] <- ar1_omega_inv(rho, length(pos))
  }

  xoi <- crossprod(x, omega_inv)
  beta <- solve(xoi %*% x, xoi %*% y)
  expect_equal(unname(pw$coefficients), unname(drop(beta)))

  u <- y - x %*% beta
  sigma_sq <- drop(crossprod(u, omega_inv) %*% u) / (nrow(x) - ncol(x))
  expect_equal(unname(summary(pw)$coefficients[, "Std. Error"]),
               unname(sqrt(diag(solve(xoi %*% x)) * sigma_sq)))
})

test_that("unbalanced panels reproduce the block diagonal GLS solution", {
  set.seed(7)
  lengths_panel <- c(25, 18, 30, 11)
  data <- do.call(rbind, lapply(seq_along(lengths_panel), function(i) {
    n <- lengths_panel[i]
    u <- rnorm(n, 0, 2)
    for (j in 2:n) u[j] <- u[j] + .6 * u[j - 1]
    data.frame(id = i, time = seq_len(n), x = rnorm(n, 10, 3), y = NA_real_)
  }))
  data$y <- 1 + 2 * data$x + rnorm(nrow(data))

  pw <- fit_quietly(y ~ x, data = data, index = c("id", "time"),
                    tol = 1e-12, max_iter = 500)
  rho <- pw$rho[NROW(pw$rho), "rho"]

  frame <- pw$model
  x <- stats::model.matrix(pw$terms, frame)
  y <- frame[, "y"]
  omega_inv <- matrix(0, nrow(x), nrow(x))
  for (i in unique(frame$id)) {
    pos <- which(frame$id == i)
    pos <- pos[order(frame$time[pos])]
    omega_inv[pos, pos] <- ar1_omega_inv(rho, length(pos))
  }
  xoi <- crossprod(x, omega_inv)
  expect_equal(unname(pw$coefficients),
               unname(drop(solve(xoi %*% x, xoi %*% y))))
})
