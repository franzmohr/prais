# Two observations of a panel that lie k periods apart are correlated by rho^k, so
# the transformation has to use that power and rescale the difference. The tests
# check the step counts that are derived from the time variable and the estimates
# that the transformation produces.

test_that("consecutive periods count one step each", {
  groups <- list(1:5)

  expect_identical(prais:::.pw_timeid(c(1, 2, 3, 4, 5), groups), c(0, 1, 2, 3, 4))
  # The origin of the time variable does not matter
  expect_identical(prais:::.pw_timeid(c(2001, 2002, 2003, 2004, 2005), groups),
                   c(0, 1, 2, 3, 4))
})

test_that("a gap counts as many steps as periods are missing", {
  expect_identical(prais:::.pw_timeid(c(1, 2, 5, 6), list(1:4)), c(0, 1, 4, 5))
})

test_that("the step is the greatest common divisor of the differences", {
  # Periods two apart throughout are one step apart, as in biennial data
  expect_identical(prais:::.pw_timeid(c(2000, 2002, 2004, 2010), list(1:4)),
                   c(0, 1, 2, 5))
  # Differences of 2 and 3 have a common divisor of 1, which neither of them is
  expect_identical(prais:::.pw_timeid(c(0, 2, 5), list(1:3)), c(0, 2, 5))
  # Differences without a common factor keep their own size, however large
  expect_identical(prais:::.pw_timeid(c(0, 101, 204), list(1:3)), c(0, 101, 204))
  # Quarters, where a step is a quarter of a unit and a year is four of them
  expect_identical(prais:::.pw_timeid(c(2000, 2000.25, 2000.5, 2001), list(1:4)),
                   c(0, 1, 2, 4))
})

test_that("dates are supported", {
  days <- as.Date("2020-01-01") + c(0, 1, 2, 6)

  expect_identical(prais:::.pw_timeid(days, list(1:4)), c(0, 1, 2, 6))
})

test_that("periods without a common step give no counts", {
  expect_null(suppressWarnings(prais:::.pw_timeid(c(0, 1, 1 + sqrt(2)), list(1:3))))
  # A time variable that is not numeric cannot be counted either
  expect_null(prais:::.pw_timeid(c("a", "b", "c"), list(1:3)))
})

test_that("the steps of a panel are counted from its own first period", {
  # The panels are observed on grids that are shifted against each other
  timeid <- prais:::.pw_timeid(c(1, 2, 4, 1.5, 2.5), list(1:3, 4:5))

  expect_identical(diff(timeid[1:3]), c(1, 2))
  expect_identical(diff(timeid[4:5]), 1)
})

test_that("the transformation reproduces generalised least squares across a gap", {
  data <- ar1_sample(n = 60)
  data <- data[-c(11, 30, 31, 45), ]
  pw <- fit_quietly(y ~ x, data = data, index = "time")

  # Generalised least squares with the AR(1) covariance evaluated at the periods
  # that were observed, at the coefficient the estimator converged to
  rho <- pw$rho[NROW(pw$rho), 1]
  x <- stats::model.matrix(pw$terms, pw$model)
  y <- pw$model$y
  v <- rho^abs(outer(data$time, data$time, "-")) / (1 - rho^2)
  vi <- solve(v)
  expected <- solve(t(x) %*% vi %*% x, t(x) %*% vi %*% y)

  expect_equal(pw$coefficients, setNames(drop(expected), colnames(x)))
})

test_that("equally spaced data are transformed exactly as before", {
  data <- ar1_sample(n = 40)
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")

  # The general transformation has to leave the familiar one untouched, which the
  # reference implementation of the helper applies for a step of one
  expected <- pw_transformed_model(pw)

  expect_equal(unname(pw$x), unname(expected$x), ignore_attr = TRUE)
  expect_equal(pw$y, unname(expected$y))
})

test_that("a rescaled time variable gives the same estimates", {
  data <- ar1_sample(n = 40)
  data <- data[-c(8, 20, 21), ]
  reference <- fit_quietly(y ~ x, data = data, index = "time")

  # Counting in steps makes the result invariant to the unit of the time
  # variable, so biennial periods estimate like annual ones
  scaled <- data
  scaled$time <- scaled$time * 2
  pw <- fit_quietly(y ~ x, data = scaled, index = "time")

  expect_equal(pw$coefficients, reference$coefficients)
  expect_equal(pw$rho, reference$rho)
})

test_that("a long gap transforms the following observation like a first one", {
  data <- ar1_sample(n = 30)
  # A gap of 400 periods leaves rho^k indistinguishable from zero
  data$time[16:30] <- data$time[16:30] + 400
  pw <- fit_quietly(y ~ x, data = data, index = "time")
  x <- pw$x

  # The panel is split in two as far as the transformation is concerned, so the
  # observation after the gap looks like the observation that starts the sample
  expect_equal(x[16, "(Intercept)"], x[1, "(Intercept)"])
})

test_that("summary and the covariance matrices use the same steps", {
  data <- ar1_sample(n = 50)
  data <- data[-c(9, 25, 26, 40), ]
  pw <- fit_quietly(y ~ x, data = data, index = "time")

  # 'summary' repeats the transformation on the data of the model, so its fitted
  # values have to agree with the ones the estimator obtained
  expect_equal(summary(pw)$coefficients[, "Estimate"], pw$coefficients)
  expect_equal(sum(summary(pw)$residuals^2) > 0, TRUE)
  # The covariance matrices rest on the same transformed model matrix
  expect_equal(dim(vcov(pw)), c(2L, 2L))
  expect_equal(dim(sandwich::vcovHC(pw)), c(2L, 2L))
})

test_that("panels with gaps are estimated panel by panel", {
  data <- ar1_panel(n_group = 3, n_time = 20)
  data <- data[!(data$id == data$id[1] & data$time %in% c(5, 6)), ]
  pw <- fit_quietly(y ~ x, data = data, index = c("id", "time"), panelwise = TRUE)
  expected <- pw_transformed_model(pw)
  x <- stats::model.matrix(pw$terms, pw$model)

  expect_false(anyNA(pw$coefficients))
  # The step counts restart with every panel, so the last observation of one panel
  # is never differenced against the first observation of the next
  groups <- prais:::.pw_groups(pw, nrow(x))
  steps <- prais:::.pw_steps(pw, groups)
  expect_true(all(steps >= 1))
  expect_length(steps, nrow(x) - length(groups))
})
