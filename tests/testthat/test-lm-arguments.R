# Arguments such as 'subset' and 'weights' are not evaluated in the usual way, so
# 'prais_winsten' is called directly instead of through a wrapper.

test_that("argument 'subset' is passed to lm", {
  data <- ar1_sample(n = 60)

  pw <- suppressMessages(
    prais_winsten(y ~ x, data = data, index = "time", subset = time <= 30))

  expect_identical(nrow(pw$model), 30L)
  expect_equal(pw$coefficients,
               fit_quietly(y ~ x, data = data[data$time <= 30, ], index = "time")$coefficients)
})

test_that("argument 'subset' may use variables of the calling environment", {
  data <- ar1_sample(n = 60)
  cutoff <- 40

  pw <- suppressMessages(
    prais_winsten(y ~ x, data = data, index = "time", subset = time <= cutoff))

  expect_identical(nrow(pw$model), 40L)
})

test_that("weighted least squares is rejected", {
  data <- ar1_sample(n = 40)
  data$weight <- 1

  expect_error(
    suppressMessages(
      prais_winsten(y ~ x, data = data, index = "time", weights = weight)),
    "does not support weighted least squares")
})
