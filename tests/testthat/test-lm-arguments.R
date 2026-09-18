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

test_that("arguments are passed on if they are forwarded through the dots of a wrapper", {
  data <- ar1_sample(n = 50)
  wrapper <- function(...) suppressMessages(prais_winsten(...))

  expect_error(wrapper(y ~ x, data = data, index = "time"), NA)
  expect_error(wrapper(y ~ x + g, data = data, index = "time",
                       contrasts = list(g = "contr.sum")), NA)
  expect_error(wrapper(y ~ x, data = data, index = "time", method = "qr"), NA)
  expect_error(wrapper(y ~ x, data = data, index = "time",
                       offset = rep(0, nrow(data))), NA)

  # The estimates must not depend on how the function was called
  expect_equal(wrapper(y ~ x, data = data, index = "time")$coefficients,
               fit_quietly(y ~ x, data = data, index = "time")$coefficients)
})

test_that("the model frame is kept even if 'model' is FALSE", {
  data <- ar1_sample(n = 50)

  pw <- suppressMessages(
    prais_winsten(y ~ x, data = data, index = "time", model = FALSE))

  expect_identical(nrow(pw$model), 50L)
  expect_equal(pw$coefficients, fit_quietly(y ~ x, data = data, index = "time")$coefficients)
})

test_that("contrasts of the estimation are used by predict", {
  data <- ar1_sample(n = 80)
  pw <- fit_quietly(y ~ x + g, data = data, index = "time",
                    contrasts = list(g = "contr.sum"))
  newdata <- data.frame(x = c(25, 35), g = factor(c("a", "b"), levels = levels(data$g)))

  reference <- stats::lm(y ~ x + g, data = data, contrasts = list(g = "contr.sum"))
  reference$coefficients <- pw$coefficients

  expect_equal(unname(predict(pw, newdata = newdata)),
               unname(stats::predict(reference, newdata = newdata)))
})

test_that("a subset that reorders the observations keeps the order of the index", {
  data <- ar1_sample(n = 40)
  set.seed(3)
  permutation <- sample(nrow(data))

  # 'lm' returns the observations in the order of 'subset', which would undo the
  # ordering by 'index' and make the transformation use the wrong lags
  permuted <- suppressMessages(
    prais_winsten(y ~ x, data = data, index = "time", subset = permutation))
  reference <- fit_quietly(y ~ x, data = data, index = "time")

  expect_false(is.unsorted(permuted$model$time))
  # 'lm' sums in the order of the observations it was given, so the results agree
  # up to the last bits rather than exactly
  expect_equal(permuted$coefficients, reference$coefficients)
  expect_equal(permuted$residuals, reference$residuals)
  expect_equal(permuted$rho, reference$rho)
})

test_that("the order of the rows of the data does not affect the estimates", {
  data <- ar1_sample(n = 40)
  reference <- fit_quietly(y ~ x, data = data, index = "time")

  set.seed(4)
  for (rows in list(rev(seq_len(nrow(data))), sample(nrow(data)))) {
    pw <- fit_quietly(y ~ x, data = data[rows, ], index = "time")
    expect_equal(pw$coefficients, reference$coefficients)
    expect_equal(pw$rho, reference$rho)
  }
})

test_that("a reordering subset keeps the panels intact", {
  panel <- ar1_panel(n_group = 4, n_time = 12)
  set.seed(5)
  permutation <- sample(nrow(panel))

  permuted <- suppressMessages(
    prais_winsten(y ~ x, data = panel, index = c("id", "time"), subset = permutation))
  reference <- fit_quietly(y ~ x, data = panel, index = c("id", "time"))

  expect_identical(permuted$model$id, reference$model$id)
  expect_identical(permuted$model$time, reference$model$time)
  expect_equal(permuted$coefficients, reference$coefficients)
})
