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
