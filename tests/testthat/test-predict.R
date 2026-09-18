test_that("the fitted values are returned if 'newdata' is omitted", {
  data <- ar1_sample(n = 60)
  pw <- fit_quietly(y ~ x, data = data, index = "time")

  expect_identical(predict(pw), pw$fitted.values)
})

test_that("'newdata' is used if it is not passed by name", {
  data <- ar1_sample(n = 60)
  pw <- fit_quietly(y ~ x, data = data, index = "time")
  newdata <- data.frame(x = c(0, 1, 2))

  expect_length(predict(pw, newdata), 3L)
  expect_identical(predict(pw, newdata), predict(pw, newdata = newdata))
  expect_equal(unname(predict(pw, newdata)),
               unname(pw$coefficients[1] + pw$coefficients[2] * newdata$x))
})

test_that("the model matrix of 'newdata' is built from the model terms", {
  data <- ar1_sample(n = 80)
  newdata <- data.frame(x = c(25, 30, 35), z = c(-1, 0, 1),
                        g = factor(c("a", "c", "b"), levels = levels(data$g)))

  formulas <- list(y ~ log(x), y ~ x + g, y ~ log(x) * g, y ~ poly(x, 2),
                   y ~ x + I(x^2), y ~ x:z, y ~ 0 + x + g)

  for (formula in formulas) {
    pw <- fit_quietly(formula, data = data, index = "time")

    # 'lm' with the coefficients of the Prais-Winsten estimator evaluates the
    # same linear predictor and serves as the reference
    reference <- stats::lm(formula, data = data)
    reference$coefficients <- pw$coefficients

    expect_equal(unname(predict(pw, newdata = newdata)),
                 unname(stats::predict(reference, newdata = newdata)),
                 info = deparse(formula))
  }
})

test_that("predictions for the estimation sample are the fitted values", {
  data <- ar1_sample(n = 80)
  pw <- fit_quietly(y ~ x + g, data = data, index = "time")

  expect_equal(unname(predict(pw, newdata = data)), unname(pw$fitted.values))
})

test_that("factor levels of the estimation are used", {
  data <- ar1_sample(n = 80)
  pw <- fit_quietly(y ~ x + g, data = data, index = "time")

  # A factor without the levels of the estimation sample
  expected <- unname(pw$coefficients["(Intercept)"] + pw$coefficients["x"] * 30 +
                       pw$coefficients["gb"])
  expect_equal(unname(predict(pw, data.frame(x = 30, g = factor("b")))), expected)
  # Characters are coerced in the same way as in 'lm'
  expect_equal(unname(predict(pw, data.frame(x = 30, g = "b"))), expected)
  # Unknown levels cannot be predicted. The message originates from 'model.frame'
  # and is translated, so it is not matched here.
  expect_error(predict(pw, data.frame(x = 30, g = factor("d"))))
})

test_that("invalid values of 'newdata' are rejected", {
  data <- ar1_sample(n = 60)
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")

  expect_error(predict(pw, newdata = as.matrix(data)), "not of class data.frame")
  expect_error(predict(pw, newdata = data.frame(x = 1)),
               "does not contain all variables")
})

test_that("missing values in 'newdata' give missing predictions", {
  data <- ar1_sample(n = 60)
  pw <- fit_quietly(y ~ x, data = data, index = "time")
  newdata <- data.frame(x = c(1, NA, 3))

  prediction <- predict(pw, newdata = newdata)

  expect_length(prediction, 3L)
  expect_true(is.na(prediction[2]))
  expect_false(anyNA(prediction[c(1, 3)]))
  expect_identical(names(prediction), row.names(newdata))
})

test_that("objects without element 'xlevels' can be used", {
  data <- ar1_sample(n = 80)
  pw <- fit_quietly(y ~ x + g, data = data, index = "time")
  newdata <- data.frame(x = c(25, 35), g = factor(c("a", "b"), levels = levels(data$g)))

  # Objects that were produced by versions prior to 1.2.0
  old <- pw
  old$xlevels <- NULL
  old$contrasts <- NULL

  expect_equal(predict(old, newdata = newdata), predict(pw, newdata = newdata))
})

test_that("predictions can be obtained for panel models", {
  data <- ar1_panel(n_group = 5, n_time = 14)
  pw <- fit_quietly(y ~ x + g, data = data, index = c("id", "time"))
  newdata <- data.frame(x = c(25, 30, 35),
                        g = factor(c("a", "b", "a"), levels = levels(data$g)))

  expect_length(predict(pw, newdata = newdata), 3L)
})
