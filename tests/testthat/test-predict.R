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

test_that("the standard errors of the predictions are those of the mean", {
  data <- ar1_sample(n = 60)
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")
  newdata <- data.frame(x = c(25, 30, 35), z = c(-1, 0, 1))

  prediction <- predict(pw, newdata = newdata, se.fit = TRUE)

  expect_identical(names(prediction), c("fit", "se.fit", "df", "residual.scale"))
  expect_identical(prediction$fit, predict(pw, newdata = newdata))
  expect_identical(prediction$df, pw$df.residual)
  expect_equal(prediction$residual.scale, summary(pw)$sigma)

  # The standard error of the predicted mean is the square root of x' V x, which
  # is obtained here without the shortcuts of the implementation
  x <- cbind(1, newdata$x, newdata$z)
  expected <- sqrt(diag(x %*% stats::vcov(pw) %*% t(x)))
  expect_equal(unname(prediction$se.fit), expected)
  expect_identical(names(prediction$se.fit), row.names(newdata))
})

test_that("the confidence interval is based on the t distribution", {
  data <- ar1_sample(n = 60)
  pw <- fit_quietly(y ~ x, data = data, index = "time")
  newdata <- data.frame(x = c(25, 30, 35))

  prediction <- predict(pw, newdata = newdata, interval = "confidence")

  expect_true(is.matrix(prediction))
  expect_identical(dimnames(prediction), list(row.names(newdata),
                                              c("fit", "lwr", "upr")))
  expect_equal(prediction[, "fit"], predict(pw, newdata = newdata))

  se <- predict(pw, newdata = newdata, se.fit = TRUE)$se.fit
  q <- stats::qt(.975, pw$df.residual)
  expect_equal(unname(prediction[, "lwr"]), unname(prediction[, "fit"] - q * se))
  expect_equal(unname(prediction[, "upr"]), unname(prediction[, "fit"] + q * se))

  # A lower level gives a narrower interval
  narrow <- predict(pw, newdata = newdata, interval = "confidence", level = .9)
  expect_true(all(narrow[, "lwr"] > prediction[, "lwr"]))
  expect_true(all(narrow[, "upr"] < prediction[, "upr"]))

  expect_error(predict(pw, newdata = newdata, interval = "confidence", level = 0),
               "must be between 0 and 1")
  expect_error(predict(pw, newdata = newdata, interval = "confidence", level = 1),
               "must be between 0 and 1")
})

test_that("standard errors and interval can be requested together", {
  data <- ar1_sample(n = 60)
  pw <- fit_quietly(y ~ x, data = data, index = "time")
  newdata <- data.frame(x = c(25, 35))

  prediction <- predict(pw, newdata = newdata, se.fit = TRUE,
                        interval = "confidence")

  expect_identical(prediction$fit,
                   predict(pw, newdata = newdata, interval = "confidence"))
  expect_identical(prediction$se.fit,
                   predict(pw, newdata = newdata, se.fit = TRUE)$se.fit)
})

test_that("standard errors are available without 'newdata'", {
  data <- ar1_sample(n = 60)
  pw <- fit_quietly(y ~ x + g, data = data, index = "time")

  prediction <- predict(pw, se.fit = TRUE, interval = "confidence")

  expect_identical(prediction$fit[, "fit"], pw$fitted.values)
  expect_length(prediction$se.fit, length(pw$fitted.values))
  # The model matrix of the estimation sample gives the same standard errors as
  # the one that is built from 'newdata'
  expect_equal(unname(prediction$se.fit),
               unname(predict(pw, newdata = data, se.fit = TRUE)$se.fit))
})

test_that("the standard errors equal those of the transformed model", {
  data <- ar1_sample(n = 60)
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")
  newdata <- data.frame(x = c(25, 30), z = c(0, 1))

  # 'lm' on the transformed data is the model that the estimator fits, so its
  # predictions have the same standard errors. The regressors of 'newdata' are not
  # transformed, because the prediction is made for the original model.
  transformed <- pw_transformed_model(pw)
  reference <- stats::lm(transformed$y ~ 0 + transformed$x)
  x <- cbind(1, newdata$x, newdata$z)
  expected <- sqrt(diag(x %*% stats::vcov(reference) %*% t(x)))

  expect_equal(unname(predict(pw, newdata = newdata, se.fit = TRUE)$se.fit),
               expected)
})

test_that("missing values in 'newdata' give missing standard errors", {
  data <- ar1_sample(n = 60)
  pw <- fit_quietly(y ~ x, data = data, index = "time")
  newdata <- data.frame(x = c(1, NA, 3))

  prediction <- predict(pw, newdata = newdata, se.fit = TRUE,
                        interval = "confidence")

  expect_true(is.na(prediction$se.fit[2]))
  expect_false(anyNA(prediction$se.fit[c(1, 3)]))
  expect_identical(unname(is.na(prediction$fit[, "lwr"])), c(FALSE, TRUE, FALSE))
})

test_that("aliased coefficients are omitted from the standard errors", {
  data <- ar1_sample(n = 60)
  data$x2 <- data$x

  deficient <- fit_quietly(y ~ x + x2, data = data, index = "time")
  full <- fit_quietly(y ~ x, data = data, index = "time")

  expect_equal(predict(deficient, data.frame(x = c(1, 2), x2 = c(1, 2)),
                       se.fit = TRUE, interval = "confidence"),
               predict(full, data.frame(x = c(1, 2)), se.fit = TRUE,
                       interval = "confidence"))
})

test_that("standard errors can be obtained for panel models", {
  data <- ar1_panel(n_group = 5, n_time = 14)
  pw <- fit_quietly(y ~ x + g, data = data, index = c("id", "time"))
  newdata <- data.frame(x = c(25, 30, 35),
                        g = factor(c("a", "b", "a"), levels = levels(data$g)))

  prediction <- predict(pw, newdata = newdata, se.fit = TRUE,
                        interval = "confidence")

  expect_length(prediction$se.fit, 3L)
  expect_true(all(prediction$se.fit > 0))
  expect_identical(dim(prediction$fit), c(3L, 3L))
})

test_that("an unknown type of interval is rejected", {
  data <- ar1_sample(n = 60)
  pw <- fit_quietly(y ~ x, data = data, index = "time")

  expect_error(predict(pw, interval = "prediction"))
})
