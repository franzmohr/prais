# The methods are registered for the generics of package 'broom', which is only
# suggested. They are called directly, so that the tests do not depend on it.

test_that("tidy returns the coefficient table of the summary", {
  data <- ar1_sample()
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")
  coeffs <- summary(pw)$coefficients

  result <- tidy.prais(pw)

  expect_identical(names(result),
                   c("term", "estimate", "std.error", "statistic", "p.value"))
  expect_identical(nrow(result), 3L)
  expect_identical(result$term, c("(Intercept)", "x", "z"))
  expect_equal(result$estimate, unname(coeffs[, "Estimate"]))
  expect_equal(result$std.error, unname(coeffs[, "Std. Error"]))
  expect_equal(result$statistic, unname(coeffs[, "t value"]))
  expect_equal(result$p.value, unname(coeffs[, "Pr(>|t|)"]))
})

test_that("the confidence intervals of tidy are based on the t distribution", {
  data <- ar1_sample()
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")

  result <- tidy.prais(pw, conf.int = TRUE, conf.level = .9)
  q <- stats::qt(.95, pw$df.residual)

  expect_identical(names(result),
                   c("term", "estimate", "std.error", "statistic", "p.value",
                     "conf.low", "conf.high"))
  expect_equal(result$conf.low, result$estimate - q * result$std.error)
  expect_equal(result$conf.high, result$estimate + q * result$std.error)
  # A wider level produces wider intervals
  expect_true(all(tidy.prais(pw, conf.int = TRUE)$conf.low < result$conf.low))
})

test_that("tidy rejects confidence levels outside the unit interval", {
  data <- ar1_sample(n = 40)
  pw <- fit_quietly(y ~ x, data = data, index = "time")

  expect_error(tidy.prais(pw, conf.int = TRUE, conf.level = 1))
  expect_error(tidy.prais(pw, conf.int = TRUE, conf.level = 0))
})

test_that("tidy omits the coefficients of linearly dependent variables", {
  data <- ar1_sample(n = 60)
  data$x2 <- data$x
  pw <- fit_quietly(y ~ x + x2, data = data, index = "time")

  result <- tidy.prais(pw)

  expect_identical(result$term, c("(Intercept)", "x"))
  expect_false(anyNA(result$estimate))
})

test_that("glance returns the statistics of the summary", {
  data <- ar1_sample()
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")
  s <- summary(pw)

  result <- glance.prais(pw)

  expect_identical(names(result),
                   c("r.squared", "adj.r.squared", "sigma", "statistic",
                     "p.value", "df", "df.residual", "nobs", "rho",
                     "dw.original", "dw.transformed"))
  expect_identical(nrow(result), 1L)
  expect_equal(result$r.squared, s$r.squared)
  expect_equal(result$adj.r.squared, s$adj.r.squared)
  expect_equal(result$sigma, s$sigma)
  expect_equal(result$statistic, unname(s$fstatistic["value"]))
  expect_equal(result$p.value, unname(stats::pf(s$fstatistic["value"],
                                                s$fstatistic["numdf"],
                                                s$fstatistic["dendf"],
                                                lower.tail = FALSE)))
  expect_identical(result$df, unname(s$fstatistic["numdf"]))
  expect_identical(result$df.residual, pw$df.residual)
  expect_identical(result$nobs, nrow(data))
  expect_equal(result$rho, pw$rho[nrow(pw$rho), "rho"])
  expect_equal(result$dw.original, unname(s$dw["original"]))
  expect_equal(result$dw.transformed, unname(s$dw["transformed"]))
})

test_that("glance reports NA for the panelwise AR(1) coefficients", {
  data <- ar1_panel()
  pw <- fit_quietly(y ~ x, data = data, index = c("id", "time"),
                    panelwise = TRUE)

  result <- glance.prais(pw)

  # There is one coefficient of rho per panel and no single Durbin-Watson statistic
  expect_true(ncol(pw$rho) > 1)
  expect_identical(result$rho, NA_real_)
  expect_identical(result$dw.original, NA_real_)
  expect_identical(result$dw.transformed, NA_real_)
  expect_false(is.na(result$r.squared))
})

test_that("augment adds the fitted values and residuals of the object", {
  data <- ar1_sample()
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")

  result <- augment.prais(pw)

  expect_identical(nrow(result), nrow(data))
  expect_true(all(c("y", "x", "z", ".fitted", ".resid") %in% names(result)))
  # The values are on the scale of the original data
  expect_equal(result$.fitted, unname(pw$fitted.values))
  expect_equal(result$.resid, unname(pw$residuals))
  expect_equal(result$.fitted + result$.resid, result$y)
})

test_that("augment predicts for new data", {
  data <- ar1_sample()
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")
  newdata <- data.frame(x = c(25, 30), z = c(0, 1))

  result <- augment.prais(pw, newdata = newdata)

  expect_identical(nrow(result), 2L)
  expect_identical(names(result), c("x", "z", ".fitted"))
  expect_equal(result$.fitted, unname(predict(pw, newdata = newdata)))
})

test_that("augment rejects data that does not belong to the model", {
  data <- ar1_sample()
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")

  expect_error(augment.prais(pw, data = data[-1, ]), "was estimated from")
  expect_error(augment.prais(pw, data = "x"), "data.frame")
  expect_error(augment.prais(pw, newdata = "x"), "data.frame")
})

test_that("the methods work for panel data", {
  data <- ar1_panel()
  pw <- fit_quietly(y ~ x, data = data, index = c("id", "time"))

  expect_identical(tidy.prais(pw)$term, c("(Intercept)", "x"))
  expect_identical(glance.prais(pw)$nobs, nrow(data))
  # The model frame of a panel contains the variables of argument 'index'
  result <- augment.prais(pw)
  expect_true(all(c("id", "time", ".fitted", ".resid") %in% names(result)))
  expect_identical(nrow(result), nrow(data))
})

test_that("the results are tibbles if the package is installed", {
  skip_if_not_installed("tibble")
  data <- ar1_sample(n = 40)
  pw <- fit_quietly(y ~ x, data = data, index = "time")

  expect_s3_class(tidy.prais(pw), "tbl_df")
  expect_s3_class(glance.prais(pw), "tbl_df")
  expect_s3_class(augment.prais(pw), "tbl_df")
})
