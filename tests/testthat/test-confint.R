test_that("the confidence intervals are based on the t distribution", {
  data <- ar1_sample()
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")
  coeffs <- summary(pw)$coefficients

  result <- confint(pw)
  q <- stats::qt(.975, pw$df.residual)

  expect_identical(dim(result), c(3L, 2L))
  expect_identical(dimnames(result),
                   list(c("(Intercept)", "x", "z"), c("2.5 %", "97.5 %")))
  expect_equal(result[, 1], coeffs[, "Estimate"] - q * coeffs[, "Std. Error"])
  expect_equal(result[, 2], coeffs[, "Estimate"] + q * coeffs[, "Std. Error"])
})

test_that("the intervals are wider than those of the normal distribution", {
  data <- ar1_sample(n = 20)
  pw <- fit_quietly(y ~ x, data = data, index = "time")

  result <- confint(pw)
  # The default method uses the quantiles of the normal distribution
  normal <- stats::confint.default(pw)

  expect_true(all(result[, 1] < normal[, 1]))
  expect_true(all(result[, 2] > normal[, 2]))
})

test_that("a coefficient is covered by its interval", {
  data <- ar1_sample()
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")

  result <- confint(pw)

  expect_true(all(result[, 1] < stats::coef(pw)))
  expect_true(all(result[, 2] > stats::coef(pw)))
  # A higher level produces wider intervals
  expect_true(all(confint(pw, level = .99)[, 1] < result[, 1]))
  expect_true(all(confint(pw, level = .99)[, 2] > result[, 2]))
})

test_that("argument 'parm' selects coefficients by name and by position", {
  data <- ar1_sample()
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")
  result <- confint(pw)

  expect_equal(confint(pw, parm = "x"), result["x", , drop = FALSE])
  expect_equal(confint(pw, parm = 2), result["x", , drop = FALSE])
  expect_equal(confint(pw, parm = c("z", "x")), result[c("z", "x"), ])
  expect_error(confint(pw, parm = "w"), "does not only contain")
})

test_that("the level is checked and is reported in the column names", {
  data <- ar1_sample(n = 40)
  pw <- fit_quietly(y ~ x, data = data, index = "time")

  expect_identical(colnames(confint(pw, level = .9)), c("5 %", "95 %"))
  expect_error(confint(pw, level = 1), "between 0 and 1")
  expect_error(confint(pw, level = 0), "between 0 and 1")
})

test_that("the intervals omit the coefficients of linearly dependent variables", {
  data <- ar1_sample(n = 60)
  data$x2 <- data$x
  pw <- fit_quietly(y ~ x + x2, data = data, index = "time")

  result <- confint(pw)

  expect_identical(rownames(result), c("(Intercept)", "x"))
  expect_false(anyNA(result))
})

test_that("tidy reports the intervals of confint", {
  data <- ar1_sample()
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")

  result <- tidy.prais(pw, conf.int = TRUE, conf.level = .9)
  ci <- confint(pw, level = .9)

  expect_equal(result$conf.low, unname(ci[, 1]))
  expect_equal(result$conf.high, unname(ci[, 2]))
})

test_that("the intervals work for panel data", {
  data <- ar1_panel()
  pw <- fit_quietly(y ~ x, data = data, index = c("id", "time"))
  coeffs <- summary(pw)$coefficients

  result <- confint(pw)

  expect_equal(result[, 2] - result[, 1],
               2 * stats::qt(.975, pw$df.residual) * coeffs[, "Std. Error"])
})
