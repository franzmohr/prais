test_that("the summary output has the documented structure", {
  data <- ar1_sample()
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")
  result <- summary(pw)

  expect_s3_class(result, "summary.prais")
  expect_true(all(c("call", "residuals", "coefficients", "rho", "sigma", "df",
                    "r.squared", "adj.r.squared", "fstatistic", "cov.unscaled",
                    "dw") %in% names(result)))
  expect_identical(colnames(result$coefficients),
                   c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
  expect_identical(rownames(result$coefficients), c("(Intercept)", "x", "z"))
  expect_identical(result$df, c(3L, nrow(data) - 3L, 3L))
  expect_identical(names(result$dw), c("original", "transformed"))
})

test_that("the t values and p values are consistent with the estimates", {
  data <- ar1_sample()
  coefs <- summary(fit_quietly(y ~ x + z, data = data, index = "time"))$coefficients

  expect_equal(coefs[, "t value"], coefs[, "Estimate"] / coefs[, "Std. Error"])
  expect_equal(coefs[, "Pr(>|t|)"],
               2 * stats::pt(abs(coefs[, "t value"]), nrow(data) - 3,
                             lower.tail = FALSE))
})

test_that("the Prais-Winsten transformation removes the serial correlation", {
  data <- ar1_sample(n = 200, rho = .7)
  result <- summary(fit_quietly(y ~ x, data = data, index = "time"))

  # The Durbin-Watson statistic of the transformed model is closer to 2
  expect_lt(abs(result$dw["transformed"] - 2), abs(result$dw["original"] - 2))
})

test_that("no F statistic is calculated if only an intercept is estimated", {
  data <- ar1_sample()
  pw <- fit_quietly(y ~ 1, data = data, index = "time")
  result <- summary(pw)

  expect_null(result$fstatistic)
  expect_identical(pw$rank, 1L)
  expect_false(any(grepl("F-statistic", capture.output(print(result)))))
})

test_that("no Durbin-Watson statistic is calculated for panel specific rho", {
  data <- ar1_panel()
  result <- summary(fit_quietly(y ~ x, data = data, index = c("id", "time"),
                                panelwise = TRUE))

  expect_null(result$dw)
  expect_false(any(grepl("Durbin-Watson", capture.output(print(result)))))
})

test_that("the print methods return their argument invisibly", {
  data <- ar1_sample(n = 40)
  pw <- fit_quietly(y ~ x, data = data, index = "time")

  expect_output(expect_identical(print(pw), pw))
  expect_output(expect_identical(print(summary(pw)), summary(pw)))
})
