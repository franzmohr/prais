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
  # The reported residuals belong to the transformed model
  expect_true(any(grepl("Residuals of the transformed model",
                        capture.output(print(result)))))
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

test_that("models without an intercept are summarised", {
  data <- ar1_sample()
  pw <- fit_quietly(y ~ 0 + x, data = data, index = "time")
  result <- summary(pw)

  expect_identical(rownames(result$coefficients), "x")
  expect_identical(result$df, c(1L, nrow(data) - 1L, 1L))
  # Without an intercept the total sum of squares is not centred
  expect_true(result$r.squared > 0 && result$r.squared <= 1)
  expect_identical(result$fstatistic[["numdf"]], 1)
  expect_output(print(result), "Residuals of the transformed model")
})

test_that("panel specific estimates of rho are printed", {
  data <- ar1_panel(n_group = 3, n_time = 12)
  pw <- fit_quietly(y ~ x, data = data, index = c("id", "time"), panelwise = TRUE)

  printed <- capture.output(print(pw))
  expect_true(any(grepl("AR(1) coefficients", printed, fixed = TRUE)))
  expect_true(any(grepl("Group", printed, fixed = TRUE)))
  # Every panel appears in the table
  for (group in as.character(unique(data$id))) {
    expect_true(any(grepl(group, printed)), info = group)
  }

  printed <- capture.output(print(summary(pw)))
  expect_true(any(grepl("AR(1) coefficients after", printed, fixed = TRUE)))
  expect_true(any(grepl("Group", printed, fixed = TRUE)))
})

test_that("the reported statistics agree with their definition", {
  data <- ar1_sample(n = 150, rho = .6)
  pw <- fit_quietly(y ~ x + z, data = data, index = "time",
                    tol = 1e-12, max_iter = 500)
  result <- summary(pw)
  rho <- pw$rho[NROW(pw$rho), "rho"]

  # The transformed model, built independently of the package
  x <- stats::model.matrix(pw$terms, pw$model)
  y <- pw$model[, "y"]
  n <- nrow(x)
  x_pw <- pw_transform_series(x, rho)
  y_pw <- c(sqrt(1 - rho^2) * y[1], y[-1] - rho * y[-n])
  residuals_pw <- c(y_pw - x_pw %*% pw$coefficients)
  rdf <- n - length(pw$coefficients)

  expect_equal(unname(result$residuals), unname(residuals_pw))
  expect_equal(result$sigma, sqrt(sum(residuals_pw^2) / rdf))

  rss <- sum(residuals_pw^2)
  sst <- sum((y_pw - mean(y_pw))^2)
  expect_equal(result$r.squared, (sst - rss) / sst)
  expect_equal(result$adj.r.squared, 1 - ((n - 1) / rdf) * (1 - (sst - rss) / sst))
  expect_equal(unname(result$fstatistic["value"]),
               unname(((sst - rss) / 2) / (rss / rdf)))
  expect_identical(unname(result$fstatistic[c("numdf", "dendf")]), c(2, rdf))
})

test_that("the Durbin-Watson statistics agree with their definition", {
  data <- ar1_sample(n = 150, rho = .6)
  pw <- fit_quietly(y ~ x + z, data = data, index = "time",
                    tol = 1e-12, max_iter = 500)
  result <- summary(pw)
  rho <- pw$rho[NROW(pw$rho), "rho"]

  x <- stats::model.matrix(pw$terms, pw$model)
  y <- pw$model[, "y"]
  n <- nrow(x)

  # The statistic of the original model uses the residuals of ordinary least squares
  residuals_ols <- stats::lm.fit(x = x, y = y)$residuals
  expect_equal(unname(result$dw["original"]),
               sum(diff(residuals_ols)^2) / sum(residuals_ols^2))

  x_pw <- pw_transform_series(x, rho)
  y_pw <- c(sqrt(1 - rho^2) * y[1], y[-1] - rho * y[-n])
  residuals_pw <- c(y_pw - x_pw %*% pw$coefficients)
  expect_equal(unname(result$dw["transformed"]),
               sum(diff(residuals_pw)^2) / sum(residuals_pw^2))
})

test_that("the unscaled covariance is the inverse cross product of the transformed model matrix", {
  data <- ar1_sample(n = 120, rho = .5)
  pw <- fit_quietly(y ~ x + z, data = data, index = "time",
                    tol = 1e-12, max_iter = 500)
  result <- summary(pw)
  rho <- pw$rho[NROW(pw$rho), "rho"]

  x_pw <- pw_transform_series(stats::model.matrix(pw$terms, pw$model), rho)
  expect_equal(unname(result$cov.unscaled), unname(solve(crossprod(x_pw))))
  # and the standard errors follow from it
  expect_equal(unname(result$coefficients[, "Std. Error"]),
               unname(sqrt(diag(solve(crossprod(x_pw))) * result$sigma^2)))
})
