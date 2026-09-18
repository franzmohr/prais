test_that("models with linearly dependent variables are estimated", {
  data <- ar1_sample(n = 60)
  data$x2 <- data$x
  data$ga <- as.numeric(data$g == "a")
  data$gb <- as.numeric(data$g == "b")
  data$gc <- as.numeric(data$g == "c")

  formulas <- list(y ~ x + x2, y ~ x + I(2 * x), y ~ x2 + x, y ~ z + x2 + x,
                   y ~ ga + gb + gc, y ~ x + x2 + I(3 * x) + z, y ~ g + ga)

  for (formula in formulas) {
    pw <- fit_quietly(formula, data = data, index = "time")
    aliased <- is.na(stats::coef(stats::lm(formula, data = data)))

    # The same coefficients are aliased as in 'lm'
    expect_identical(unname(is.na(pw$coefficients)), unname(aliased),
                     info = deparse(formula))
    expect_identical(pw$rank, sum(!aliased))
    expect_false(anyNA(pw$fitted.values), info = deparse(formula))
    expect_false(anyNA(pw$residuals), info = deparse(formula))
  }
})

test_that("a variable that is collinear with the intercept is aliased", {
  data <- ar1_sample(n = 60)
  data$constant <- 1

  pw <- fit_quietly(y ~ x + constant, data = data, index = "time")

  expect_true(is.na(pw$coefficients["constant"]))
  expect_false(anyNA(pw$fitted.values))
})

test_that("a rank deficient model equals its full rank counterpart", {
  data <- ar1_sample(n = 60)
  data$x2 <- data$x

  deficient <- fit_quietly(y ~ x + x2, data = data, index = "time")
  full <- fit_quietly(y ~ x, data = data, index = "time")

  expect_equal(deficient$coefficients[c("(Intercept)", "x")], full$coefficients)
  expect_equal(deficient$rho, full$rho)
  expect_equal(deficient$fitted.values, full$fitted.values)

  deficient_summary <- summary(deficient)
  full_summary <- summary(full)

  # Aliased coefficients are omitted from the coefficient table, as in 'summary.lm'
  expect_identical(nrow(deficient_summary$coefficients), 2L)
  expect_equal(deficient_summary$coefficients, full_summary$coefficients)
  expect_equal(deficient_summary$sigma, full_summary$sigma)
  expect_equal(deficient_summary$fstatistic, full_summary$fstatistic)
  expect_identical(dim(deficient_summary$cov.unscaled), c(2L, 2L))
  expect_identical(deficient_summary$df, c(2L, 58L, 3L))

  for (type in c("const", "HC0", "HC1")) {
    expect_equal(vcovHC(deficient, type = type), vcovHC(full, type = type))
  }
  expect_equal(unname(predict(deficient, data.frame(x = c(1, 2), x2 = c(1, 2)))),
               unname(predict(full, data.frame(x = c(1, 2)))))
})

test_that("singularities are reported by the print method", {
  data <- ar1_sample(n = 60)
  data$x2 <- data$x

  deficient <- capture.output(print(summary(fit_quietly(y ~ x + x2, data = data,
                                                        index = "time"))))
  full <- capture.output(print(summary(fit_quietly(y ~ x, data = data,
                                                   index = "time"))))

  expect_true(any(grepl("1 not defined because of singularities", deficient)))
  expect_true(any(full == "Coefficients:"))
})

test_that("panel covariance matrices omit aliased coefficients", {
  data <- ar1_panel(n_group = 5, n_time = 14)
  data$x2 <- data$x

  deficient <- fit_quietly(y ~ x + x2, data = data, index = c("id", "time"))
  full <- fit_quietly(y ~ x, data = data, index = c("id", "time"))

  expect_identical(dim(vcovPC(deficient)), c(2L, 2L))
  expect_equal(vcovPC(deficient), vcovPC(full))
})
