test_that("vcovHC with type 'const' reproduces the standard errors of the summary", {
  data <- ar1_sample()
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")

  expect_equal(sqrt(diag(vcovHC(pw, type = "const"))),
               summary(pw)$coefficients[, "Std. Error"])
})

test_that("vcovHC returns a named covariance matrix for every type", {
  data <- ar1_sample()
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")
  names_coef <- names(pw$coefficients)

  for (type in c("const", "HC0", "HC1")) {
    result <- vcovHC(pw, type = type)
    expect_identical(dim(result), c(3L, 3L))
    expect_identical(dimnames(result), list(names_coef, names_coef))
    expect_true(all(diag(result) > 0))
    # A covariance matrix is symmetric
    expect_equal(result, t(result))
  }
})

test_that("the HC1 estimator scales the HC0 estimator", {
  data <- ar1_sample()
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")
  n <- nrow(data)

  expect_equal(vcovHC(pw, type = "HC1"),
               vcovHC(pw, type = "HC0") * n / pw$df.residual)
})

test_that("unknown types are rejected", {
  data <- ar1_sample(n = 40)
  pw <- fit_quietly(y ~ x, data = data, index = "time")

  expect_error(vcovHC(pw, type = "HC3"))
})

test_that("vcovPC returns a symmetric covariance matrix", {
  data <- ar1_panel()
  pw <- fit_quietly(y ~ x, data = data, index = c("id", "time"))
  names_coef <- names(pw$coefficients)

  for (pairwise in c(FALSE, TRUE)) {
    result <- vcovPC(pw, pairwise = pairwise)
    expect_identical(dim(result), c(2L, 2L))
    expect_identical(dimnames(result), list(names_coef, names_coef))
    expect_true(all(diag(result) > 0))
    expect_equal(result, t(result))
  }
})

test_that("panel corrected standard errors differ from the summary", {
  data <- ar1_panel()
  pw <- fit_quietly(y ~ x, data = data, index = c("id", "time"))

  expect_false(isTRUE(all.equal(unname(sqrt(diag(vcovPC(pw)))),
                                unname(summary(pw)$coefficients[, "Std. Error"]))))
})

test_that("vcovPC requires panel data", {
  data <- ar1_sample(n = 40)

  expect_error(vcovPC(fit_quietly(y ~ x, data = data, index = "time")),
               "require panel data")
  expect_error(vcovPC(fit_quietly(y ~ x, data = data, index = NULL)),
               "require panel data")
})

test_that("vcovPC reports panels without a common period", {
  # The two panels are observed in periods that do not overlap
  data <- rbind(data.frame(id = 1, time = 1:10), data.frame(id = 2, time = 21:30))
  data$x <- stats::rnorm(nrow(data))
  data$y <- 1 + 2 * data$x + stats::rnorm(nrow(data))
  pw <- fit_quietly(y ~ x, data = data, index = c("id", "time"))

  # Only the periods that are common to all panels would be used, of which there
  # are none. That produced a covariance matrix of NaN before.
  expect_error(vcovPC(pw, pairwise = FALSE), "do not have a period in common")

  # Matching the panels by period is still possible
  result <- vcovPC(pw, pairwise = TRUE)
  expect_false(anyNA(result))
  expect_true(all(is.finite(result)))
})

test_that("vcovPC works for unbalanced panels", {
  data <- rbind(data.frame(id = 1, time = 1:20), data.frame(id = 2, time = 5:20),
                data.frame(id = 3, time = 1:12), data.frame(id = 4, time = 8:20))
  data$x <- stats::rnorm(nrow(data))
  data$y <- 1 + 2 * data$x + stats::rnorm(nrow(data))
  pw <- fit_quietly(y ~ x, data = data, index = c("id", "time"))

  common <- vcovPC(pw, pairwise = FALSE)
  matched <- vcovPC(pw, pairwise = TRUE)

  for (result in list(common, matched)) {
    expect_false(anyNA(result))
    expect_equal(result, t(result))
    expect_true(all(eigen(result)$values > 0))
  }
  # Using all matched observations is not the same as using the common periods
  expect_false(isTRUE(all.equal(common, matched)))
})
