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

test_that("vcovHC agrees with the sandwich package on the transformed model", {
  skip_if_not_installed("sandwich")
  data <- ar1_sample(n = 120, rho = .6)
  pw <- fit_quietly(y ~ x + z, data = data, index = "time", tol = 1e-12, max_iter = 500)

  transformed <- pw_transformed_model(pw)
  reference <- stats::lm(transformed$y ~ transformed$x - 1)

  for (type in c("HC0", "HC1")) {
    expect_equal(unname(vcovHC(pw, type = type)),
                 unname(sandwich::vcovHC(reference, type = type)),
                 tolerance = 1e-8, info = type)
  }
})

test_that("vcovPC agrees with the pcse package", {
  skip_if_not_installed("pcse")
  data <- ar1_panel(n_group = 7, n_time = 20, rho = .5)
  pw <- fit_quietly(y ~ x, data = data, index = c("id", "time"),
                    tol = 1e-12, max_iter = 500)

  transformed <- pw_transformed_model(pw)
  reference <- pcse::pcse(stats::lm(transformed$y ~ transformed$x - 1),
                          groupN = transformed$frame$id,
                          groupT = transformed$frame$time)

  expect_equal(unname(sqrt(diag(vcovPC(pw, pairwise = FALSE)))),
               unname(reference$pcse), tolerance = 1e-6)
})

test_that("vcovPC is correct for panels that begin in different periods", {
  skip_if_not_installed("pcse")
  # The panels are ordered by period, so within a period the observations follow
  # the panel. If the panels do not all begin in the same period, that order is
  # not the order in which the panels first appear, which the covariances used to
  # be indexed by.
  periods <- list(1:20, 5:20, 3:20, 8:20)
  set.seed(77)
  data <- do.call(rbind, lapply(seq_along(periods), function(i)
    data.frame(id = i, time = periods[[i]])))
  data$x <- stats::rnorm(nrow(data), 5, 2)
  data$y <- 1 + 2 * data$x + stats::rnorm(nrow(data), 0, 2)

  pw <- suppressWarnings(fit_quietly(y ~ x, data = data, index = c("id", "time"),
                                     tol = 1e-12, max_iter = 500))
  # the panels appear in a different order than they are numbered
  expect_false(identical(as.character(unique(pw$model$id)),
                         as.character(sort(unique(pw$model$id)))))

  transformed <- pw_transformed_model(pw)
  model <- stats::lm(transformed$y ~ transformed$x - 1)
  for (pairwise in c(FALSE, TRUE)) {
    reference <- pcse::pcse(model, groupN = transformed$frame$id,
                            groupT = transformed$frame$time, pairwise = pairwise)
    expect_equal(unname(sqrt(diag(vcovPC(pw, pairwise = pairwise)))),
                 unname(reference$pcse), tolerance = 1e-6)
  }
})

test_that("vcov reproduces the standard errors of the summary", {
  data <- ar1_sample()
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")
  names_coef <- names(pw$coefficients)

  result <- vcov(pw)

  expect_identical(dim(result), c(3L, 3L))
  expect_identical(dimnames(result), list(names_coef, names_coef))
  expect_equal(sqrt(diag(result)), summary(pw)$coefficients[, "Std. Error"])
  expect_equal(result, t(result))
})

test_that("vcov equals the semirobust estimator of type 'const'", {
  data <- ar1_sample()
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")

  expect_equal(vcov(pw), vcovHC(pw, type = "const"))
})

test_that("vcov omits the coefficients of linearly dependent variables", {
  data <- ar1_sample(n = 60)
  data$x2 <- data$x
  pw <- fit_quietly(y ~ x + x2, data = data, index = "time")

  result <- vcov(pw)

  expect_identical(dimnames(result), list(c("(Intercept)", "x"),
                                          c("(Intercept)", "x")))
  expect_false(anyNA(result))
})

test_that("vcov works for panel data", {
  data <- ar1_panel()
  pw <- fit_quietly(y ~ x, data = data, index = c("id", "time"))

  expect_equal(sqrt(diag(vcov(pw))), summary(pw)$coefficients[, "Std. Error"])
})
