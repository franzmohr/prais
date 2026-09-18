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
