# Package 'lmtest' is only suggested, so the tests that call it are skipped if it
# is not available. The components 'x' and 'y' themselves are checked without it.

test_that("the transformed model matrix and response are added for a time series", {
  data <- ar1_sample()
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")
  expected <- pw_transformed_model(pw)

  expect_true(is.matrix(pw$x))
  expect_true(is.vector(pw$y))
  expect_equal(unname(pw$x), unname(expected$x), ignore_attr = TRUE)
  expect_identical(colnames(pw$x), colnames(expected$x))
  expect_equal(pw$y, unname(expected$y))
})

test_that("the transformed data reproduce the coefficients of the estimator", {
  data <- ar1_sample()
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")

  expect_equal(stats::lm.fit(x = pw$x, y = pw$y)$coefficients, pw$coefficients)
})

test_that("the transformed data are not added for panel data", {
  data <- ar1_panel()
  pw <- fit_quietly(y ~ x, data = data, index = c("id", "time"))

  # 'x' would partially match 'xlevels', so the names are checked directly
  expect_false("x" %in% names(pw))
  expect_false("y" %in% names(pw))
})

test_that("observations that were dropped are not part of the transformed data", {
  data <- ar1_sample()
  data$x[5] <- NA
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")
  expected <- pw_transformed_model(pw)

  # The observation with the missing value is omitted from the model frame, so
  # the transformed data only cover the observations that were used
  expect_identical(nrow(pw$x), nrow(data) - 1L)
  expect_identical(length(pw$y), nrow(data) - 1L)
  expect_equal(unname(pw$x), unname(expected$x), ignore_attr = TRUE)
  expect_equal(pw$y, unname(expected$y))
})

test_that("dwtest reports the Durbin-Watson statistic of the transformed model", {
  skip_if_not_installed("lmtest")
  data <- ar1_sample()
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")
  dw <- summary(pw)$dw

  result <- lmtest::dwtest(pw)

  expect_equal(unname(result$statistic), unname(dw["transformed"]))
  # The statistic of the original model is what the test reported before the
  # transformed data were added
  expect_false(isTRUE(all.equal(unname(result$statistic), unname(dw["original"]))))
  expect_equal(unname(lmtest::dwtest(lm(y ~ x + z, data = data))$statistic),
               unname(dw["original"]))
})

test_that("bgtest and bptest use the transformed model", {
  skip_if_not_installed("lmtest")
  data <- ar1_sample()
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")
  ols <- lm(y ~ x + z, data = data)

  for (test in list(lmtest::bgtest, lmtest::bptest)) {
    expect_false(isTRUE(all.equal(unname(test(pw)$statistic),
                                  unname(test(ols)$statistic))))
  }
})
