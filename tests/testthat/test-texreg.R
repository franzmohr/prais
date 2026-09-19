# Package 'texreg' enhances 'prais' and provides a method for its generic
# 'extract', which turns an estimated model into the regression tables that
# 'screenreg', 'texreg' and 'htmlreg' produce. The method lives in 'texreg' and
# reads the object of this package from the outside, so a change to the summary
# or to the estimated object can break it without breaking anything here. The
# first test states the components the method relies on and runs always. The
# tests that call 'texreg' itself are skipped where it is not installed, since
# it is only a suggested package.

test_that("the components that texreg::extract relies on are present", {
  data <- ar1_sample(n = 60)
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")
  s <- summary(pw)

  # 'extract' takes the last row of the matrix of the iterations as the AR(1)
  # coefficient and averages it, which requires a matrix rather than a vector
  expect_true(is.matrix(s$rho))
  expect_gte(nrow(s$rho), 1L)
  expect_true(is.numeric(s$rho[nrow(s$rho), ]))

  # The coefficients are read by position, the estimate from the first column,
  # the standard error from the second and the p value from the fourth
  expect_true(is.matrix(stats::coef(s)))
  expect_identical(ncol(stats::coef(s)), 4L)
  expect_identical(colnames(stats::coef(s))[c(1, 2, 4)],
                   c("Estimate", "Std. Error", "Pr(>|t|)"))

  # The goodness of fit measures and the number of observations, which is taken
  # from the length of the residuals rather than from the model frame
  expect_true(is.numeric(s$r.squared))
  expect_true(is.numeric(s$adj.r.squared))
  expect_length(pw$residuals, nrow(data))
})

test_that("texreg::extract reproduces the summary of a time series model", {
  skip_if_not_installed("texreg")

  data <- ar1_sample(n = 60)
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")
  s <- summary(pw)
  result <- texreg::extract(pw)

  expect_s4_class(result, "texreg")

  # The AR(1) coefficient is appended to the coefficients as a row of its own,
  # for which there is no standard error and no p value
  expect_identical(result@coef.names, c(rownames(stats::coef(s)), "rho"))
  expect_equal(unname(result@coef),
               unname(c(stats::coef(s)[, "Estimate"], pw$rho[nrow(pw$rho), 1])))
  expect_equal(unname(result@se),
               unname(c(stats::coef(s)[, "Std. Error"], NA_real_)))
  expect_equal(unname(result@pvalues),
               unname(c(stats::coef(s)[, "Pr(>|t|)"], NA_real_)))

  expect_identical(result@gof.names, c("R$^2$", "Adj. R$^2$", "Num. obs."))
  expect_equal(result@gof, c(s$r.squared, s$adj.r.squared, nrow(data)))
  expect_identical(result@gof.decimal, c(TRUE, TRUE, FALSE))
})

test_that("texreg::extract averages the panel specific estimates of rho", {
  skip_if_not_installed("texreg")

  data <- ar1_panel(n_group = 4, n_time = 15)
  pw <- fit_quietly(y ~ x, data = data, index = c("id", "time"),
                    panelwise = TRUE)
  result <- texreg::extract(pw)

  # With 'panelwise = TRUE' the last row of 'rho' holds one coefficient per
  # panel, which 'extract' reduces to their mean
  expect_identical(NCOL(pw$rho), 4L)
  expect_identical(result@coef.names, c("(Intercept)", "x", "rho"))
  expect_equal(unname(result@coef[length(result@coef)]),
               mean(pw$rho[nrow(pw$rho), ]))

  # A pooled model reports the single coefficient unchanged
  pooled <- fit_quietly(y ~ x, data = data, index = c("id", "time"))
  expect_equal(unname(texreg::extract(pooled)@coef[3]),
               unname(pooled$rho[nrow(pooled$rho), 1]))
})

test_that("texreg renders a table for a model of class prais", {
  skip_if_not_installed("texreg")

  data <- ar1_sample(n = 60)
  pw <- fit_quietly(y ~ x + z, data = data, index = "time")
  output <- utils::capture.output(texreg::screenreg(list(pw)))

  expect_true(any(grepl("rho", output, fixed = TRUE)))
  expect_true(any(grepl("Num. obs.", output, fixed = TRUE)))

  # The arguments of 'extract' drop a goodness of fit row. They are checked on
  # 'extract' rather than on the rendered table, because the name of the row
  # that stays, "Adj. R$^2$", contains the name of the row that goes.
  expect_identical(texreg::extract(pw, include.rsquared = FALSE)@gof.names,
                   c("Adj. R$^2$", "Num. obs."))
  expect_identical(texreg::extract(pw, include.nobs = FALSE)@gof.names,
                   c("R$^2$", "Adj. R$^2$"))
})
