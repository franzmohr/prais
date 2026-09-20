# The bundled dataset is the one from the textbook, so it is the natural place
# to check the estimates against something that was not written for this
# package. The other tests build the covariance matrix of the AR(1) process
# themselves, which anchors the estimator to the same understanding of the
# transformation that it implements.

test_that("the estimates agree with an independent GLS implementation", {
  skip_if_not_installed("nlme")

  data("barium")
  f <- lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6
  pw <- fit_quietly(f, data = barium, index = "t", tol = 1e-12, max_iter = 500)
  rho <- unname(pw$rho[NROW(pw$rho), 1])

  # 'nlme::gls' solves the same generalised least squares problem and was
  # written by other authors. Holding rho at the estimate of 'prais_winsten'
  # leaves both with the same system to solve, so the coefficients have to
  # agree. What this adds over the tests that build the covariance matrix in
  # the helper is that a misunderstanding of the transformation cannot be
  # shared by both implementations.
  gls_fit <- nlme::gls(f, data = barium, method = "ML",
                       correlation = nlme::corAR1(value = rho, form = ~ t,
                                                  fixed = TRUE))

  expect_equal(unname(stats::coef(pw)), unname(stats::coef(gls_fit)),
               tolerance = 1e-8)
})

test_that("the estimates of the textbook example do not drift", {
  # These values were taken from this package rather than from an outside
  # source, so they do not show that the estimates are right. They fail if a
  # change moves them, which the comparison above cannot do for rho itself,
  # because it holds rho fixed at whatever the estimator returned.
  data("barium")
  pw <- fit_quietly(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6,
                    data = barium, index = "t")

  expect_equal(unname(pw$rho[NROW(pw$rho), 1]), 0.2932138675, tolerance = 1e-7)
  expect_equal(unname(stats::coef(pw)[["lchempi"]]), 2.940963446, tolerance = 1e-7)
  expect_equal(unname(stats::coef(pw)[["lgas"]]), 1.046298674, tolerance = 1e-7)
  expect_equal(unname(stats::coef(pw)[["afdec6"]]), -0.5768112411, tolerance = 1e-7)
})
