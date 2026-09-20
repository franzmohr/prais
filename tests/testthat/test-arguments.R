test_that("'rhoweight' is ignored if rho is not panel specific", {
  data <- ar1_sample(n = 40)

  for (weight in c("T", "T1")) {
    expect_error(fit_quietly(y ~ x, data = data, index = "time", rhoweight = weight), NA)
    expect_error(fit_quietly(y ~ x, data = data, index = NULL, rhoweight = weight), NA)
  }

  # The weights only combine panel specific estimates, so they cannot change a
  # single estimate of rho
  expect_equal(fit_quietly(y ~ x, data = data, index = "time", rhoweight = "T")$coefficients,
               fit_quietly(y ~ x, data = data, index = "time")$coefficients)

  panel <- ar1_panel(n_group = 4, n_time = 12)
  expect_equal(fit_quietly(y ~ x, data = panel, index = c("id", "time"),
                           rhoweight = "T")$rho,
               fit_quietly(y ~ x, data = panel, index = c("id", "time"))$rho)
})

test_that("'max_iter' and 'tol' are validated", {
  data <- ar1_sample(n = 40)
  panel <- ar1_panel(n_group = 4, n_time = 12)

  for (value in list(0, -1, NA, c(1, 2))) {
    expect_error(fit_quietly(y ~ x, data = data, index = "time", max_iter = value),
                 "greater than zero")
  }
  # A matrix of the estimates of rho is allocated for panel specific estimates,
  # which was indexed out of bounds if 'max_iter' was zero
  expect_error(fit_quietly(y ~ x, data = panel, index = c("id", "time"),
                           panelwise = TRUE, max_iter = 0),
               "greater than zero")

  expect_error(fit_quietly(y ~ x, data = data, index = "time", tol = -1),
               "non-negative")
  expect_error(fit_quietly(y ~ x, data = data, index = "time", tol = NA),
               "non-negative")

  # Admissible values are still accepted
  expect_error(fit_quietly(y ~ x, data = data, index = "time", max_iter = 1), NA)
  expect_error(fit_quietly(y ~ x, data = data, index = "time", tol = 0), NA)
})

test_that("models whose residuals do not vary are rejected", {
  # A saturated model has no residual degrees of freedom
  saturated <- data.frame(time = 1:2, x = c(0.3, -1.2), y = c(1.4, 0.2))
  expect_error(fit_quietly(y ~ x, data = saturated, index = "time"),
               "residual degrees of freedom")

  # A model that fits perfectly leaves residuals of zero, for which rho is not
  # defined. Whether the residuals are exactly zero depends on the floating point
  # arithmetic of the platform, so only the guard itself is checked here.
  expect_error(
    prais:::.pw_no_variation_error(),
    "do not vary")

  # The smallest sample that leaves a residual degree of freedom works
  small <- data.frame(time = 1:3, x = c(0.3, -1.2, 0.8), y = c(1.4, 0.2, -0.7))
  expect_error(fit_quietly(y ~ x, data = small, index = "time"), NA)
})

test_that("the panel positions match the estimates of rho", {
  panel <- ar1_panel(n_group = 5, n_time = 12)
  pw <- fit_quietly(y ~ x, data = panel, index = c("id", "time"), panelwise = TRUE)
  groups <- prais:::.pw_groups(pw, nrow(pw$model))

  expect_identical(sort(unlist(groups)), seq_len(nrow(pw$model)))
  # The order of the panels is the order of the columns of 'rho'
  expect_identical(as.character(unique(pw$model$id)), dimnames(pw$rho)[[2]])
  for (i in seq_along(groups)) {
    expect_identical(unique(as.character(pw$model$id[groups[[i]]])),
                     dimnames(pw$rho)[[2]][i])
  }
})

test_that("the panel positions do not depend on the type of the ID variable", {
  # The positions were obtained with a comparison per panel before, which is
  # replicated here to make sure that 'split' groups the observations in the
  # same way and in the same order
  by_comparison <- function(ids) {
    names_group <- unique(ids)
    result <- list()
    for (i in seq_along(names_group)) {
      pos <- which(ids == names_group[i])
      names(pos) <- NULL
      result <- c(result, list(pos))
    }
    result
  }

  ids <- list(
    integer = rep(c(3L, 1L, 2L), each = 4),
    numeric = rep(c(10, 2, 7), each = 4),
    character = rep(c("b", "a", "c"), each = 4),
    factor = factor(rep(c("b", "a"), each = 6)),
    unused_level = factor(rep(c("b", "a"), each = 6), levels = c("a", "b", "zz")),
    single = rep(1L, 8),
    unbalanced = c(1, 1, 1, 2, 2, 3, 3, 3, 3)
  )

  for (name in names(ids)) {
    expect_identical(prais:::.pw_split_groups(ids[[name]]),
                     by_comparison(ids[[name]]), info = name)
  }
})

test_that("the panels of the estimation and of the methods agree", {
  # The ID variable is deliberately not sorted
  panel <- expand.grid(time = 1:14, id = c(30, 10, 20))
  panel$x <- stats::rnorm(nrow(panel))
  panel$y <- 1 + 2 * panel$x + stats::rnorm(nrow(panel))

  pw <- fit_quietly(y ~ x, data = panel, index = c("id", "time"), panelwise = TRUE)

  expect_identical(prais:::.pw_split_groups(pw$model$id),
                   prais:::.pw_groups(pw, nrow(pw$model)))
  expect_identical(dimnames(pw$rho)[[2]], as.character(unique(pw$model$id)))
})

test_that("panel specific rho requires two observations per panel", {
  data <- rbind(ar1_panel(n_group = 2, n_time = 12),
                data.frame(id = 3, time = 1, x = 0.5, g = factor("a", levels = c("a", "b")),
                           y = 2.1))

  # The panel with a single observation has no lagged residual
  expect_error(fit_quietly(y ~ x, data = data, index = c("id", "time"), panelwise = TRUE),
               "at least two observations per panel")
  expect_error(fit_quietly(y ~ x, data = data, index = c("id", "time"),
                           panelwise = TRUE, rhoweight = "T"),
               "at least two observations per panel")
  # The offending panel is named
  expect_error(fit_quietly(y ~ x, data = data, index = c("id", "time"), panelwise = TRUE),
               "3")

  # A pooled estimate only needs the panels that do have a lag
  expect_error(fit_quietly(y ~ x, data = data, index = c("id", "time")), NA)
})

test_that("the estimate of rho is bounded to the interval [-1, 1]", {
  # Errors that follow an explosive process push the regression of the residuals
  # on their lag above one, where the scale of the first observation,
  # (1 - rho^2)^(1 / 2), would not be real
  set.seed(1234567)
  n <- 60
  u <- numeric(n)
  e <- rnorm(n)
  for (i in 2:n) {
    u[i] <- 1.25 * u[i - 1] + e[i]
  }
  data <- data.frame(x = rnorm(n, 30, 5), time = 1:n)
  data$y <- 10 + u

  pw <- fit_quietly(y ~ x, data = data, index = "time")
  rho <- pw$rho[NROW(pw$rho), 1]

  expect_true(abs(rho) <= 1)
  # The first observation is kept, which a scale of NaN would have dropped
  expect_equal(nrow(pw$x), n)
  expect_false(anyNA(pw$x))
  expect_false(anyNA(pw$y))
  expect_false(anyNA(summary(pw)$coefficients))
})

test_that("panel-specific estimates of rho are bounded to the interval [-1, 1]", {
  set.seed(1234567)
  n_time <- 30
  panel <- NULL
  for (i in 1:3) {
    u <- numeric(n_time)
    e <- rnorm(n_time)
    for (j in 2:n_time) {
      u[j] <- 1.25 * u[j - 1] + e[j]
    }
    panel <- rbind(panel, data.frame(id = i, time = 1:n_time,
                                     x = rnorm(n_time, 30, 5), y = 10 + u))
  }

  for (weight in c("none", "T", "T1")) {
    pw <- fit_quietly(y ~ x, data = panel, index = c("id", "time"),
                      panelwise = TRUE, rhoweight = weight)
    rho <- pw$rho[NROW(pw$rho), ]
    expect_true(all(abs(rho) <= 1))
  }
})

test_that("argument 'index' can be omitted", {
  data <- ar1_sample()

  expect_error(fit_quietly(y ~ x, data = data), NA)
  # Without an index the observations are taken in the order of the data, which
  # is what passing NULL has always done
  expect_equal(fit_quietly(y ~ x, data = data)$coefficients,
               fit_quietly(y ~ x, data = data, index = NULL)$coefficients)
  expect_null(fit_quietly(y ~ x, data = data)$timeid)
})
