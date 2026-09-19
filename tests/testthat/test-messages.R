test_that("the history of the iterations can be suppressed", {
  data <- ar1_sample(n = 40)

  messages <- capture.output(
    invisible(prais_winsten(y ~ x, data = data, index = "time")),
    type = "message")
  expect_true(any(grepl("Iteration 0: rho", messages)))
  expect_true(any(grepl("Iteration 1: rho", messages)))

  expect_silent(suppressMessages(prais_winsten(y ~ x, data = data, index = "time")))
})

test_that("the maximum number of iterations is only reported if it is reached", {
  data <- ar1_sample(n = 40)

  messages <- capture.output(
    invisible(prais_winsten(y ~ x, data = data, index = "time", max_iter = 1)),
    type = "message")
  expect_true(any(grepl("maximum number of iterations", messages)))

  # rho converges well within the default number of iterations
  messages <- capture.output(
    invisible(prais_winsten(y ~ x, data = data, index = "time")),
    type = "message")
  expect_false(any(grepl("maximum number of iterations", messages)))
})

test_that("gaps in the time variable are accepted silently", {
  data <- ar1_sample(n = 50)

  # The transformation accounts for the distance between the periods, so a gap is
  # no longer a reason to warn
  expect_warning(fit_quietly(y ~ x, data = data[-(20:30), ], index = "time"), NA)
  expect_warning(fit_quietly(y ~ x, data = data, index = "time"), NA)
})

test_that("periods without a common step are reported", {
  data <- ar1_sample(n = 30)
  # The distance between the last two periods is irrational, so the periods are
  # not multiples of a common step and their distance cannot be counted
  data$time[30] <- data$time[29] + sqrt(2)

  expect_warning(fit_quietly(y ~ x, data = data, index = "time"),
                 "not multiples of a common step")
})

test_that("periods are only compared within a panel", {
  panel <- ar1_panel(n_group = 3, n_time = 10)

  # Every period appears once per panel, which is not a duplicate
  expect_error(fit_quietly(y ~ x, data = panel, index = c("id", "time")), NA)
  expect_warning(fit_quietly(y ~ x, data = panel, index = c("id", "time")), NA)
})

test_that("duplicated periods are rejected", {
  data <- ar1_sample(n = 40)
  data$time[3] <- data$time[2]
  expect_error(fit_quietly(y ~ x, data = data, index = "time"), "uniquely identify")

  panel <- ar1_panel(n_group = 3, n_time = 10)
  panel$time[3] <- panel$time[2]
  expect_error(fit_quietly(y ~ x, data = panel, index = c("id", "time")),
               "uniquely identify")
})

test_that("variables of 'index' that are not in the data are reported", {
  data <- ar1_sample(n = 40)

  expect_error(fit_quietly(y ~ x, data = data, index = "period"), "period")
})
