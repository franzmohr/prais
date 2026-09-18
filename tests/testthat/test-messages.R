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

test_that("an unequally spaced time variable is reported", {
  data <- ar1_sample(n = 50)

  expect_warning(fit_quietly(y ~ x, data = data[-(20:30), ], index = "time"),
                 "not equally spaced")
  # Equally spaced data are accepted silently
  expect_warning(fit_quietly(y ~ x, data = data, index = "time"), NA)
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
