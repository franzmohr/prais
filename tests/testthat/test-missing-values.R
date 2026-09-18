test_that("observations with missing values are dropped as in 'lm'", {
  data <- ar1_sample(n = 40)
  data$x[5] <- NA

  pw <- fit_quietly(y ~ x, data = data, index = "time")

  expect_identical(nrow(pw$model), 39L)
  expect_false(anyNA(pw$model))
  expect_length(pw$fitted.values, 39L)
  expect_length(pw$residuals, 39L)
  expect_false(anyNA(pw$coefficients))
})

test_that("missing values give the same result as listwise deletion", {
  data <- ar1_sample(n = 40)
  data$x[5] <- NA
  complete <- data[!is.na(data$x), ]

  pw <- fit_quietly(y ~ x, data = data, index = "time")
  # Deleting the incomplete observation leaves a gap in the time variable
  reference <- suppressWarnings(fit_quietly(y ~ x, data = complete, index = "time"))

  expect_equal(pw$coefficients, reference$coefficients)
  expect_equal(pw$rho, reference$rho)
  expect_equal(pw$fitted.values, reference$fitted.values)
})

test_that("missing values in the response are dropped", {
  data <- ar1_sample(n = 40)
  data$y[c(9, 30)] <- NA

  pw <- fit_quietly(y ~ x, data = data, index = "time")

  expect_identical(nrow(pw$model), 38L)
})

test_that("panel models are estimated when values are missing", {
  data <- ar1_panel(n_group = 4, n_time = 15)
  data$x[c(7, 33)] <- NA
  complete <- data[!is.na(data$x), ]

  for (args in list(list(), list(panelwise = TRUE),
                    list(panelwise = TRUE, rhoweight = "T"))) {
    pw <- do.call(fit_quietly, c(list(y ~ x, data = data, index = c("id", "time")), args))
    expect_identical(nrow(pw$model), nrow(complete))
    expect_false(anyNA(pw$fitted.values))
  }

  pw <- fit_quietly(y ~ x, data = data, index = c("id", "time"))
  reference <- suppressWarnings(fit_quietly(y ~ x, data = complete, index = c("id", "time")))
  expect_equal(pw$coefficients, reference$coefficients)
})

test_that("the ID and time variables remain aligned with the model frame", {
  data <- ar1_panel(n_group = 5, n_time = 12)
  data$x[c(4, 20, 41)] <- NA

  pw <- fit_quietly(y ~ x, data = data, index = c("id", "time"))

  complete <- data[!is.na(data$x), ]
  complete <- complete[order(complete$id), ]
  complete <- complete[order(complete$time), ]

  expect_identical(pw$model$id, complete$id)
  expect_identical(pw$model$time, complete$time)
  expect_equal(pw$model$y, complete$y)
  expect_equal(pw$model$x, complete$x)
})

test_that("missing values in the variables of 'index' are rejected", {
  data <- ar1_panel(n_group = 3, n_time = 10)
  data$time[3] <- NA

  expect_error(fit_quietly(y ~ x, data = data, index = c("id", "time")),
               "must not contain NA values")
})

test_that("row names of the data do not affect the estimates", {
  data <- ar1_sample(n = 40)
  data$x[5] <- NA
  renamed <- data
  row.names(renamed) <- paste0("obs", nrow(data):1)

  expect_equal(unname(fit_quietly(y ~ x, data = data, index = "time")$coefficients),
               unname(fit_quietly(y ~ x, data = renamed, index = "time")$coefficients))
})

test_that("summary and covariance methods work when values are missing", {
  data <- ar1_panel(n_group = 4, n_time = 15)
  data$x[c(7, 33)] <- NA
  pw <- fit_quietly(y ~ x, data = data, index = c("id", "time"))

  expect_s3_class(summary(pw), "summary.prais")
  expect_identical(dim(vcovHC(pw)), c(2L, 2L))
  expect_identical(dim(vcovPC(pw)), c(2L, 2L))
})
