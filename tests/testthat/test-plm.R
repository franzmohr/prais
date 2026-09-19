test_that("a model of class 'plm' gives the same estimates as formula and data", {
  skip_if_not_installed("plm")

  data <- ar1_panel()
  pooled <- plm::plm(y ~ x, data = data, index = c("id", "time"), model = "pooling")

  pw_plm <- fit_quietly(pooled)
  pw <- fit_quietly(y ~ x, data = data, index = c("id", "time"))

  # The calls differ, because they are the calls the user made
  pw_plm$call <- NULL
  pw$call <- NULL
  expect_equal(pw_plm, pw)
})

test_that("the arguments of the estimation are passed on if a 'plm' model is used", {
  skip_if_not_installed("plm")

  data <- ar1_panel()
  pooled <- plm::plm(y ~ x, data = data, index = c("id", "time"), model = "pooling")

  pw_plm <- fit_quietly(pooled, panelwise = TRUE, rhoweight = "T1")
  pw <- fit_quietly(y ~ x, data = data, index = c("id", "time"),
                    panelwise = TRUE, rhoweight = "T1")

  expect_equal(pw_plm$coefficients, pw$coefficients)
  expect_equal(pw_plm$rho, pw$rho)
})

test_that("terms that have to be evaluated again are taken from the data", {
  skip_if_not_installed("plm")

  data <- ar1_panel()
  data$id <- factor(data$id)
  pooled <- plm::plm(y ~ log(x) + factor(id), data = data,
                     index = c("id", "time"), model = "pooling")

  pw_plm <- fit_quietly(pooled)
  pw <- fit_quietly(y ~ log(x) + factor(id), data = data, index = c("id", "time"))

  expect_equal(pw_plm$coefficients, pw$coefficients)
  expect_true("log(x)" %in% names(pw_plm$coefficients))
})

test_that("a 'pdata.frame' gives the same estimates as the data it was built from", {
  skip_if_not_installed("plm")

  data <- ar1_panel()
  pdata <- plm::pdata.frame(data, index = c("id", "time"))
  pooled <- plm::plm(y ~ x, data = pdata, model = "pooling")

  pw_plm <- fit_quietly(pooled)
  pw <- fit_quietly(y ~ x, data = data, index = c("id", "time"))

  expect_equal(pw_plm$coefficients, pw$coefficients)
  expect_equal(pw_plm$rho, pw$rho)
})

test_that("the gaps of a panel are respected if a 'pdata.frame' is used", {
  skip_if_not_installed("plm")

  # The periods of a 'pdata.frame' are the levels of a factor, which only order
  # the observations. If they were not turned back into numbers, the gap would be
  # treated as if the observations that surround it were consecutive.
  data <- ar1_panel(n_group = 3, n_time = 20)
  data <- data[data$time != 10, ]
  pdata <- plm::pdata.frame(data, index = c("id", "time"))
  pooled <- plm::plm(y ~ x, data = pdata, model = "pooling")

  pw_plm <- fit_quietly(pooled)
  pw <- fit_quietly(y ~ x, data = data, index = c("id", "time"))

  expect_equal(pw_plm$coefficients, pw$coefficients)
  expect_equal(pw_plm$timeid, pw$timeid)
  expect_true(any(diff(pw_plm$timeid) == 2))
})

test_that("the methods of the package work on a model that was estimated from 'plm'", {
  skip_if_not_installed("plm")

  data <- ar1_panel()
  pooled <- plm::plm(y ~ x, data = data, index = c("id", "time"), model = "pooling")
  pw_plm <- fit_quietly(pooled)

  expect_s3_class(pw_plm, "prais")
  expect_identical(pw_plm$index, c("id", "time"))
  expect_equal(summary(pw_plm)$coefficients,
               summary(fit_quietly(y ~ x, data = data,
                                   index = c("id", "time")))$coefficients)
  expect_equal(predict(pw_plm, newdata = data[1:5, ]),
               predict(fit_quietly(y ~ x, data = data, index = c("id", "time")),
                       newdata = data[1:5, ]))
})

test_that("the call of the estimation is the call the user made", {
  skip_if_not_installed("plm")

  data <- ar1_panel()
  pooled <- plm::plm(y ~ x, data = data, index = c("id", "time"), model = "pooling")
  pw_plm <- suppressMessages(prais_winsten(pooled))

  expect_identical(pw_plm$call, quote(prais_winsten(formula = pooled)))
})

test_that("only pooled models are supported", {
  skip_if_not_installed("plm")

  data <- ar1_panel()
  for (model in c("within", "random", "fd", "between")) {
    fitted <- plm::plm(y ~ x, data = data, index = c("id", "time"), model = model)
    expect_error(fit_quietly(fitted), "pooled OLS procedure")
  }
})

test_that("instrumental variable models are not supported", {
  skip_if_not_installed("plm")

  data <- ar1_panel()
  fitted <- plm::plm(y ~ x | g, data = data, index = c("id", "time"),
                     model = "pooling")

  expect_error(fit_quietly(fitted), "more than one part")
})

test_that("'data' and 'index' must not be specified for a model of class 'plm'", {
  skip_if_not_installed("plm")

  data <- ar1_panel()
  pooled <- plm::plm(y ~ x, data = data, index = c("id", "time"), model = "pooling")

  expect_error(fit_quietly(pooled, data = data), "Argument 'data' must not be specified")
  expect_error(fit_quietly(pooled, index = c("id", "time")),
               "Argument 'index' must not be specified")
})

test_that("an informative error is raised if the data of the model are gone", {
  skip_if_not_installed("plm")

  pooled <- local({
    data <- ar1_panel()
    plm::plm(y ~ x, data = data, index = c("id", "time"), model = "pooling")
  })
  # The local environment the model was estimated in is kept by its formula, so
  # the data are only out of reach once that environment is replaced
  environment(pooled$formula) <- new.env(parent = emptyenv())

  expect_error(fit_quietly(pooled), "could not be found")
})
