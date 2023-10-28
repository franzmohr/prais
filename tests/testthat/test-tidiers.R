skip_on_cran()

skip_if_not_installed("modeltests")
skip_if_not_installed("wooldridge")
library(modeltests)

test_that("prais::prais tidier arguments", {
  check_arguments(tidy.prais)
  check_arguments(glance.prais)
})

test_that("tidy.prais", {
  data(barium, package = "wooldridge")
  r <- prais::prais_winsten(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6,
                            data = barium, index = "t")

  td <- tidy(r)
  td_ci <- tidy(r, conf.int = TRUE)
  check_tidy_output(td)
  check_dims(td, expected_cols = 5, expected_rows = 7)
  check_dims(td_ci, expected_cols = 7, expected_rows = 7)
})

test_that("glance.prais", {
  data(barium, package = "wooldridge")
  r <- prais::prais_winsten(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6,
                            data = barium, index = "t")

  gl <- glance(r)
  check_glance_outputs(gl)
  # check_dims(td, expected_cols = 5, expected_rows = 7)

})

