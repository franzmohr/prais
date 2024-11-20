skip_on_cran()

skip_if_not_installed("broom")
skip_if_not_installed("lmtest")
skip_if_not_installed("modeltests")
library(lmtest)
library(modeltests)

test_df <- tibble::tribble(
  ~share, ~r_year, ~country, ~year,
  5.3,       3,               "BGR",  1994,
  1.12,       6,               "BGR",  1997,
  49.81,      10,               "BGR",  2001,
  15.88,      14,               "BGR",  2005,
  47.11,      18,               "BGR",  2009,
  11.81,      22,               "BGR",  2013,
  0,      23,               "BGR",  2014,
  7.22,      26,               "BGR",  2017,
  2.8,       5,               "CZE",  1996,
  1.12,       7,               "CZE",  1998,
  2.78,      11,               "CZE",  2002,
  0,      15,               "CZE",  2006,
  20.02,      19,               "CZE",  2010,
  30.65,      22,               "CZE",  2013,
  0,      26,               "CZE",  2017,
  9.95,       4,               "EST",  1995,
  4.03,       8,               "EST",  1999,
  24.6,      12,               "EST",  2003,
  7.1,      16,               "EST",  2007,
  0,      20,               "EST",  2011,
  8.69,      24,               "EST",  2015,
  5.6,      28,               "EST",  2019,
  1,      12,               "HRV",  2003,
  1.8,      16,               "HRV",  2007,
  4.05,      20,               "HRV",  2011,
  24.22,      24,               "HRV",  2015,
  0,      25,               "HRV",  2016,
  22.62,      29,               "HRV",  2020,
  0,       7,               "HUN",  1998,
  0,      11,               "HUN",  2002,
  0,      15,               "HUN",  2006,
  7.48,      19,               "HUN",  2010,
  0,      23,               "HUN",  2014,
  4.79,      27,               "HUN",  2018,
  11.13,       5,               "LTU",  1996,
  21.11,       9,               "LTU",  2000,
  28.44,      13,               "LTU",  2004,
  16.84,      17,               "LTU",  2008,
  9.75,      21,               "LTU",  2012,
  8.35,      25,               "LTU",  2016,
  3.39,      29,               "LTU",  2020,
  12.71,       4,               "LVA",  1995,
  28.5,       7,               "LVA",  1998,
  36.3,      11,               "LVA",  2002,
  1.48,      15,               "LVA",  2006,
  0,      19,               "LVA",  2010,
  20.83,      20,               "LVA",  2011,
  16.27,      23,               "LVA",  2014,
  42.75,      27,               "LVA",  2018,
  5.17,       6,               "POL",  1997,
  10.2,      10,               "POL",  2001,
  2.62,      14,               "POL",  2005,
  0,      16,               "POL",  2007,
  1.06,      20,               "POL",  2011,
  20.03,      24,               "POL",  2015,
  0,      28,               "POL",  2019,
  5.46,       5,               "ROU",  1996,
  1.4,       9,               "ROU",  2000,
  2.2,      13,               "ROU",  2004,
  0,      17,               "ROU",  2008,
  13.99,      21,               "ROU",  2012,
  11.66,      25,               "ROU",  2016,
  9.08,      29,               "ROU",  2020,
  6.42,       3,               "SVK",  1994,
  8.01,       7,               "SVK",  1998,
  8.01,      11,               "SVK",  2002,
  0,      15,               "SVK",  2006,
  15.88,      19,               "SVK",  2010,
  4.09,      21,               "SVK",  2012,
  12.24,      25,               "SVK",  2016,
  15.66,      29,               "SVK",  2020,
  1.06,       5,               "SVN",  1996,
  4.34,       9,               "SVN",  2000,
  2.62,      13,               "SVN",  2004,
  0,      17,               "SVN",  2008,
  38.1,      20,               "SVN",  2011,
  35.83,      23,               "SVN",  2014,
  15.21,      27,               "SVN",  2018
)

test_that("prais::prais tidier arguments", {
  check_arguments(tidy.prais)
  check_arguments(glance.prais)
})

test_that("tidy.prais", {
  data(barium, package = "prais")
  r <- prais::prais_winsten(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6,
                            data = barium, index = "t")

  td <- tidy(r)
  td_ci <- tidy(r, conf.int = TRUE)
  check_tidy_output(td)
  check_dims(td, expected_cols = 5, expected_rows = 7)
  check_dims(td_ci, expected_cols = 7, expected_rows = 7)

})

test_that("glance.prais", {
  data(barium, package = "prais")
  r <- prais::prais_winsten(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6,
                            data = barium, index = "t")

  gl <- glance(r)
  check_glance_outputs(gl)
  # check_dims(td, expected_cols = 5, expected_rows = 7)

})

# glance.coeftest uses glance.prais in the background (as it uses saved prais object)
test_that("glance.prais works after coeftest", {
  r2 <- prais::prais_winsten(share ~ r_year,
                             data = test_df,
                             index = c("country", "year"),
                             twostep = TRUE, panelwise = TRUE, rhoweight = "T1") %>%
    coeftest(., vcov. = vcovPC(., pairwise = TRUE), save = TRUE)

  gl2 <- glance(r2)
  check_glance_outputs(gl2)
})

