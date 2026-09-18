# prais 1.1.5

* Fixed the handling of missing values in `prais_winsten`. Observations that were
dropped by `lm` are now also removed from the data that is used to construct the
panel and lag positions, which previously caused errors such as
"subscript out of bounds" or "NA/NaN/Inf in 'y'".
* `prais_winsten` now throws an informative error if the variables specified in
argument `index` contain `NA` values.
* Fixed argument `newdata` of `predict.prais`, which was positioned after `...` and
was therefore ignored when it was not passed by name. `predict(object, newdata)`
silently returned the fitted values instead of the predictions.
* `predict.prais` now builds the model matrix of `newdata` from the model terms.
Transformed variables, factors and interactions are handled correctly, and the
variables in `newdata` no longer have to be named after the model coefficients.
* `prais_winsten` returns the new elements `xlevels` and `contrasts`.
* Fixed the estimation of models with linearly dependent variables, which failed with
the error "NA/NaN/Inf in 'x'". As in `lm`, the coefficients of such variables are set
to NA and omitted from the fitted values, the coefficient table of `summary.prais`,
the covariance matrices of `vcovHC.prais` and `vcovPC.prais` and from `predict.prais`.
* `print.summary.prais` reports the number of coefficients that are not defined
because of singularities.

# prais 1.1.4

* Added the function `predict.prais`.
* No calculation of F-statistic in `summary.prais` when only an intercept is estimated.

# prais 1.1.3

* Added the `barium` example data set.
* Additional check for argument `index` of `prais_winsten`.

# prais 1.1.2

* Fix Lazy Data NOTE issue from CRAN results
* Fix declared imports NOTE issue from CRAN results
* Estimates of rho are bounded to the range [-1, 1] during each iteration
* Mandatory specification of argument `prais_winsten$index`
* Added functionality to estimate panel-specific AR(1) coefficients
* Added functionality for panel-corrected standard errors (PCSE) with `vcovPC.prais`
* Fix typo in `vcovHC.prais` documentation

# prais 1.1.1

* Add function for semirobust standard errors (#2)
* Add functionality to expand factors to a set of dummy variables (#1)
* Small fixes in the documentation of prais_winsten() and summary.prais()
* Changed upper cases in GitHub link to lower cases in DESCRIPTION

# prais 1.1.0

* Added a `NEWS.md` file to track changes to the package
* Added panel data functionality
* Updated documentation

# prais 1.0.0

* Added S3 summary and print functions
* Added Durbin-Watson test to summary function

# prais 0.0.1

* Initial release
