# prais 1.2.0

* Added Michael Škvrňák as a contributor.
* Fixed `vcovPC.prais` for panels that do not all begin in the same period. The
covariances of the panels were indexed by the order in which the panels first
appear, while the observations of a period follow the panel, so the covariances
were assigned to the wrong panels. The results now agree with package `pcse`.
* The covariance matrix of the coefficients is obtained from the QR decomposition
of the transformed model matrix. Inverting its cross product forms the normal
equations, which squares the condition number and cost accuracy if the regressors
are close to collinear.
* The observations are brought back into the order of argument `index` after the
model frame was built. A `subset` that reorders the observations undid that order
and made the transformation use the wrong lags.
* Estimation and `summary.prais` are considerably faster for data with many
panels, and the check of argument `index` and the alignment of the observations
no longer dominate the run time of larger samples. The positions of the observations were obtained by appending to a vector
once per panel, which copies the whole vector in every iteration.
* Changed the license to GPL (>= 2). Package `pcse`, which `prais` depends on, is
licensed under GPL (>= 3), with which GPL-2 alone is not compatible.
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
* Arguments such as `subset` and `weights` are now passed on to `lm` correctly. They
could not be evaluated when they were passed through the dots, which also made the
check for weighted least squares unreachable.
* `vcovPC.prais` throws an informative error if it is used on a model that was not
estimated from panel data.
* `vcovHC.prais` and `vcovPC.prais` no longer build an n x n matrix, which required a
prohibitive amount of memory for larger samples. The results are unchanged.
* The history of the iterations is reported with `message` instead of `cat` and can
be suppressed with `suppressMessages`.
* The message about the maximum number of iterations is no longer shown if rho
converged in the last admissible iteration.
* `prais_winsten` throws an error if the variables specified in argument `index` are
not contained in `data` or do not uniquely identify the observations, and warns if
the time variable is not equally spaced.
* `summary.prais` returns the residuals of the transformed model, so that they are on
the same scale as the reported residual standard error.
* `rhoweight` no longer fails with "object 'n_groups' not found" if rho is not
panel specific. The weights only combine panel-specific estimates and are ignored
otherwise.
* Arguments `max_iter` and `tol` are validated. `max_iter = 0` was silently treated
as one iteration and made the estimation of panel-specific rho fail with
"subscript out of bounds".
* Models whose residuals do not vary, such as a saturated or a perfectly fitting
model, are rejected with an informative error instead of failing with
"length of 'dimnames' [2] not equal to array extent".
* `print.summary.prais` states that the reported residuals belong to the
transformed model.
* `vcovPC.prais` reports panels that do not have a period in common instead of
returning a covariance matrix of NaN. Argument `pairwise = TRUE` can be used to
match the observations of two panels by period in that case.
* `prais_winsten` reports panels with less than two observations if `panelwise` is
`TRUE`, because a panel-specific rho cannot be obtained from a single observation.
* Documentation fixes: `predict.prais` has its own help page, the description of
argument `...` of `prais_winsten` is no longer overwritten, the note on values of rho
above 1 reflects that rho is bounded, `summary.prais` documents the components it
actually returns, and several typos were corrected.

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
