# prais 1.2.0.9000

* `prais_winsten` accepts a fitted panel model of class `plm` in place of its
arguments `formula`, `data` and `index`, which are taken from the model (#11).
Since the estimator obtains all its estimates by ordinary least squares, only
models that were estimated with `model = "pooling"` are supported, and the within
and the random effects model are rejected with a message that points at the dummy
variables of the formula, because their own transformation does not leave the
AR(1) structure of the errors intact. The data are obtained by evaluating the
`data` argument of the call of the model, so the object it refers to must still be
available. The model frame of the fitted model cannot be used instead, because its
columns are named after the terms of the formula, so that a term such as
`factor(id)` or `log(x)` could not be evaluated again. If the data are a
`pdata.frame`, the time variable is turned back into a number, since the index of
a `pdata.frame` is stored as factors, which order the periods but do not carry the
distances between them that the transformation of a gap requires. Thanks to Julian
Jäcker for the request.
* Gaps in the time variable are taken into account by the Prais-Winsten
transformation. Two observations of a panel that lie *k* periods apart are
correlated by *rho^k* under an AR(1) process, so an observation whose predecessor
lies *k* periods back is now differenced against that power and rescaled to keep
the variance of the transformed errors constant. The estimates are those of
generalised least squares evaluated at the periods that were observed, while
previously the observations that surround a gap were treated as if they were
consecutive. As a gap grows, the transformation approaches the one of the first
observation of a panel, so that an observation after a long gap effectively starts
a new spell. Equally spaced data are unaffected, and the warning that a gap used
to raise is gone. The same applies to the gap that an observation dropped for
missing values leaves behind (#12). Thanks to Sebastian Krantz for pointing out
that the lags have to be taken from the time variable rather than from adjacent
rows.
* The distances between the periods are counted in steps of the greatest common
divisor of the differences of the time variable, so that quarterly, monthly or
biennial data are handled without further arguments and the estimates no longer
depend on the unit the periods are expressed in. `Date` and `POSIXct` variables
are supported, which the check of the time variable previously skipped, because
`is.numeric` is `FALSE` for them. If the periods are not multiples of a common
step, a warning is issued and the observations that surround a gap are treated as
if they were consecutive, as before.
* The estimate of *rho* is still obtained from the residuals of an observation and
its predecessor, whether or not a gap lies between them. Since the correlation
across a gap is *rho^k* rather than *rho*, the estimate is attenuated towards zero
if a large share of the observations follows a gap. This is now documented.
* Added the component `timeid` to objects of class `prais`, which holds the
periods of the observations counted in whole steps from the first period of their
panel. `summary.prais` and the covariance matrices repeat the transformation on
the data of the model and need the same distances, and the time variable is not
part of the model frame of a time series.

* Added methods for the generics `tidy`, `glance` and `augment` of package
`broom`, which summarise the coefficients, the goodness of fit and the fitted
values of an estimated model in tidy data frames. `glance` also reports the AR(1)
coefficient and the Durbin-Watson statistics, which are `NA` if the model was
estimated with `panelwise = TRUE`, because there is one coefficient per panel in
that case. The generics are the interface through which packages for publishable
regression output, such as `gtsummary` and `modelsummary`, obtain the results of
a model (#13). The methods are registered when `broom` is loaded, so `broom`
remains a suggested package and the generics are not re-exported, unlike in the
version of the extension that was part of release 1.1.3. Since delayed registration requires
R (>= 3.6.0), the minimum version of R was raised accordingly.
* `predict.prais` gained the arguments `se.fit`, `interval` and `level`, so that
the standard errors and the confidence interval of the predicted conditional mean
can be obtained (#10). They are based on the covariance matrix of `vcov.prais` and
the quantiles of the *t* distribution with the residual degrees of freedom of the
model, so they agree with the standard errors of `summary.prais` and the intervals
of `confint.prais`. Prediction intervals for an individual observation are not
available, because their variance would depend on the serial correlation of the
error of the predicted period, which the predictions do not use. The default
result is unchanged. Thanks to Angel Paternina for the request.
* Added `vcov.prais`, which returns the covariance matrix of the coefficients
that the standard errors of `summary.prais` are based on. Functions that obtain
the covariance matrix from a model, such as `lmtest::coeftest`, previously
failed with "no applicable method for 'vcov'" and now work without passing the
matrix explicitly.
* Added `confint.prais`. The intervals are based on the *t* distribution with the
residual degrees of freedom of the model, so they agree with the p-values of
`summary.prais` and with the intervals of `tidy`. Without the method they would
be obtained by `confint.default`, which uses the quantiles of the normal
distribution and is therefore too narrow in small samples.
* The transformed model matrix and the transformed response are added to the
estimated object as the components `x` and `y`. The tests of package `lmtest`,
such as `dwtest`, `bgtest` and `bptest`, do not use the residuals of the model
they are given, but take those two components and re-estimate the model by
ordinary least squares. Without them both were taken from the model frame, which
holds the original data, so the tests described the untransformed model and
reported the same Durbin-Watson statistic as before the correction for serial
correlation (#16). They now refer to the estimated model. The components are not
added for panel data, because the tests difference the residuals over all
observations at once, which mixes the last observation of a panel with the first
observation of the next one. `summary` reports a Durbin-Watson statistic that
respects the panels instead.
* The Prais-Winsten transformation is applied to all panels at once instead of
one panel at a time, which allocated a copy of the involved rows for every panel.
The results are unchanged.
* Fixed `summary.prais` and the covariance matrices `vcovHC.prais` and
`vcovPC.prais` for panel models whose formula contains a transformed term, such
as `log(x)` or `poly(x, 2)`, whose source column is not itself part of the model
frame. They failed with "object 'x' not found". The ID and time variables were
appended to the model frame with `cbind`, which returns a new data frame and
drops its `terms` attribute, so the frame was no longer recognised as a model
frame and `model.matrix` evaluated the variables of the formula against it again
instead of taking the columns it already holds. The variables are now assigned as
columns, which leaves the attributes of the frame intact. Single time series were
not affected, and neither were terms such as `factor(id)` that refer to a
variable of the index, because that column had just been appended.

# prais 1.2.0

* Added Michael Škvrňák as a contributor.
* Releases are archived on Zenodo. `citation("prais")` reports the DOI of the
archive.
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
