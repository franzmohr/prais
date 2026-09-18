This is an update, which mainly fixes bugs:

* Estimation failed if a variable of the model contained NA values.
* `predict.prais` ignored argument `newdata` if it was not passed by name, and it
could not handle transformed variables, factors and interactions.
* Estimation failed if the model contained linearly dependent variables.
* `vcovPC.prais` returned a covariance matrix of NaN if the panels did not have a
period in common.
* Arguments that are not evaluated in the usual way, such as `subset` and `weights`,
were not passed on to `lm` correctly.
* `rhoweight` failed if the AR(1) coefficient was not panel specific.
* Models for which the AR(1) coefficient cannot be obtained, such as a panel with a
single observation if `panelwise = TRUE`, and inadmissible values of `max_iter`,
failed with errors that did not point to the cause.
* `vcovHC.prais` and `vcovPC.prais` no longer build an n x n matrix, which required a
prohibitive amount of memory for larger samples.
* The history of the iterations is reported with `message` instead of `cat`, so that
it can be suppressed.

The variables specified in argument `index` are now checked. Periods that do not
uniquely identify the observations are rejected, which is stricter than before.

The package also gained unit tests.

## Test environments
GitHub Actions: ubuntu 24.04 (R-devel, R-release, R-oldrel-1), macOS (R-release),
windows (R-release)
win-builder: R-release
local: Windows 11, R 4.6.1

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

`texreg` is the only reverse dependency and enhances `prais`. Its `extract` method
was checked against this version and gives the same results as before.
