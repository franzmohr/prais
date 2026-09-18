This is an update, which mainly fixes bugs:

* Estimation failed if a variable of the model contained NA values.
* `predict.prais` ignored argument `newdata` if it was not passed by name, and it
could not handle transformed variables, factors and interactions.
* Estimation failed if the model contained linearly dependent variables.
* Arguments that are not evaluated in the usual way, such as `subset` and `weights`,
were not passed on to `lm` correctly.
* `vcovHC.prais` and `vcovPC.prais` no longer build an n x n matrix, which required a
prohibitive amount of memory for larger samples.
* The history of the iterations is reported with `message` instead of `cat`, so that
it can be suppressed.

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
