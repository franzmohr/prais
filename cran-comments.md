This is a feature update.

* `prais_winsten` accepts a fitted panel model of class `plm` in place of its
arguments `formula`, `data` and `index`, which are taken from the model. Only
models that were estimated with `model = "pooling"` are supported, because the
Prais-Winsten estimator obtains all its estimates by ordinary least squares.
* The Prais-Winsten transformation takes gaps in the time variable into account.
Two observations of a panel that lie *k* periods apart are correlated by *rho^k*
under an AR(1) process, so an observation whose predecessor lies *k* periods back
is differenced against that power and rescaled to keep the variance of the
transformed errors constant.
* `predict.prais` gained the arguments `se.fit`, `interval` and `level`, so that
the standard errors and the confidence interval of the predicted conditional mean
can be obtained.
* Added `vcov.prais` and `confint.prais`. Functions that obtain the covariance
matrix from a model, such as `lmtest::coeftest`, previously failed with "no
applicable method for 'vcov'".
* Added methods for the generics `tidy`, `glance` and `augment` of package
`broom`.
* Added a vignette that walks through the estimation of a time series model and
of a panel model.
* Fixed `summary.prais`, `vcovHC.prais` and `vcovPC.prais` for panel models whose
formula contains a transformed term, such as `log(x)`, whose source column is not
part of the model frame. They failed with "object 'x' not found".

Please note that results change for users whose time variable contains gaps,
including the gap that an observation dropped for missing values leaves behind.
The observations that surround a gap were previously treated as if they were
consecutive, and the warning that a gap used to raise is gone. Equally spaced
data are unaffected.

Results also change for the tests of package `lmtest`, such as `dwtest`, `bgtest`
and `bptest`, applied to a time series model. Those tests do not use the residuals
of the model they are given, but take the components `x` and `y` and re-estimate
the model by ordinary least squares. Both are now the transformed model matrix and
the transformed response, while previously they were absent and the tests fell
back on the model frame, so that they described the untransformed model.

The minimum version of R is raised from 3.2.0 to 3.6.0, because the methods for
the generics of `broom` are registered on load, which requires that version.
Packages `broom`, `plm`, `tibble`, `knitr`, `rmarkdown` and `texreg` are added to
the suggested packages, `knitr` and `rmarkdown` for the new vignette and `texreg`
for the tests of the reverse dependency.

## Test environments
local: Windows 11, R 4.6.1

TODO before submission: run and confirm the remaining environments.
GitHub Actions: ubuntu 24.04 (R-devel, R-release, R-oldrel-1), macOS (R-release),
windows (R-release)
win-builder: R-release, R-devel

## R CMD check results

0 errors | 0 warnings | 1 note

