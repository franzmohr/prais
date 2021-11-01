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
