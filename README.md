
prais
=====

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/prais)](https://cran.r-project.org/package=prais)

Overview
--------

prais implements the Prais-Winsten estimator for models with strictly exogenous regressors and AR(1) serial correlation of the errors.

Installation
------------

### CRAN

``` r
install.packages("prais")
```

### Development version

``` r
# install.packages("devtools")
devtools::install_github("franzmohr/prais")
```

Usage
-----

The following code uses the development version.

``` r
library(prais)

# Load data
library(wooldridge) # install.packages("wooldridge")
data("barium")

pw <- prais.winsten(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6, data = barium)
```

    ## [1] "Iteration 1: rho = 0.2708"
    ## [1] "Iteration 2: rho = 0.291"
    ## [1] "Iteration 3: rho = 0.293"
    ## [1] "Iteration 4: rho = 0.2932"
    ## [1] "Iteration 5: rho = 0.2932"
    ## [1] "Iteration 6: rho = 0.2932"
    ## [1] "Iteration 7: rho = 0.2932"

``` r
pw
```

    ## 
    ## Call:
    ## prais.winsten(formula = lchnimp ~ lchempi + lgas + lrtwex + befile6 + 
    ##     affile6 + afdec6, data = barium)
    ## 
    ## Coefficients:
    ## (Intercept)      lchempi         lgas       lrtwex      befile6  
    ##   -37.07770      2.94095      1.04638      1.13279     -0.01648  
    ##     affile6       afdec6  
    ##    -0.03316     -0.57681  
    ## 
    ## AR(1) Coefficient rho: 0.2932
