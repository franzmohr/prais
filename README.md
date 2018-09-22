
prais
=====

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/prais)](https://cran.r-project.org/package=prais)

Overview
--------

`prais` implements the Prais-Winsten estimator for models with strictly exogenous regressors and AR(1) serial correlation of the errors.

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

``` r
library(prais)
library(wooldridge)

data("barium")

pw <- prais_winsten(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6, data = barium)

coefficients(pw)
```

    ##                 Estimate Std. Error     t value     Pr(>|t|)
    ## (Intercept) -37.07769023 23.8255020 -1.55621864 1.222048e-01
    ## lchempi       2.94094946  0.6619340  4.44296512 1.942364e-05
    ## lgas          1.04637980  1.0222671  1.02358744 3.080229e-01
    ## lrtwex        1.13279139  0.5299505  2.13754184 3.451687e-02
    ## befile6      -0.01647866  0.3340632 -0.04932797 9.607373e-01
    ## affile6      -0.03315632  0.3366048 -0.09850222 9.216926e-01
    ## afdec6       -0.57681221  0.3577087 -1.61251917 1.093913e-01