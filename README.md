
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

    ## Iteration 1: rho = 0.2708
    ## Iteration 2: rho = 0.291
    ## Iteration 3: rho = 0.293
    ## Iteration 4: rho = 0.2932
    ## Iteration 5: rho = 0.2932
    ## Iteration 6: rho = 0.2932
    ## Iteration 7: rho = 0.2932

``` r
summary(pw)
```

    ## 
    ## Call:
    ## prais.winsten(formula = lchnimp ~ lchempi + lgas + lrtwex + befile6 + 
    ##     affile6 + afdec6, data = barium)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.01146 -0.39152  0.06758  0.35063  1.35021 
    ## 
    ## AR(1) Coefficient rho after 7 Iterations: 0.2932
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -37.07770   22.77830  -1.628   0.1061    
    ## lchempi       2.94095    0.63284   4.647 8.46e-06 ***
    ## lgas          1.04638    0.97734   1.071   0.2864    
    ## lrtwex        1.13279    0.50666   2.236   0.0272 *  
    ## befile6      -0.01648    0.31938  -0.052   0.9589    
    ## affile6      -0.03316    0.32181  -0.103   0.9181    
    ## afdec6       -0.57681    0.34199  -1.687   0.0942 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5733 on 124 degrees of freedom
    ## Multiple R-squared:  0.2021, Adjusted R-squared:  0.1635 
    ## F-statistic: 31.41 on 1 and 124 DF,  p-value: 1.285e-07
