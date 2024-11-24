
# prais

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/prais)](https://cran.r-project.org/package=prais)
[![R-CMD-check](https://github.com/FranzMohr/prais/workflows/R-CMD-check/badge.svg)](https://github.com/FranzMohr/prais/actions)

## Overview

`prais` implements the Prais-Winsten estimator for models with strictly
exogenous regressors and AR(1) serial correlation of the errors.

## Installation

### CRAN

``` r
install.packages("prais")
```

### Development version

``` r
# install.packages("devtools")
devtools::install_github("franzmohr/prais")
```

## Usage

``` r
# Load the package
library(prais)

# Load the data
data("barium")

pw <- prais_winsten(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6,
                    data = barium, index = "t")
```

    ## Iteration 0: rho = 0
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
    ## prais_winsten(formula = lchnimp ~ lchempi + lgas + lrtwex + befile6 + 
    ##     affile6 + afdec6, data = barium, index = "t")
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.99386 -0.32219  0.03748  0.40226  1.50282 
    ## 
    ## AR(1) coefficient rho after 7 iterations: 0.2932
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -37.07582   22.77843  -1.628   0.1061    
    ## lchempi       2.94096    0.63284   4.647 8.46e-06 ***
    ## lgas          1.04630    0.97734   1.071   0.2864    
    ## lrtwex        1.13277    0.50666   2.236   0.0272 *  
    ## befile6      -0.01648    0.31938  -0.052   0.9589    
    ## affile6      -0.03316    0.32181  -0.103   0.9181    
    ## afdec6       -0.57681    0.34199  -1.687   0.0942 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5733 on 124 degrees of freedom
    ## Multiple R-squared:  0.2021, Adjusted R-squared:  0.1635 
    ## F-statistic: 5.235 on 6 and 124 DF,  p-value: 7.764e-05
    ## 
    ## Durbin-Watson statistic (original): 1.458 
    ## Durbin-Watson statistic (transformed): 2.087

## Robust standard errors

### White’s estimator

``` r
library(lmtest)

coeftest(pw, vcov. = vcovHC(pw, "HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##               Estimate Std. Error t value  Pr(>|t|)    
    ## (Intercept) -37.075825  20.897425 -1.7742   0.07849 .  
    ## lchempi       2.940963   0.599549  4.9053 2.866e-06 ***
    ## lgas          1.046299   0.925151  1.1309   0.26026    
    ## lrtwex        1.132774   0.495127  2.2878   0.02384 *  
    ## befile6      -0.016478   0.327779 -0.0503   0.95999    
    ## affile6      -0.033158   0.277297 -0.1196   0.90501    
    ## afdec6       -0.576811   0.422552 -1.3651   0.17470    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### Panel-corrected standard errors (PCSE)

Estimate a panel model, for which PCSE should be obtained.

``` r
# Example 2 in the documentation of Stata function xtpcse

# Load data
data <- haven::read_dta("http://www.stata-press.com/data/r14/grunfeld.dta")

# Estimate
x <- prais_winsten(invest ~ mvalue + kstock, data = data, index = c("company", "year"),
                   twostep = TRUE, panelwise = TRUE, rhoweight = "T1")

# Results
summary(x)
```

    ## 
    ## Call:
    ## prais_winsten(formula = invest ~ mvalue + kstock, data = data, 
    ##     index = c("company", "year"), twostep = TRUE, panelwise = TRUE, 
    ##     rhoweight = "T1")
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -305.52  -42.61    4.15   33.23  343.52 
    ## 
    ## AR(1) coefficient rho after 1 iterations: 0.906
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -39.125687  26.362285  -1.484    0.139    
    ## mvalue        0.095016   0.007683  12.367  < 2e-16 ***
    ## kstock        0.306005   0.036630   8.354 1.17e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 42.41 on 197 degrees of freedom
    ## Multiple R-squared:  0.5468, Adjusted R-squared:  0.5422 
    ## F-statistic: 118.8 on 2 and 197 DF,  p-value: < 2.2e-16
    ## 
    ## Durbin-Watson statistic (original): 0.2097 
    ## Durbin-Watson statistic (transformed): 1.473

Obtain PCSE by using only those residuals from periods that are common
to all panels by setting `pairwise = FALSE`.

``` r
coeftest(x, vcov. = vcovPC(x, pairwise = FALSE))
```

    ## 
    ## t test of coefficients:
    ## 
    ##               Estimate Std. Error t value  Pr(>|t|)    
    ## (Intercept) -39.125687  30.503546 -1.2827    0.2011    
    ## mvalue        0.095016   0.012993  7.3126 6.434e-12 ***
    ## kstock        0.306005   0.060372  5.0687 9.202e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Obtain PCSE by using all observations that can be matched by period
between two panels by setting `pairwise = TRUE`.

``` r
coeftest(x, vcov. = vcovPC(x, pairwise = TRUE))
```

    ## 
    ## t test of coefficients:
    ## 
    ##               Estimate Std. Error t value  Pr(>|t|)    
    ## (Intercept) -39.125687  30.503546 -1.2827    0.2011    
    ## mvalue        0.095016   0.012993  7.3126 6.434e-12 ***
    ## kstock        0.306005   0.060372  5.0687 9.202e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## References

Beck, N. L. and Katz, J. N. (1995): What to do (and not to do) with
time-series cross-section data. American Political Science Review 89,
634-647.

Prais, S. J. and Winsten, C. B. (1954): Trend Estimators and Serial
Correlation. Cowles Commission Discussion Paper, 383 (Chicago).

Wooldridge, J. M. (2016). Introductory Econometrics. A Modern Approach.
6th ed. Mason, OH: South-Western Cengage Learning.
