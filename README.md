
# prais

[![CRAN
status](https://www.r-pkg.org/badges/version/prais)](https://cran.r-project.org/package=prais)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/prais)](https://cran.r-project.org/package=prais)
[![Total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/prais)](https://cran.r-project.org/package=prais)
[![R-CMD-check](https://github.com/franzmohr/prais/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/franzmohr/prais/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/franzmohr/prais/graph/badge.svg)](https://app.codecov.io/gh/franzmohr/prais)
[![License: GPL (\>=
2)](https://img.shields.io/badge/license-GPL%20%28%3E%3D%202%29-blue.svg)](https://www.gnu.org/licenses/gpl-2.0)
[![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.22837936-blue.svg)](https://doi.org/10.5281/zenodo.22837936)

[![GitHub
Sponsors](https://img.shields.io/badge/Sponsor-%E2%9D%A4-ea4aaa?logo=github-sponsors&logoColor=white)](https://github.com/sponsors/franzmohr)
[![Buy Me a
Coffee](https://img.shields.io/badge/Buy%20Me%20a%20Coffee-FFDD00?logo=buymeacoffee&logoColor=black)](https://buymeacoffee.com/franzmohr)

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
summary(pw)
```

    ## 
    ## Call:
    ## prais_winsten(formula = lchnimp ~ lchempi + lgas + lrtwex + befile6 + 
    ##     affile6 + afdec6, data = barium, index = "t")
    ## 
    ## Residuals of the transformed model:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.01146 -0.39152  0.06758  0.35063  1.35021 
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

### Panel models estimated with plm

A pooled panel model that was estimated with `plm::plm` can be passed to
`prais_winsten` instead of the formula, the data and the index, which are
taken from the model. Since the Prais-Winsten estimator obtains all its
estimates by ordinary least squares, only models that were estimated with
`model = "pooling"` are supported. Fixed effects can be estimated by adding
the dummy variables to the formula, as in `y ~ x + factor(id)`.

``` r
library(plm)

data("Grunfeld", package = "plm")

pooled <- plm(inv ~ value + capital, data = Grunfeld,
              index = c("firm", "year"), model = "pooling")

summary(prais_winsten(pooled, panelwise = TRUE, rhoweight = "T1"))
```


## Confidence intervals and predictions

`confint` gives confidence intervals for the coefficients. They are
based on the *t* distribution with the residual degrees of freedom of
the model, so they agree with the p-values that `summary` reports.

``` r
confint(pw)
```

    ##                   2.5 %    97.5 %
    ## (Intercept) -82.1607236 8.0090737
    ## lchempi       1.6883996 4.1935273
    ## lgas         -0.8881332 2.9807305
    ## lrtwex        0.1299595 2.1355894
    ## befile6      -0.6486200 0.6156637
    ## affile6      -0.6701089 0.6037933
    ## afdec6       -1.2536974 0.1000749

`vcov` returns the covariance matrix of the coefficients those standard
errors come from. Functions that obtain the covariance matrix from a
model, such as `lmtest::coeftest`, therefore work without being passed a
matrix explicitly.

``` r
sqrt(diag(vcov(pw)))
```

    ## (Intercept)     lchempi        lgas      lrtwex     befile6     affile6 
    ##  22.7784326   0.6328381   0.9773411   0.5066564   0.3193797   0.3218095 
    ##      afdec6 
    ##   0.3419860

`predict` returns the conditional mean of the model. Its standard errors
and confidence intervals describe that mean and come from the same
covariance matrix, so they agree with `confint`. The AR(1) structure of
the errors is not used, which is why intervals for an individual
observation are not available.

``` r
head(predict(pw, interval = "confidence"))
```

    ##           fit      lwr      upr
    ## [1,] 5.362201 5.017814 5.706587
    ## [2,] 5.495592 5.186166 5.805019
    ## [3,] 5.453788 5.151540 5.756037
    ## [4,] 5.612285 5.290419 5.934151
    ## [5,] 5.638605 5.338288 5.938921
    ## [6,] 5.673724 5.334911 6.012538

With `se.fit = TRUE` the standard errors are returned alongside the
predictions, together with the residual degrees of freedom and the
residual standard error.

``` r
fcst <- predict(pw, se.fit = TRUE)

head(fcst$se.fit)
```

    ## [1] 0.1739959 0.1563327 0.1527062 0.1626175 0.1517301 0.1711803

Argument `newdata` predicts for observations that were not part of the
estimation. The variables of the formula do not have to be transformed
beforehand.

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
    ## Residuals of the transformed model:
    ##      Min       1Q   Median       3Q      Max 
    ## -175.949  -17.726   -1.899    8.252  184.278 
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

## Tidy model output

Methods for the generics `tidy`, `glance` and `augment` of package
[broom](https://cran.r-project.org/package=broom) summarise the
coefficients, the goodness of fit and the fitted values in tidy data
frames. They are the interface through which packages for publishable
regression output, such as `gtsummary` and `modelsummary`, obtain the
results of a model. `broom` has to be installed to use them.

``` r
library(broom)

tidy(pw, conf.int = TRUE)
```

    ## # A tibble: 7 × 7
    ##   term        estimate std.error statistic    p.value conf.low conf.high
    ##   <chr>          <dbl>     <dbl>     <dbl>      <dbl>    <dbl>     <dbl>
    ## 1 (Intercept) -37.1       22.8     -1.63   0.106       -82.2       8.01 
    ## 2 lchempi       2.94       0.633    4.65   0.00000846    1.69      4.19 
    ## 3 lgas          1.05       0.977    1.07   0.286        -0.888     2.98 
    ## 4 lrtwex        1.13       0.507    2.24   0.0272        0.130     2.14 
    ## 5 befile6      -0.0165     0.319   -0.0516 0.959        -0.649     0.616
    ## 6 affile6      -0.0332     0.322   -0.103  0.918        -0.670     0.604
    ## 7 afdec6       -0.577      0.342   -1.69   0.0942       -1.25      0.100

``` r
glance(pw)
```

    ## # A tibble: 1 × 11
    ##   r.squared adj.r.squared sigma statistic  p.value    df df.residual  nobs   rho
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>       <int> <int> <dbl>
    ## 1     0.202         0.164 0.573      5.24  7.76e-5     6         124   131 0.293
    ## # ℹ 2 more variables: dw.original <dbl>, dw.transformed <dbl>

`glance` also reports the AR(1) coefficient and the Durbin-Watson
statistics of the original and the transformed model. All three are `NA`
if the model was estimated with `panelwise = TRUE`, because there is one
coefficient per panel in that case.

``` r
augment(pw)
```

    ## # A tibble: 131 × 9
    ##    lchnimp lchempi  lgas lrtwex befile6 affile6 afdec6 .fitted  .resid
    ##      <dbl>   <dbl> <dbl>  <dbl>   <dbl>   <dbl>  <dbl>   <dbl>   <dbl>
    ##  1    5.40    4.61  22.8   4.46       0       0      0    5.36  0.0335
    ##  2    4.55    4.61  22.9   4.45       0       0      0    5.50 -0.944 
    ##  3    5.39    4.62  22.9   4.45       0       0      0    5.45 -0.0631
    ##  4    5.76    4.63  22.9   4.47       0       0      0    5.61  0.148 
    ##  5    4.74    4.65  22.9   4.46       0       0      0    5.64 -0.897 
    ##  6    4.86    4.65  23.0   4.44       0       0      0    5.67 -0.810 
    ##  7    4.70    4.65  23.0   4.41       0       0      0    5.67 -0.969 
    ##  8    3.68    4.66  23.0   4.41       0       0      0    5.65 -1.97  
    ##  9    5.57    4.67  23.0   4.38       0       0      0    5.64 -0.0719
    ## 10    5.16    4.68  23.0   4.41       0       0      0    5.73 -0.572 
    ## # ℹ 121 more rows

## Citation

To cite `prais` in publications, use

``` r
citation("prais")
```

Releases are archived on Zenodo. To cite the package as a whole, rather
than one particular version, use the concept DOI
[10.5281/zenodo.22837936](https://doi.org/10.5281/zenodo.22837936),
which always resolves to the most recent release.

## References

Beck, N. L. and Katz, J. N. (1995): What to do (and not to do) with
time-series cross-section data. American Political Science Review 89,
634-647.

Prais, S. J. and Winsten, C. B. (1954): Trend Estimators and Serial
Correlation. Cowles Commission Discussion Paper, 383 (Chicago).

Wooldridge, J. M. (2016). Introductory Econometrics. A Modern Approach.
6th ed. Mason, OH: South-Western Cengage Learning.
