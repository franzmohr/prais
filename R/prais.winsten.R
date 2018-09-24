#' Prais-Winsten Estimator for AR(1) Serial Correlation
#'
#' The Prais-Winsten estimator takes into account AR(1) serial correlation of the errors
#' in a linear regression model. The procedure recursively estimates the coefficients
#' and the error autocorrelation of the specified model until sufficient convergence of
#' the AR(1) coefficient is attained. All estimates are obtained by OLS.
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param max_iter integer specifying the maximum number of iterations. The default is 50.
#' @param tol numeric specifying the maximum absolute difference between the estimator of rho in the current and the previous iteration that has to be attained to reach convergence. The default is 1e-6.
#' @param twostep logical. If \code{TRUE}, the estimation will stop after the first iteration.
#' @param ... additional arguments to be passed to \code{lm}.
#'
#' @return a list of class "prais" containing the following components:
#' \item{coefficients}{a named vector of coefficients.}
#' \item{rho}{a named matrix of rho from all iterations of the estimator.}
#' \item{residuals}{the residuals, that is response minus the fitted values, of the last iteration.}
#' \item{rank}{the numeric rank of the fitted linear model.}
#' \item{fitted.values}{the fitted mean values of the last iteration.}
#' \item{df.residual}{the residual degrees of freedom.}
#' \item{call}{the matched call.}
#' \item{cov.unscaled}{the unscaled variance-covariance matrix.}
#' \item{r.squared}{R^2, the 'fraction of variance explained by the model'.}
#' \item{model}{the original model frame before the application of the Prais-Winsten transformation.}
#'
#' @references
#' Prais, S. J. and Winsten, C. B. (1954): Trend Estimators and Serial Correlation. Cowles Commission Discussion Paper, 383 (Chicago).
#'
#' Wooldridge, J. M. (2013): Introductory Econometrics. A Modern Approach. 5th ed. Mason, OH: South-Western Cengage Learning Cengage.
#'
#' @examples
#'
#' # Generate sample
#' set.seed(1234567)
#' n <- 100
#' x <- sample(20:40, n, replace = TRUE)
#' rho <- .9
#' u <- rnorm(n, 0, 5)
#' for (i in 2:n) {
#'   u[i] <- u[i] + rho * u[i - 1]
#' }
#' pw_sample <- data.frame("x" = x, "y" = 10 + 1.5 * x + u)
#'
#' # Estimate
#' pw <- prais.winsten(y ~ x, data = pw_sample)
#' summary(pw)
#'
#'@export
prais.winsten <- function(formula, max_iter = 50L, tol = 1e-6, twostep = FALSE, ...){
  cl <- match.call()
  lm_temp <- stats::lm(formula = formula, ...)
  mod <- lm_temp$model

  intercept <- "(Intercept)" %in% names(lm_temp$coefficients)
  y_name <- names(mod)[1]
  x_name <- names(mod)[-1]
  x_formula <- paste(x_name, collapse = " + ")

  n <- nrow(mod)
  pos_t <- 2:n
  pos_t_lag <- 1:(n - 1)

  res <- lm_temp$res
  res_lag <- c(NA, res[-n])

  if (intercept) {
    mod <- data.frame(mod[, y_name], 1, mod[, x_name])
    names(mod) <- c(y_name, "const", x_name)
    sample_temp <- mod
    sample_formula <- stats::as.formula(paste(paste(paste(y_name, "~" ), "-1 + const +"), x_formula))
  } else {
    sample_temp <- mod
    sample_formula <- stats::as.formula(paste(paste(paste(y_name, "~" ), "-1 +"), x_formula))
  }

  rho_last <- 1000
  rho <- 0
  rho_stats <- c(0)
  if (twostep) {max_iter <- 1}
  i <- 1
  while(i <= max_iter & abs(rho - rho_last) > tol) {
    rho_lm <- stats::lm(res ~ res_lag - 1)
    rho_last <- rho
    rho <- rho_lm$coeff[1]
    rho_stats <- append(rho_stats, rho)

    sample_temp[, y_name] <- c((1 - rho^2)^(1 / 2) * mod[1, y_name], mod[pos_t, y_name] - rho * mod[pos_t_lag, y_name])
    if (intercept) {
      sample_temp[, "const"] <- c((1 - rho^2)^(1 / 2), rep(1 - rho, n - 1))
    }
    sample_temp[1, x_name] <- (1 - rho^2)^(1 / 2) * mod[1, x_name]
    sample_temp[-1, x_name] <- mod[pos_t, x_name] - rho * mod[pos_t_lag, x_name]

    lm_temp <- stats::lm(sample_formula, data = sample_temp)

    fit <- as.matrix(mod[, -1]) %*% lm_temp$coefficients
    res <- mod[, y_name] - fit
    res_lag <- c(NA, res[-n])
    cat("Iteration ", i, ": rho = ", round(rho, 4), "\n", sep = "")
    i <- i + 1
    if (i - 1 == max_iter & !twostep) {message("Estimation was stopped, because the maximum number of iterations was reached.")}
  }

  rank <- ncol(mod) - 1
  df.residual <- n - rank
  coeffs <- lm_temp$coefficients

  y_lm <- lm_temp$mod[, y_name]
  rss <- sum(lm_temp$residuals^2)
  sst <- sum((y_lm - mean(y_lm))^2)
  mss <- sst - rss
  r.squared <-  mss / sst

  if (intercept) {
    x_lm <- as.matrix(lm_temp$mod[, c("const", x_name)])
  } else {
    x_lm <- as.matrix(lm_temp$mod[, x_name])
  }
  cov.unscaled <- rss / df.residual * solve(crossprod(x_lm))

  if (intercept) {
    names(coeffs)[which(names(coeffs) == "const")] <- "(Intercept)"
    pos_intercept <- which(dimnames(cov.unscaled)[[1]] == "const")
    dimnames(cov.unscaled)[[1]][pos_intercept] <- "(Intercept)"
    dimnames(cov.unscaled)[[2]][pos_intercept] <- "(Intercept)"
    mod <- mod[, -which(names(mod) == "const")]
  }
  rho_stats <- matrix(rho_stats, dimnames = list(0:(length(rho_stats) - 1), "rho"))

  if (intercept) {
    x_name <- c("(Intercept)", x_name)
  }

  result <- list("coefficients" = coeffs,
                 "rho" = rho_stats,
                 "residuals" = lm_temp$residuals,
                 "rank" = rank,
                 "fitted.values" = lm_temp$fitted.values,
                 "df.residual" = df.residual,
                 "call" = cl,
                 "cov.unscaled" = cov.unscaled,
                 "r.squared" = r.squared,
                 "model" = mod)
  class(result) <- "prais"
  return(result)
}
