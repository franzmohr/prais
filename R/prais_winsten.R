#' Prais-Winsten Estimator for AR(1) Serial Correlation
#'
#' The Prais-Winsten estimator takes into account AR(1) serial correlation of the errors
#' in a linear regression model. The procedure recursively estimates the coefficients
#' and the error autocorrelation of the specified model until sufficient convergence of
#' the AR(1) coefficient is reached. All estimates are obtained by OLS.
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param max_iter integer specifying the maximum number of iterations. The default is 50.
#' @param tol numeric specifying the maximum absolute difference between the estimator of rho in the current and the previous iteration that has to be attained to reach convergence. The default is 1e-6.
#' @param twostep logical. If \code{TRUE}, the estimation will stop after the first iteration.
#' @param ... further arguments passed to or from other methods.
#'
#' @return a list of class "prais" containing the following components:
#' \item{coefficients}{a named vector of coefficients.}
#' \item{rho}{the values of the AR(1) coefficient \eqn{\rho} from all iterations.}
#' \item{residuals}{the residuals, that is the response minus the fitted values.}
#' \item{fitted.values}{the fitted mean values.}
#' \item{rank}{the numeric rank of the fitted linear model.}
#' \item{df.residual}{the residual degrees of freedom.}
#' \item{call}{the matched call.}
#' \item{model}{the original model frame, i.e., before the Prais-Winsten transformation.}
#'
#' @references
#' Prais, S. J. and Winsten, C. B. (1954): Trend Estimators and Serial Correlation. Cowles Commission Discussion Paper, 383 (Chicago).
#'
#' Wooldridge, J. M. (2013): Introductory Econometrics. A Modern Approach. 5th ed. Mason, OH: South-Western Cengage Learning Cengage.
#'
#' @examples
#' # Generate an artificial sample
#' set.seed(1234567)
#' n <- 100
#' x <- sample(20:40, n, replace = TRUE)
#' rho <- .5
#' u <- rnorm(n, 0, 5)
#' for (i in 2:n) {
#'   u[i] <- u[i] + rho * u[i - 1]
#' }
#' pw_sample <- data.frame("x" = x, "y" = 10 + 1.5 * x + u)
#'
#' # Estimate
#' pw <- prais_winsten(y ~ x, data = pw_sample)
#' summary(pw)
#'
#'@export
prais_winsten <- function(formula, max_iter = 50L, tol = 1e-6, twostep = FALSE, ...){
  cl <- match.call()
  lm_temp <- stats::lm(formula = formula, ...)
  #lm_temp <- stats::lm(formula = formula, data = data)
  mod <- as.matrix(lm_temp$model)

  if (!is.null(lm_temp$weights)) {
    stop("prais_winsten does not support weighted least squares yet.")
  }

  n <- NROW(mod)
  intercept <- "(Intercept)" %in% names(lm_temp$coefficients)
  y_name <- dimnames(mod)[[2]][1]
  x_name <- dimnames(mod)[[2]][-1]
  if (intercept) {
    x_name_est <- c("(Intercept)", x_name)
    mod_int <- cbind(mod[, 1], 1, mod[, -1])
    dimnames(mod_int) <- list(NULL, c(y_name, x_name_est))
  } else {
    x_name_est <- x_name
    mod_int <- mod
  }

  res <- lm_temp$res[-1]
  res_lag <- lm_temp$res[-n]

  rho_stats <- c(0)
  rho_last <- 1000
  rho <- 0
  if (twostep) {max_iter <- 1}
  i <- 1
  cat("Iteration ", 0, ": rho = ", round(rho, 4), "\n", sep = "")
  while(i <= max_iter & abs(rho - rho_last) > tol) {
    rho_lm <- stats::lm.fit(x = matrix(res_lag), y = matrix(res))
    rho_last <- rho
    rho <- rho_lm$coefficients[1]
    rho_stats <- append(rho_stats, rho)

    sample_temp <- pw_transform(mod, rho, intercept = intercept)
    y_temp <- matrix(sample_temp[, y_name], dimnames = list(NULL, y_name))
    x_temp <- matrix(sample_temp[, x_name_est], nrow = n, dimnames = list(NULL, x_name_est))
    lm_temp <- stats::lm.fit(y = y_temp, x = x_temp)

    fit <- as.matrix(mod_int[, x_name_est]) %*% lm_temp$coefficients
    res_temp <- mod[, y_name] - fit
    res <- res_temp[-1]
    res_lag <- res_temp[-n]
    cat("Iteration ", i, ": rho = ", round(rho, 4), "\n", sep = "")
    i <- i + 1
    if (i - 1 == max_iter & !twostep) {message("Estimation was stopped, because the maximum number of iterations was reached.")}
  }

  rho_stats <- matrix(rho_stats, dimnames = list(0:(length(rho_stats) - 1), "rho"))

  result <- list("coefficients" = lm_temp$coefficients,
                 "rho" = rho_stats,
                 "residuals" = c(res_temp),
                 "rank" = lm_temp$rank,
                 "fitted.values" = c(fit),
                 "df.residual" = lm_temp$df.residual,
                 "call" = cl,
                 "qr" = lm_temp$qr,
                 "model" = as.data.frame(mod))

  class(result) <- "prais"
  return(result)
}
