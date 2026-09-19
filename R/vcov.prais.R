#' Covariance Matrix of the Coefficients of the Prais-Winsten Estimator
#'
#' Returns the estimated covariance matrix of the coefficients of a model of
#' class \code{"prais"}.
#'
#' @param object an object of class \code{"prais"}, usually, a result of a call to
#' \code{\link{prais_winsten}}.
#' @param ... not used.
#'
#' @details The covariance matrix is obtained from the Prais-Winsten transformed
#' model. It is the matrix the standard errors of \code{\link{summary.prais}} are
#' based on and is identical to \code{vcovHC(object, type = "const")}. Robust
#' alternatives are available in \code{\link{vcovHC.prais}} and
#' \code{\link{vcovPC.prais}}.
#'
#' Coefficients of linearly dependent variables are \code{NA} and are omitted, as
#' in the covariance matrix of an object of class \code{"lm"}.
#'
#' @return An object of class "matrix" containing the estimate of the covariance
#' matrix of the coefficients.
#'
#' @examples
#' # Generate an artificial sample
#' set.seed(1234567)
#' n <- 100
#' x <- sample(20:40, n, replace = TRUE)
#' rho <- .5
#'
#' # AR(1) errors
#' u <- rnorm(n, 0, 5)
#' for (i in 2:n) {
#'   u[i] <- u[i] + rho * u[i - 1]
#' }
#' pw_sample <- data.frame("x" = x, "y" = 10 + 1.5 * x + u, "time" = 1:n)
#'
#' # Estimate
#' pw <- prais_winsten(y ~ x, data = pw_sample, index = "time")
#'
#' # Covariance matrix and confidence intervals
#' vcov(pw)
#' confint(pw)
#'
#' @seealso \code{\link{vcovHC.prais}}, \code{\link{vcovPC.prais}},
#' \code{\link{confint.prais}}
#' @export
vcov.prais <- function(object, ...) {
  s <- summary(object)

  # Models without coefficients have no covariance matrix, as in 'vcovHC.prais'
  if (is.null(s$cov.unscaled)) {
    return(matrix(NA, 0, 0))
  }

  return(s$sigma^2 * s$cov.unscaled)
}
