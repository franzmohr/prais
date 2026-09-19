#' Confidence Intervals for the Coefficients of the Prais-Winsten Estimator
#'
#' Computes confidence intervals for the coefficients of a model of class
#' \code{"prais"}.
#'
#' @param object an object of class \code{"prais"}, usually, a result of a call to
#' \code{\link{prais_winsten}}.
#' @param parm a specification of the coefficients for which the intervals are
#' required. Either a vector of names or a vector of positions. If it is omitted,
#' the intervals of all coefficients are computed.
#' @param level the confidence level of the intervals. Defaults to \code{.95}.
#' @param ... not used.
#'
#' @details The intervals are based on the \emph{t} distribution with the residual
#' degrees of freedom of the model and the standard errors of
#' \code{\link{summary.prais}}, so they agree with the p-values that the summary
#' reports. Without this method the intervals would be obtained by
#' \code{confint.default}, which uses the quantiles of the normal distribution and
#' is therefore too narrow in small samples.
#'
#' Coefficients of linearly dependent variables are \code{NA} and are omitted, as
#' in \code{\link{vcov.prais}}.
#'
#' @return A matrix with a row per coefficient and the lower and upper bound of
#' the interval in its columns, which are named after the corresponding quantiles.
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
#' # Confidence intervals
#' confint(pw)
#' confint(pw, parm = "x", level = .9)
#'
#' @seealso \code{\link{vcov.prais}}, \code{\link{tidy.prais}}
#' @export
confint.prais <- function(object, parm, level = .95, ...) {
  if (level <= 0 | level >= 1) {
    stop("Argument 'level' must be between 0 and 1.")
  }

  # Coefficients of linearly dependent variables are NA and are omitted, as in
  # the covariance matrix
  coeffs <- object$coefficients
  coeffs <- coeffs[!is.na(coeffs)]
  x_names <- names(coeffs)

  if (missing(parm)) {
    parm <- x_names
  } else {
    if (is.numeric(parm)) {
      parm <- x_names[parm]
    }
    if (!all(parm %in% x_names)) {
      stop("Argument 'parm' does not only contain coefficients of the model.")
    }
  }

  a <- (1 - level) / 2
  a <- c(a, 1 - a)
  # The quantile of the t distribution is used, so that the intervals agree with
  # the p values of the summary
  q <- stats::qt(a, object$df.residual)
  se <- sqrt(diag(stats::vcov(object)))[parm]

  result <- coeffs[parm] + se %o% q
  dimnames(result) <- list(parm,
                           paste(format(100 * a, trim = TRUE,
                                        scientific = FALSE, digits = 3), "%"))

  return(result)
}
