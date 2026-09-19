#' Predict Method for Objects of Class prais
#'
#' Predicted values and their standard errors based on Prais-Winsten object.
#'
#' @details The predictions are the conditional mean of the model, i.e. the product
#' of the regressors and the coefficients. The AR(1) structure of the errors is not
#' used, so the result does not contain the forecast of the serially correlated part
#' of the error term.
#'
#' The standard errors and the intervals describe that conditional mean. They are
#' obtained from the covariance matrix of \code{\link{vcov.prais}}, which comes
#' from the Prais-Winsten transformed model, and the quantiles of the \emph{t}
#' distribution with the residual degrees of freedom of the model, so they agree
#' with the standard errors of \code{\link{summary.prais}} and the intervals of
#' \code{\link{confint.prais}}.
#'
#' Prediction intervals for an individual observation are not available, because
#' they would require an assumption about the error of the predicted period, whose
#' variance depends on the serial correlation and, for a genuine forecast, on the
#' distance to the last observed period. Neither is used by the predictions, as
#' described above.
#'
#' @param object an object of class \code{"prais"}, usually, a result of a call to
#' \code{\link{prais_winsten}}.
#' @param newdata an optional data frame in which to look for variables with which to
#' predict. It must contain the variables that appear in \code{formula}, which do not
#' have to be transformed beforehand. If omitted, the fitted values are used.
#' @param se.fit logical. If \code{TRUE}, the standard errors of the predictions
#' are returned as well. Defaults to \code{FALSE}.
#' @param interval a character specifying the type of interval that is added to the
#' predictions. Either \code{"none"}, which is the default, or \code{"confidence"}
#' for the confidence interval of the conditional mean.
#' @param level the confidence level of the interval. Defaults to \code{.95}.
#' @param ... further arguments passed to or from other methods.
#'
#' @return If \code{se.fit} is \code{FALSE} and \code{interval} is \code{"none"},
#' a vector of predictions. If an interval is requested, a matrix with the columns
#' \code{"fit"}, \code{"lwr"} and \code{"upr"}. If \code{se.fit} is \code{TRUE}, a
#' list containing the following elements:
#' \item{fit}{a vector of predictions or, if an interval was requested, the matrix
#' that is described above.}
#' \item{se.fit}{a vector of the standard errors of the predicted means.}
#' \item{df}{the residual degrees of freedom of the model.}
#' \item{residual.scale}{the square root of the estimated variance of the random
#' error, as in \code{\link{summary.prais}}.}
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
#' # Predict
#' fcst <- predict(pw)
#'
#' # Predictions with standard errors and confidence intervals
#' newdata <- data.frame("x" = c(25, 30, 35))
#' predict(pw, newdata = newdata, se.fit = TRUE)
#' predict(pw, newdata = newdata, interval = "confidence")
#'
#' @references
#'
#' Prais, S. J. and Winsten, C. B. (1954): Trend Estimators and Serial Correlation. Cowles Commission Discussion Paper, 383 (Chicago).
#'
#' @seealso \code{\link{vcov.prais}}, \code{\link{confint.prais}}
#' @export
predict.prais <- function(object, newdata = NULL, se.fit = FALSE,
                          interval = c("none", "confidence"), level = .95, ...) {

  interval <- match.arg(interval)

  if (level <= 0 | level >= 1) {
    stop("Argument 'level' must be between 0 and 1.")
  }

  # Anything beyond the predictions themselves requires the model matrix of the
  # predicted observations, which is not needed for the fitted values
  extra <- se.fit | interval != "none"

  # Coefficients of linearly dependent variables are NA and are omitted, as in
  # the covariance matrix
  coeffs <- object$coefficients
  coeffs <- coeffs[!is.na(coeffs)]
  x_names <- names(coeffs)

  x <- NULL

  if (is.null(newdata)) {

    fit <- object$fitted.values
    if (extra) {
      # The model matrix is built from the model frame, as in 'summary.prais'. It
      # follows the order of the fitted values, because both originate from that
      # frame.
      x <- stats::model.matrix(object$terms, data = object$model)
      x <- x[, x_names, drop = FALSE]
    }

  } else {

    if (!is.data.frame(newdata)) {
      stop("Object 'newdata' is not of class data.frame.")
    }

    mt <- stats::delete.response(object$terms)

    if (!all(all.vars(mt) %in% names(newdata))) {
      stop("Object 'newdata' does not contain all variables of the model.")
    }

    # Objects that were produced by earlier versions of the package do not contain
    # element 'xlevels'. For those the factor levels are obtained from the model
    # frame of the original estimation.
    xlev <- object$xlevels
    if (is.null(xlev)) {
      xlev <- stats::.getXlevels(mt, object$model)
    }

    # Build the model matrix in the same way as during the estimation, so that
    # transformed variables, factors and interactions are treated consistently.
    mf <- stats::model.frame(mt, newdata, na.action = stats::na.pass, xlev = xlev)
    x <- stats::model.matrix(mt, mf, contrasts.arg = object$contrasts)

    if (!all(x_names %in% dimnames(x)[[2]])) {
      stop("The model matrix of 'newdata' does not contain all variables of the model.")
    }
    x <- x[, x_names, drop = FALSE]

    fit <- c(x %*% coeffs)
    names(fit) <- row.names(newdata)
  }

  if (!extra) {
    return(fit)
  }

  # The covariance matrix of the coefficients and the residual standard error are
  # both taken from the summary, which repeats the Prais-Winsten transformation.
  # It is obtained once here, so that the data are not transformed twice.
  s <- summary(object)
  if (is.null(s$cov.unscaled)) {
    # Models without coefficients have no covariance matrix, as in 'vcov.prais'.
    # Their predictions do not depend on estimates, so they have no standard error.
    covm <- matrix(NA_real_, 0, 0)
  } else {
    covm <- s$sigma^2 * s$cov.unscaled
  }

  # The variance of the predicted mean is x' V x, which is the diagonal of X V X'
  # and is obtained without forming that matrix. Rounding can make a variance
  # minimally negative, which would give NaN.
  se <- sqrt(pmax(rowSums((x %*% covm) * x), 0))
  names(se) <- names(fit)

  if (interval != "none") {
    # The quantile of the t distribution is used, so that the intervals agree
    # with those of 'confint.prais'
    q <- stats::qt((1 + level) / 2, object$df.residual)
    fit <- cbind("fit" = fit, "lwr" = fit - q * se, "upr" = fit + q * se)
  }

  if (!se.fit) {
    return(fit)
  }

  return(list("fit" = fit,
              "se.fit" = se,
              "df" = object$df.residual,
              "residual.scale" = s$sigma))
}
