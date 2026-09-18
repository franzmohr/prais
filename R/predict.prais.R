#' Predict Method for Objects of Class prais
#'
#' Predicted values based on Prais-Winsten object.
#'
#' @details The predictions are the conditional mean of the model, i.e. the product
#' of the regressors and the coefficients. The AR(1) structure of the errors is not
#' used, so the result does not contain the forecast of the serially correlated part
#' of the error term.
#'
#' @param object an object of class \code{"prais"}, usually, a result of a call to
#' \code{\link{prais_winsten}}.
#' @param newdata an optional data frame in which to look for variables with which to
#' predict. It must contain the variables that appear in \code{formula}, which do not
#' have to be transformed beforehand. If omitted, the fitted values are used.
#' @param ... further arguments passed to or from other methods.
#'
#' @return A vector of predictions.
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
#' @references
#'
#' Prais, S. J. and Winsten, C. B. (1954): Trend Estimators and Serial Correlation. Cowles Commission Discussion Paper, 383 (Chicago).
#'
#' @export
predict.prais <- function(object, newdata = NULL, ...) {

  if (is.null(newdata)) {
    return(object$fitted.values)
  }

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

  # Coefficients of linearly dependent variables are NA and are omitted
  coeffs <- object$coefficients
  coeffs <- coeffs[!is.na(coeffs)]
  x_names <- names(coeffs)
  if (!all(x_names %in% dimnames(x)[[2]])) {
    stop("The model matrix of 'newdata' does not contain all variables of the model.")
  }

  fcst <- c(x[, x_names, drop = FALSE] %*% coeffs)
  names(fcst) <- row.names(newdata)

  return(fcst)
}
