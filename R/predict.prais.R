#' Predict Method for Objects of Class prais
#'
#' Predicted values based on Prais-Winsten object.
#'
#' @param object an object of class \code{"prais"}, usually, a result of a call to
#' \code{\link{prais_winsten}}.
#' @param newdata an optional data frame in which to look for variables with with to predict. If omitted, fitted values are used.
#' @param ... further arguments passed to or from other methods.
#'
#' @return A vector of or predictions.
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
#' @rdname prais_winsten
predict.prais <- function(object, ..., newdata = NULL) {

  if (is.null(newdata)) {
    fcst <- object$fitted.values
  } else {

    if (!"data.frame" %in% class(newdata)) {
      stop("Object 'newdata' is not of class data.frame.")
    }

    n <- NROW(newdata)
    vars <- names(object$coefficients)
    data_names <- names(newdata)
    # Add intercept
    if ("(Intercept)" %in% vars & !"(Intercept)" %in% names(newdata)) {
      newdata <- cbind(newdata, rep(1, n))
      names(newdata) <- c(data_names, "(Intercept)")
    }
    if (!all(vars %in% names(newdata))) {
      stop("Object 'newdata' does not contain all variables of the model.")
    }
    newdata <- newdata[, names(object$coefficients)]
    fcst <- c(as.matrix(newdata) %*% matrix(object$coefficients))
  }

  return(fcst)
}
