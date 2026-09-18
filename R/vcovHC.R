#' Semirobust Covariance Matrix Estimators
#'
#' Semirobust covariance matrix estimators for models of class \code{"prais"}.
#'
#' @param x an object of class \code{"prais"}, usually, the result of a call to \code{\link{prais_winsten}}.
#' @param type a character string specifying the estimation type.
#' @param ... not used.
#'
#' @details \code{vcovHC} is a function for estimating a robust covariance matrix of parameters for
#' the Prais-Winsten estimator. The weighting schemes specified by type are analogous to those in
#' \code{\link[sandwich]{vcovHC}} in package \href{https://cran.r-project.org/package=sandwich}{sandwich}
#' with the caveat that only \code{"const"}, \code{"HC0"} and \code{"HC1"} are available.
#'
#' @return An object of class "matrix" containing the estimate of the asymptotic covariance matrix
#' of coefficients.
#'
#' @seealso \code{\link[sandwich]{vcovHC}}
#' @export
vcovHC.prais <- function(x, type = c("const", "HC1", "HC0"), ...) {
  type <- match.arg(type)

  coeffs <- x$coefficients
  # Coefficients of linearly dependent variables are NA and are omitted, as in
  # the covariance matrix of an object of class 'lm'
  coeffs <- coeffs[!is.na(coeffs)]
  if (length(coeffs) > 0) {
    x_names <- names(coeffs)
    rho <- x$rho[NROW(x$rho), ]
    intercept <- "(Intercept)" %in% names(x$coefficients)

    mt <- x$terms
    mt_model <- x$model
    y_orig <- as.matrix(mt_model[, attributes(mt)$response])
    y_name <- names(mt_model)[attributes(mt)$response]
    dimnames(y_orig) <- list(NULL, y_name)
    x_orig <- stats::model.matrix.default(x$terms, x$model)
    mod <- cbind(y_orig, x_orig)

    groups <- .pw_groups(x, nrow(mod))

    pw_data <- .pw_transform(mod, rho = rho, intercept = intercept, groups = groups)
    pw_data <- stats::na.omit(pw_data)

    rdf <- x$df.residual
    x_pw <- as.matrix(pw_data[, x_names])
    pw_fit <- x_pw %*% coeffs
    res <- c(pw_data[, 1] - pw_fit)
    cov.unscaled <- .pw_cov_unscaled(x_pw)

    n <- nrow(x_pw)
    switch(type,
           const = {omega <- rep(sum(res^2) / rdf, n)},
           HC0 = {omega <- res^2},
           HC1 = {omega <- res^2 * n / rdf})

    # The meat of the sandwich is crossprod(x_pw, diag(omega) %*% x_pw). It is
    # calculated by scaling the rows of x_pw, because an n x n matrix would need
    # a prohibitive amount of memory for larger samples.
    meat <- crossprod(x_pw * sqrt(omega))

    result <- cov.unscaled %*% meat %*% cov.unscaled
  } else {
    result <- matrix(NA, 0, 0)
  }
  return(result)
}
