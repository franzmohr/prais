#' @export
vcovHC.prais <- function(x, type = c("const", "HC0", "HC1"), ...) {
  type <- match.arg(type)

  coeffs <- x$coefficients
  x_names <- names(coeffs)
  rho <- x$rho[NROW(x$rho), "rho"]
  intercept <- "(Intercept)" %in% names(x$coefficients)

  mod <- x$model
  pw_data <- pw_transform(mod, rho = rho, intercept = intercept)
  if (intercept) {
    p_int <- 1L
    sst <- sum((pw_data[, 1] - mean(pw_data[, 1]))^2)
    nam <- names(mod)
    mod <- as.matrix(cbind(mod[, 1], 1, mod[, -1]))
    dimnames(mod) <- list(NULL, c(nam[1], "(Intercept)", nam[-1]))
  } else {
    p_int <- 0L
    sst <- sum(pw_data[, 1]^2)
  }

  n <- NROW(pw_data)
  rdf <- x$df.residual
  x_pw <- as.matrix(pw_data[, x_names])
  pw_fit <- x_pw %*% coeffs
  res <- c(pw_data[, 1] - pw_fit)
  cov.unscaled <- solve(crossprod(x_pw))

  switch(type,
         const = {omega <- rep(sum(res^2) / rdf, n)},
         HC0 = {omega <- res^2},
         HC1 = {omega <- res^2 * n / rdf})

  result <- tcrossprod(cov.unscaled, x_pw) %*% diag(omega, n) %*% x_pw %*% cov.unscaled
  return(result)
}
