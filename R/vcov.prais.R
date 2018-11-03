#' @export
vcov.summary.prais <- function(x, ...) {
  return(x$sigma^2 * x$cov.unscaled)
}

#' @export
vcov.prais <- function(x, ...) {
  vcov.summary.prais(summary.prais(x), ...)
}
