#' Summarising the Prais-Winsten Estimator
#'
#' summary method for class "prais".
#'
#' @param object an object of class "prais", usually, a result of a call to \code{\link{prais.winsten}}.
#'
#' @return a list of summary statistics:
#' \item{call}{}
#' \item{residuals}{}
#' \item{coefficients}{}
#' \item{rho}{}
#' \item{sigma}{}
#' \item{df}{}
#' \item{r.squared}{}
#' \item{adj.r.squared}{}
#' \item{fstatistic}{}
#' \item{cov.unscaled}{}#'
#'
#' @export
summary.prais <- function(object){
  cl <- object$call
  p <- object$rank
  rdf <- object$df.residual
  n <- rdf + p
  r.squared <- object$r.squared
  adj.r.squared <- 1 - ((n - 1) / (n - p)) * (1 - r.squared)
  res <- object$residuals
  rss <- sum(res^2)
  sigma_sq <- rss / rdf
  sigma <- sqrt(sigma_sq)
  mss <- r.squared / (1 - r.squared) * rss

  est <- object$coefficients
  se <- sqrt(diag(object$cov.unscaled))
  tval <- est / se
  coeffs <- cbind(`Estimate` = est,
                  `Std. Error` = se,
                  `t value` = tval,
                  `Pr(>|t|)` = 2 * stats::pt(abs(tval), rdf, lower.tail = FALSE))

  if ("(Intercept)" %in% names(est)) {
    df.int <- p - 1
  } else {
    df.int <- p
  }
  fstatistic <- c(value = (mss / (p - df.int)) / sigma_sq,
                  numdf = p - df.int, dendf = rdf)

  result <- list("call" = cl,
                 "residuals" = res,
                 "coefficients" = coeffs,
                 "rho" = object$rho,
                 "sigma" = sigma,
                 "df" = c(p, rdf),
                 "r.squared" = r.squared,
                 "adj.r.squared" = adj.r.squared,
                 "fstatistic" = fstatistic,
                 "cov.unscaled" = object$cov.unscaled)
  class(result) <- "summary.prais"
  return(result)
}
