#' Glance at a Prais-Winsten Object
#'
#' Summarises a model of class \code{"prais"} in a tidy data frame with a single
#' row of goodness-of-fit statistics. It is a method for the generic \code{glance}
#' of package \href{https://cran.r-project.org/package=broom}{broom}, which has to
#' be installed to use it.
#'
#' @param x an object of class \code{"prais"}, usually, a result of a call to
#' \code{\link{prais_winsten}}.
#' @param ... not used.
#'
#' @details The statistics are those of \code{\link{summary.prais}}. In addition to
#' the columns that are reported for a linear model, the result contains the AR(1)
#' coefficient and the Durbin-Watson statistics. Both are \code{NA} if the model was
#' estimated with \code{panelwise = TRUE}, because there is one coefficient per panel
#' in that case. The full set of estimates is available as \code{rho} of the estimated
#' object. Statistics that are not defined for a model, such as the F statistic of a
#' model without regressors, are \code{NA}.
#'
#' @return A \code{\link[tibble]{tibble}} with a single row and the columns
#' \item{r.squared}{R^2, the fraction of variance explained by the model.}
#' \item{adj.r.squared}{R^2 adjusted for the number of coefficients.}
#' \item{sigma}{the square root of the estimated variance of the random error.}
#' \item{statistic}{the F statistic of the model.}
#' \item{p.value}{the p-value of the F statistic.}
#' \item{df}{the numerator degrees of freedom of the F statistic.}
#' \item{df.residual}{the residual degrees of freedom.}
#' \item{nobs}{the number of observations that were used for the estimation.}
#' \item{rho}{the final estimate of the AR(1) coefficient \eqn{\rho}.}
#' \item{dw.original}{the Durbin-Watson statistic of the original linear model.}
#' \item{dw.transformed}{the Durbin-Watson statistic of the Prais-Winsten estimator.}
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
#' # Goodness of fit
#' if (requireNamespace("broom", quietly = TRUE)) {
#'   broom::glance(pw)
#' }
#'
#' @seealso \code{\link{tidy.prais}}, \code{\link{augment.prais}}
#' @exportS3Method broom::glance
glance.prais <- function(x, ...) {
  s <- summary(x)

  # Statistics that are not defined for a model are NULL in the summary and are
  # reported as NA, so that the result always has the same columns
  fstat <- s$fstatistic
  if (is.null(fstat)) {
    statistic <- NA_real_
    p.value <- NA_real_
    df <- NA_real_
  } else {
    statistic <- unname(fstat["value"])
    p.value <- stats::pf(fstat["value"], fstat["numdf"], fstat["dendf"],
                         lower.tail = FALSE)
    p.value <- unname(p.value)
    df <- unname(fstat["numdf"])
  }

  # With one coefficient of rho per panel neither rho nor the Durbin-Watson
  # statistic is a single number
  rho <- s$rho[NROW(s$rho), ]
  if (length(rho) > 1) {
    rho <- NA_real_
  }
  dw <- s$dw
  if (is.null(dw)) {
    dw <- c("original" = NA_real_, "transformed" = NA_real_)
  }

  result <- data.frame("r.squared" = .na_if_null(s$r.squared),
                       "adj.r.squared" = .na_if_null(s$adj.r.squared),
                       "sigma" = s$sigma,
                       "statistic" = statistic,
                       "p.value" = p.value,
                       "df" = df,
                       "df.residual" = x$df.residual,
                       "nobs" = NROW(x$model),
                       "rho" = unname(rho),
                       "dw.original" = unname(dw["original"]),
                       "dw.transformed" = unname(dw["transformed"]),
                       stringsAsFactors = FALSE)

  return(.as_tibble(result))
}
