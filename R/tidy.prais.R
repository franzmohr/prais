#' Tidy a Prais-Winsten Object
#'
#' Summarises the coefficients of a model of class \code{"prais"} in a tidy
#' data frame with one row per coefficient. It is a method for the generic
#' \code{tidy} of package
#' \href{https://cran.r-project.org/package=broom}{broom}, which has to be
#' installed to use it.
#'
#' @param x an object of class \code{"prais"}, usually, a result of a call to
#' \code{\link{prais_winsten}}.
#' @param conf.int logical. If \code{TRUE}, confidence intervals are added to the
#' result. Defaults to \code{FALSE}.
#' @param conf.level the confidence level of the intervals. Defaults to \code{.95}.
#' @param ... not used.
#'
#' @details The estimates, standard errors, test statistics and p-values are those
#' of \code{\link{summary.prais}}. The confidence intervals are based on the
#' \emph{t} distribution with the residual degrees of freedom of the model, so they
#' agree with the reported p-values. Coefficients of linearly dependent variables
#' are \code{NA} and are omitted, as in \code{summary.prais}.
#'
#' @return A \code{\link[tibble]{tibble}} with one row per coefficient and the
#' columns
#' \item{term}{the name of the coefficient.}
#' \item{estimate}{the estimated value of the coefficient.}
#' \item{std.error}{the standard error of the estimate.}
#' \item{statistic}{the \emph{t} statistic of the estimate.}
#' \item{p.value}{the two-sided p-value of the \emph{t} statistic.}
#' \item{conf.low}{(if \code{conf.int = TRUE}) the lower bound of the confidence interval.}
#' \item{conf.high}{(if \code{conf.int = TRUE}) the upper bound of the confidence interval.}
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
#' # Tidy the coefficients
#' if (requireNamespace("broom", quietly = TRUE)) {
#'   broom::tidy(pw, conf.int = TRUE)
#' }
#'
#' @seealso \code{\link{glance.prais}}, \code{\link{augment.prais}}
#' @exportS3Method broom::tidy
tidy.prais <- function(x, conf.int = FALSE, conf.level = .95, ...) {
  coeffs <- summary(x)$coefficients

  # Models without coefficients produce an empty table instead of an error
  if (is.null(coeffs)) {
    coeffs <- matrix(numeric(0), 0, 4,
                     dimnames = list(NULL, c("Estimate", "Std. Error",
                                             "t value", "Pr(>|t|)")))
  }

  term <- rownames(coeffs)
  if (is.null(term)) {
    term <- character(0)
  }

  result <- data.frame("term" = term,
                       "estimate" = unname(coeffs[, "Estimate"]),
                       "std.error" = unname(coeffs[, "Std. Error"]),
                       "statistic" = unname(coeffs[, "t value"]),
                       "p.value" = unname(coeffs[, "Pr(>|t|)"]),
                       stringsAsFactors = FALSE)

  if (conf.int) {
    if (conf.level <= 0 | conf.level >= 1) {
      stop("Argument 'conf.level' must be between 0 and 1.")
    }
    # The quantile of the t distribution is used, so that the intervals agree
    # with the p-values of the summary
    q <- stats::qt(1 - (1 - conf.level) / 2, x$df.residual)
    result$conf.low <- result$estimate - q * result$std.error
    result$conf.high <- result$estimate + q * result$std.error
  }

  return(.as_tibble(result))
}
