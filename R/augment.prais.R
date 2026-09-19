#' Augment Data with Information from a Prais-Winsten Object
#'
#' Adds the fitted values and residuals of a model of class \code{"prais"} to the
#' data it was estimated from, or the predictions for new data. It is a method for
#' the generic \code{augment} of package
#' \href{https://cran.r-project.org/package=broom}{broom}, which has to be installed
#' to use it.
#'
#' @param x an object of class \code{"prais"}, usually, a result of a call to
#' \code{\link{prais_winsten}}.
#' @param data a data frame with the observations the model was estimated from.
#' Defaults to the model frame of the object, which contains the observations in
#' the order of argument \code{index} and without the observations that were dropped
#' because of missing values.
#' @param newdata an optional data frame in which to look for variables with which
#' to predict. If it is specified, the residuals are omitted from the result, because
#' the response is not required for a prediction.
#' @param ... not used.
#'
#' @details The fitted values and residuals are on the scale of the original data,
#' i.e. they are those of the estimated object and not those of the Prais-Winsten
#' transformed model, which \code{\link{summary.prais}} reports. The predictions for
#' \code{newdata} are obtained with \code{\link{predict.prais}} and do not contain a
#' forecast of the serially correlated part of the error term.
#'
#' @return A \code{\link[tibble]{tibble}} that contains \code{data}, or
#' \code{newdata}, and the columns
#' \item{.fitted}{the fitted values, or the predictions for \code{newdata}.}
#' \item{.resid}{(only if \code{newdata} is not specified) the residuals on the
#' scale of the original data.}
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
#' # Add fitted values and residuals
#' if (requireNamespace("broom", quietly = TRUE)) {
#'   broom::augment(pw)
#' }
#'
#' @seealso \code{\link{tidy.prais}}, \code{\link{glance.prais}}
#' @exportS3Method broom::augment
augment.prais <- function(x, data = x$model, newdata = NULL, ...) {

  if (!is.null(newdata)) {
    if (!is.data.frame(newdata)) {
      stop("Object 'newdata' is not of class data.frame.")
    }
    result <- newdata
    result$.fitted <- unname(stats::predict(x, newdata = newdata))
    return(.as_tibble(result))
  }

  if (!is.data.frame(data)) {
    stop("Object 'data' is not of class data.frame.")
  }
  if (NROW(data) != length(x$fitted.values)) {
    stop("Object 'data' does not contain the observations the model was estimated from. It has ",
         NROW(data), " rows, while the model was estimated from ",
         length(x$fitted.values), " observations.")
  }

  result <- data
  result$.fitted <- unname(x$fitted.values)
  result$.resid <- unname(x$residuals)

  return(.as_tibble(result))
}
