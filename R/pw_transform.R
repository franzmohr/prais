#' Prais-Winsten Transformation
#'
#' Performs the Prais-Winsten transformation for a given data set.
#'
#' @param data a named matrix containing the data, which should be transformed.
#' @param rho the AR(1) serial correlation coefficient \eqn{\rho}.
#' @param intercept logal. If TRUE (default) a column of intercept values is added.
#'
#' @return a named matrix.
#'
#' @export
pw_transform <- function(data, rho, intercept = TRUE) {
  data[-1,] <- data[-1,] - rho * data[-NROW(data),]
  data[1,] <- (1 - rho^2)^(1 / 2) * data[1,]
  if (intercept) {
    data <- cbind(data, "(Intercept)" = c((1 - rho^2)^(1 / 2), rep(1 - rho, NROW(data) - 1)))
  }
  return(data)
}
