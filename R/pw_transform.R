#' Prais-Winsten Transformation
#'
#' Performs the Prais-Winsten transformation for a given data set.
#'
#' @param data a named matrix containing the data that should be transformed.
#' @param rho the AR(1) serial correlation coefficient \eqn{\rho}.
#' @param intercept logal. If TRUE (default) a column of intercept values is added.
#' @param groups a list containing a vector of positions of each
#' ID variable as its elements. Only required for panel data.
#'
#' @return A named matrix.
#'
#' @export
pw_transform <- function(data, rho, intercept = TRUE, groups = NULL) {
  if (is.null(groups)) {
    groups <- list(1:NROW(data))
  }
  interc <- rep(NA, NROW(data))
  for (i in 1:length(groups)) {
    n_group <- length(groups[[i]])
    data[groups[[i]][-1],] <- data[groups[[i]][-1],] - rho * data[groups[[i]][-n_group],]
    data[groups[[i]][1],] <- (1 - rho^2)^(1 / 2) * data[groups[[i]][1],]
    if (intercept) {
      interc[groups[[i]]] <- c((1 - rho^2)^(1 / 2), rep(1 - rho, n_group - 1))
    }
  }
  if (intercept) {
    data <- cbind(data, "(Intercept)" = interc)
  }
  return(data)
}
