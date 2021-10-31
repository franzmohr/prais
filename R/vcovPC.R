#' Extract Panel-Corrected Variance Covariance Matrix
#'
#' Panel-corrected covariance matrix estimators for models of class \code{"prais"}.
#'
#' @param x an object of class \code{"prais"}, usually, the result of a call to \code{\link{prais_winsten}}.
#' @param pairwise logical. If \code{FALSE} (default), only those residuals from periods that are common to
#' all panels are used to computed the covariances. If \code{TRUE} all observations that can be matched by
#' period between two panels are used.
#' @param ... not used.
#'
#' @details \code{vcovPC} is a function for estimating a panel-corrected covariance matrix of parameters for
#' the Prais-Winsten estimator.
#'
#' @return An object of class "matrix".
#'
#' @references
#' Beck, N. L. and Katz, J. N. (1995): What to do (and not to do) with time-series cross-section data. American Political Science Review 89, 634-647.
#'
#' @seealso \code{\link[pcse]{vcovPC}}
#' @export
vcovPC.prais <- function(x, pairwise = FALSE, ...) {

  coeffs <- x$coefficients
  if (length(coeffs) > 0) {
    x_names <- names(coeffs)
    rho <- x$rho[NROW(x$rho), ]
    panelwise <- length(rho) > 1
    intercept <- "(Intercept)" %in% names(x$coefficients)

    mt <- x$terms
    mt_model <- x$model
    y_orig <- as.matrix(mt_model[, attributes(mt)$response])
    y_name <- names(mt_model)[attributes(mt)$response]
    dimnames(y_orig) <- list(NULL, y_name)
    x_orig <- stats::model.matrix.default(x$terms, x$model)
    mod <- cbind(y_orig, x_orig)
    index <- x$index
    groups_temp <- unique(mt_model[, index[1]])
    groups <- c()
    for (i in 1:length(groups_temp)){
      pos_temp <- which(mt_model[, index[1]] == groups_temp[i])
      names(pos_temp) <- NULL
      groups <- c(groups, list(pos_temp))
      rm(pos_temp)
    }
    rm(groups_temp)
    n_group <- length(groups)

    pw_data <- .pw_transform(mod, rho = rho, intercept = intercept, groups = groups)
    pw_data <- stats::na.omit(pw_data)

    x_pw <- as.matrix(pw_data[, x_names])
    pw_fit <- x_pw %*% coeffs
    res <- c(pw_data[, 1] - pw_fit)
    cov.unscaled <- solve(crossprod(x_pw))
    n <- length(res)

    positions <- mt_model[, index]
    group_names <- unique(positions[, 1])
    timetable <- unique(positions[, 2])
    timetable <- timetable[order(timetable)]
    timetable <- data.frame(timetable)
    names(timetable) <- index[2]
    for (i in group_names) {
      temp <- data.frame(positions[positions[, 1] == i, 2])
      names(temp) <- index[2]
      temp[, i] <- temp[, index[2]]
      timetable <- merge(timetable, temp, by = index[2], all = TRUE)
      rm(temp)
    }
    timetable <- as.matrix(timetable)
    fulltime <- timetable

    if (!pairwise) {
      timetable <- stats::na.omit(timetable)
    }

    omega <- diag(NA_real_, n_group)
    dimnames(omega) <- list(group_names, group_names)
    for (i in 1:n_group) {
      for (j in i:n_group) {
        pos_i <- positions[, 1] == group_names[i] & positions[, 2] %in% timetable[, group_names[i]]
        pos_j <- positions[, 1] == group_names[j] & positions[, 2] %in% timetable[, group_names[j]]
        temp <- merge(data.frame("time" = positions[pos_i, index[2]], "i" = res[pos_i]),
                      data.frame("time" = positions[pos_j, index[2]], "j" = res[pos_j]),
                      by = "time",
                      all = TRUE)
        temp <- stats::na.omit(temp)
        omega[j, i] <- sum(temp[, "i"] * temp[, "j"]) / nrow(temp)
      }
    }
    low_tri <- omega[lower.tri(omega)]
    omega <- t(omega)
    omega[lower.tri(omega)] <- low_tri
    diag_omega <- diag(NA_real_, n)
    pos <- 0
    for (i in fulltime[, index[2]]) {
      temp <- names(stats::na.omit(fulltime[fulltime[, index[2]] == i, ][-1]))
      ltemp <- length(temp)
      diag_omega[pos + 1:ltemp, pos + 1:ltemp] <- omega[temp, temp]
      pos <- pos + ltemp
    }
    result <- tcrossprod(cov.unscaled, x_pw) %*% diag_omega %*% x_pw %*% cov.unscaled
  } else {
    result <- matrix(NA, 0, 0)
  }
  return(result)
}
