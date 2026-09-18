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

  if (is.null(x$index) || length(x$index) < 2) {
    stop("Panel-corrected standard errors require panel data. Use argument 'index' of 'prais_winsten' to specify the ID and the time variable.")
  }

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
    index <- x$index
    groups <- .pw_groups(x, nrow(mod))
    n_group <- length(groups)

    pw_data <- .pw_transform(mod, rho = rho, intercept = intercept, groups = groups)
    pw_data <- stats::na.omit(pw_data)

    x_pw <- as.matrix(pw_data[, x_names])
    pw_fit <- x_pw %*% coeffs
    res <- c(pw_data[, 1] - pw_fit)
    cov.unscaled <- .pw_cov_unscaled(x_pw)

    positions <- mt_model[, index]
    group_names <- as.character(unique(positions[, 1]))
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

    if (!pairwise) {
      timetable <- stats::na.omit(timetable)
      # Without a common period every covariance would be a sum over no
      # observations, which would silently produce NaN
      if (nrow(timetable) == 0) {
        stop("The panels do not have a period in common, so no covariances can be obtained from the periods that are common to all panels. Use 'pairwise = TRUE' to match the observations of two panels by period.")
      }
    }

    omega <- diag(NA_real_, n_group)
    dimnames(omega) <- list(group_names, group_names)
    for (i in seq_len(n_group)) {
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
    # The meat of the sandwich is crossprod(x_pw, omega_full %*% x_pw), where
    # omega_full is block diagonal with one block per period. It is accumulated
    # block by block, because an n x n matrix would need a prohibitive amount of
    # memory for larger samples. The block of a period is indexed by the panels of
    # the observations themselves. Taking the panels from the columns of the time
    # table instead assumed that they appear in the same order, which is not the
    # case if the panels do not all begin in the same period.
    meat <- matrix(0, length(x_names), length(x_names),
                   dimnames = list(x_names, x_names))
    panel_of_obs <- as.character(positions[, 1])
    rows_by_period <- split(seq_len(nrow(x_pw)), positions[, 2])
    for (rows in rows_by_period) {
      x_temp <- x_pw[rows, , drop = FALSE]
      block <- omega[panel_of_obs[rows], panel_of_obs[rows], drop = FALSE]
      meat <- meat + crossprod(x_temp, block %*% x_temp)
    }

    result <- cov.unscaled %*% meat %*% cov.unscaled
  } else {
    result <- matrix(NA, 0, 0)
  }
  return(result)
}
