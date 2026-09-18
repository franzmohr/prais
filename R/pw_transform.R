
.pw_transform <- function(data, rho, intercept, groups) {
  panelwise <- length(rho) > 1
  j <- 1
  for (i in seq_along(groups)) {
    n_group <- length(groups[[i]])
    if (panelwise) {j <- i}
    if (intercept) {
      data[groups[[i]], 2] <- c((1 - rho[j]^2)^(1 / 2), rep(1 - rho[j], n_group - 1))
      data[groups[[i]][-1], -2] <- data[groups[[i]][-1], -2] - rho[j] * data[groups[[i]][-n_group], -2]
      data[groups[[i]][1], -2] <- (1 - rho[j]^2)^(1 / 2) * data[groups[[i]][1], -2]
      } else {
        data[groups[[i]][-1],] <- data[groups[[i]][-1],] - rho[j] * data[groups[[i]][-n_group],]
        data[groups[[i]][1],] <- (1 - rho[j]^2)^(1 / 2) * data[groups[[i]][1],]
    }
  }

  return(data)
}

# Returns the row positions of the panels of a fitted model. For time series data
# all observations belong to a single group.
.pw_groups <- function(object, n) {
  if (is.null(object$index)) {
    return(list(seq_len(n)))
  }
  .pw_split_groups(object$model[, object$index[1]])
}

# Row positions of every panel. 'split' is used instead of a comparison per panel,
# which would require a pass over all observations for every panel. The levels keep
# the panels in the order in which they appear, which is the order of the estimates
# of rho.
.pw_split_groups <- function(ids) {
  unname(split(seq_along(ids), factor(ids, levels = unique(ids))))
}

# Checks the time variable of every panel. Duplicated periods mean that the
# variables of argument 'index' do not identify the observations, which makes the
# estimates meaningless. Gaps are permitted, but the observations that surround a
# gap are treated as if they were consecutive, which is worth a warning.
.pw_check_time <- function(data, index, panel) {
  time <- data[, index[length(index)]]
  n <- length(time)
  if (n < 2) {
    return(invisible(NULL))
  }

  # The observations are ordered once, so that the periods of a panel are
  # adjacent. Checking every panel on its own would require a pass over all
  # observations for every panel.
  if (panel) {
    ids <- data[, index[1]]
    pos <- order(ids, time)
  } else {
    ids <- rep.int(1L, n)
    pos <- order(time)
  }
  ids <- ids[pos]
  time <- time[pos]

  # TRUE wherever an observation belongs to the same panel as the previous one
  same <- ids[-1L] == ids[-n]

  # A duplicated period appears as two equal periods in a row
  if (any(same & time[-1L] == time[-n])) {
    stop("The variables specified in argument 'index' do not uniquely identify the observations.")
  }

  if (is.numeric(time)) {
    starts <- c(1L, which(!same) + 1L)
    ends <- c(which(!same), n)
    n_obs <- ends - starts + 1L
    keep <- n_obs > 1L
    if (any(keep)) {
      first <- starts[keep]
      # Every difference of a panel has to equal the first difference of that
      # panel. Comparing the span of a panel instead would accept differences
      # such as 2, 1, 3, which are not equally spaced but span 3 times 2.
      steps <- rep(time[first + 1L] - time[first], n_obs[keep] - 1L)
      if (any((time[-1L] - time[-n])[same] != steps)) {
        warning("The time variable is not equally spaced. Observations that surround a gap are treated as if they were consecutive.")
      }
    }
  }

  invisible(NULL)
}

# Raised if the residuals of the model do not vary, so that rho cannot be obtained
# from them. Whether that happens depends on the floating point arithmetic of the
# platform, which is why it is kept in a function of its own.
.pw_no_variation_error <- function() {
  stop("The AR(1) coefficient could not be estimated, because the residuals of the model do not vary.")
}

# Inverse of the cross product of the model matrix, obtained from its QR
# decomposition. Inverting the cross product directly forms the normal equations,
# which squares the condition number of the model matrix and costs accuracy if the
# regressors are close to collinear.
.pw_cov_unscaled <- function(x) {
  qr_x <- qr(x)
  result <- chol2inv(qr.R(qr_x))
  # 'qr' pivots columns if it has to, which the result is reordered for
  pivot <- sort.list(qr_x$pivot)
  result <- result[pivot, pivot, drop = FALSE]
  # 'chol2inv' does not carry the names of the variables, which the callers use
  dimnames(result) <- list(colnames(x), colnames(x))
  result
}
