
# Applies the Prais-Winsten transformation to all panels at once. Working on one
# panel at a time allocates a copy of the involved rows for every panel, which
# dominated the run time of samples with many panels.
.pw_transform <- function(data, rho, intercept, groups) {
  n_obs <- lengths(groups)
  # Positions of the first observation of every panel, of the observations that
  # have a predecessor within their panel, and of those predecessors
  first <- vapply(groups, function(x) {x[1L]}, numeric(1))
  rest <- unlist(lapply(groups, function(x) {x[-1L]}), use.names = FALSE)
  lagged <- unlist(lapply(groups, function(x) {x[-length(x)]}), use.names = FALSE)

  # One value of rho per panel, repeated for the observations it applies to
  if (length(rho) > 1) {
    rho_panel <- rho
    rho_rest <- rep(rho, n_obs - 1L)
  } else {
    rho_panel <- rep(rho, length(groups))
    rho_rest <- rho
  }
  scale_first <- (1 - rho_panel^2)^(1 / 2)

  # The intercept is transformed to a constant and is not differenced
  columns <- if (intercept) -2L else seq_len(ncol(data))

  # The right hand side is evaluated before it is assigned, so the differences use
  # the untransformed values throughout
  data[rest, columns] <- data[rest, columns, drop = FALSE] -
    rho_rest * data[lagged, columns, drop = FALSE]
  data[first, columns] <- scale_first * data[first, columns, drop = FALSE]

  if (intercept) {
    data[unlist(groups, use.names = FALSE), 2] <- rep(1 - rho_panel, n_obs)
    data[first, 2] <- scale_first
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
