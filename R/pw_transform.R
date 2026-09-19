
# Row positions that the transformation works on: the first observation of every
# panel, the observations that have a predecessor within their panel, and those
# predecessors. They are derived in one place, because the step counts of the time
# variable have to line up with 'rest' and 'lagged'.
.pw_lag_positions <- function(groups) {
  list(first = vapply(groups, function(x) {x[1L]}, numeric(1)),
       rest = unlist(lapply(groups, function(x) {x[-1L]}), use.names = FALSE),
       lagged = unlist(lapply(groups, function(x) {x[-length(x)]}), use.names = FALSE))
}

# Sum of the geometric series 1 + z + ... + z^(k - 1), which relates the variance
# of the sum of k innovations of an AR(1) process to the variance of one of them.
# The closed form (1 - z^k) / (1 - z) is 0 / 0 if 'z' reaches one, which happens
# when rho attains one of its bounds, so the limit 'k' is used there.
.pw_geom_sum <- function(z, k) {
  z <- rep_len(z, length(k))
  result <- as.numeric(k)
  pos <- abs(1 - z) > 1e-8
  result[pos] <- (1 - z[pos]^k[pos]) / (1 - z[pos])
  result
}

# Applies the Prais-Winsten transformation to all panels at once. Working on one
# panel at a time allocates a copy of the involved rows for every panel, which
# dominated the run time of samples with many panels.
#
# 'steps' gives the number of periods between an observation and its predecessor
# within the same panel, in the order of 'rest'. Two observations k periods apart
# are correlated by rho^k, so the difference is taken against that power and is
# scaled to keep the variance of the transformed errors constant. Both reduce to
# the familiar rho and 1 where the periods are consecutive, so equally spaced data
# are unaffected. As a gap grows, rho^k approaches zero and the scale approaches
# (1 - rho^2)^(1 / 2), so that the observation after a long gap is transformed
# like the first observation of a panel. If 'steps' is NULL, every pair is treated
# as if the periods were consecutive.
.pw_transform <- function(data, rho, intercept, groups, steps = NULL) {
  n_obs <- lengths(groups)
  pos <- .pw_lag_positions(groups)
  first <- pos$first
  rest <- pos$rest
  lagged <- pos$lagged

  # One value of rho per panel, repeated for the observations it applies to
  if (length(rho) > 1) {
    rho_panel <- rho
    rho_rest <- rep(rho, n_obs - 1L)
  } else {
    rho_panel <- rep(rho, length(groups))
    rho_rest <- rep_len(rho, length(rest))
  }
  scale_first <- (1 - rho_panel^2)^(1 / 2)

  if (is.null(steps)) {
    rho_lag <- rho_rest
    scale_rest <- 1
  } else {
    rho_lag <- rho_rest^steps
    scale_rest <- 1 / sqrt(.pw_geom_sum(rho_rest^2, steps))
  }

  # The intercept is transformed to a constant and is not differenced
  columns <- if (intercept) -2L else seq_len(ncol(data))

  # The right hand side is evaluated before it is assigned, and the first
  # observation of a panel is never among 'rest', so the differences use the
  # untransformed values throughout
  data[rest, columns] <- scale_rest * (data[rest, columns, drop = FALSE] -
    rho_lag * data[lagged, columns, drop = FALSE])
  data[first, columns] <- scale_first * data[first, columns, drop = FALSE]

  if (intercept) {
    data[rest, 2] <- scale_rest * (1 - rho_lag)
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

# Step counts of a fitted model, for the functions that repeat the transformation
# on the data of the model. Component 'timeid' is stored by 'prais_winsten' and is
# NULL if the model was estimated without an index, or if the periods could not be
# expressed as multiples of a common step.
.pw_steps <- function(object, groups) {
  if (is.null(object$timeid)) {
    return(NULL)
  }
  pos <- .pw_lag_positions(groups)
  object$timeid[pos$rest] - object$timeid[pos$lagged]
}

# Greatest common divisor of whole numbers by Euclid's algorithm. The values are
# doubles, but '%%' is exact for whole numbers below 2^53, which the differences of
# a time variable stay well under.
.pw_integer_gcd <- function(x) {
  result <- x[1L]
  for (i in seq_along(x)[-1L]) {
    a <- result
    b <- x[i]
    while (b > 0) {
      remainder <- a %% b
      a <- b
      b <- remainder
    }
    result <- a
    # No smaller divisor can follow, so the remaining values cannot change it
    if (result == 1) {
      break
    }
  }
  result
}

# Length of one step, that is the greatest common divisor of the differences of
# the time variable. The differences are floating point numbers, so their
# fractional part is cleared first and the divisor of the resulting whole numbers
# is then exact. Returns NULL if the periods are not multiples of a common step,
# so that their distance cannot be expressed as a count.
.pw_time_unit <- function(diffs, max_denominator = 1000L, tol = 1e-8) {
  # Only the distinct differences matter, which keeps the work independent of the
  # number of observations
  values <- unique(diffs)
  if (any(!is.finite(values)) || any(values <= 0)) {
    return(NULL)
  }

  # A factor that turns the fractional parts into whole numbers turns the whole
  # differences into whole numbers as well, and there are far fewer distinct
  # fractional parts than differences. Periods measured in whole numbers, which is
  # the common case, leave a single part of zero and stop at the first factor.
  parts <- unique(values - floor(values))
  denominator <- NULL
  for (factor in seq_len(max_denominator)) {
    scaled <- parts * factor
    if (all(abs(scaled - round(scaled)) <= tol * pmax(1, abs(scaled)))) {
      denominator <- factor
      break
    }
  }
  if (is.null(denominator)) {
    return(NULL)
  }

  whole <- round(values * denominator)
  if (any(whole < 1)) {
    return(NULL)
  }
  .pw_integer_gcd(whole) / denominator
}

# Expresses the periods as a whole number of steps, counted from the first period
# of the panel an observation belongs to. Counting within the panel keeps the
# result exact if the panels are observed on grids that are shifted against each
# other, and the differences of the result are the distances the transformation
# needs. Returns NULL if the periods do not permit it, in which case the
# observations that surround a gap are treated as if they were consecutive.
.pw_timeid <- function(time, groups) {
  # 'is.numeric' is FALSE for dates, which are the most common time variable
  # after plain numbers
  if (inherits(time, c("Date", "POSIXct", "POSIXlt", "difftime"))) {
    time <- as.numeric(time)
  }
  if (!is.numeric(time)) {
    return(NULL)
  }

  pos <- .pw_lag_positions(groups)
  diffs <- time[pos$rest] - time[pos$lagged]
  if (length(diffs) == 0L) {
    return(NULL)
  }

  unit <- .pw_time_unit(diffs)
  if (is.null(unit)) {
    warning("The periods of the time variable are not multiples of a common step. ",
            "Observations that surround a gap are treated as if they were consecutive.")
    return(NULL)
  }

  # The first period of a panel, repeated for the observations of that panel
  positions <- unlist(groups, use.names = FALSE)
  start <- rep(time[pos$first], lengths(groups))
  timeid <- numeric(length(time))
  timeid[positions] <- round((time[positions] - start) / unit)
  timeid
}

# Checks the time variable of every panel. Duplicated periods mean that the
# variables of argument 'index' do not identify the observations, which makes the
# estimates meaningless.
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
