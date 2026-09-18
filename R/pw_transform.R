
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

  # 'split' is used instead of a comparison per panel, which would require a pass
  # over all observations for every panel
  if (panel) {
    time_by_panel <- split(time, data[, index[1]])
  } else {
    time_by_panel <- list(time)
  }

  gaps <- FALSE
  for (time_i in time_by_panel) {
    time_i <- sort(time_i)
    if (anyDuplicated(time_i)) {
      stop("The variables specified in argument 'index' do not uniquely identify the observations.")
    }
    if (length(time_i) > 2 && is.numeric(time_i)) {
      steps <- unique(diff(time_i))
      if (length(steps) > 1) {
        gaps <- TRUE
      }
    }
  }

  if (gaps) {
    warning("The time variable is not equally spaced. Observations that surround a gap are treated as if they were consecutive.")
  }

  invisible(NULL)
}

# Raised if the residuals of the model do not vary, so that rho cannot be obtained
# from them. Whether that happens depends on the floating point arithmetic of the
# platform, which is why it is kept in a function of its own.
.pw_no_variation_error <- function() {
  stop("The AR(1) coefficient could not be estimated, because the residuals of the model do not vary.")
}
