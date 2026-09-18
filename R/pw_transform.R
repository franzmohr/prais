
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
  ids <- object$model[, object$index[1]]
  group_names <- unique(ids)
  groups <- vector("list", length(group_names))
  for (i in seq_along(group_names)) {
    pos <- which(ids == group_names[i])
    names(pos) <- NULL
    groups[[i]] <- pos
  }
  groups
}

# Checks the time variable of every panel. Duplicated periods mean that the
# variables of argument 'index' do not identify the observations, which makes the
# estimates meaningless. Gaps are permitted, but the observations that surround a
# gap are treated as if they were consecutive, which is worth a warning.
.pw_check_time <- function(data, index, panel) {
  time <- data[, index[length(index)]]
  ids <- if (panel) data[, index[1]] else rep(1L, length(time))

  gaps <- FALSE
  for (i in unique(ids)) {
    time_i <- sort(time[ids == i])
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
