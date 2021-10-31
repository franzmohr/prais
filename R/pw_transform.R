#' @export
.pw_transform <- function(data, rho, intercept, groups) {
  panelwise <- length(rho) > 1
  j <- 1
  for (i in 1:length(groups)) {
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
