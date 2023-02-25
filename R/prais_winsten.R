#' Prais-Winsten Estimator for AR(1) Serial Correlation
#'
#' The Prais-Winsten estimator takes into account AR(1) serial correlation of the errors
#' in a linear regression model. The procedure recursively estimates the coefficients
#' and the error autocorrelation of the specified model until sufficient convergence of
#' the AR(1) coefficient is reached. All estimates are obtained by OLS.
#'
#' @param formula an object of class \code{"formula"} (or one that can be coerced to that class):
#' a symbolic description of the model to be fitted.
#' @param data a data frame containing the variables in the model. If panel data is used,
#' it must also contain the ID and time variables.
#' @param index a character vector specifying the ID and time variables. If only one variable
#' is provided, it is assumed to be the time variable and the data will be reordered
#' accordingly.
#' @param max_iter integer specifying the maximum number of allowed iterations. Default is 50.
#' @param tol numeric specifying the maximum absolute difference between the estimator of \eqn{rho}
#' in the current and the previous iteration that has to be attained to reach convergence.
#' Default is 1e-6.
#' @param twostep logical. If \code{TRUE}, the estimation will stop after the first iteration.
#' @param panelwise logical. If \code{TRUE}, \eqn{\rho} will be calculated for each panel separately.
#' Default is \code{FALSE}. Only used for panel data. See 'Details'.
#' @param rhoweight character specifying how \eqn{\rho} should be calculated if \code{panelwise = TRUE}.
#' See 'Details'.
#' @param ... arguments passed to \code{\link[stats]{lm}}.
#'
#' @details If \eqn{\rho} takes a value above 1 during the estimation process,
#' the Prais-Winsten transformation cannot be applied to the first
#' observations, because \eqn{(1 - \rho^2)^{(1 / 2)}} is not real. These observations
#' are dropped during the respective iteration and the estimator effectively becomes
#' the Cochrane-Orcutt estimator.
#'
#' If \code{panelwise = TRUE}, \code{twostep = FALSE} and \code{rhoweight = "none"},
#' each individual estimate of \eqn{rho} is re-estimated until convergence is achieved for all coefficients.
#'
#' If \code{panelwise = TRUE}, the calculation of \eqn{\rho} can be further specified in argument
#' \code{rhoweight}. If \code{rhoweight = "none"}, \eqn{\rho} is assumed to be panel-specific. If
#' \code{rhoweight = "T"}, \eqn{\rho} is calculated as a weighted mean of panel-specific estimates, where
#' the number of available observations per panel, i.e. \eqn{T_i}, is used as weight. If \code{rhoweight = "T1"},
#' \eqn{\rho} is calculated as a weighted mean of panel-specific estimates, where the number of available
#' observations per panel minus one, i.e. \eqn{T_i - 1}, is used as weight.
#'
#' @return A list of class \code{"prais"} containing the following components:
#' \item{coefficients}{a named vector of coefficients.}
#' \item{rho}{the values of the AR(1) coefficient \eqn{\rho} from all iterations.}
#' \item{residuals}{the residuals, that is the response minus the fitted values.}
#' \item{fitted.values}{the fitted mean values.}
#' \item{rank}{the numeric rank of the fitted linear model.}
#' \item{df.residual}{the residual degrees of freedom.}
#' \item{call}{the matched call.}
#' \item{terms}{the terms object used.}
#' \item{model}{the original model frame, i.e., before the Prais-Winsten transformation.}
#' \item{index}{a character specifying the ID and time variables.}
#'
#' @references
#' Beck, N. L. and Katz, J. N. (1995): What to do (and not to do) with time-series cross-section data. American Political Science Review 89, 634-647.
#'
#' Prais, S. J. and Winsten, C. B. (1954): Trend Estimators and Serial Correlation. Cowles Commission Discussion Paper, 383 (Chicago).
#'
#' Wooldridge, J. M. (2013): Introductory Econometrics. A Modern Approach. 5th ed. Mason, OH: South-Western Cengage Learning Cengage.
#'
#' @examples
#' # Generate an artificial sample
#' set.seed(1234567)
#' n <- 100
#' x <- sample(20:40, n, replace = TRUE)
#' rho <- .5
#'
#' # AR(1) errors
#' u <- rnorm(n, 0, 5)
#' for (i in 2:n) {
#'   u[i] <- u[i] + rho * u[i - 1]
#' }
#' pw_sample <- data.frame("x" = x, "y" = 10 + 1.5 * x + u, "time" = 1:n)
#'
#' # Estimate
#' pw <- prais_winsten(y ~ x, data = pw_sample, index = "time")
#' summary(pw)
#'
#'@export
prais_winsten <- function(formula, data, index, max_iter = 50L, tol = 1e-6,
                          twostep = FALSE, panelwise = FALSE, rhoweight = c("none", "T", "T1"), ...){

  cl <- match.call()
  rhoweight <- match.arg(rhoweight)
  data <- as.data.frame(data)

  panel <- FALSE
  if (!is.null(index)){
    if (length(index) > 2) {
      stop("Argument 'index' can only have up to 2 elements.")
    }
    if (length(index) == 2) {
      panel <- TRUE
    } else {
      panelwise <- FALSE
    }
  }

  if (length(index) == 1) {
    data <- data[order(data[, index]), ]
  }
  if (panel){
    data <- data[order(data[, index[1]]), ]
    data <- data[order(data[, index[2]]), ]
    group_names <- unique(data[, index[1]])
    n_groups <- length(group_names)
    groups <- c()
    for (i in 1:n_groups){
      pos_temp <- which(data[, index[1]] == group_names[i])
      names(pos_temp) <- NULL
      groups <- c(groups, list(pos_temp))
      rm(pos_temp)
    }
  } else {
    groups <- list(1:nrow(data))
  }

  lm_temp <- stats::lm(formula = formula, data = data, ...)

  if (!is.null(lm_temp$weights)) {
    stop("prais_winsten does not support weighted least squares yet.")
  }

  mt <- lm_temp$terms
  mt_model <- lm_temp$model
  y_orig <- as.matrix(mt_model[, attributes(mt)$response])
  y_name <- names(mt_model)[attributes(mt)$response]
  dimnames(y_orig) <- list(NULL, y_name)
  x_orig <- stats::model.matrix(lm_temp)
  x_name <- dimnames(x_orig)[[2]]
  mod <- cbind(y_orig, x_orig)
  rm(list = c("y_orig", "x_orig"))

  intercept <- "(Intercept)" %in% x_name
  n <- nrow(mod)

  # Calculate residuals of the first estimation
  res <- lm_temp$residuals
  if (panel) {
    pos_res <- pos_res_lag <- c()
    if (panelwise) {
      for (i in 1:n_groups){
        n_temp <- length(groups[[i]])
        pos_res <- c(pos_res, list(groups[[i]][-1]))
        pos_res_lag <- c(pos_res_lag, list(groups[[i]][-n_temp]))
      }
    } else {
      for (i in 1:n_groups){
        n_temp <- length(groups[[i]])
        pos_res <- c(pos_res, groups[[i]][-1])
        pos_res_lag <- c(pos_res_lag, groups[[i]][-n_temp])
      }
    }
  } else {
    pos_res <- 2:n
    pos_res_lag <- 1:(n - 1)
  }

  if (panelwise & rhoweight == "none") {
    rho_last <- rep(1000, n_groups)
    rho <- rep(0, n_groups)
    rho_stats <- matrix(0, n_groups, max_iter + 1)
  } else {
    rho_last <- 1000
    rho <- 0
    rho_stats <- c(rho)
  }
  if (rhoweight != "none") {
    wrho <- rep(NA, n_groups)
    for (i in 1:n_groups) {
      wrho[i] <- length(groups[[i]])
    }
    if (rhoweight == "T1") {
      wrho <- wrho - 1
    }
    wrho <- wrho / sum(wrho)
  }
  if (twostep) {max_iter <- 1}
  i <- 1
  update <- TRUE
  if (!panelwise) {
    cat("Iteration 0: rho = ", round(rho, 4), "\n", sep = "")
  }
  while(update) {

    if (panelwise & rhoweight == "none") {
      for (j in 1:n_groups) {
        rho_lm <- stats::lm.fit(x = matrix(res[pos_res_lag[[j]]]), y = matrix(res[pos_res[[j]]]))
        rho_last[j] <- rho[j]
        if (abs(rho_lm$coefficients[1]) > 1) {
          rho[j] <- ifelse(rho_lm$coefficients[1] < -1, -1, 1)
        } else {
          rho[j] <- rho_lm$coefficients[1]
        }
        rho_stats[j, i + 1] <- rho[j]
      }
    } else {
      rho_last <- rho
      if (panelwise) {
        rho <- 0
        for (j in 1:n_groups) {
          rho_lm <- stats::lm.fit(x = matrix(res[pos_res_lag[[j]]]), y = matrix(res[pos_res[[j]]]))
          if (abs(rho_lm$coefficients[1]) > 1) {
            rho_lm$coefficients[1] <- ifelse(rho_lm$coefficients[1] < -1, -1, 1)
          } else {
            rho_lm$coefficients[1] <- rho_lm$coefficients[1]
          }
          rho <- rho + rho_lm$coefficients[1] * wrho[j]
        }
      } else {
        rho_lm <- stats::lm.fit(x = matrix(res[pos_res_lag]), y = matrix(res[pos_res]))
        rho <- rho_lm$coefficients[1]
      }
      rho_stats <- append(rho_stats, rho)
    }

    sample_temp <- .pw_transform(mod, rho, intercept = intercept, groups = groups)
    sample_temp <- stats::na.omit(sample_temp)
    y_temp <- matrix(sample_temp[, 1], dimnames = list(NULL, y_name))
    x_temp <- matrix(sample_temp[, -1], nrow = nrow(sample_temp), dimnames = list(NULL, x_name))
    lm_temp <- stats::lm.fit(y = y_temp, x = x_temp)

    fit <- as.matrix(mod[, -1]) %*% lm_temp$coefficients
    res <- mod[, y_name] - fit

    if (!panelwise) {
      cat("Iteration ", i, ": rho = ", round(rho, 4), "\n", sep = "")
    }
    i <- i + 1
    if (i - 1 == max_iter & !twostep) {message("Estimation was stopped, because the maximum number of iterations was reached.")}

    update <- i <= max_iter & any(abs(rho - rho_last) > tol)
  }

  if (panelwise & rhoweight == "none") {
    rho_stats <- rho_stats[, 1:i]
    rho_stats <- t(rho_stats)
    dimnames(rho_stats) <- list(0:(i - 1), group_names)
  } else {
    rho_stats <- matrix(rho_stats, dimnames = list(0:(length(rho_stats) - 1), "rho"))
  }

  result <- list("coefficients" = lm_temp$coefficients,
                 "rho" = rho_stats,
                 "residuals" = c(res),
                 "rank" = lm_temp$rank,
                 "fitted.values" = c(fit),
                 "df.residual" = lm_temp$df.residual,
                 "call" = cl,
                 "terms" = mt,
                 "qr" = lm_temp$qr,
                 "model" = mt_model)

  if (panel) {
    result$index <- index
    names_mod <- names(result$model)
    if (!index[1] %in% names(result$model)) {
      result$model <- cbind(result$model, data[, index[1]])
      names_mod <- c(names_mod, index[1])
    }
    if (!index[2] %in% names(result$model)) {
      result$model <- cbind(result$model, data[, index[2]])
      names_mod <- c(names_mod, index[2])
    }
    names(result$model) <- names_mod
  }

  class(result) <- "prais"
  return(result)
}
