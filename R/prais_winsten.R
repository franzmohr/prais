#' Prais-Winsten Estimation Procedure for AR(1) Serial Correlation
#'
#' The Prais-Winsten estimation procedure takes into account serial correlation of type AR(1) in a linear model.
#' The procedure is an iterative method that recursively estimates the beta coefficients and the error autocorrelation
#' of the specified model until convergence of rho, i.e. the AR(1) coefficient, is attained. All estimates are obtained by OLS.
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param iter an integer specifying the maximum number of iterations.
#' @param tol the maximum (positive) value of the absolute difference between the estimator of rho in the current and the previous iteration that has to be attained to reach convergence and to stop the estimation. If not specified the value 1e-08 is used.
#' @param ... further arguments passed on to \code{lm}.
#'
#' @references
#' Prais, S. J. and Winsten, C. B. (1954): Trend Estimators and Serial Correlation. Cowles Commission Discussion Paper, 383 (Chicago).
#'
#' Wooldridge, J. M. (2013): Introductory Econometrics. A Modern Approach. 5th ed. Mason, OH: South-Western Cengage Learning Cengage.
#'
#'@export
prais_winsten <- function(formula, iter = 50, tol = 1e-6, ...){
  # Estimate the model for the first time with OLS
  lm_temp <- stats::lm(formula = formula, ...)
  mod <- lm_temp$model

  intercept <- "(Intercept)" %in% names(lm_temp$coefficients)
  y_name <- names(mod)[1]
  x_name <- names(mod)[-1]

  n <- nrow(mod)

  res <- lm_temp$res
  res_lag <- c(NA, res[-n])

  pos_t <- 2:n
  pos_t_lag <- 1:(n - 1)

  x_formula <- paste(x_name, collapse = " + ")
  if (intercept) {
    mod <- data.frame(mod[, y_name], 1, mod[, x_name])
    names(mod) <- c(y_name, "const", x_name)
    sample_temp <- mod
    sample_formula <- stats::as.formula(paste(paste(paste(y_name, "~" ), "-1 + const +"), x_formula))
  } else {
    sample_temp <- mod
    sample_formula <- stats::as.formula(paste(paste(paste(y_name, "~" ), "-1 +"), x_formula))
  }

  rho_stats <- matrix(0, 1, 4)
  for (i in 1:iter){
    # Estimate rho
    rho_lm <- stats::lm(res ~ res_lag - 1)
    rho <- rho_lm$coeff[1]
    rho_stats <- rbind(rho_stats, stats::coefficients(summary(rho_lm))["res_lag", ])

    # Rescale variables
    if (abs(rho - rho_stats[NROW(rho_stats) - 1, "Estimate"]) > tol) {
      sample_temp[, y_name] <- c((1 - rho^2)^(1 / 2) * mod[1, y_name], mod[pos_t, y_name] - rho * mod[pos_t_lag, y_name])
      if (intercept) {
        sample_temp[, "const"] <- c((1 - rho^2)^(1 / 2), rep(1 - rho, n - 1))
      }
      sample_temp[1, x_name] <- (1 - rho^2)^(1 / 2) * mod[1, x_name]
      sample_temp[-1, x_name] <- mod[pos_t, x_name] - rho * mod[pos_t_lag, x_name]

      lm_temp <- stats::lm(sample_formula, data = sample_temp)

      fit <- as.matrix(mod[, -1]) %*% lm_temp$coefficients
      res <- mod[, y_name] - fit
      res_lag <- c(NA, res[-n])
    } else {
      break
    }
  }

  lm_temp$residuals[] <- res
  lm_temp$fitted.values[] <- fit
  lm_temp$model <- mod
  s_lm_temp <- stats::summary.lm(lm_temp)
  coefs <- s_lm_temp$coefficients
  if (intercept) {
    dimnames(coefs)[[1]][1] <- "(Intercept)"
    lm_temp$model <- lm_temp$model[, -2]
  }
  rho_stats <- rho_stats[-1, ]
  dimnames(rho_stats)[[1]] <- 1:nrow(rho_stats)

  result <- list("coefficients" = coefs,
                 "residuals" = lm_temp$residuals,
                 "fitted.values" = lm_temp$fitted.values,
                 "rho" = rho_stats,
                 "model" = lm_temp$model)

  return(result)
}
