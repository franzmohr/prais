#' Prais-Winsten Estimator for AR(1) Serial Correlation
#'
#' The Prais-Winsten estimator takes into account AR(1) serial correlation of the errors
#' in a linear regression model. The procedure recursively estimates the coefficients
#' and the error autocorrelation of the specified model until sufficient convergence of
#' the AR(1) coefficient is reached. All estimates are obtained by OLS.
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class):
#' a symbolic description of the model to be fitted.
#' @param data a data frame containing the variables in the model. If panel data is used,
#' it must also contain the ID and time variables.
#' @param max_iter integer specifying the maximum number of allowed iterations. Default is 50.
#' @param tol numeric specifying the maximum absolute difference between the estimator of \eqn{rho}
#' in the current and the previous iteration that has to be attained to reach convergence.
#' Default is 1e-6.
#' @param twostep logical. If \code{TRUE}, the estimation will stop after the first iteration.
#' @param index a character vector specifying the ID and time variables. If only one variable
#' is provided, it is assumed to be the time variable and the data will be reordered
#' accordingly. If \code{NULL} (default), the function assumes that the provided sample is
#' an ordered time series with the earliest observations in the first row.
#' @param ... arguments passed to \code{\link[stats]{lm}}.
#'
#' @details If \eqn{\rho} takes a value above 1 during the estimation process,
#' the Prais-Winsten transformation cannot be applied to the first
#' observations, because \eqn{(1 - \rho^2)^{(1 / 2)}} is not real. These observations
#' are dropped during the respective iteration and the estimator effectively becomes
#' the Cochrane-Orcutt estimator.
#'
#' @return A list of class "prais" containing the following components:
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
#' pw_sample <- data.frame("x" = x, "y" = 10 + 1.5 * x + u)
#'
#' # Estimate
#' pw <- prais_winsten(y ~ x, data = pw_sample)
#' summary(pw)
#'
#'@export
prais_winsten <- function(formula, data, max_iter = 50L, tol = 1e-6, twostep = FALSE, index = NULL, ...){
  cl <- match.call()
  data <- as.data.frame(data)

  panel <- FALSE
  if (!is.null(index)){
    if (length(index) == 2) {
      panel <- TRUE
    }
  }

  if (length(index) >= 1) {
    data <- data[order(data[, index[2]]), ]
  }
  if (panel){
    data <- data[order(data[, index[1]]), ]
    groups_temp <- unique(data[, index[1]])
    groups <- c()
    for (i in 1:length(groups_temp)){
      pos_temp <- which(data[, index[1]] == groups_temp[i])
      names(pos_temp) <- NULL
      groups <- c(groups, list(pos_temp))
      rm(pos_temp)
    }
    rm(groups_temp)
  }

  lm_temp <- stats::lm(formula = formula, data = data, ...)
  mt <- lm_temp$terms
  mt_model <- lm_temp$model
  if (any(attr(mt, "dataClasses") == "factor")) {
    y_name <- names(mt_model)[1]
    if ("(Intercept)" %in% dimnames(stats::model.matrix(lm_temp))[[2]]) {
      mt_model <- cbind(mt_model[, 1], stats::model.matrix(lm_temp)[, -1])
    } else {
      mt_model <- cbind(mt_model[, 1], stats::model.matrix(lm_temp))
    }
    dimnames(mt_model)[[2]][1] <- y_name
    mt_model <- as.data.frame(mt_model)
  }

  if (any(grepl(":", names(lm_temp$coefficients)))){
    mod_names <- names(mt_model)
    x_names <- names(lm_temp$coefficients)
    inter_term <- which(grepl(":", x_names))
    for (i in inter_term){
      x_name_i <- strsplit(x_names[i], ":")[[1]]
      mt_model <- cbind(mt_model, mt_model[, x_name_i[1]] * mt_model[, x_name_i[2]])
      mod_names <- c(mod_names, x_names[i])
    }
    names(mt_model) <- mod_names
  }

  mod <- as.matrix(mt_model)

  if (!is.null(lm_temp$weights)) {
    stop("prais_winsten does not support weighted least squares yet.")
  }

  intercept <- "(Intercept)" %in% names(lm_temp$coefficients)
  n <- NROW(mod)
  y_name <- dimnames(mod)[[2]][1]
  x_name <- dimnames(mod)[[2]][-1]
  if (intercept) {
    x_name_est <- c("(Intercept)", x_name)
    mod_int <- cbind(mod[, 1], 1, mod[, -1])
    dimnames(mod_int) <- list(NULL, c(y_name, x_name_est))
  } else {
    x_name_est <- x_name
    mod_int <- mod
  }

  if (panel) {
    res <- c()
    res_lag <- res
    for (i in 1:length(groups)){
      n_temp <- length(groups[[i]])
      res_temp <- lm_temp$res[groups[[i]]]
      res <- c(res, res_temp[-1])
      res_lag <- c(res_lag, res_temp[-n_temp])
    }
  } else {
    res <- lm_temp$res[-1]
    res_lag <- lm_temp$res[-n]
    groups <- NULL
  }

  rho_last <- 1000
  rho <- 0
  rho_stats <- c(rho)
  if (twostep) {max_iter <- 1}
  i <- 1
  cat("Iteration 0: rho = ", round(rho, 4), "\n", sep = "")
  while(i <= max_iter & abs(rho - rho_last) > tol) {
    rho_lm <- stats::lm.fit(x = matrix(res_lag), y = matrix(res))
    rho_last <- rho
    rho <- rho_lm$coefficients[1]
    rho_stats <- append(rho_stats, rho)

    sample_temp <- pw_transform(mod, rho, intercept = intercept, groups = groups)
    sample_temp <- stats::na.omit(sample_temp)
    y_temp <- matrix(sample_temp[, y_name], dimnames = list(NULL, y_name))
    x_temp <- matrix(sample_temp[, x_name_est], nrow = NROW(sample_temp), dimnames = list(NULL, x_name_est))
    lm_temp <- stats::lm.fit(y = y_temp, x = x_temp)

    fit <- as.matrix(mod_int[, x_name_est]) %*% lm_temp$coefficients
    res_temp <- mod[, y_name] - fit

    if (panel) {
      res <- c()
      res_lag <- res
      for (j in 1:length(groups)){
        res_temp_p <- res_temp[groups[[j]]]
        n_temp <- length(res_temp_p)
        res <- c(res, res_temp_p[-1])
        res_lag <- c(res_lag, res_temp_p[-n_temp])
      }
    } else {
      res <- res_temp[-1]
      res_lag <- res_temp[-n]
    }

    cat("Iteration ", i, ": rho = ", round(rho, 4), "\n", sep = "")
    i <- i + 1
    if (i - 1 == max_iter & !twostep) {message("Estimation was stopped, because the maximum number of iterations was reached.")}
  }

  rho_stats <- matrix(rho_stats, dimnames = list(0:(length(rho_stats) - 1), "rho"))

  result <- list("coefficients" = lm_temp$coefficients,
                 "rho" = rho_stats,
                 "residuals" = c(res_temp),
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
