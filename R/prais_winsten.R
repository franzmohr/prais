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
#' accordingly. The specified variables must not contain \code{NA} values.
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
#' @details Observations with missing values in the variables of \code{formula} are
#' dropped before estimation, as in \code{\link[stats]{lm}}. Since the Prais-Winsten
#' transformation uses the previous observation of a panel, the observations that
#' surround a dropped one are treated as if they were consecutive.
#'
#' Estimates of \eqn{\rho} are bounded to the interval \eqn{[-1, 1]}, because the
#' transformation of the first observation of a panel requires
#' \eqn{(1 - \rho^2)^{(1 / 2)}} to be real. If \eqn{\rho} attains one of those bounds,
#' the first observation of each panel becomes zero and does not contribute to the
#' estimates, so that the estimator effectively becomes the Cochrane-Orcutt estimator.
#'
#' The time variable is only used to order the observations. If it is not equally
#' spaced, a warning is issued, because the observations that surround a gap are
#' treated as if they were consecutive.
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
#' \item{xlevels}{a record of the levels of the factors used in fitting.}
#' \item{contrasts}{the contrasts used, if the model contains factors.}
#' \item{index}{a character specifying the ID and time variables. Only added if
#' panel data were used.}
#'
#' @references
#' Beck, N. L. and Katz, J. N. (1995): What to do (and not to do) with time-series cross-section data. American Political Science Review 89, 634-647.
#'
#' Prais, S. J. and Winsten, C. B. (1954): Trend Estimators and Serial Correlation. Cowles Commission Discussion Paper, 383 (Chicago).
#'
#' Wooldridge, J. M. (2013): Introductory Econometrics. A Modern Approach. 5th ed. Mason, OH: South-Western Cengage Learning.
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

  if (length(max_iter) != 1 || !is.finite(max_iter) || max_iter < 1) {
    stop("Argument 'max_iter' must be a single integer greater than zero.")
  }
  if (length(tol) != 1 || !is.finite(tol) || tol < 0) {
    stop("Argument 'tol' must be a single non-negative number.")
  }

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

  # 'rhoweight' is only used to combine panel-specific estimates of rho
  if (!panelwise) {
    rhoweight <- "none"
  }

  if (length(index) > 0) {
    if (!all(index %in% names(data))) {
      stop("Not all variables specified in argument 'index' are contained in 'data': ",
           paste(index[!index %in% names(data)], collapse = ", "))
    }
    if (anyNA(data[, index])) {
      stop("The variables specified in argument 'index' must not contain NA values.")
    }
    .pw_check_time(data, index, panel)
  }

  if (length(index) == 1) {
    data <- data[order(data[, index]), ]
  }
  if (panel){
    data <- data[order(data[, index[1]]), ]
    data <- data[order(data[, index[2]]), ]
  }

  # 'lm' is called by rebuilding the call, because arguments such as 'subset' and
  # 'weights' are not evaluated in the usual way and cannot be passed on through
  # '...'. 'formula' and 'data' refer to the objects of this function, so that the
  # ordered data are used.
  lm_call <- match.call(expand.dots = TRUE)
  lm_call <- lm_call[!names(lm_call) %in% c("index", "max_iter", "tol", "twostep",
                                            "panelwise", "rhoweight")]
  lm_call[[1L]] <- quote(stats::lm)
  lm_call$formula <- quote(formula)
  lm_call$data <- quote(data)
  # The model frame is needed to obtain the observations that entered the model
  lm_call$model <- TRUE

  # Arguments that another function forwarded through its own dots appear as
  # '..1', '..2' and so on, which cannot be evaluated here. They are replaced by
  # their values, which are available in the frame of the calling function.
  dot_args <- match.call(expand.dots = FALSE)[["..."]]
  for (arg in names(dot_args)) {
    if (nzchar(arg) && is.symbol(dot_args[[arg]]) &&
        grepl("^[.][.][0-9]+$", as.character(dot_args[[arg]]))) {
      lm_call[[arg]] <- eval(dot_args[[arg]], parent.frame())
    }
  }

  lm_temp <- eval(lm_call)

  if (!is.null(lm_temp$weights)) {
    stop("prais_winsten does not support weighted least squares yet.")
  }

  # Without residual degrees of freedom the residuals are zero and rho cannot be
  # obtained from them
  if (lm_temp$df.residual < 1) {
    stop("The model does not have residual degrees of freedom, so the AR(1) coefficient cannot be estimated.")
  }

  # 'lm' omits incomplete observations, so 'data' is reduced to the rows that
  # entered the model. Otherwise row positions obtained from 'data' would not
  # refer to the same observations as the rows of 'mod'. Matching the row names
  # is expensive for larger samples, so it is skipped if the model frame already
  # consists of the same rows in the same order.
  if (!identical(attr(lm_temp$model, "row.names"), attr(data, "row.names"))) {
    data <- data[rownames(lm_temp$model), , drop = FALSE]
  }

  # Argument 'subset' of 'lm' can reorder the observations, which would undo the
  # ordering by 'index' and make the transformation use the wrong lags. The order
  # is therefore restored after the model frame was built. In the usual case the
  # observations are already in the right order and nothing is copied.
  reorder <- NULL
  if (length(index) > 0) {
    if (panel) {
      reorder <- order(data[, index[2]], data[, index[1]])
    } else {
      reorder <- order(data[, index])
    }
    if (is.unsorted(reorder)) {
      data <- data[reorder, , drop = FALSE]
    } else {
      reorder <- NULL
    }
  }

  if (panel){
    group_names <- unique(data[, index[1]])
    n_groups <- length(group_names)
    groups <- .pw_split_groups(data[, index[1]])
  } else {
    groups <- list(seq_len(nrow(data)))
  }

  # A panel-specific rho is obtained from the residuals of a panel and their lag,
  # for which at least two observations are required
  if (panelwise) {
    n_obs <- vapply(groups, length, numeric(1))
    if (any(n_obs < 2)) {
      stop("Panel-specific estimates of rho require at least two observations per panel. ",
           "Too few observations for: ",
           paste(group_names[n_obs < 2], collapse = ", "))
    }
  }

  mt <- lm_temp$terms
  mt_model <- lm_temp$model
  # Keep the factor metadata of the initial fit, because 'lm_temp' is overwritten
  # during the iterations. It is needed to build the model matrix in 'predict'.
  mt_xlevels <- lm_temp$xlevels
  mt_contrasts <- lm_temp$contrasts
  y_orig <- as.matrix(mt_model[, attributes(mt)$response])
  y_name <- names(mt_model)[attributes(mt)$response]
  dimnames(y_orig) <- list(NULL, y_name)
  x_orig <- stats::model.matrix(lm_temp)
  x_name <- dimnames(x_orig)[[2]]
  mod <- cbind(y_orig, x_orig)
  rm(list = c("y_orig", "x_orig"))

  # The model frame follows the order in which 'lm' returned the observations, so
  # it is brought into the same order as 'data'
  if (!is.null(reorder)) {
    terms_model <- attr(mt_model, "terms")
    mt_model <- mt_model[reorder, , drop = FALSE]
    attr(mt_model, "terms") <- terms_model
    mod <- mod[reorder, , drop = FALSE]
  }

  intercept <- "(Intercept)" %in% x_name
  n <- nrow(mod)

  # Calculate residuals of the first estimation. They follow the order in which
  # 'lm' returned the observations, which is not the order of 'mod' if the
  # observations had to be brought back into the order of the index.
  res <- lm_temp$residuals
  if (!is.null(reorder)) {
    res <- res[reorder]
  }
  if (panel) {
    # The positions are obtained with 'lapply' instead of appending to a vector
    # in a loop, which copies the whole vector in every iteration and made the
    # cost grow with the square of the number of panels
    pos_res <- lapply(groups, function(x) {x[-1]})
    pos_res_lag <- lapply(groups, function(x) {x[-length(x)]})
    if (!panelwise) {
      pos_res <- unlist(pos_res, use.names = FALSE)
      pos_res_lag <- unlist(pos_res_lag, use.names = FALSE)
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
    wrho <- lengths(groups)
    if (rhoweight == "T1") {
      wrho <- wrho - 1
    }
    wrho <- wrho / sum(wrho)
  }
  if (twostep) {max_iter <- 1}
  i <- 1
  update <- TRUE
  if (!panelwise) {
    message("Iteration 0: rho = ", round(rho, 4))
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

    if (any(!is.finite(rho))) {
      .pw_no_variation_error()
    }

    sample_temp <- .pw_transform(mod, rho, intercept = intercept, groups = groups)
    sample_temp <- stats::na.omit(sample_temp)
    y_temp <- matrix(sample_temp[, 1], dimnames = list(NULL, y_name))
    x_temp <- matrix(sample_temp[, -1], nrow = nrow(sample_temp), dimnames = list(NULL, x_name))
    lm_temp <- stats::lm.fit(y = y_temp, x = x_temp)

    # Coefficients of linearly dependent variables are NA. They are omitted, so
    # that the fitted values and the residuals do not become NA as well.
    pos_coef <- !is.na(lm_temp$coefficients)
    fit <- as.matrix(mod[, -1])[, pos_coef, drop = FALSE] %*% lm_temp$coefficients[pos_coef]
    res <- mod[, y_name] - fit

    if (!panelwise) {
      message("Iteration ", i, ": rho = ", round(rho, 4))
    }
    i <- i + 1

    # The message must only appear if the iterations were stopped before rho
    # converged
    converged <- all(abs(rho - rho_last) <= tol)
    update <- i <= max_iter & !converged
    if (i > max_iter & !converged & !twostep) {
      message("Estimation was stopped, because the maximum number of iterations was reached.")
    }
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

  # As in 'lm', 'contrasts' is only added if the model contains factors
  result$xlevels <- mt_xlevels
  result$contrasts <- mt_contrasts

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
