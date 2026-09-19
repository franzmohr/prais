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
#' dropped before estimation, as in \code{\link[stats]{lm}}. The gap that a dropped
#' observation leaves in the time variable is treated like any other gap, which is
#' described below.
#'
#' Estimates of \eqn{\rho} are bounded to the interval \eqn{[-1, 1]}, because the
#' transformation of the first observation of a panel requires
#' \eqn{(1 - \rho^2)^{(1 / 2)}} to be real. If \eqn{\rho} attains one of those bounds,
#' the first observation of each panel becomes zero and does not contribute to the
#' estimates, so that the estimator effectively becomes the Cochrane-Orcutt estimator.
#'
#' The time variable orders the observations and gives the distance between them.
#' Gaps are allowed: two observations of a panel that lie \eqn{k} periods apart have
#' the correlation \eqn{\rho^k} under an AR(1) process, so such an observation is
#' transformed to
#' \deqn{(1 + \rho^2 + ... + \rho^{2(k - 1)})^{-1 / 2} (x_t - \rho^k x_{t - k}),}
#' which is the generalised least squares transformation evaluated at the periods
#' that were observed. For consecutive periods this is the usual
#' \eqn{x_t - \rho x_{t - 1}}, so equally spaced data are unaffected. As a gap grows,
#' \eqn{\rho^k} approaches zero and the factor approaches \eqn{(1 - \rho^2)^{(1 / 2)}},
#' so that the observation after a long gap is transformed like the first observation
#' of a panel.
#'
#' The distances are counted in steps of the smallest length that divides all
#' differences of the time variable, so that a variable measured in, say, quarters
#' or days is handled without further arguments, and so that the estimates do not
#' depend on the unit the periods are expressed in. \code{Date} and \code{POSIXct}
#' variables are supported. If the periods are not multiples of a common step, a
#' warning is issued and the observations that surround a gap are treated as if they
#' were consecutive. The same applies without a warning if the time variable is not
#' numeric, such as a character vector, which only orders the observations and
#' carries no distance between them.
#'
#' The estimate of \eqn{\rho} itself is obtained from the residuals of an
#' observation and its predecessor, whether or not a gap lies between them. Since
#' the correlation across a gap is \eqn{\rho^k} rather than \eqn{\rho}, the estimate
#' is attenuated towards zero if a large share of the observations follows a gap.
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
#' \item{timeid}{the periods of the observations, counted in whole steps from the
#' first period of the panel they belong to. \code{NULL} if no index was given, or
#' if the periods are not multiples of a common step.}
#' \item{x}{the model matrix after the Prais-Winsten transformation. Only added
#' if the data are a single time series.}
#' \item{y}{the response after the Prais-Winsten transformation. Only added if
#' the data are a single time series.}
#'
#' @section Diagnostic tests:
#'
#' The tests of package \code{lmtest}, such as \code{\link[lmtest]{dwtest}},
#' \code{\link[lmtest]{bgtest}} and \code{\link[lmtest]{bptest}}, do not use the
#' residuals of the model they are given. They take the model matrix and the
#' response from the components \code{x} and \code{y} of the object and
#' re-estimate the model by ordinary least squares. Since those components hold
#' the transformed data, the tests describe the estimated model and not the
#' original one. The residuals they obtain are the residuals of
#' \code{\link{summary.prais}}, so that \code{dwtest} reports the Durbin-Watson
#' statistic of the transformed model together with a p-value.
#'
#' For panel data the components are not added, because the tests difference the
#' residuals over all observations at once, which mixes the last observation of a
#' panel with the first observation of the next one. The tests then fall back on
#' the model frame, which holds the original data, and their results do not refer
#' to the estimated model. \code{\link{summary.prais}} reports a Durbin-Watson
#' statistic that respects the panels instead.
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

  # Distance of every observation from its predecessor within the same panel,
  # which the transformation uses to raise rho to that power. Without an index
  # there is no time variable, so the observations are taken to be consecutive.
  timeid <- NULL
  if (length(index) > 0) {
    timeid <- .pw_timeid(data[, index[length(index)]], groups)
  }
  steps <- NULL
  if (!is.null(timeid)) {
    lag_pos <- .pw_lag_positions(groups)
    steps <- timeid[lag_pos$rest] - timeid[lag_pos$lagged]
    # Equally spaced periods leave nothing for the general transformation to do,
    # and skipping it keeps the arithmetic of the common case untouched
    if (all(steps == 1)) {
      steps <- NULL
    }
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

    sample_temp <- .pw_transform(mod, rho, intercept = intercept, groups = groups,
                                 steps = steps)
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

  # 'summary' and the covariance matrices repeat the transformation on the data of
  # the model, for which they need the same distances between the periods. The
  # time variable is not part of the model frame of a time series, so the step
  # counts are carried by the object itself.
  result$timeid <- timeid

  if (panel) {
    result$index <- index
    # The columns are assigned instead of appended with 'cbind', which builds a
    # new data frame and drops the 'terms' attribute of the model frame. Without
    # it the frame is no longer recognised as a model frame, and 'model.matrix'
    # evaluates the variables of the formula against it again, which fails for a
    # term such as 'log(x)' whose source column is not part of the frame.
    for (id in index) {
      if (!id %in% names(result$model)) {
        result$model[[id]] <- data[[id]]
      }
    }
  }

  # The tests of package 'lmtest', such as 'dwtest', 'bgtest' and 'bptest', do not
  # use the residuals of the model they are given. They take the model matrix and
  # the response from components 'x' and 'y' and re-estimate the model by OLS. If
  # those components are absent, both are taken from the model frame, which holds
  # the original data, so that the tests would describe the untransformed model
  # and never see the correction for serial correlation. The transformed data of
  # the final iteration are therefore added. They are omitted for panel data,
  # because the tests difference the residuals over all observations at once,
  # which mixes the last observation of a panel with the first observation of the
  # next one.
  if (!panel) {
    result$x <- x_temp
    result$y <- c(y_temp)
  }

  class(result) <- "prais"
  return(result)
}
