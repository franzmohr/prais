#' Summarising the Prais-Winsten Estimator
#'
#' summary method for class "prais".
#'
#' @param object an object of class "prais", usually, a result of a call to
#' \code{\link{prais_winsten}}.
#' @param x an object of class "summary.prais", usually, a result of a call to
#' \code{\link{summary.prais}}.
#' @param digits the number of significant digits to use when printing.
#' @param signif.stars logical. If \code{TRUE}, 'significance stars' are printed
#' for each coefficient.
#' @param ... further arguments passed to or from other methods.
#'
#' @return \code{summary.prais} returns a list of class "summary.prais",
#' which contains the following components:
#' \item{call}{the matched call.}
#' \item{residuals}{the residuals, that is the response minus the fitted values.}
#' \item{coefficients}{a named vector of coefficients.}
#' \item{rho}{the values of the AR(1) coefficient \eqn{\rho} from all iterations.}
#' \item{sigma}{the square root of the estimated variance of the random error.}
#' \item{df}{degrees of freedom, a 3-vector \emph{(p, n-p, p*)},
#' the first being the number of non-aliased coefficients, the last being
#' the total number of coefficients.}
#' \item{r.squared}{R^2, the 'fraction of variance explained by the model',
#' \deqn{R^2 = 1 - \frac{\sum {(y_i - \hat{y}_i)^2}}{\sum {(y_i - \overline{y})^2}},}
#' where \eqn{\overline{y}} is the mean of \eqn{y_i} for \eqn{y_i = 1, ..., N}
#' if there is an intercept and zero otherwise.}
#' \item{adj.r.squared}{the above \emph{R^2} statistic \emph{'adjusted'}, penalising
#' for higher \emph{p}.}
#' \item{fstatistic}{(for models including non-intercept terms) a 3-vector with the
#' value of the F-statistic with its numerator and denominator degrees of freedom.}
#' \item{cov.unscaled}{a \eqn{p \times p} matrix of (unscaled) covariances of the
#' \emph{coef[j], j=1, ..., p}.}
#' \item{dw}{a named 2-vector with the Durbin-Watson statistic of the original
#' linear model and the Prais-Winsten estimator.}
#' \item{index}{a character specifying the ID and time variables.}
#'
#' @export
summary.prais <- function(object, ...){
  cl <- object$call

  coeffs <- object$coefficients
  x_names <- names(coeffs)
  rho <- object$rho[NROW(object$rho), "rho"]
  intercept <- "(Intercept)" %in% names(object$coefficients)

  mod <- object$model
  n <- nrow(mod)
  if (is.null(object$index)) {
    panel <- FALSE
    groups <- list(1:n)
  } else {
    index <- object$index
    groups_temp <- unique(mod[, index[1]])
    groups <- c()
    for (i in 1:length(groups_temp)){
      pos_temp <- which(mod[, index[1]] == groups_temp[i])
      names(pos_temp) <- NULL
      groups <- c(groups, list(pos_temp))
      rm(pos_temp)
    }
    rm(groups_temp)
    panel <- TRUE
  }

  if (panel) {
    names_mod <- names(mod)
    names_mod <- c(names_mod[1], names_mod[which(names_mod %in% x_names)])
    mod <- as.data.frame(mod[, names_mod])
    names(mod) <- names_mod
  }

  pw_data <- .pw_transform(mod, rho = rho, intercept = intercept, groups = groups)
  if (intercept) {
    p_int <- 1L
    sst <- sum((pw_data[, 1] - mean(pw_data[, 1], na.rm = TRUE))^2, na.rm = TRUE)
    nam <- names(mod)
    mod <- as.matrix(cbind(mod[, 1], 1, mod[, -1]))
    dimnames(mod) <- list(NULL, c(nam[1], "(Intercept)", nam[-1]))
  } else {
    p_int <- 0L
    sst <- sum(pw_data[, 1]^2)
  }

  x_pw <- as.matrix(pw_data[, x_names])
  pw_fit <- x_pw %*% coeffs
  res_pw <- c(pw_data[, 1] - pw_fit)

  p <- object$rank
  rdf <- object$df.residual
  rss <- sum(res_pw^2, na.rm = TRUE)
  sigma_sq <- rss / rdf
  sigma <- sqrt(sigma_sq)
  cov.unscaled <- NULL
  df <- c(0L, rdf, 0L)
  coeffs <- NULL
  r.squared <- NULL
  adj.r.squared <- NULL
  fstatistic <- NULL
  if (p > 0) {
    cov.unscaled <- solve(crossprod(stats::na.omit(x_pw)))
    dimnames(cov.unscaled) <- list(x_names, x_names)
    df <- c(p, rdf, NCOL(object$qr$qr))
    est <- object$coefficients
    se <- sqrt(diag(cov.unscaled) * sigma_sq)
    tval <- est / se
    coeffs <- cbind(`Estimate` = est,
                    `Std. Error` = se,
                    `t value` = tval,
                    `Pr(>|t|)` = 2 * stats::pt(abs(tval), rdf, lower.tail = FALSE))

    if (rss < sst) {
      mss <- sst - rss
      r.squared <-  mss / sst
      adj.r.squared <- 1 - ((n - p_int) / rdf) * (1 - r.squared)
      fstatistic <- c(value = (mss / (p - p_int)) / sigma_sq,
                      numdf = p - p_int,
                      dendf = rdf)
    }

    res <- stats::lm.fit(y = mod[, 1], x = as.matrix(mod[, -1]))$residuals
  } else {
    res <- mod[, 1]
  }

  d_res <- c()
  d_res_pw <- c()
  if (panel){
    for (i in 1:length(groups)){
      d_res <- c(d_res, diff(res[groups[[i]]]))
      d_res_pw <- c(d_res_pw, diff(res_pw[groups[[i]]]))
    }
  } else {
    d_res <- diff(res)
    d_res_pw <- diff(res_pw)
  }

  dw_orig <- sum(d_res^2) / sum(res^2)
  dw_pw <- sum(d_res_pw^2, na.rm = TRUE) / sum(res_pw^2, na.rm = TRUE)
  dw <- c(original = dw_orig, transformed = dw_pw)

  result <- list("call" = cl,
                 "residuals" = object$residuals,
                 "coefficients" = coeffs,
                 "rho" = object$rho,
                 "sigma" = sigma,
                 "df" = df,
                 "r.squared" = r.squared,
                 "adj.r.squared" = adj.r.squared,
                 "fstatistic" = fstatistic,
                 "cov.unscaled" = cov.unscaled,
                 "dw" = dw)

  class(result) <- "summary.prais"
  return(result)
}
