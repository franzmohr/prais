#' @include summary.prais.R
#' @rdname summary.prais
#'
#' @export
print.summary.prais <- function(x, digits = max(3L, getOption("digits") - 3L),
                                signif.stars = getOption("show.signif.stars"), ...){
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  resid <- x$residuals
  df <- x$df
  rdf <- df[2L]
  if (rdf > 5L) {
    cat("Residuals:\n", sep = "")
    nam <- c("Min", "1Q", "Median", "3Q", "Max")
    if (length(dim(resid)) == 2L) {
      rq <- structure(apply(t(resid), 1L, stats::quantile), dimnames = list(nam, dimnames(resid)[[2L]]))
    } else {
      zz <- zapsmall(stats::quantile(resid), digits + 1L)
      rq <- structure(zz, names = nam)
    }
    print(rq, digits = digits, ...)
  }
  rho <- x$rho[, "rho"]
  n_iter <- length(rho) - 1
  rho <- rho[length(rho)]
  cat("\nAR(1) coefficient rho after ", n_iter,
      " Iterations: ", formatC(rho, digits = digits), "\n", sep = "")
  if (length(x$coefficients)) {
    cat("\nCoefficients:\n")
    coefs <- x$coefficients
    stats::printCoefmat(coefs, digits = digits, signif.stars = signif.stars, na.print = "NA", ...)
  } else {
    cat("\nNo coefficients\n")
  }
  cat("\nResidual standard error:",
      format.default(signif(x$sigma, digits)), "on", rdf, "degrees of freedom")
  cat("\n")
  if (!is.null(x$fstatistic)) {
    cat("Multiple R-squared: ", formatC(x$r.squared, digits = digits))
    cat(",\tAdjusted R-squared: ", formatC(x$adj.r.squared, digits = digits),
        "\nF-statistic:", formatC(x$fstatistic[1L], digits = digits),
        "on", x$fstatistic[2L],
        "and", x$fstatistic[3L],
        "DF,  p-value:", format.pval(stats::pf(x$fstatistic[1L], x$fstatistic[2L],
                                               x$fstatistic[3L], lower.tail = FALSE),
                                     digits = digits))
    cat("\n")
  }
  if(!is.null(x$dw)) {
    cat("\nDurbin-Watson statistic (original):", formatC(x$dw["original"], digits = digits),
        "\nDurbin-Watson statistic (transformed):", formatC(x$dw["transformed"], digits = digits))
    cat("\n")
  }
  cat("\n")
  invisible(x)
}
