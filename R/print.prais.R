#' @export
print.prais <- function(x, digits = max(3L, getOption("digits") - 3L), ...){
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  if (length(x$coefficients)) {
    cat("Coefficients:\n")
    print.default(format.default(x$coefficients, digits = digits),
                  print.gap = 2L, quote = FALSE)
  } else {
    cat("No coefficients\n")
  }
  cat("\nAR(1) Coefficient rho: ",
      format.default(c("rho" = x$rho[NROW(x$rho), "rho"]), digits = digits),
      "\n\n", sep = "")
  invisible(x)
}
