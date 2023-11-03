#' @importFrom generics glance
#' @export
generics::glance

#' @importFrom generics tidy
#' @export
generics::tidy


#' Tidy
#'
#' @param x A `prais` object returned from [prais::prais_winsten()].
#' @param conf.int Logical indicating whether or not to include
#' a confidence interval in the tidied output. Defaults to FALSE.
#' @param conf.level The confidence level to use for the confidence
#' interval if conf.int = TRUE. Must be strictly greater than 0 and less than 1.
#' Defaults to 0.95, which corresponds to a 95 percent confidence interval.
#' @param ... Additional arguments. Not used. Needed to match generic
#'   signature only. **Cautionary note:** Misspelled arguments will be
#'   absorbed in `...`, where they will be ignored. If the misspelled
#'   argument has a default value, the default value will be used.
#'   For example, if you pass `conf.lvel = 0.9`, all computation will
#'   proceed using `conf.level = 0.95`.
#'
#' # load libraries for models and data
#' library(prais)
#' library(wooldridge)
#' data("barium")
#'
#' pw <- prais_winsten(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6,
#' data = barium, index = "t")
#'
#' tidy(pw)
#'
#' glance(pw)
#'
#' @export
tidy.prais <- function(x, conf.int = FALSE, conf.level = .95, ...) {

  co <- summary(x)$coefficients
  ret <- tibble::as_tibble(
    co,
    rownames = "term",
  )
  colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")

  if (conf.int) {
    class(x) <- "lm"
    ci <- stats::confint(x, level = conf.level)
    ci_df <- tibble::tibble(
      term = row.names(ci),
      conf.low = ci[,1],
      conf.high = ci[,2]
    )
    ret <- merge(ret, ci_df, by = "term", sort = FALSE)
  }

  ret
}


#' Glance
#'
#' @inherit tidy.prais params examples
#'
#'
#' @export
#' @seealso [glance()], [prais::prais_winsten()]
#' @family prais_tidiers
glance.prais <- function(x, ...) {
  summary_x <- summary(x)
  class(x) <- "lm"

  if(!is.null(summary_x$fstatistic)){
    stat <- summary_x$fstatistic["value"]
    p.val <- stats::pf(
        summary_x$fstatistic["value"],
        summary_x$fstatistic["numdf"],
        summary_x$fstatistic["dendf"],
        lower.tail = FALSE
    )
  }else{
    stat <- NA_real_
    p.val <- NA_real_
  }

  tibble::tibble(
    rho = unname(utils::tail(x$rho, 1)[,1]),
    r.squared = summary_x$r.squared,
    adj.r.squared = summary_x$adj.r.squared,
    sigma = summary_x$sigma,
    statistic = stat,
    p.value = p.val,
    dw.original = unname(summary_x$dw[1]),
    dw.transformed = unname(summary_x$dw[2]),
    nobs = stats::nobs(x)
  )

}
