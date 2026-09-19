
# Converts the result of a method for a generic of package 'broom' to a tibble,
# which is the object the generics are expected to return. The methods are only
# reachable through the generics, which require 'broom' and therefore 'tibble'.
# A data frame is returned if a method is called directly without 'tibble'.
.as_tibble <- function(x) {
  if (requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(x))
  }
  return(x)
}

# Statistics that are not defined for a model are NULL in an object of class
# 'summary.prais'. They are reported as NA, so that the result of 'glance' always
# has the same columns.
.na_if_null <- function(x) {
  if (is.null(x)) {
    return(NA_real_)
  }
  return(unname(x))
}
