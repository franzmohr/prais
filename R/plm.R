# Recovers the arguments of 'prais_winsten' from a fitted panel model of class
# 'plm'. The formula and the index of the panel are carried by the object
# itself, while the data have to be obtained from the call of the model. See
# '.pw_plm_data' for why the model frame of the object cannot be used instead.
.pw_from_plm <- function(object, parent) {

  if (!requireNamespace("plm", quietly = TRUE)) {
    stop("Package 'plm' must be installed to estimate a model of class 'plm'.")
  }

  # The Prais-Winsten estimator obtains all its estimates by OLS, so that only
  # the pooled model has the form the transformation assumes. The within and the
  # random effects model apply a transformation of their own, which does not
  # leave the AR(1) structure of the errors that rho is estimated from intact.
  model <- object$args$model
  if (!identical(model, "pooling")) {
    stop("The Prais-Winsten estimator is a pooled OLS procedure, so only models ",
         "that were estimated with plm(..., model = \"pooling\") are supported, ",
         "but the model was estimated with model = \"", model, "\". Fixed effects ",
         "can be obtained by adding the dummy variables to the formula, as in ",
         "'y ~ x + factor(id)'.")
  }

  fml <- stats::formula(object)

  # 'plm' allows the instruments of an instrumental variable model to be given in
  # a second part of the formula, which 'lm' has no use for and which would
  # silently become part of the regressors
  if (length(attr(fml, "rhs")) > 1) {
    stop("Formulas with more than one part on the right hand side, such as the ",
         "instruments of an instrumental variable model, are not supported.")
  }

  # 'plm' returns a formula of class 'Formula', which keeps the parts of the
  # formula in attributes. Only the plain formula is passed on, so that the terms
  # of the model are built as they are for any other formula.
  fml <- stats::as.formula(paste(deparse(fml), collapse = " "),
                           env = environment(fml))

  data <- .pw_plm_data(object, parent)

  # A nested panel can have a third index variable, which groups the individuals
  # and does not take part in the transformation
  index <- names(plm::index(object))
  if (length(index) > 2) {
    index <- index[1:2]
  }
  if (!all(index %in% names(data))) {
    stop("Not all index variables of the model are contained in its data: ",
         paste(index[!index %in% names(data)], collapse = ", "))
  }

  # 'pdata.frame' keeps the index as factors, which carry the order of the
  # periods but not their distance, so that the gaps of the panel would be lost
  data[, index[2]] <- .pw_plm_time(data[, index[2]])

  list("formula" = fml, "data" = data, "index" = index)
}

# Obtains the data of a fitted panel model by evaluating the 'data' argument of
# its call. The model frame of the object cannot be used instead, because its
# columns are named after the terms of the formula, so that a term such as
# 'factor(country)' or 'log(x)' could not be evaluated again.
.pw_plm_data <- function(object, parent) {

  data_call <- object$call$data
  if (is.null(data_call)) {
    stop("The call of the model does not contain the data, which are required to ",
         "estimate the model again.")
  }

  # The data are usually found in the environment the model was estimated in,
  # which the formula of the model carries
  env <- environment(stats::formula(object))
  if (!is.environment(env)) {
    env <- parent
  }
  data <- tryCatch(eval(data_call, env), error = function(e) NULL)
  if (!is.data.frame(data) && !identical(env, parent)) {
    data <- tryCatch(eval(data_call, parent), error = function(e) NULL)
  }
  if (!is.data.frame(data)) {
    stop("The data of the model could not be found. Estimating a model of class ",
         "'plm' requires the object that its argument 'data' refers to, in this ",
         "case '", paste(deparse(data_call), collapse = " "), "', to be available, ",
         "because the model frame of a fitted model only contains the evaluated ",
         "terms of its formula.")
  }

  .pw_strip_pseries(as.data.frame(data))
}

# Removes the class and the index attribute that 'pdata.frame' adds to the
# columns of the data, so that the model frame is built from plain vectors.
.pw_strip_pseries <- function(data) {
  for (i in seq_along(data)) {
    if (inherits(data[[i]], "pseries")) {
      x <- data[[i]]
      attr(x, "index") <- NULL
      cl <- setdiff(class(x), "pseries")
      class(x) <- if (length(cl) > 0) cl else NULL
      data[[i]] <- x
    }
  }
  data
}

# Turns the time variable of a 'pdata.frame' back into a number. The periods are
# stored as the levels of a factor, which only order the observations, while the
# transformation needs the distances between the periods to account for the gaps
# of a panel. Variables whose levels are not numbers are left as they are and
# only order the observations, as any other non-numeric time variable does.
.pw_plm_time <- function(time) {
  if (!is.factor(time)) {
    return(time)
  }
  periods <- suppressWarnings(as.numeric(levels(time)))
  if (anyNA(periods)) {
    return(time)
  }
  periods[as.integer(time)]
}
