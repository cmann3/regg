# ============================================================================ #
# CONTENTS                                                                     #
#   - get_model_matrix   : obtain model matrix (y, X) from rgo_model object    #
#   - get_x              : obtain X matrix from rgo_model object               #
#   - get_y              : obtain y matrix/vector from rgo_model object        #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - find_else  -> devel_helpers.R                                            #
#   - get_data   -> get_data.R                                                 #
#   - reg_select -> reg_select.R                                               #
# ============================================================================ #

#' Obtain & Extract Regression Model Components
#'
#' These functions are intended to be used inside of methods to extract regression
#' components - such as the X or y matrices - for fitting.
#'
#' @param x \code{rgo_model} object
#' @param write should the requested object be inserted in \code{x}, if it not found
#' @param ... objects passed to methods
#'
#' @return the requested object, usually numeric vector or matrix
#' @rdname get_component
#' @name get_component
#'
NULL


#' @describeIn get_component Obtain the cook's distance.
#' @importFrom stats cooks.distance
#' @export
get_cooks <- function(x) cooks.distance(x)

#' @describeIn get_component Obtain the number of degrees of freedom of the residual
#' @export
get_df <- function(x, write = TRUE){
  fit_find(x, "df.residual", {
    q <- x[["qr"]]
    if (is.null(q)){
      X <- get_x(x)
      nrow(X) - ncol(X) # not always correct if singular ...
    } else {
      NROW(q$qr) - q$rank
    }
  }, write = write)
}

#' @describeIn get_component Obtain the regression fitted values
#' @importFrom stats fitted
#' @export
get_fitted <- function(x) fitted(x)

#' @describeIn get_component Obtain the leverage values for the regression.
#' @importFrom stats hatvalues
#' @export
get_hat <- function(x) hatvalues(x)

#' @describeIn get_component Obtain the model matrix associated with \code{\link{reg_select}}.
#' @importFrom stats na.omit
#' @export
get_model_matrix <- function(x, write = TRUE){
  dat    <- get_data(x)
  mm     <- reg_select(dat, quoted = x$selection, numeric = TRUE)
  na_act <- x$na.action
  if (is.null(na_act)) na_act <- na.omit
  if (inherits(mm, "rgo_var_model_list")){
    if (!write) return(lapply(mm, na_act))
    x$models <- list()
    env      <- x
    for (i in seq_along(mm)){
       mi <- structure(
        new.env(parent = env),
        class = class(x)
      )
      env              <- mi
      ma               <- attributes(mm[[i]])
      m                <- na_act(mm[[i]])
      mi$has_intercept <- isTRUE(as.logical(attr(m, "intercept")))
      mi$excluded      <- attr(m, "na.action")
      mi$hide          <- ma$hide
      mi$nrow          <- nrow(m)
      is_y             <- colnames(m) %in% ma$yvar
      mi$y             <- m[,  is_y, drop = TRUE]
      mi$X             <- m[, !is_y, drop = FALSE]
      mi$yvar          <- ma$yvar
      mi$dummies       <- ma$dummies
      x$models[[i]]    <- mi
      x$multi_models   <- TRUE
    }
    return(x$models)
  }
  ma     <- attributes(mm)
  mm     <- na_act(mm)
  if (write){
    ### TODO: Write Z if IV
    x$has_intercept <- isTRUE(as.logical(attr(mm, "intercept")))
    x$excluded      <- attr(mm, "na.action")
    x$hide          <- ma$hide
    x$nrow          <- nrow(mm)
    is_y            <- colnames(mm) %in% ma$yvar
    x$y             <- mm[,  is_y, drop = TRUE]
    x$X             <- mm[, !is_y, drop = FALSE]
    x$yvar          <- ma$yvar
    x$dummies       <- ma$dummies
  }
  return(mm)
}

#' @describeIn get_component Obtain the regression offset
#' @export
get_offset <- function(x, write = TRUE){
  o <- x$offset
  if (is.null(o)) return(NULL)
  if (is.language(o)) o <- reg_eval(o, x, numeric = TRUE, intercept = FALSE)
  return(o)
}

get_qr <- function(x)
  fit_find(x, "qr", NULL, write = FALSE)

#' @describeIn get_component Obtain the regression residuals.
#' @importFrom stats residuals
#' @export
get_resids <- function(x) residuals(x)

#' @describeIn get_component Obtain the vector of weights
#' @export
get_weights <- function(x, write = TRUE){
  w <- x$weights
  if (is.null(w)) return(NULL)
  if (is.language(w)) w <- reg_eval(w, x, numeric = TRUE, intercept = FALSE)
  if (!is.numeric(w)) stop(sprintf("Type Error: regression weights (class = %s) must be numeric.", class(w)[1]))
  if (!is.null(x$excluded) && NROW(w) != x$N){
    if (is.matrix(w)){w <- w[-(x$excluded),]
    } else {w <- w[-(x$excluded)]}
  }
  if (write) x$weights <- w
  return(w)
}

#' @describeIn get_component Obtain the X matrix of independent variables
#' @export
get_x <- function(x, write = TRUE)
  find_else(x, "X", {
    mm   <- get_model_matrix(x, write = write)
    if (is.list(mm)){lapply(mm, function(m){
      is_y <- colnames(m) %in% attr(m, "yvar")
      m[, !is_y, drop = FALSE]
    })} else {
      is_y <- colnames(mm) %in% attr(mm, "yvar")
      mm[, !is_y, drop = FALSE]
    }
  }, write = write)

#' @describeIn get_component Obtain the y matrix or vector of dependent variables
#' @export
get_y <- function(x, write = TRUE)
  find_else(x, "y", {
    mm   <- get_model_matrix(x, write = write)
    if (is.list(mm)){lapply(mm, function(m){
      is_y <- colnames(m) %in% attr(m, "yvar")
      m[, is_y, drop = FALSE]
    })} else {
      is_y <- colnames(mm) %in% attr(mm, "yvar")
      mm[, is_y, drop = FALSE]
    }
  }, write = write)


#' @describeIn get_component Does the model contain an intercept?
#' @export
has_intercept <- function(x, write = TRUE)
  find_else(x, "has_intercept", {
    mm <- get_model_matrix(x, write = write)
    if (is.list(mm)){sapply(mm, function(m) isTRUE(as.logical(attr(m, "intercept"))))
    } else {isTRUE(as.logical(attr(mm, "intercept")))}
  }, write = write)
