# ============================================================================ #
# CONTENTS                                                                     #
#   - find_else      : search for an object in ggreg or evaluate expression    #
#   - fit_find       : fit regg model, then search for object else evaluate    #
#   - length_models  : count number of models                                  #
#   - reg_eval       : evaluate quoted expression for regg arguments           #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - get_data   -> get_data.R                                                 #
#   - reg_select -> reg_select.R                                               #
# ============================================================================ #

#' Find Object in Regression Model Else Evaluate Expression
#'
#' These functions look for an object within a \code{rgo} object. If not found,
#' an expression, \code{expr}, will be evaluated and returned. The result
#' may be written to the object so that it is easily available in the future.
#' \code{fit_find} attempts to fit a regression model first - if it is unfitted -
#' then searches for the object. These functions are intended for \code{regg}
#' developers.
#'
#' @param x object of class \code{rgo} to be searched
#' @param what character vector to search
#' @param expr expression to be evaluated if object is not found
#' @param mode the mode or type of object sought. See details in \code{\link[base]{exists}}.
#' @param inherits should previous models be searched?
#' @param write should the evaluated expression be inserted in \code{x} as \code{what}?
#' @param ... objects passed to \code{\link{fit.rgo_model}}
#'
#' @return R object
#' @export
#'
find_else <- function(x, what, expr, mode = "any", inherits = TRUE, write = TRUE){
  val <- get0(what, envir = x, mode = mode, inherits = inherits, ifnotfound = NULL)
  if (!is.null(val)) return(val)
  val <- eval(expr)
  if (write) x[[what]] <- val
  return(val)
}


#' @rdname find_else
#' @importFrom generics fit
#' @export
fit_find <- function(x, what, expr, mode = "any", inherits = FALSE, write = TRUE, ...){
  x   <- fit(x, ...)
  val <- get0(what, envir = x, mode = mode, inherits = inherits, ifnotfound = NULL)
  if (!is.null(val)) return(val)
  val <- eval(expr)
  if (write) x[[what]] <- val
  return(val)
}


#' Return All Nested Models in \code{rgo} Regression Object
#'
#' @param x object of class \code{rgo_m} to be searched
#'
#' @return list containing objects containing class \code{rgo_m}
#' @export
#'
find_models <- function(x) UseMethod("find_models")

#' @exportS3Method find_models regg
find_models.regg <- function(x){
  m <- Reduce(c, lapply(x$models, find_models))
  if (!is.null(m) && !is.list(m)) return(list(m))
  return(m)
}

#' @exportS3Method find_models rgo_model
find_models.rgo_model <- function(x){
  if (length(x[["models"]]) == 0) return(x)
  Reduce(c, lapply(x$models, find_models))
}


#' Calculate the Number of Models in an \code{rgo} Object
#'
#' @param x object of class \code{rgo} to be searched
#'
#' @return R object
#' @export
#'
length_models <- function(x) UseMethod("length_models")

#' @exportS3Method length_models regg
length_models.regg <- function(x) return(sum(sapply(x[["models"]], length_models)))

#' @exportS3Method length_models rgo_model
length_models.rgo_model <- function(x){
  n <- length(x[["models"]])
  if (n == 0) return(1L)
  return(sum(sapply(x[["models"]], length_models)))
}


#' Evaluate a Expression in \code{regg}
#'
#' This function is intended for developers to evaluate quoted expressions passed
#' to \code{\link{regg}} using \code{\link{reg_select}} semantics.
#'
#' @param x expression to be evaluated
#' @param where \code{rgo} object
#' @param ... objects passed to \code{\link{reg_select}}
#'
#' @return R object
#' @export
#'
reg_eval <- function(x, where, ...){
  if (is.language(x))
    return(reg_select(get_data(where), quoted = x, model = x, ...))
  else
    return(x)
}
