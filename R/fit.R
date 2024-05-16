# ============================================================================ #
# CONTENTS                                                                     #
#   - fit [methods]   : fit an rgo_model based on the method.                  #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - find_else  -> devel_helpers.R                                            #
#   - ols        -> method_ols.R                                               #
#   - se_default -> se_default.R                                               #
# ============================================================================ #

#' Fit a \code{regg} Regression Model
#'
#' @param x object to be fitted
#' @param refit should the model be fitted again if it is already fitted?
#' @param keep should the \code{X} and \code{y} matrices be kept after fitting?
#' @param ... objects passed to methods
#'
#' @details
#' \code{\link{regg}} and \code{rgo_model} objects are initialized with a flag \code{"is_fitted"}
#' set to \code{FALSE}. \code{ggr_fit} first checks whether this flag is \code{TRUE}.
#' If so, the object is returned, unless the \code{refit} argument is set to \code{TRUE}.
#' If the flag is \code{FALSE}, then the following takes place.
#'
#' First, \code{\link{get_model_matrix}} is run to set the \code{X} and \code{y}
#' variable fields inside of the environment. Next, the method of fitting the
#' model is called on the \code{ggr_model} object and the \code{"is_fitted"} flag
#' is set to \code{TRUE}. Any post-fit functions are applied to model, the standard
#' errors and other regression statistics are calculated, then statistical tests
#' are performed.
#'
#' @return \code{ggr_model} or \code{ggreg} object
#' @rdname fit
#' @name fit
#'
NULL

#' @rdname fit
#' @exportS3Method fit regg
#' @importFrom generics fit
fit.regg <- function(x, ...){
  for (mod in x$models) fit(mod, ...)
  return(x)
}

#' @rdname fit
#' @exportS3Method fit rgo_model
fit.rgo_model <- function(x, refit = FALSE, keep = FALSE, ...){
  if (isTRUE(x[["is_fitted"]]) && !refit) return(x)

  method <- find_else(x, "method", ols, write = FALSE)
  se     <- find_else(x, "se_method", se_default, write = FALSE)

  get_model_matrix(x)
  method(x, ...)
  x$is_fitted <- TRUE

  post <- x$post_fit
  if (is.list(post)){for (p in post) p(x)
  } else if (!is.null(post)) post(x)

  se(x, ...)
  for (stat in x$default_stats) stat(x, ...)
  for (test in x$default_tests) test(x, ...)

  for (stat in x$stat_fns) stat(x, ...)
  for (test in x$test_fns) test(x, ...)

  if (!keep) rm(list = c("X", "y"), envir = x)
  return(x)
}


#' @rdname fit
#' @exportS3Method refit regg
#' @importFrom generics fit refit
refit.regg <- function(x, ...) fit(x, refit = TRUE, ...)

#' @rdname fit
#' @exportS3Method refit rgo_model
#' @importFrom generics fit refit
refit.rgo_model <- function(x, ...) fit(x, refit = TRUE, ...)

