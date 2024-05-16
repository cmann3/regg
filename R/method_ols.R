# ============================================================================ #
# CONTENTS                                                                     #
#   - ols : method for estimating linear models                                #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - regg    -> regg.R                                                        #
#   - fit_ols -> fit.ols.R                                                     #
#   - get_components.R :                                                       #
#       get_offset, get_weights, get_x, get_y, has_intercept                   #
# ============================================================================ #

#' Ordinary Least Squares Regression
#'
#' Fit a linear model using the \code{regg} ecosystem; equivalent to \code{\link[stats]{lm}}.
#'
#' @param x a data set, \code{rgo_model_matrix}, or \code{\link{regg}} object
#' @param ... If unnamed, the description of the model terms using \code{\link{reg_select}}.
#' If named, arguments passed to \code{\link{regg}}. See 'details' below.
#'
#'
#' @return object of class 'regg'
#' @export
#'
ols <- function(x, ...) UseMethod("ols")

#' @exportS3Method ols default
ols.default <- function(x, ...) regg(x, ..., method = ols, response = 1,
                                      class = "ols",
                                      default_tests = list(test_t, test_f),
                                      default_stats = list(stat_r2, stat_adj_r2))

#' @exportS3Method ols rgo_model
#' @importFrom stats lm.fit lm.wfit
ols.rgo_model <- function(x, ...){
  X <- get_x(x)
  if (isTRUE(x[["multi_models", inherits = FALSE]])){
    for (mod in x$models) ols(mod)
    return(x)
  }
  y <- get_y(x)
  w <- get_weights(x)
  o <- get_offset(x)
  has_intercept <- has_intercept(x)
  fit_ols(x, X, y, weights = w, offset = o, intercept = has_intercept)
}
