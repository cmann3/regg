# ============================================================================ #
# CONTENTS                                                                     #
#   - add_stat   : add statistical function to regg object                     #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
# ============================================================================ #


#' Add a Statistic to \code{regg} Object
#'
#' This function is used to add a statistic function to a \code{ggr} object. It
#' is intended to be called by the \code{regg} method of the original regression
#' statistic function.
#'
#' @param x object of class \code{regg} or \code{rgo_model}
#' @param stat function for estimating the statistic, to be applied to the regression model
#' @param ... objects passed to methods
#'
#' @return the object \code{x}
#' @export
#'
add_stat <- function(x, stat, ...) UseMethod("add_stat")

#' @exportS3Method add_stat rgo_model
add_stat.rgo_model <- function(x, stat, ..., args = NULL){
  if (is.null(args)) args <- eval(substitute(alist(...)))
  if (!is.function(stat)) stop("Type Error: Statistic must be a function to be added to the regression model.")
  if (isTRUE(x[["multi_models", inherits = FALSE]])){
    for (mod in x$models) add_stat(mod, stat, ..., args = args)
    return(x)
  }
  if (length(args) > 0){
    fn_call    <- as.call(c(list(stat, as.symbol("x")), args))
    stat       <- function(x, ...) x
    body(stat) <- fn_call
  }

  if (func_in(stat, x$stat_fns) || func_in(stat, x$default_stats)) return(x)
  x$stat_fns <- c(x$stat_fns, stat)

  if (isTRUE(x[["is_fitted"]]))
    stat(x)

  return(x)
}

#' @exportS3Method add_stat regg
add_stat.regg <- function(x, stat, ...){
  for (mod in x$models) add_stat(mod, stat, ...)
  return(x)
}
