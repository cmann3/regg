# ============================================================================ #
# CONTENTS                                                                     #
#   - add_test   : add test function to regg object                            #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
# ============================================================================ #


#' Add a Test to \code{regg} Object
#'
#' This function is used to add a test function to a \code{ggr} object. It
#' is intended to be called by the \code{regg} method of the original regression
#' test function.
#'
#' @param x object of class \code{regg} or \code{rgo_model}
#' @param test function for estimating the test, to be applied to the regression model
#' @param ... objects passed to methods
#' @param args a quoted list of function arguments to be passed instead of \code{...}
#'
#' @return the object \code{x}
#' @export
#'
add_test <- function(x, test, ...) UseMethod("add_test")

#' @exportS3Method add_test rgo_model
add_test.rgo_model <- function(x, test, ..., args = NULL){
  if (is.null(args)) args <- eval(substitute(alist(...)))
  if (!is.function(test)) stop("Type Error: test must be a function to be added to the regression model.")
  if (isTRUE(x[["multi_models", inherits = FALSE]])){
    for (mod in x$models) add_test(mod, test, ...)
    return(x)
  }
  if (length(args) > 0){
    fn_call    <- as.call(c(list(stat, as.symbol("x")), args))
    test       <- function(x, ...) x
    body(test) <- fn_call
  }

  if (func_in(test, x$test_fns) || func_in(test, x$default_tests)) return(x)
  x$test_fns <- c(x$test_fns, test)
  if (isTRUE(x[["is_fitted"]]))
    test(x)

  return(x)
}

#' @exportS3Method add_test regg
add_test.regg <- function(x, test, ...){
  for (mod in x$models) add_test(mod, test, ...)
  return(x)
}
