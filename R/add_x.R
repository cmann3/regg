# ============================================================================ #
# CONTENTS                                                                     #
#   - add_x  : build a regression from the previous by adding more X variables #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - init_regg -> regg_init.R                                                 #
# ============================================================================ #

#' Estimate a New Regression by Adding Independent Variables
#'
#' Builds a new regression model using the regression in \code{x} as the basic template,
#' including the model terms. Unnamed objects passed to \code{add_x} are included
#' as additional independent variables, using grammar from \code{\link{reg_select}}.
#' Variables from the previous regression can be removed with the \code{-} prefix.
#'
#' @param x \code{rgo} object containing a regression model.
#' @param ... If unnamed, variables to be added as independent variables in the
#' new regression using \code{\link{reg_select}}. If named, additional fields
#' passed to \code{\link{regg}}.
#'
#' @return environment of class \code{regg}
#' @export
#'
add_x <- function(x, ...) UseMethod("add_x")

#' @exportS3Method add_x regg
add_x.regg <- function(x, ...){
  n <- length(x$models)
  if (n == 0) stop("SEARCH ERROR: No models found to add X.")
  mod <- x$models[[n]]
  if (length(mod$models) > 0) mod <- mod$models[[length(mod$models)]]
  dots <- eval(substitute(alist(...)))
  if (is.null(names(dots))){
    quoted <- dots
    named  <- list()
  } else {
    w_blank <- names(dots) == ""
    quoted  <- dots[ w_blank]
    named   <- dots[!w_blank]
  }
  quoted  <- c(mod$selection, quoted)
  to_call <- as.call(c(list(as.symbol("init_regg"), x, quoted_terms = quoted,
                            default_stats = mod$default_stats,
                            default_tests = mod$default_tests,
                            class = class(mod)[1]), named))
  eval(to_call)
}
