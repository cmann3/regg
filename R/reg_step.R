# ============================================================================ #
# CONTENTS                                                                     #
#   - reg_step : build a series of sequential regressions from X variables     #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - init_regg -> regg_init.R                                                 #
# ============================================================================ #

#' Sequentially Add X Variables to Regression
#'
#' Adds a series of regression models to \code{x}, by sequentially adding the
#' unnamed objects passed to the function.
#'
#' @param x \code{rgo} object containing a regression model.
#' @param ... If unnamed, variables to be added as independent variables in the
#' new regression using \code{\link{reg_select}}. If named, additional fields
#' passed to \code{\link{regg}}.
#'
#' @return environment of class \code{regg}
#' @export
#'
reg_step <- function(x, ...) UseMethod("reg_step")

#' @exportS3Method reg_step regg
reg_step.regg <- function(x, ...){
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
    quoted <- dots[ w_blank]
    named  <- dots[!w_blank]
  }
  if (length(quoted) == 0) stop("LENGTH ERROR: No terms found to be added to the original model.")
  new_quote <- mod$selection
  for (i in seq_along(quoted)){
    new_quote <- c(new_quote, quoted[[i]])
    if (i == 1){
      to_call <- as.call(c(list(as.symbol("init_regg"), x, quoted_terms = new_quote,
                                default_stats = mod$default_stats,
                                default_tests = mod$default_tests,
                                class = class(mod)[1]), named))
      eval(to_call)
    } else {
      init_regg(x, quoted_terms = new_quote,
                default_stats = mod$default_stats,
                default_tests = mod$default_tests, class = class(mod)[1])
    }
  }
  return(x)
}

