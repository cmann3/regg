# ============================================================================ #
# CONTENTS                                                                     #
#   - test_t : calculate t-statistic and p-values for regression coefficients  #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - get_df       -> get_components.R                                         #
#   - se_default   -> se_default.R                                             #
# ============================================================================ #

#' Perform t-Test on Regression Coefficients
#'
#' Estimates the t-statistic and p-values associated with the hypothesis test
#' that each coefficient is equal to 0.
#'
#' @param x object of class \code{rgo_model} or \code{regg} from which standard errors are calculated.
#' @param ...  Other arguments passed on to methods
#'
#' @return environment of class \code{regg} or \code{rgo_model}
#' @export
#'
test_t <- function(x, ...) UseMethod("test_t")

#' @exportS3Method test_t regg
test_t.regg <- function(x, ...) add_test(x, test_t, ...)

#' @exportS3Method test_t rgo_model
#' @importFrom stats coef pt
test_t.rgo_model <- function(x, ...){
  fit(x)
  se <- x$stats_coef$se
  if (is.null(se)){
    if (is.null(x$se)){ se <- se_default(x)
    } else { se <- x$se(x) }
  }
  x$stats_coef$tstat <- coef(x) / se
  x$stats_coef$pval  <- 2 * pt(abs(x$stats_coef$tstat), get_df(x), lower.tail = FALSE)
  return(x)
}


