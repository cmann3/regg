# ============================================================================ #
# CONTENTS                                                                     #
#   - test_f : perform F-test                                                  #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - as_test                     -> test_helpers.R                            #
#   - get_df, has_intercept       -> get_components.R                          #
#   - get_mss, get_rank, get_rss  -> get_reg_stats.R                           #
# ============================================================================ #

#' Perform F-Test on Regression
#'
#' Estimates the F-statistic and p-values associated with the hypothesis test
#' that all coefficients are equal to 0, unless otherwise specified.
#'
#' @param x object of class \code{rgo_model} or \code{regg}.
#' @param ...  Other arguments passed on to methods
#'
#' @return environment of class \code{regg} or \code{rgo_model}
#' @export
#'
test_f <- function(x, ...) UseMethod("test_f")

#' @exportS3Method test_f regg
test_f.regg <- function(x, ...) add_test(x, test_f, ...)

#' @exportS3Method test_f rgo_model
#' @importFrom stats coef pt
test_f.rgo_model <- function(x, ...){
  fit(x)
  df_num    <- get_rank(x) - has_intercept(x)
  df_den    <- get_df(x)
  stat      <- (get_mss(x)/get_rss(x)) * (df_den / df_num)
  x$tests$f <- as_test(stat  = stat,
                       df    = c(df_num, df_den),
                       pval  = pf(stat, df_num, df_den, lower.tail = FALSE),
                       label = "F Test")
  return(x$tests$f)
}


