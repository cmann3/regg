# ============================================================================ #
# CONTENTS                                                                     #
#   - se_default : default method for estimating standard errors               #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - get_x    -> get_components.R                                             #
#   - get_mss  -> get_reg_stats.R                                              #
# ============================================================================ #

#' Calculate Default Standard Errors
#'
#' Default function for estimating standard errors, as calculated from the QR
#' matrix, if available.
#'
#' @param x object of class \code{rgo_model} or \code{regg} from which standard errors are calculated.
#' @param ...  Other arguments passed on to methods
#'
#' @return environment of class \code{regg} or \code{rgo_model}
#' @export
#'
se_default <- function(x, ...) UseMethod("se_default")

#' @exportS3Method se_default rgo_model
se_default.rgo_model <- function(x, ...){
  R <- x[["cov.unscaled"]]
  s <- x[["sigma"]]
  if (is.null(R)){
    qr <- x[["qr"]]
    if (is.null(qr)){
      X <- get_x(x)
      R <- solve(crossprod(x))
    } else {
      p <- 1L:qr$rank
      R <- chol2inv(qr$qr[p, p, drop=FALSE])
    }
    x$cov.unscaled <- R
  }
  if (is.null(s)){
    s       <- sqrt(get_rss(x)/get_df(x))
    x$sigma <- s
  }
  x$stats_coef$se <- sqrt(diag(R)) * s
  return(x)
}
