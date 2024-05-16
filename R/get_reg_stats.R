# ============================================================================ #
# CONTENTS                                                                     #
#   - get_mss     : obtain mean sum of squares                                 #
#   - get_rank    : obtain regression rank (# X vars)                          #
#   - get_rss     : obtain the residual sum of squares                         #
#   - get_se      : obtain the standard errors of the model                    #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - fit_find                     -> devel_helpers.R                          #
#   - get_weights, has_intercept   -> get_components.R                         #
# ============================================================================ #

#' Obtain & Extract Regression Statistics
#'
#' These functions are intended to be used inside of methods to extract regression
#' statistics - such as the residual sum of squares (rss) - for calculating other
#' statistics or tests.
#'
#' @param x \code{rgo_model} object
#' @param write should the requested object be inserted in \code{x}, if it not found
#'
#' @return the requested object, usually numeric vector or matrix
#' @rdname get_stats
#' @name get_stats
#'
NULL


#' @describeIn get_stats Obtain the mean sum of squares
#' @export
#' @importFrom stats fitted
get_mss <- function(x, write = TRUE){
  fit_find(x, "mss", {
    f <- fitted(x)
    w <- get_weights(x)
    if (has_intercept(x)){
      if (is.null(w)){return(sum((f - mean(f))^2))}
      m <- sum(w * f / sum(w))
      return(sum(w * (f - m)^2))
    } else if (is.null(w)) {return(sum(f^2))}
    return(sum(w * f^2))
  }, write = write)
}

#' @describeIn get_stats Obtain the rank of the regression
#' @export
#' @importFrom stats coef
get_rank <- function(x, write = TRUE){
  fit_find(x, "rank", {
    if (exists("qr", x, inherits = FALSE)) return(x$qr$rank)
    NROW(coef(x))
  }, write = write)
}

#' @describeIn get_stats Obtain the sum of squared residuals
#' @export
#' @importFrom stats resid
get_rss <- function(x, write = TRUE){
  fit_find(x, "rss", {
    r <- resid(x)
    w <- get_weights(x)
    if (is.null(w)) return(sum(r^2))
    return(sum(w * r^2))
  }, write = write)
}


#' @describeIn get_stats Obtain the standard errors for the model coefficients
#' @export
#' @importFrom generics fit
#' @importFrom stats resid
get_se <- function(x, write = TRUE){
  fit(x)
  se   <- x[["stats_coef"]]$se
  if (!is.null(se)) return(se)
  meth <- find_else(x, "se_method", se_default, write = FALSE)
  if (is.null(meth)) meth <- se_default
  meth(x)
  se   <- x[["stats_coef"]]$se
  if (!is.null(se)) return(se)
  stop("Search Error: Standard errors could not be found or estimated.")
}

