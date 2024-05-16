# ============================================================================ #
# CONTENTS (METHODS)                                                           #
#   - coef, fitted, residuals, weights                                         #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - fit_find     -> devel_helpers.R                                          #
#   - get_x, get_y -> get_components.R                                         #
# ============================================================================ #

#' Extract Regression Objects
#'
#' Obtain common regression objects such as the coefficients, fitted values, and residuals.
#'
#' @param x \code{regg} or \code{rgo_model} object
#' @param write should the requested object be inserted in \code{x}, if it not there
#' @param ... objects passed to methods
#'
#' @return the object, extracted from \code{x}
#' @rdname extract_reg
#' @name extract_reg
#'
NULL


#' @rdname extract_reg
#' @exportS3Method coef rgo_model
coef.rgo_model <- function(object, write = TRUE, ...)
  fit_find(object, "coefficients", {
    ## TODO: multi models!
    stop("Search Error: 'coefficients' could not be found in ggreg object. The method of fitting the model did not create a coefficient object.")
  }, write = write)


#' @rdname influence_regg
#' @exportS3Method cooks.distance rgo_model
#' @importFrom stats influence weighted.residuals
deviance.rgo_model <- function(object, write = FALSE, ...)
  fit_find(object, "deviance", sum(weighted.residuals(object)^2, na.rm = TRUE),
           write = write)


#' @rdname extract_reg
#' @exportS3Method fitted rgo_model
#' @importFrom stats coef
fitted.rgo_model <- function(object, write = TRUE, ...)
  fit_find(object, "fitted.values", {
    ## TODO: multi models!
    X  <- get_x(object)
    cf <- coef(x)
    X %*% cf
  }, write = write)


#' @rdname extract_reg
#' @exportS3Method residuals rgo_model
#' @importFrom stats fitted
residuals.rgo_model <- function(object, type = c("working", "response", "deviance", "pearson", "partial"), write = TRUE, ...){
  r <- fit_find(object, "residuals", {
    ## TODO: multi models!
    fitted <- fitted(object, write = write, ...)
    y      <- get_y(object)
    y - fitted
  }, write = write)
  type <- match.arg(type)
  if (type %in% c("working", "response")) return(r)
  if (type == "partial" ) return(r + predict(object, type = "terms"))
  if (type %in% c("deviance", "pearson")){
    w <- weights(object, write = write, ...)
    if (!is.null(w)) r <- r * sqrt(w)
    return(r)
  }
  stop(sprintf("INPUT ERROR: type '%s' not recognized.", type))
}


#' @rdname extract_reg
#' @exportS3Method weights rgo_model
#' @importFrom generics fit
weights.rgo_model <- function(object, write = TRUE, ...){
  fit(object)
  w <- object$weights
  if (!is.null(w) && !is.numeric(w)) return(get_weights(x, write = write, ...))
  return(w)
}

