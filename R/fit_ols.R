# ============================================================================ #
# CONTENTS                                                                     #
#   - fit_ols    : back-end QR decomposition calculator for linear fitting     #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - has_intercept   -> get_components.R                                      #
# ============================================================================ #


#' Fit a Model using Ordinary Least Squares
#'
#' This is used by \code{\link{ols}} and related methods. It is not intended to
#' be used directly for analysis, but within a regression method to compute the
#' relevant QR matrix and coefficients, and add the appropriate fields to the
#' model object. It is a wrapper for \code{\link[stats]{lm.fit}} and
#' \code{\link[stats]{lm.wfit}}.
#'
#' @param model object of class \code{rgo_model}
#' @param X numeric matrix of independent variables
#' @param y numeric vector or matrix of predictor variables
#' @param weights numeric vector of weights or \code{NULL}
#' @param offset numeric vector to specify a priori known component to be included in the predictor.
#' @param intercept does the model include an intercept? If \code{NULL}, \code{fit_ols} will attempt to determine automatically.
#'
#' @return the regression model
#' @importFrom stats lm.fit lm.wfit
#' @export
#'
fit_ols <- function(model, X, y, weights = NULL, offset = NULL, intercept = NULL){
  if (is.null(intercept)) intercept <- has_intercept(model)
  if (is.null(weights)){
    fitted <- lm.fit(X, y, offset = offset, singular.ok = TRUE)
    model$mss  <- if (intercept){
      sum((fitted$fitted.values - mean(fitted$fitted.values))^2)
    } else { sum(fitted$fitted.values^2) }
    model$rss  <- sum(fitted$residuals^2)
  } else {
    fitted <- lm.wfit(X, y, weights, offset = o, singular.ok = TRUE)
    model$mss  <- if (has_intercept){
      m <- sum(w * fitted$fitted.values/sum(w))
      sum(w * (fitted$fitted.values - m)^2)
    } else { sum(w * fitted$fitted.values^2) }
    model$rss  <- sum(w * fitted^residuals^2)
  }
  list2env(fitted, envir = model)
  model$nrow  <- NROW(model$qr$qr)
  model$sigma <- sqrt(model$rss / model$df.residual)
  return(model)
}
