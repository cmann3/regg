# ============================================================================ #
# CONTENTS (METHODS)                                                           #
#   - influence                                                                #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - fit_find     -> devel_helpers.R                                          #
#   - get_x, get_y -> get_components.R                                         #
# ============================================================================ #

#' regg Deletion Diagnostics & Influence Measures
#'
#' Compute leave-one-out deletion diagnostics for \code{regg} models. See
#' \code{\link[stats]{influence.measures}} from the \code{stats} package.
#'
#' @param model \code{rgo_model} object
#' @param write should the requested object be inserted in \code{x}, if it not there
#' @param ... objects passed to methods
#'
#' @return the object, extracted from \code{x}
#' @rdname influence_regg
#' @name influence_regg
#'
NULL


#' @rdname influence_regg
#' @exportS3Method cooks.distance rgo_model
#' @importFrom stats influence weighted.residuals
cooks.distance.rgo_model <- function(model, write = TRUE, ...)
  fit_find(model, "cooks.distance", {
    infl  <- influence(model)
    res   <- weighted.residuals(model)
    sd    <- sqrt(sum(res^2, na.rm = TRUE)/get_df(model))
    cooks <- ((res/c(outer(1 - infl$hat, sd)))^2 * infl$hat)/get_rank(model)
    cooks[is.infinite(cooks)] <- NaN
    if (write) model[["cooks.distance"]] <- cooks
    return(cooks)
  })


#' @rdname influence_regg
#' @exportS3Method dfbeta rgo_model
#' @importFrom stats influence
dfbeta.rgo_model <- function(model, write = TRUE, ...)
  fit_find(model, "dfbeta", influence(model, write = write)$coefficients, write = write)


#' @rdname influence_regg
#' @exportS3Method dfbetas rgo_model
#' @importFrom stats dfbeta influence
dfbetas.rgo_model <- function(model, write = TRUE, ...)
  fit_find(model, "dfbetas", {
    qr <- model[["qr"]]
    if (is.null(qr)) stop("INPUT ERROR: Model fit did not produce a QR matrix and dfbetas cannot be estimated.")
    db   <- dfbeta(model, write = write, ...)
    if (length(dim(db)) == 3L) db <- aperm(db, c(1L, 3:2))
    sig  <- influence(model, write = write, ...)$sigma
    db / outer(sig, sqrt(diag(chol2inv(qr$qr, qr$rank))))
  }, write = write)


#' @rdname influence_regg
#' @exportS3Method hatvalues rgo_model
#' @importFrom stats influence
hatvalues.rgo_model <- function(model, write = TRUE, ...)
  fit_find(model, "hatvalues", influence(model, write = write)$hat, write = write)


#' @rdname influence_regg
#' @exportS3Method influence rgo_model
#' @importFrom stats lm.influence
influence.rgo_model <- function(model, write = TRUE, ...)
  fit_find(model, "influence", {
    infl <- lm.influence(model, ...)
    if (write) model$influence <- infl
    infl
  })


#' @rdname influence_regg
#' @exportS3Method rstudent rgo_model
#' @importFrom generics fit
#' @importFrom stats influence
rstandard.rgo_model <- function(model, write = TRUE, type = c("sd.1", "predictive"), ...){
  fit(model)
  type <- match.arg(type)
  infl <- influence(model, write = write)
  if (type == "sd.1"){
    sd  <- sqrt(sum(weighted.residuals(object)^2))/get_df(model)
    den <- c(outer(sqrt(1 - infl$hat), sd))
  } else if (type == "predictive"){den <- 1 - infl$hat}
  r <- infl$wt.res/den
  r[is.infinite] <- NaN
  return(r)
}


#' @rdname influence_regg
#' @exportS3Method rstudent rgo_model
#' @importFrom stats influence
rstudent.rgo_model <- function(model, write = TRUE, ...)
  fit_find(model, "rstudent", {
    infl <- influence(model, write = write, ...)
    r    <- infl$wt.res/(infl$sigma * sqrt(1 - infl$hat))
    r[is.infinite] <- NaN
    r
  }, write = write)
