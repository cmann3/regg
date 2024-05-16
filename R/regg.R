# ============================================================================ #
# CONTENTS                                                                     #
#   - regg   : major function, create a regression object to be estimated      #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - init_regg -> regg_init.R                                                 #
#   - copy_regg -> regg_helpers.R                                              #
# ============================================================================ #

#' Create a New Regression Model
#'
#' Master regression modeling function to be called by regression methods.
#'
#' @param x Object passed to \code{regg} and upon which the methods are dispatched.
#' @param ...  Other arguments passed on to methods or quoted and stored in the returned environment..
#'
#' @details
#' A \code{regg} object is an \code{\link[base]{environment}} with two classes:
#' \code{regg} and \code{rgo} *(inherited by almost all objects created in the
#' regg package)*. It contains two primary fields: \code{"data"}, containing the
#' location to the underlying data set used by the model, and \code{"models"}, a
#' list of \code{rgo_model} objects. Each \code{rgo_model} represents an individual
#' regression model with its own terms, coefficients, fitting method, etc.
#'
#' Most of the named fields passed to \code{...} are folded into the \code{rgo_model}
#' objects, which are chained environments that inherit from the previous model
#' or \code{regg} object. This allows methods, data, and fitting parameters to
#' be inherited across regression models. Note that these fields are not evaluated
#' at the time of being added to the environment. If these fields need to be accessed,
#' they should be evaluated first via \code{\link{reg_eval}}.
#'
#' Any unnamed field in \code{...} is treated as a regression term where the first
#' object is the response variable by default and the rest represent independent
#' variables. These objects are evaluated using \code{\link{reg_select}}.
#'
#' The most important field passed to \code{regg} is \code{method}. This contains
#' the method of fitting the regression model. The \code{method} should be a function
#' that evaluates an \code{rgo_model} object, adds a \code{"coefficient"} field,
#' and returns the object. Note that the model is only fit when the object is
#' activated through printing, accessing model components, or other objects that
#' explicitly need to fitted regression model. See \code{\link{fit}} for more
#' details about the fitting process. Elements of the model, such as the model matrix
#' or regression weights, can be accessed without fitting the model through functions
#' such as \code{\link{get_x}} and \code{\link{get_y}}. If not method is supplied,
#' \code{\link{ols}} will be used.
#'
#' \code{regg} accepts three important fields by the user - \code{se}, \code{stats},
#' and \code{tests}. \code{se} is a function describing the method of calculating
#' the regression standard errors. \code{stats} is a list of functions used to
#' estimate regression statistics such as the R-squared or AIC. If a character
#' is used instead of a function, it will be prefaced by \code{"stat_"} and the
#' search path will be examined for a function of the relevant name. For example,
#' \code{"r2"} will link to the function \code{\link{stat_r2}}. The \code{tests}
#' field is also a list of function or character values, but \code{"t"} would
#' link to the function \code{\link{test_t}}.
#'
#' When developing a method that uses \code{regg}, you can use \code{default_se},
#' \code{default_stats}, and \code{default_tests} to set standard statistics and
#' so forth that you wish to be estimated each time the method is used to fit the
#' regression model.
#'
#' Other notable fields include \code{"label"} which is for the user to label the
#' model in the printed results, and standard fields in \code{\link[stats]{lm}}
#' such as \code{"weights"}, \code{"subset"}, or \code{"na.action"}.
#'
#'
#' @return environment of class \code{regg}
#' @export
#'
regg <- function(x, ...) UseMethod("regg")

#' @exportS3Method regg data.frame
#' @importFrom rlang quo
regg.data.frame <- function(x, ...){
  core_env <- new.env(hash = TRUE, parent = emptyenv(), size = 29L)
  core_env$data   <- quo(x)
  core_env$models <- list()
  core_env$nrow   <- nrow(data)
  core_env$style  <- list()
  class(core_env) <- c("regg", "rgo_m", "rgo")
  init_regg(core_env, ...)
}

#' @exportS3Method regg regg
regg.regg <- function(x, ...) init_regg(reg_copy(x, deep = FALSE), ...)

