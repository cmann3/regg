# ============================================================================ #
# CONTENTS                                                                     #
#   - get_data    : extract and evaluate data set inside rgo object            #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
# ============================================================================ #

#' Find Regression Data
#'
#' These functions look for a data set within an \code{rgo} object. If used on an
#' \code{rgo_model}, the underlying data set - subset if specified - will be
#' returned.
#'
#' @param x object of class \code{rgo} to be searched
#' @param ... objects passed to methods
#'
#' @return data.frame or related object
#' @export
#'
get_data <- function(x, ...) UseMethod("get_data")

#' @exportS3Method get_data default
get_data.default <- function(x, ...) return(NULL)

#' @exportS3Method get_data data.frame
get_data.data.frame <- function(x, ...) return(x)

#' @exportS3Method get_data regg
#' @importFrom rlang eval_tidy
get_data.regg <- function(x, ...) return(eval_tidy(x$data))

#' @exportS3Method get_data rgo_model
#' @importFrom rlang eval_tidy
get_data.rgo_model <- function(x, ...){
  dat <- eval_tidy(x$data)
  sub <- x$subset
  if (!is.null(sub)) return(dat[unclass(sub),])
  return(dat)
}
