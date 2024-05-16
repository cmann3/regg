# ============================================================================ #
# CONTENTS                                                                     #
#   - is_regg, is_rgo_model                                                    #
#                                                                              #
# ============================================================================ #


#' Check Whether Input is a 'regg' Object
#'
#' @param x R object to be tested
#'
#' @return \code{TRUE} if \code{x} inherits \code{"regg"}, otherwise \code{FALSE}
#' @export
#'
is_regg <- function(x) inherits(x, "regg")


#' Check Whether Input is an 'rgo_model' Object
#'
#' @param x R object to be tested
#'
#' @return \code{TRUE} if \code{x} inherits \code{"rgo_model"}, otherwise \code{FALSE}
#' @export
#'
is_rgo_model <- function(x) inherits(x, "rgo_model")
