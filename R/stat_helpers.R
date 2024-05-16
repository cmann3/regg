# ============================================================================ #
# CONTENTS                                                                     #
#   - as_stat : create a specialized numeric stat object                       #
#   - is_stat : test whether object is an 'rgo_stat'                           #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
# ============================================================================ #

#' Create an \code{rgo_test} Object
#'
#' \code{rgo_test} objects are lists containing statistics, associated degrees
#' of freedom, p-values, and other fields containing relevant test information.
#'
#' @param stat,df,pval numeric values or vectors
#' @param label character name to display when printed
#' @param ...  Other objects included in the attributes.
#'
#' @return list of class \code{"rgo_test"}
#' @export
#'
as_stat <- function(x, label = NULL, ...)
  structure(
    x,
    label = label,
    class = c("rgo_stat", class(x)),
    ...
  )


#' Test Whether Object is an \code{rgo_stat}
#'
#' @param x object to be tested
#'
#' @return \code{TRUE} or \code{FALSE}
#' @export
#'
is_stat <- function(x) inherits(x, "rgo_stat")
