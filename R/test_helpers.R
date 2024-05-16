# ============================================================================ #
# CONTENTS                                                                     #
#   - as_test : create a specialized list cotaining test information           #
#   - is_test : test whether object is an 'rgo_test'                           #
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
#' @param ...  Other objects include in the list.
#'
#' @return list of class \code{"rgo_test"}
#' @export
#'
as_test <- function(stat, pval = NULL, df = NULL, label = NULL, ...)
  structure(
    list(stat = stat, df = df, pval = pval, ...),
    label = label,
    class = c("rgo_test", "list")
  )


#' Test Whether Object is an \code{rgo_test}
#'
#' @param x object to be tested
#'
#' @return \code{TRUE} or \code{FALSE}
#' @export
#'
is_test <- function(x) inherits(x, "rgo_test")

