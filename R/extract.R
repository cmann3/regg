# ============================================================================ #
# CONTENTS                                                                     #
#   - `[`, `$`, `[[`   : Methods for extracting elements from rgo objects      #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
# ============================================================================ #

#' Extract Elements from Regression Models
#'
#' @param x object of class \code{rgo}
#' @param name,i character describing object to extract
#' @param inherits should earlier models be searched? Defaults to \code{TRUE}
#' when using \code{`$`}, and \code{FALSE} when using \code{`[[`}.
#' @param ... objects passed to methods
#'
#' @return R object
#'
#' @rdname extract
#' @name extract
#'
NULL

#' @rdname extract
#' @export
`$.rgo_model` <- function(x, name)
  get0(name, envir = x, inherits = TRUE, ifnotfound = NULL)

#' @rdname extract
#' @export
`[[.rgo_model` <- function(x, i, inherits = FALSE, ...)
  get0(i, envir = x, inherits = inherits, ifnotfound = NULL)

