# ============================================================================ #
# CONTENTS                                                                     #
#   - use_stars      : use specific levels of significance (& markings)        #
#   - stars_default  : a star function for use in use_stars                    #
#   - length_models  : count number of models                                  #
#   - reg_eval       : evaluate quoted expression for regg arguments           #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
# ============================================================================ #

#' Use Specified Significance Indicators
#'
#' Specify the noted significance levels in regression output and how they are
#' marked.
#'
#' @param x object of class \code{rgo}
#' @param ... numeric values representing the noted significance levels, named with the desired mark.
#' @param list named list or numeric vector. Used as an alternative to \code{...}.
#' Alternatively, a function that accepts a vector of numbers and returns a
#' character vector.
#'
#' @return the object \code{x}
#' @export
#'
use_stars <- function(x, ..., list = NULL) UseMethod("use_stars")

#' @exportS3Method use_stars rgo
use_stars.rgo <- function(x, ..., list = NULL){
  x$star_fn <- star_list(..., list = NULL)
  return(x)
}

#' @export
#' @describeIn use_stars the default significance levels and markings in R, to be added to \code{list}
stars_default <- function(){
  list <- c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.1)
  f <- function(x){
    sapply(x, function(val){
      for (i in seq_along(list)){
        if (val < list[[i]]) return(names(list)[i])
      }
      return("")
    })
  }
  attr(f, "signif_levels") <- list
  return(f)
}

### --------------------------- UNEXPORTED --------------------------------- ###

star_list <- function(..., list = NULL){
  if (is.null(list)){list <- list(...)
  } else if (is.function(list)) return(list)
  if (is.null(names(list)) || any(names(list) == ""))
    stop("All objects in the star list must be named by the specified mark.")
  list <- sort(sapply(list, function(i) i))
  f <- function(x){
    sapply(x, function(val){
      for (i in seq_along(list)){
        if (val < list[[i]]) return(names(list)[i])
      }
      return("")
    })
  }
  attr(f, "signif_levels") <- list
  return(f)
}
