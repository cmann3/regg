# ============================================================================ #
# CONTENTS                                                                     #
#   - find_regg  : searches rgo environment chain to return regg object        #
#   - func_in    : check if function is in list                                #
#   - reg_copy   : copy an rgo object                                          #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
# ============================================================================ #

find_regg <- function(x){
  if (is_regg(x)) return(x)
  if (is.environment(x)){
    parent <- parent.env(x)
    if (identical(parent, baseenv())) stop("TYPE ERROR: Input is not nor links to a 'regg' object.")
    return(find_regg(x))
  }
  stop("TYPE ERROR: Input is not nor links to a 'regg' object.")
}

func_in <- function(fn, list){
  for (f in list) if (identical(fn, f)) return(TRUE)
  return(FALSE)
}

reg_copy <- function(x, ...) UseMethod("reg_copy")
#' @exportS3Method reg_copy default
reg_copy.default <- function(x, ...) return(x)
#' @exportS3Method reg_copy list
reg_copy.list   <- function(x, deep = TRUE, ...){
  if (!deep) return(x)
  z <- lapply(x, reg_copy, deep = TRUE, ...)
  attributes(z) <- attributes(x)
  return(z)
}
#' @exportS3Method reg_copy regg
reg_copy.regg   <- function(x, deep = TRUE, ...){
  e <- new.env(parent = parent.env(x))
  if (deep){
    for (i in ls(x)) e[[i]] <- reg_copy(x[[i]], deep = TRUE, ...)
  } else {
    for (i in ls(x)) e[[i]] <- x[[i]]
  }
  attributes(e) <- attributes(x)
  return(e)
}
#' @exportS3Method reg_copy rgo_model
reg_copy.rgo_model   <- function(x, deep = TRUE, ...){
  e <- new.env(parent = parent.env(x))
  if (deep){
    for (i in ls(x)) e[[i]] <- reg_copy(x[[i]], deep = TRUE, ...)
  } else {
    for (i in ls(x)) e[[i]] <- x[[i]]
  }
  attributes(e) <- attributes(x)
  return(e)
}
