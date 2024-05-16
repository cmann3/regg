# ============================================================================ #
# CONTENTS                                                                     #
#   - lag      :  shift values in vector down                                  #
#   - lead     : shift values in vector up                                     #
#                                                                              #
# CONTENTS (unexported)                                                        #
#   - do_window : apply function to rgo_var, applying by groups, if applicable #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - reg_select_helpers.R                                                     #
#       - get_env                                                              #
# ============================================================================ #

#' Window Functions for \code{reg_select}
#'
#' A collection of functions to be used in \code{\link{reg_select}}. The default,
#' if used outside of \code{reg_select}, is the same as their \link{dplyr} equivalent.
#'
#' @param x R object
#' @param n integer describing the number of observations to lead or lag
#' @param ... objects passed to methods
#'
#' @return object with same type as \code{x}
#'
#' @rdname window_functions
#' @name window_functions
#'
NULL

#' @rdname window_functions
#' @export
lag <- function(x, n = 1L, ...) UseMethod("lag")
#' @exportS3Method lag default
#' @importFrom dplyr lag
lag.default <- function(x, n = 1L, ...) dplyr::lag(x, n = n[1], ...)
#' @exportS3Method lag rgo_var
lag.rgo_var <- function(x, n = 1L, prefix = "L", ...) do_window(
  x,
  function(x, n = 1, ...){
    l <- length(x)
    n <- abs(n)
    if (n == 0) return(x)
    if (n >= l) return(rep(NA, l))
    c(rep(NA, n), x[1:(l-n)])
  },
  attr(get_env(x)$parent$data, "groups"), n = n[1],
  label = function(lab) paste0("L", n[1], ".", lab),
  ...
)

#' @rdname window_functions
#' @export
lead <- function(x, n = 1L, ...) UseMethod("lead")
#' @exportS3Method lead default
#' @importFrom dplyr lag
lead.default <- function(x, n = 1L, ...) dplyr::lead(x, n = n[1], ...)
#' @exportS3Method lead rgo_var
lead.rgo_var <- function(x, n = 1, ...) do_window(
  x,
  function(x, n = 1L, ...){
    l <- length(x)
    n <- abs(n)
    if (n == 0) return(x)
    if (n >= l) return(rep(NA, l))
    c(x[(n+1):l], rep(NA, n))
  },
  attr(get_env(x)$parent$data, "groups"), n = n[1],
  label = function(lab) paste0("Ld", n, ".", lab),
  ...
)


### --------------------------- UNEXPORTED --------------------------------- ###
do_window <- function(x, f, groups = NULL, ...) UseMethod("do_window")
#' @exportS3Method do_window default
do_window.default <- function(x, f, groups = NULL, ...) return(f(x, ...))
#' @exportS3Method do_window rgo_var
do_window.rgo_var <- function(x, f, groups = NULL, label = NULL, ...){
  if (NCOL(x) == 0 || NROW(x) == 0) return(x)
  if (is.null(groups)){
    nr <- NROW(x)
    if (is.matrix(x)){
      for (j in 1:ncol(x)) x[1:nr, j] <- f(x[1:nr, j], ...)
    } else { x[1:nr] <- f(x[1:nr], ...) }

  } else if (is.matrix(x)) {
    for (r in groups$.rows) for (j in 1:ncol(x)) x[r, j] <- f(x[r, j], ...)
  } else {
    for (r in groups$.rows) x[r] <- f(x[r], ...)
  }
  if (!is.null(label)){
    attr(x, "label") <- label(attr(x, "label"))
    if (is.matrix(x)) colnames(x) <- label(colnames(x))
  }
  return(x)
}
#' @exportS3Method do_window rgo_var_list
do_window.rgo_var_list <- function(x, f, groups = NULL, ...){
  for (i in seq_along(x)) x[[i]] <- do_window(x[[i]], f, groups, ...)
  return(x)
}
