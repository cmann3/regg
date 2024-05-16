# ============================================================================ #
# CONTENTS                                                                     #
#   - as_lambda   : create purrr style functions from R objects                #
#   - unexported  : [all internal]                                             #
#     extract_args_formula, extract_args_period, make_fn                       #
# ============================================================================ #

#' Create purrr-style Functions from R Objects
#'
#' Function creation from formulas, calls, and other R objects, similar to
#' \code{\link[purrr]{as_mapper}}.
#'
#' @param x A function, formula, call, or vector
#' @param envir environment associated with created function
#' @param ... arguments passed to methods.
#'
#' @details
#' \code{as_lambda} is a convenience function used to quickly create functions for
#' use in functions throughout the \code{regg} package, similar to lambdas in the
#' \code{purrr} package. However, there are some minor differences between the
#' two, as described below, due to how lambdas are used in \code{regg}.
#'
#' When a function is passed to \code{as_lambda}, it is returned as is.
#'
#' When a formula is passed to \code{as_lambda}, e.g. \code{~ .x - 5}, the object
#' on the right-hand side of \code{~} becomes the function body with any variable
#' prefaced with a period \code{.} following by a letter, e.g. \code{.a} or \code{.z},
#' becoming an argument name. The argument names are sorted so that \code{~ .b - .a}
#' becomes \code{function(.a, .b, ...) .b - .a}. Note that \code{..1}, \code{...2},
#' etc. can be used to reference objects beyond those explicitly labelled. Therefore,
#' \code{as_lambda} provides more flexibility when creating functions.
#'
#' Two-sided formula can also be used. In this case, the objects on the left-hand
#' side of \code{~}, separated by a \code{+}, are interpreted as the variable names
#' in the resulting functions. For example, \code{i ~ i + 1} is equivalent to
#' \code{function (i, ...) i + 1}. Similarly, \code{i + j ~ i * j + 1} is equivalent to
#' \code{function(i, j, ...) i * j + 1}.
#'
#' Calls that are passed to \code{as_lambda} are treated the same as the right-hand
#' side of a formula. \code{as_lambda( substitute(sqrt(.x)) )} is equivalent to
#' \code{function(.x, ...) sqrt(.x)}
#'
#' One of the main uses of lambdas in \code{regg} is to rename model variables
#' using \code{:=} inside of \code{\link{reg_select}}. One way that this shows up
#' is how \code{as_lambda} handles numeric vectors. As with \code{\link[purrr]{as_mapper}},
#' numeric vectors are converted to functions that extract elements specified by
#' the numeric vector. The difference is how it handles single, character values.
#' Here, it will extract the individual characters. For example, \code{as_lambda(2:4)("hello")}
#' will return \code{"ell"}.
#'
#'
#'
#' @return `function` object
#' @export
#'
#' @examples
#' as_lambda(\(i) i + 1)
#' as_lambda(seq(1,5, by = 2))
#' as_lambda(x + y ~ x[1] * y)
#' as_lambda(~ .x + .y)
#'
as_lambda <- function(x, envir = parent.frame(), ...) UseMethod("as_lambda")
#' @exportS3Method as_lambda call
as_lambda.call <- function(x, envir = parent.frame(), vars = NULL, ...){
  if (x[[1]] == "~") return(as_lambda(eval(x), envir = envir, vars = vars, ...))
  if (!is.null(vars)) return(make_fn(args = vars, body = x, envir = envir))
  vars  <- extract_args_period(x)
  if (length(vars) > 1){
    num_args <- grepl("\\.\\.\\d+$", vars)
    vars     <- c(sort(vars[!num_args]), sort(vars[num_args]))
  }
  return(make_fn(args = vars, body = x, envir = envir, ...))
}
#' @exportS3Method as_lambda character
as_lambda.character <- function(x, envir = parent.frame(), ...) as_lambda(parse(text = x[1])[[1]], ...)
#' @exportS3Method as_lambda default
as_lambda.default <- function(x, envir = parent.frame(), ...) return(x)
#' @exportS3Method as_lambda formula
as_lambda.formula  <- function(x, envir = parent.frame(), ...){
  if (length(x) == 2) return(as_lambda(x[[2]], envir = envir, ...))
  args <- extract_args_formula(x[[2]])
  return(as_lambda(x[[3]], vars = args, envir = parent.frame(), ...))
}
#' @exportS3Method as_lambda numeric
as_lambda.numeric <- function(x, ...){
  if (length(x) == 1){bod1 <- substitute(.x[[y]])
  } else {bod1 <- substitute(.x[y])}
  bod1[[3]] <- x
  bod <- substitute({
    if (is.character(.x) && length(.x) == 1){
      .x <- strsplit(.x, "")[[1]]
      .x <- .x[y]
      return(paste(.x, collapse = ""))
    }
  })
  bod[[3]]  <- bod1
  bod[[2]][[3]][[3]][[3]][[3]] <- x
  make_fn(".x", bod, parent.frame())
}

### --------------------------- UNEXPORTED --------------------------------- ###
extract_args_formula <- function(x){
  if (is.name(x)) return(as.character(x))
  if (is.character(x)) return(x)
  if (is.call(x) && x[[1]] == "+")
    return(Reduce(c, sapply(seq_along(x)[-1], function(i) extract_args_formula(x[[i]]))))
  if (is.call(x))
    stop(sprintf("Type Error: Lambda arguments on the left-hand side of formula must be separated by '+', not '%s'.", x[[1]]))
  stop("Type Error: Left-hand side of formula not recognized as a lambda argument.")
}

extract_args_period <- function(x){
  if (is.name(x)){
    if (grepl("^\\.[A-Za-z]", as.character(x))) return(as.character(x))
    return(NULL)
  }
  if (is.call(x))
    return(Reduce(c, sapply(seq_along(x)[-1], function(i) extract_args_period(x[[i]]))))
  return(NULL)
}

make_fn <- function(args, body, envir){
  fn              <- function(x) x
  body(fn)        <- body
  arg             <- vector("list", length(args)+1)
  for (i in seq_along(arg)) arg[[i]] <- substitute()
  names(arg)      <- c(args, "...")
  formals(fn)     <- as.pairlist(arg)
  environment(fn) <- envir
  class(fn)       <- c("rgo_lambda", "function")
  return(fn)
}
