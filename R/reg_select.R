# ============================================================================ #
# CONTENTS                                                                     #
#   - reg_select   : major function, select variables for model matrix         #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - reg_select_var                     -> reg_select_var.R                   #
#   - get_col_labels, is_temp, model_env -> reg_select_helpers.R               #
# ============================================================================ #

#' \code{tidyselect} for Regression Models
#'
#' Select and convert variables in a data frame to a model matrix for use in a
#' regression, using a mini-language that is mostly compatible with
#' \code{\link[dplyr]{select}} from the tidyverse. \code{reg_select} allows for
#' mathematical operations inside of the selection and includes more functions.
#'
#' @param data data.frame or similar object containing columns to be selected
#' @param ... columns to be selected. See details below.
#' @param drop If \code{TRUE}, the result is coerced to the lowest possible dimension.
#' Only works if \code{numeric} is \code{FALSE}.
#' @param intercept should an intercept be included in the result? An intercept is
#' only included if \code{numeric} is \code{TRUE}.
#' @param intercept_name character providing name of the intercept in the return value
#' @param model regression model from which information about the regression can be drawn
#' @param numeric should character & factor vectors be converted into a matrix with
#' values of 0 or 1? If \code{TRUE}, a \code{matrix} will be returned, otherwise a \code{data.frame}.
#' @param overwrite_names should variable names include functions/operations used
#' @param quoted optional quoted list to be used in place of \code{...}
#' @param response logical value specifying whether or not the first argument should
#' be interpreted as the dependent variable. Alternatively, a numeric vector can
#' be supplied to specify which variables, by position, are the dependent variables.
#'
#' @details
#' \code{reg_select} is similar to a cross between dplyr's \code{\link[dplyr]{select}}
#' and \code{\link[dplyr]{transmute}}. It allows the user to select columns from
#' the supplied data set and perform mathematical operations, such as calculating
#' the logarithm of multiple columns at once, or multiplying two columns together.
#' Furthermore, character and factor variables are automatically converted to
#' a matrix of dummy variables, as long as \code{dummy = TRUE} *(default)* in
#' the function call.
#'
#' Most of the selection helpers from \code{\link[dplyr]{select}} are available
#' in \code{reg_select}. For example, the following \code{tidyselect} helpers can
#' be used inside of \code{reg_select}.
#'
#' \itemize{
#'   \item {-}{ Remove variables from the selection.}
#'   \item {:}{ Selects a range of consecutive variables.}
#'   \item {c()}{ Combines selections.}
#'   \item {everything()}{ Matches all variables.}
#'   \item {last_col()}{ Selects the last variable, possibly with an offset.}
#'   \item {group_cols()}{ Selects all grouping columns.}
#'   \item {starts_with()}{ Selects all column whose name starts with the supplied prefix.}
#'   \item {ends_with()}{ Selects all columns whose name ends with the supplied suffix.}
#'   \item {contains()}{ Find which column names contain the literal string.}
#'   \item {matches()}{ Matches all column names via the supplied regular expression.}
#'   \item {num_range()}{ Matches a numerical range like \code{x01}, \code{x02}, \code{x03}.}
#'   \item {all_of()}{ Matches variable names in a character vector. All names must be present, otherwise an error is thrown.}
#'   \item {any_of()}{ Same as \code{all_of()}, except that no error is thrown for names that don't exist.}
#'   \item {where()}{ Applies a function to all variables and selects those for which the function returns \code{TRUE}.}
#' }
#'
#' Notice that \code{!}, \code{&}, and \code{|} are not available. This is a
#' compromise to allow operations to be performed on variables during the selection
#' process. Instead, use the following functions.
#'
#' \itemize{
#'   \item {complement()}{ Finds the complement of a set of variables. (\code{!})}
#'   \item {intersect()}{ Finds the intersection of two sets of variables, or the variables that are included in both. (\code{&})}
#'   \item {union()}{ Finds the union of two sets of variables. (\code{|})}
#' }
#'
#' \code{reg_select} first searches the supplied data for any column names associated
#' with symbols passed in \code{...}. Symbol names that are prefaced with two dots,
#' \code{..}, will search the parent environment rather than the data set. Alternatively,
#' anything wrapped by the \code{var()} function will be evaluated in the parent
#' environment first before proceeding.
#'
#' Note that mathematical functions can be applied to the output of the helper
#' functions. For example, \code{log(starts_with("s"))} would find all columns
#' whose name begins with \code{"s"}, then takes the logarithm. Note that
#' functions from the \code{\link[base]{Math}} and \code{\link[base]{Complex}}
#' groups will not be applied to dummy variables created from character vectors.
#' However, functions from the \code{\link[base]{Ops}} and \code{\link[base]{Summary}}
#' groups do apply to dummy variables.
#'
#' Many window functions from \code{\link{dplyr}} - such as \code{\link[dplyr]{lag}} and
#' \code{\link[dplyr]{lead}} - are available to use within \code{ggr_select}. These
#' functions should operate correctly with grouped data.
#'
#' New functions that are useful for regression analysis are also available.
#'
#' \itemize{
#'   \item {ar()}{ Include the specified lags of the dependent variable(s). \code{use_y} in \code{ggr_select} must not be \code{FALSE}. }
#'   \item {current()}{ Access the current \code{rgo_model}.}
#'   \item {previous()}{ Access the previous \code{rgo_model}. This may be chained to access even earlier models.}
#'   \item {trend()}{ Create a trend variable.}
#'   \item {y()}{ Access the dependent variable of the current \code{rgo_model}.}
#' }
#'
#' In addition, \code{:=} can be used to give variables a new name. If the object
#' on the left-hand side of \code{:=} is a character or symbol, it will become
#' the new name of the object on the right-hand side. If the object on the right
#' is a matrix, then all instances of the old name in the columns will be replaced
#' with the name on the left. If a function, or object convertable to a function
#' via \code{\link{as_lambda}}, is on the left-hand side, then it will be applied
#' to the original name and all column names.
#'
#' Other functions may also be used to control the output and printing behavior
#' of the resulting model.
#'
#' \itemize{
#'   \item {hide()}{ Columns are included in the output, but are not shown in regression results.}
#'   \item {temp()}{ Columns are not included in the returned output.}
#' }
#'
#'
#' @return `rgo_modelmatrix` object
#' @importFrom rlang is_quosure eval_tidy
#' @export
#'
reg_select <- function(data, ...,
                       drop            = TRUE,
                       intercept       = TRUE,
                       intercept_name  = "(Intercept)",
                       model           = NULL,
                       numeric         = TRUE,
                       overwrite_names = TRUE,
                       quoted          = NULL,
                       response        = TRUE){

  ### Obtain selection items
  if (is.null(quoted)){quoted <- eval(substitute(alist(...)))
  } else if (!is.list(quoted)){quoted <- list(quoted)}
  if (length(quoted) == 0) return(NULL)

  ### Create environments that are passed around
  ml                <- new.env(parent = emptyenv())
  ml$data           <- data
  ml$grouped        <- !is.null(attr(data, "groups"))
  ml$hide           <- list()
  ml$indexed        <- !is.null(attr(data, "index"))
  ml$intercept      <- intercept
  ml$intercept_name <- intercept_name
  ml$model          <- model
  ml$models         <- list()
  ml$numeric        <- numeric
  ml$nrow           <- nrow(data)
  ml$overwrite      <- overwrite_names
  ml$response       <- response
  ml$yvar           <- character(0)

  env <- model_env(ml)

  ### Loop across items
  n <- 0
  for (i in seq_along(quoted)){
    q <- quoted[[i]]
    result <- reg_select_var(q, env = env)
    combine_var(result, env)
    if (!inherits(result, "rgo_var_env")) n <- n + 1
    if (n <= response){
      ml$yvar <- c(ml$yvar, get_col_labels(result))
      if (n == response && ml$intercept) combine_var(as_var(rep(1, ml$nrow), ml$intercept_name, env), env)
    }
  }
  if (length(env$new) > 0) ml$models[[length(ml$models) + 1]] <- env
  models <- lapply(ml$models, function(m){
    m$new <- m$new[!sapply(m$new, is_temp)]
    if (numeric){
      #if (m$intercept) combine_var(as_var(rep(1, ml$nrow), intercept_name, m), m)
      new_frame <- Reduce(cbind, m$new)
      class(new_frame) <- c("rgo_model_matrix", "rgo", "matrix")
    } else if (drop && length(m$new) == 1){
      new_frame <- m$new[[1]]
    } else {
      new_frame <- as.data.frame(lapply(m$new, unclass))
    }
    attr(new_frame, "dummies")   <- if (length(m$dummies) > 0) m$dummies
    attr(new_frame, "hide")      <- if (length(m$hide) > 0)    m$hide
    attr(new_frame, "intercept") <- m$intercept
    attr(new_frame, "response")  <- m$response
    attr(new_frame, "yvar")      <- m$yvar
    return(new_frame)
  })

  if (length(models) == 1) return(models[[1]])
  names(models) <- sapply(ml$models, get_name)
  class(models) <- "rgo_var_model_list"
  return(models)
}
