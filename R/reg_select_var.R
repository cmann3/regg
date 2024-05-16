# ============================================================================ #
# CONTENTS                                                                     #
#   - reg_select_var   : primary, internal function for 'reg_select'           #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - reg_select_fns.R                                                         #
#       regs_everything, regs_find, regs_last_col, regs_num_range, regs_range, #
#       regs_where                                                             #
#       regw_trend                                                             #
#   - reg_select_helpers.R                                                     #
#       as_var, get_col_labels, maybe_selection, rename_var, rm_rgo_var        #
# ============================================================================ #

### --------------------------- UNEXPORTED --------------------------------- ###

reg_select_var <- function(x, env, ...) UseMethod("reg_select_var")

#' @exportS3Method reg_select_var default
reg_select_var.default <- function(x, env, ...) return(x)

#' @exportS3Method reg_select_var character
reg_select_var.character <- function(x, env, ..., initial = TRUE){
  if (initial)
    return(maybe_selection(x[x %in% colnames(env$parent$data)], env))
  return(x)
}

#' @exportS3Method reg_select_var formula
reg_select_var.formula <- function(x, env, ...){
  env <- model_env(parent.env(env))
  if (length(x) == 3){
    lhs <- select_formula(x[[2]], env, ...)
    rhs <- c(as_var(rep(1,env$nrow), env$intercept_name, env), select_formula(x[[3]], env, ...))
    if (is.null(lhs)){ nlhs <- 0
    } else if (inherits(lhs, "rgo_var_list")){
      env$response <- 1:length(lhs)
      env$yvar     <- get_col_labels(lhs)
    } else if (inherits(lhs, "reg_selection")){
      env$response <- 1:length(lhs[[1]])
      env$yvar     <- lhs[[1]]
    } else {
      env$response <- 1
      env$yvar     <- get_col_labels(lhs)
    }
    env$new <- c(lhs, rhs)
    return(env)
  }
  env$new      <- c(as_var(rep(1, env$nrow), env$intercept_name, env), select_formula(x[[3]], env, ...))
  env$response <- 0
  env$yvar     <- character(0)
  return(env)
}

#' @exportS3Method reg_select_var name
reg_select_var.name <- function(x, env, enclos = NULL, ...){
  if (x == ".") return(regs_everything(env, ...))
  if (grepl("^\\.\\.\\.?[a-zA-Z]", x))
    return(reg_select_var(eval(as.symbol(substring(x, 3))), env, enclos = enclos, ...))
  find_var(as.character(x), env, enclos, ...)
}

#' @exportS3Method reg_select_var numeric
reg_select_var.numeric <- function(x, env, ..., initial = TRUE){
  if (initial){
    if (all(x == 0)){
      env$intercept <- FALSE
      return(rm_rgo_var(env$intercept_name, env, ...))
    }
    if (all(x == 1)) env$intercept <- TRUE
    return(as_var(1, env$intercept_name, env))
  }
  return(x)
}

#' @importFrom rlang as_label
#' @exportS3Method reg_select_var call
reg_select_var.call <- function(x, env, ..., initial = TRUE){
  first <- x[[1]]
  if (first == "~") return(reg_select_var.formula(x, env, ...))

  ### PRELIMINARY FUNCTIONS  ---------------------------------------------------
  if (initial){
    if (first == ":="){
      lhs <- x[[2]]
      rhs <- reg_select_var(x[[3]], env, ...)
      return(rename_var(rhs, lhs, ...))
    }
    if (first == "-" && length(x) == 2)
      return(rm_rgo_var(x[[2]], env, ...))
  }
  if (first == "model") return(regs_model(x[-1], env, ..., initial = TRUE))
  if (first == "var"){
    vars <- unique(Reduce(c, sapply(x[-1], function(i){
      results <- eval(i)
      if (!is.character(results)) stop("Type Error: only characters may be passed to 'var' in reg_select.")
      return(results)
    })))
    return(reg_select_var.character(vars, env, ..., initial = TRUE))
  }
  if (first == ":" && is.name(x[[2]]) && is.name(x[[3]])){
    vals <- regs_range(as.character(x[[2]]), as.character(x[[3]]), env, ...)
    if (is.null(vals)){
      return(as_var(eval(x), as_label(x), env, ...))
    }
    return(vals)
  }

  ### SELECTION FUNCTIONS ------------------------------------------------------
  all_of      <- function(x, ...) regs_all_of(x, env, ...)
  any_of      <- function(x, ...) regs_any_of(x, env, ...)
  contains    <- function(x, ...) regs_find(x, env, fixed = TRUE, ...)
  ends_with   <- function(x, ...) regs_find(paste0(x, "$"), env, ...)
  everything  <- function(...) regs_everything(env, ...)
  last_col    <- function(offset = 0L, ...) regs_last_col(offset, env, ...)
  matches     <- function(x, ...) regs_find(x, env, vars, perl = TRUE, ...)
  num_range   <- function(prefix, range, ...) regs_num_range(env, prefix, range, ...)
  starts_with <- function(x, ...) regs_find(paste0("^", x), env, ...)
  where       <- function(fn, ...) regs_where(fn, env, ...)

  ### WINDOW FUNCTIONS ---------------------------------------------------------
  trend  <- function(label = "trend", ...) regw_trend(env, label, ...)

  ### INFORMATION FUNCTIONS ----------------------------------------------------
  hide   <- function(x, ...){
    class(x) <- c("rgo_hide", class(x))
    return(x)
  }
  temp   <- function(x, ...){
    attr(x, "temp") <- TRUE
    return(x)
  }

  ### MODEL FUNCTIONS ----------------------------------------------------------
  ar      <- function(n = 1L, ...){
    if (n < 1L) return(NULL)
    y <- regs_y(env)
    l <- lapply(1:n, function(i) lag(y, n = i, ...))
    names(l) <- sapply(l, get_name)
    class(l) <- c("rgo_var_list", "rgo_var", "list")
    return(l)
  }
  cross    <- function(...) regm_cross(env, ...)
  current  <- function() return(env$model)
  previous <- function(e = NULL) if (is.null(e)) parent.env(env$model) else parent.env(e)
  y        <- function() regs_y(env)

  ### MAIN ---------------------------------------------------------------------
  for (i in seq_along(x)[-1])
    x[[i]] <- reg_select_var(x[[i]], env, ..., initial = FALSE)
  return(as_var(eval(x), as_label(x), env, ...))
}




