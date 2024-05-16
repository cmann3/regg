# ============================================================================ #
# CONTENTS                                                                     #
#   - Complex   : Arg, Conj, Im, Mod, Re                                       #
#   - Math      : abs, sqrt, round, exp, log, cos, sin, tan, etc.              #
#   - Ops       : +, -, *, /, ^, %%, %/%, &, |, !, ==, !=, <, <=, >, >=        #
#   - Summary   : all, any, sum, prod, min, max, range                         #
#   - mean                                                                     #
#                                                                              #
#   unexported                                                                 #
#   - do_op        : internal for Ops                                          #
#   - do_summary   : internal for Summary                                      #
#   - regs_summary : for summary functions not in 'Summary'                    #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - reg_select_helpers.R                                                     #
#       as_var, get_env, get_name                                              #
# ============================================================================ #

#' @exportS3Method Complex dummy_var
Complex.dummy_var <- function(z){
  warning(sprintf("Ignoring the application of function '%s' to dummy variable '%s'.\n",
                  .Generic, get_name(z)))
  return(z)
}

#' @exportS3Method Complex rgo_selection
Complex.rgo_selection <- function(z){
  f <- get0(.Generic)
  f(get_selection(z))
}

#' @exportS3Method Complex rgo_var
Complex.rgo_var <- function(z){
  n       <- length(z)
  f       <- get0(.Generic)
  lab     <- get_name(z)
  new_lab <- if (isTRUE(get_env(z)$overwrite)) paste0(.Generic, "(", lab, ")") else lab
  attr(z, "label") <- new_lab
  if (n == 0) return(z)
  z[1:n]  <- f(z[1:n])
  return(z)
}

#' @exportS3Method Complex rgo_var_list
Complex.rgo_var_list <- function(z){
  f <- get0(.Generic)
  for (i in seq_along(z)) z[[i]] <- f(z[[i]])
  return(z)
}


#' @exportS3Method Math dummy_var
Math.dummy_var <- function(x, ...){
  warning(sprintf("Ignoring the application of function '%s' to dummy variable '%s'.",
                  .Generic, get_name(x)))
  return(x)
}

#' @exportS3Method Math rgo_selection
Math.rgo_selection <- function(x, ...){
  f <- get0(.Generic)
  f(get_selection(x), ...)
}

#' @exportS3Method Math rgo_var
Math.rgo_var <- function(x, ...){
  n       <- length(x)
  f       <- get0(.Generic)
  lab     <- get_name(x)
  if (isTRUE(get_env(x)$overwrite)){
    attr(x, "label") <- paste0(.Generic, "(", lab, ")")
    if (is.matrix(x)) colnames(x) <- paste0(.Generic, "(", colnames(x), ")")
  }
  if (n == 0) return(x)
  x[1:n]  <- f(x[1:n], ...)
  return(x)
}

#' @exportS3Method Math rgo_var_list
Math.rgo_var_list <- function(x, ...){
  f <- get0(.Generic)
  for (i in seq_along(x)) x[[i]] <- f(x[[i]], ...)
  return(x)
}

#' @importFrom rlang as_label
#' @exportS3Method Ops rgo_var
Ops.rgo_var <- function(e1, e2){
  f <- get0(.Generic)
  if (inherits(e1, "rgo_selection")) e1 <- get_selection(e1)
  if (inherits(e2, "rgo_selection")) e2 <- get_selection(e2)
  return(do_op(f, e1, e2, .Generic, sys.call()[[2]], sys.call()[[3]]))
}

#' @exportS3Method Summary rgo_var
Summary.rgo_var <- function(..., na.rm = TRUE){
  f        <- get0(.Generic)
  dots     <- list(...)
  new_list <- list()
  for (var in dots) if (!is.null(get_env(var))) env <- get_env(var)
  grouped  <- env$parent$grouped
  groups   <- attr(env$parent$data, "groups")
  for (i in seq_along(dots)){
    var      <- do_summary(f, na.rm, dots[[i]], .Generic, groups)
    if (is.list(var)){new_list <- c(new_list, var)
    } else {new_list[[get_name(var)]] <- var}
  }
  nlist <- length(new_list)
  if (nlist == 0) return(NULL)
  if (nlist == 1) return(new_list[[1]])
  attr(new_list, "env") <- env
  class(new_list)       <- c("rgo_var_list", "rgo_var", "list")
  return(new_list)
}

#' @exportS3Method mean rgo_var
mean.rgo_var <- function(x, na.rm = TRUE, ...)
  regs_summary(mean, x, "mean", na.rm = na.rm, ...)


### --------------------------- UNEXPORTED --------------------------------- ###

do_op <- function(f, lhs, rhs, generic, lab_lhs, lab_rhs){
  nlhs <- NROW(lhs)
  nrhs <- NROW(rhs)
  if (nlhs == 0 || nrhs == 0) return(NULL)
  if (inherits(lhs, "rgo_var_list")){
    new_lhs <- list()
    for (i in seq_along(lhs)) new_lhs <- c(new_lhs, do_op(f, lhs[[i]], rhs, generic, lab_lhs, lab_rhs))
    names(new_lhs)       <- sapply(new_lhs, get_name)
    class(new_lhs)       <- c("rgo_var_list", "rgo_var", "list")
    attr(new_lhs, "env") <- get_env(lhs)
    return(new_lhs)
  } else if (inherits(rhs, "rgo_var_list")){
    new_rhs <- list()
    for (i in seq_along(rhs)) new_rhs <- c(new_rhs, do_op(f, lhs, rhs[[i]], generic, lab_lhs, lab_rhs))
    names(new_rhs)       <- sapply(new_rhs, get_name)
    class(new_rhs)       <- c("rgo_var_list", "rgo_var", "list")
    attr(new_rhs, "env") <- get_env(rhs)
    return(new_rhs)
  } else if (inherits(lhs, "rgo_var")){
    env <- get_env(lhs)
    if (inherits(rhs, "rgo_var")){
      new_name <- sprintf("%s %s %s", get_name(lhs), generic, get_name(rhs))
      if (is.matrix(lhs)){
        if (is.matrix(rhs)){
          ncl <- ncol(lhs)
          ncr <- ncol(rhs)
          m <- matrix(NA, nlhs, ncl * ncr)
          colnames(m) <- rep("", ncl*ncr)
          for (i in 1:ncol(lhs)) for (j in 1:ncol(rhs)){
            m[,(j*(i-1)+j)] <- f(lhs[1:nlhs, i], rhs[1:nrhs, j])
            colnames(m)[(j*(i-1)+j)] <- sprintf("%s %s %s", colnames(lhs)[i], generic, colnames(rhs)[j])
          }
          attr(m, "env")   <- env
          attr(m, "label") <- new_name
          class(m)         <- c("rgo_var", class(m))
          return(m)
        }
        for (i in 1:ncol(lhs)){
          lhs[,i] <- f(lhs[1:nlhs,i], rhs[1:nrhs])
          colnames(lhs)[i] <- sprintf("%s %s %s", colnames(lhs)[i], generic, get_name(rhs))
        }
        attr(lhs, "label") <- new_name
        return(lhs)
      } else if (is.matrix(rhs)){
        for (j in 1:ncol(rhs)){
          rhs[,j] <- f(lhs[1:nlhs], rhs[1:nrhs,j])
          colnames(rhs)[j] <- sprintf("%s %s %s", get_name(lhs), generic, colnames(rhs)[j])
        }
        attr(rhs, "label") <- new_name
        return(rhs)
      } else {
        lhs <- f(lhs[1:nlhs], rhs[1:nrhs])
        attr(lhs, "env") <- env
        class(lhs)       <- c("rgo_var", class(lhs))
      }
    } else if (is.matrix(lhs)){
      if (is.matrix(rhs)){
        ncl <- ncol(lhs)
        ncr <- ncol(rhs)
        m <- matrix(NA, nlhs, ncl * ncr)
        colnames(m) <- rep("", ncl*ncr)
        for (i in 1:ncol(lhs)) for (j in 1:ncol(rhs)){
          m[,(j*(i-1)+j)] <- f(lhs[1:nlhs, i], rhs[1:nrhs, j])
          colnames(m)[(j*(i-1)+j)] <- sprintf("%s %s %s", colnames(lhs)[i], generic, colnames(rhs)[j])
        }
        attr(m, "env")   <- env
        attr(m, "label") <- sprintf("%s %s %s", get_name(lhs), generic, lab_rhs)
        class(m)         <- c("rgo_var", class(m))
        return(m)
      }
      for (i in 1:ncol(lhs)){
        lhs[,i] <- f(lhs[1:nlhs,i], rhs[1:nrhs])
        colnames(lhs)[i] <- sprintf("%s %s %s", colnames(lhs)[i], generic, lab_rhs)
      }
      attr(lhs, "label") <- sprintf("%s %s %s", get_name(lhs), generic, lab_rhs)
      return(lhs)
    } else if (is.matrix(rhs)){
      for (j in 1:ncol(rhs)){
        rhs[,j] <- f(lhs[1:nlhs], rhs[1:nrhs,j])
        colnames(rhs)[j] <- sprintf("%s %s %s", get_name(lhs), generic, colnames(rhs)[j])
      }
      attr(rhs, "label") <- new_name
      attr(m, "env")     <- env
      class(rhs)         <- c("rgo_var", class(rhs))
      return(rhs)
    } else {
      new_name <- sprintf("%s %s %s", get_name(lhs), generic, lab_rhs)
      lhs               <- f(lhs[1:nlhs], rhs)
      attr(lhs, "env")  <- env
      class(lhs)        <- c("rgo_var", class(lhs))
    }
    attr(lhs, "label") <- new_name
    class(lhs) <- class(lhs)[-(which(class(lhs) == "dummy_var"))]
    return(lhs)
  } else {
    new_name <- sprintf("%s %s %s", lab_lhs, generic, get_name(rhs))
    env      <- get_env(rhs)
    if (is.matrix(lhs)){
      if (is.matrix(rhs)){
        ncl <- ncol(lhs)
        ncr <- ncol(rhs)
        m <- matrix(NA, nlhs, ncl * ncr)
        colnames(m) <- rep("", ncl*ncr)
        for (i in 1:ncol(lhs)) for (j in 1:ncol(rhs)){
          m[,(j*(i-1)+j)] <- f(lhs[1:nlhs, i], rhs[1:nrhs, j])
          colnames(m)[(j*(i-1)+j)] <- sprintf("%s %s %s", colnames(lhs)[i], generic, colnames(rhs)[j])
        }
        attr(m, "env")   <- env
        attr(m, "label") <- new_name
        class(m)         <- c("rgo_var", class(m))
        return(m)
      }
      for (i in 1:ncol(lhs)){
        lhs[,i] <- f(lhs[1:nlhs,i], rhs[1:nrhs])
        colnames(lhs)[i] <- sprintf("%s %s %s", colnames(lhs)[i], generic, get_name(rhs))
      }
      attr(lhs, "label") <- new_name
      attr(lhs, "env")   <- env
      class(lhs)         <- c("rgo_var", class(lhs))
      return(lhs)
    } else if (is.matrix(rhs)){
      for (j in 1:ncol(rhs)){
        rhs[,j] <- f(lhs[1:nlhs], rhs[1:nrhs,j])
        colnames(rhs)[j] <- sprintf("%s %s %s", lab_lhs, generic, colnames(rhs)[j])
      }
    } else {
      rhs              <- f(lhs, rhs[1:nrhs])
      attr(rhs, "env") <- env
      class(rhs)       <- c("rgo_var", class(rhs))
    }
    class(rhs) <- class(rhs)[-(which(class(rhs) == "dummy_var"))]
    attr(rhs, "label") <- new_name
    return(rhs)
  }
}

do_summary <- function(f, na.rm, var, generic, groups = NULL, ...){
  if (inherits(var, "rgo_selection")) var <- get_selection(var)
  if (inherits(var, "rgo_var_list")){
    results <- lapply(var, function(i) do_summary(f, na.rm, i, generic, groups, ...))
    names(results) <- sapply(results, get_name)
    return(results)
  } else if (inherits(var, "rgo_var")){
    if (is.null(groups)){
      if (is.matrix(var)){
        for (i in 1:ncol(var)) var[, i] <- f(var[, i], na.rm = na.rm, ...)
        colnames(var) <- sprintf("%s(%s)", generic, colnames(var))
      } else {var[1:length(var)]  <- f(var[1:length(var)], na.rm = na.rm, ...)}
    } else {
      if (is.matrix(var)){
        for (r in groups$.rows){
          for (i in 1:ncol(var)) var[r, i] <- f(var[r, i], na.rm = na.rm, ...)
        }
        colnames(var) <- sprintf("%s(%s)", generic, colnames(var))
      } else{
        for (r in groups$.rows){
          var[r] <- f(var[r], na.rm = na.rm, ...)
        }
      }
    }
    attr(var, "label") <- sprintf("%s(%s)", generic, get_name(var))
    return(var)
  } else {
    stop("Summary functions involving 'reg_select' variables may only include other 'reg_select' variables.")
  }
}

regs_summary <- function(f, x, label = "", na.rm = TRUE, ...){
  val <- do_summary(f, na.rm = na.rm, x, label, groups = get_env(x)$groups, ...)
  if (is.list(val)){
    attr(val, "env") <- get_env(x)
    class(val)       <- c("rgo_var_list", "rgo_var", "list")
  }
  return(val)
}
