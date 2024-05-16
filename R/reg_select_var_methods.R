# ============================================================================ #
# CONTENTS                                                                     #
#   - c.rgo_var          : combine rgo_var objects                             #
#   - complement.rgo_var : find variables not in each selection                #
#   - intersect.rgo_var  : find variables within each selection                #
#   - union.rgo_var      : find variables in any of the selections             #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - reg_select_helpers.R                                                     #
#       as_var, get_name                                                       #
# ============================================================================ #

#' @exportS3Method c rgo_var
c.rgo_var <- function(...){
  combined   <- list(...)
  new_list   <- list()
  nlist      <- 1
  to_convert <- numeric(0)
  conv_label <- numeric(0)
  env        <- NULL
  for (i in seq_along(combined)){
    val <- combined[[i]]
    if (is.null(env)) env <- get_env(val)
    if (inherits(val, "rgo_selection")) val <- get_selection(val)
    if (inherits(val, "rgo_var_list")){
      n                           <- length(val)
      if (n == 0) next
      new_list[nlist:(nlist+n-1)] <- val
      nlist                       <- nlist + n
    } else if (inherits(val, "rgo_var")){
      new_list[[nlist]] <- val
      nlist             <- nlist + 1
    } else if (inherits(val, "rgo_model_env")){
      new_list          <- c(new_list, val$new)
      nlist             <- length(new_list) + 1
      # check for dummies??
    } else {
      to_convert        <- c(to_convert, nlist)
      conv_label        <- c(conv_label, i)
      new_list[[nlist]] <- val
      nlist             <- nlist + 1
    }
  }
  if (length(to_convert) > 0){
    labels <- names(combined)
    if (is.null(labels))   stop("Naming Error: All variables in the resulting model matrix must be named.")
    labels <- labels[conv_label]
    if (any(labels == "")) stop("Naming Error: All variables in the resulting model matrix must be named.")
    for (i in seq_along(to_convert)){
      w <- to_convert[i]
      new_list[[w]] <- as_var(new_list[[w]], conv_label[i], env)
    }
  }
  names(new_list)       <- sapply(new_list, get_name)
  class(new_list)       <- c("rgo_var_list", "rgo_var", "list")
  attr(new_list, "env") <- env
  return(new_list)
}

#' @exportS3Method intersect rgo_var
intersect.rgo_var <- function(x, y, ...){
  if (!inherits(y, "rgo_var"))
    stop(sprintf("Type Error: object x (class=%s) may not intersect with y (class = %s)",
                 class(x)[1], class(y)[1]))
  if (inherits(x, "rgo_selection")){
    if (inherits(y, "rgo_selection")){
      x[[1]] <- intersect(x[[1]], y[[1]])
    } else if (inherits(y, "rgo_var_list")){
      x[[1]] <- intersect(x[[1]], names(x))
    } else {x[[1]] <- intersect(x[[1]], get_name(x))}
    if (length(x[[1]]) == 0) return(NULL)
    return(x)
  }
  env <- get_env(x)
  if (inherits(x, "rgo_var_list")){
    if (inherits(y, "rgo_var_list")){
      x <- x[which(names(x) %in% names(y))]
    } else if (inherits(y, "rgo_selection")){
      x <- x[which(names(x)) %in% y[[1]]]
    } else if (get_name(y) %in% names(x)) {
      return(y)
    } else { return(NULL) }
  } else if (inherits(y, "rgo_var_list")) {
    if (get_name(x) %in% names(y)) return(x)
    return(NULL)
  } else if (inherits(y, "rgo_selection")) {
    if (get_name(x) %in% y[[1]]) return(x)
    return(NULL)
  } else {
    if (get_name(x) == get_name(y)) return(x)
    return(NULL)
  }
  if (length(x) == 1) return(x[[1]])
  attr(x, "env") <- env
  class(x) <- c("rgo_var_list", "rgo_var", "list")
  return(x)
}

#' @exportS3Method union rgo_var
union.rgo_var <- function(x, y, ...){
  if (!inherits(y, "rgo_var"))
    stop(sprintf("Type Error: object x (class=%s) may not intersect with y (class = %s)",
                 class(x)[1], class(y)[1]))
  if (inherits(x, "rgo_selection")){
    if (inherits(y, "rgo_selection")){
      x[[1]] <- union(x[[1]], y[[1]])
      return(x)
    }
    x <- get_selection(x)
  }
  env <- get_env(x)
  if (inherits(x, "rgo_var_list")){
    if (inherits(y, "rgo_var_list")){
      y <- y[-(which(names(y) %in% names(x)))]
      return(c(x, y))
      ### TODO: Stopped here!
    } else if (inherits(y, "rgo_selection")){
      x <- x[which(names(x)) %in% y[[1]]]
    } else if (get_name(y) %in% names(x)) {
      return(y)
    } else { return(NULL) }
  } else if (inherits(y, "rgo_var_list")) {
    if (get_name(x) %in% names(y)) return(x)
    return(NULL)
  } else if (inherits(y, "rgo_selection")) {
    if (get_name(x) %in% y[[1]]) return(x)
    return(NULL)
  } else {
    if (get_name(x) == get_name(y)) return(x)
    return(NULL)
  }
  if (length(x) == 1) return(x[[1]])
  attr(x, "env") <- env
  class(x) <- c("rgo_var_list", "rgo_var", "list")
  return(x)
}

