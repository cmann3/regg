# ============================================================================ #
# CONTENTS (unexported)                                                        #
#   - regm_cross                                                               #
#   - regs_find, regs_all_of, regs_any_of, regs_everything,  regs_last_col,    #
#     regs_model, regs_num_range, regs_range, regs_where , regs_y              #
#   - regw_trend                                                               #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - reg_select_helpers.R                                                     #
#       - as_var, extrap, get_name, rgo_selection, maybe_selection             #
# ============================================================================ #

### --------------------------- UNEXPORTED --------------------------------- ###

regm_cross <- function(env, ...){
  l <- flatten(list(...))
  n <- length(l)
  x <- list()
  if (n == 0) return(NULL)
  if (n == 1) return(l[[1]]) ## MAYBE NOT!
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      val <- l[[i]] * l[[j]]
      x[[get_name(val)]] <- val
    }
  }
  structure(x, class = c("rgo_var_list", "list"))
}

regs_find <- function(match, env, vars = NULL, ignore.case = TRUE, ...){
  if (is.null(vars)) vars <- colnames(env$parent$data)
  found <- vars[grepl(match, vars, ignore.case = ignore.case, ...)]
  maybe_selection(found, env, ...)
}

regs_all_of <- function(x, env, vars = NULL, ...){
  if (!is.null(vars)) vars <- colnames(env$parent$data)
  found <- x %in% vars
  if (all(found)) return(maybe_selection(x, env, ...))
  stop(sprintf("These variables were not found in the data: %s",
               paste(x[!found], collapse = ", ")))
}

regs_any_of <- function(x, env, vars = NULL, ...){
  if (!is.null(vars)) vars <- colnames(env$parent$data)
  found <- x %in% vars
  maybe_selection(x[found], env, ...)
}

regs_everything <- function(env, vars = NULL, ...){
  if (is.null(vars)) vars <- colnames(env$parent$data)
  rgo_selection(vars, env)
}

regs_last_col <- function(offset = 0L, env, vars = NULL, ...){
  if (is.null(vars)) vars <- colnames(env$parent$data)
  n <- length(vars) - abs(offset)
  if (any(n <= 0)) n <- n[-(which(n <= 0))]
  maybe_selection(vars[n], env, ...)
}

regs_model <- function(x, env, ..., initial = TRUE){
  env <- model_env(parent.env(env))
  for (q in x){
    result <- reg_select_var(q, env = env, ..., initial = TRUE)
    combine_var(result, env)
  }
  return(env)
}

regs_num_range <- function(env, prefix, range, suffix = "", width = NULL, vars = NULL, ...){
  if (is.null(vars))    vars <- colnames(env$parent$data)
  if (!is.null(width)) range <- sprintf(paste0("%.0", width, "d"), range)
  to_find <- paste0(prefix, range, suffix)
  n <- which(vars %in% to_find)
  maybe_selection(vars[n], env, ...)
}

regs_range <- function(start, end, env, vars = NULL, ...){
  if (is.null(vars)) vars <- colnames(env$parent$data)
  if (all(start == end)) return(find_var(start, env, ...))
  start <- which(vars == start)
  end   <- which(vars == end)
  if (length(start) == 0 || length(end) == 0) return(NULL)
  rgo_selection(vars[seq(start, end)], env)
}

regs_where <- function(fn, env, ..., vars = NULL){
  if (is.null(vars)) vars <- colnames(env$parent$data)
  sel <- vars[sapply(vars, function(i) isTRUE(fn(env$parent$data[[i]], ...)))]
  maybe_selection(sel, env, ...)
}

regs_y <- function(env){
  n <- env$response
  if (n < 1 || n > length(env$new)) return(NULL)
  if (n == 1) return(env$new[[n]])
  return(env$new[1:n])
}




regw_trend <- function(env, label = "trend", ...){
  if (env$parent$grouped){
    groups <- attr(env$parent$data, "groups")
    val    <- numeric(env$parent$nrow)
    for (g in groups$.rows){
      ng <- length(g)
      if (ng > 0) val[g] <- 1:ng
    }
    return(as_var(val, label, env, ...))
  }
  return(as_var(1:env$parent$nrow, label, env, ...))
}




