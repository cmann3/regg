# ============================================================================ #
# CONTENTS                                                                     #
#   - as_var        : convert object to labelled variable carrying environment #
#   - combine_var   : combine variables into rgo_var_list                      #
#   - find_var      : find variable in data set / environments, convert to var #
#   - flatten       : unnest list of items                                     #
#   - get_env       : obtain passing environment from rgo_var                  #
#   - get_name      : obtain variable label/name from rgo_var                  #
#   - get_selection : obtain variables from rgo_selection                      #
#   - maybe_selection : convert to rgo_selection unless only one variable      #
#   - rename_var    : provide new label for rgo_var                            #
#   - rgo_selection : create list of variable names & data set to pull from    #
#   - rm_rgo_var    : remove variables, used for '-' in reg_select             #
#   - select_formula: parse formulas (~), evaluated with reg_select_var        #
# ============================================================================ #

### --------------------------- UNEXPORTED --------------------------------- ###
as_var <- function(x, name = NULL, env = NULL, ...) UseMethod("as_var")
#' @exportS3Method as_var default
as_var.default <- function(x, name = NULL, env = NULL, ...){
  if (isTRUE(env$parent$numeric)){
    x = as.matrix(x)
    colnames(x) <- name
  }
  return(structure(
    x,
    label = name,
    env   = env,
    class = c("rgo_var", class(x))
  ))
}
#' @exportS3Method as_var NULL
as_var.NULL <- function(x, name = NULL, env = NULL, ...) NULL
#' @exportS3Method as_var character
as_var.character <- function(x, name = NULL, env = NULL, leave_out = NULL, prefix = TRUE, sep = "_", ...){
  numeric <- env$numeric
  if (!isTRUE(numeric)) return(structure(x, label = name, env = env, class = c("rgo_var", class(x))))
  levels <- sort(unique(x))
  if (is.null(leave_out)){lo <- 1
  } else {lo <- which(levels == leave_out)}
  mat <- Reduce(cbind, lapply(levels[-lo], function(i) as.numeric(x == i)))
  if (prefix){colnames(mat) <- paste(name, levels[-lo], sep = sep)
  } else {colnames(mat)       <- levels[-lo]}
  attr(mat, "label")  <- name
  attr(mat, "levels") <- levels
  attr(mat, "env")    <- env
  class(mat) <- c("dummy_var", "rgo_var", "matrix")
  return(mat)
}
#' @exportS3Method as_var factor
as_var.factor <- function(x, name = NULL, env = NULL, leave_out = NULL, prefix = TRUE, sep = "_", ...){
  numeric <- env$numeric
  if (!isTRUE(numeric)) return(structure(x, label = name, env = env, class = c("rgo_var", class(x))))
  levels <- sort(unique(x))
  if (is.null(leave_out)){lo <- 1
  } else {lo <- which(levels == leave_out)}
  mat <- Reduce(cbind, lapply(levels[-lo], function(i) as.numeric(x == i)))
  if (prefix){colnames(mat) <- paste(name, levels[-lo], sep = sep)
  } else {colnames(mat)       <- levels[-lo]}
  attr(mat, "label")  <- name
  attr(mat, "levels") <- levels
  attr(mat, "env")    <- env
  class(mat) <- c("dummy_var", "rgo_var", "matrix")
  return(mat)
}
#' @exportS3Method as_var rgo_selection
as_var.rgo_selection <- function(x, name = NULL, env = NULL, ...) return(x)
#' @exportS3Method as_var rgo_var
as_var.rgo_var <- function(x, name = NULL, env = NULL, ...) return(x)
#' @exportS3Method as_var rgo_var_list
as_var.rgo_var_list <- function(x, name = NULL, env = NULL, ...) return(x)

combine_var <- function(x, env, ...) UseMethod("combine_var")
#' @exportS3Method combine_var dummy_var
combine_var.dummy_var <- function(x, env, ...){
  if (is.null(colnames(x)))
    stop("Name Error: No column names provided for the dummy variables.")
  if (nrow(x) != env$nrow)
    stop(sprintf("Dimension Error: Number of rows for dummy matrix %s (nrow=%d) does not match number of rows in data set (nrow=%d)", get_name(x), nrow(x), env$nrow))
  env$new[[get_name(x)]]     <- x
  env$dummies[[get_name(x)]] <- colnames(x)
  if (is_hidden(x)) env$hidden <- c(env$hidden, attr(x, "hide"))
  return(env)
}
#' @exportS3Method combine_var rgo_hide
combine_var.rgo_hide <- function(x, env, ...){
  cols <- list(get_name(x), get_col_labels(x))
  env  <- NextMethod("combine_var", x, env, ...)
  if (is.null(env$hide)){env$hide <- list(cols)
  } else {env$hide[[length(env$hide)+1]] <- cols}
  return(env)
}
#' @exportS3Method combine_var rgo_selection
combine_var.rgo_selection <- function(x, env, ...){
  sel <- !(x[[1]] %in% names(env$new))
  x[[1]] <- x[[1]][sel]
  for (var in get_selection(x)) combine_var(var, env)
  if (is_hidden(x)) env$hidden <- c(env$hidden, attr(x, "hide"))
  return(env)
}
#' @exportS3Method combine_var rgo_var
combine_var.rgo_var <- function(x, env, ...){
  label <- get_name(x)
  if (NROW(x) != env$nrow){
    if (is.matrix(x))
      stop(sprintf("Dimension Error: Number of rows for matrix %s (nrow=%d) does not match number of rows in data set (nrow=%d)", label, nrow(x), env$nrow))
    x <- rep(x, length.out = env$nrow)
  }
  env$new[[label]] <- x
  if (is_hidden(x)) env$hidden <- c(env$hidden, attr(x, "hide"))
  return(env)
}
#' @exportS3Method combine_var rgo_var_list
combine_var.rgo_var_list <- function(x, env, ...){
  sel   <- !(names(x) %in% names(env$new))
  new_x <- x[sel]
  for (var in x[sel]) combine_var(var, env)
  return(env)
}
#' @exportS3Method combine_var rgo_var_env
combine_var.rgo_var_env <- function(x, env, ...){
  ml    <- parent.env(env)
  label <- get_name(x)
  if (is.null(label)) label <- sprintf("(%s)", length(ml$models)+1)
  ml$models[[label]] <- x
  ml$hide            <- c(ml$hide, x$hide)
  return(env)
}
#' @exportS3Method combine_var NULL
combine_var.NULL <- function(x, env, ...) return(env)

find_var <- function(name, env, enclos = NULL, ...){
  if (exists(name, env$new)) return(env$new[[name]])
  if (exists(name, env$parent$data)) return(as_var(env$parent$data[[name]], name, env, ...))
  if (!is.null(enclos) && exists(name, enclos)) return(as_var(enclos[[name]], name, env, ...))
  return(get0(name, inherits = TRUE, ifnotfound = NULL))
}

flatten <- function(x, ...) UseMethod("flatten")
#' @exportS3Method flatten default
flatten.default <- function(x, ...) return(x)
#' @exportS3Method flatten data.frame
flatten.data.frame <- function(x, ...) return(x)
#' @exportS3Method flatten list
flatten.list    <- function(x, ...){
  new_list <- list()
  for (i in x){
    val <- flatten(i)
    if (is.list(val) && !is.data.frame(val)){new_list <- c(new_list, val)
    } else {new_list[[length(new_list)+1]] <- val}
  }
  return(new_list)
}
#' @exportS3Method flatten rgo_selection
flatten.rgo_selection <- function(x, ...) get_selection(x)

get_col_labels <- function(x){
  if (inherits(x, "rgo_var_env")) return(x$yvar)
  if (inherits(x, "rgo_var_list")) return(sapply(x, function(i) Reduce(c, get_col_labels(i))))
  if (is.matrix(x)) return(colnames(x))
  return(attr(x, "label"))
}

get_env  <- function(x) attr(x, "env")

get_name <- function(x) attr(x, "label")

get_selection <- function(x, ...){
  l <- lapply(x[[1]], function(i){
    as_var(x[[2]]$parent$data[[i]], i, x[[2]], ...)
  })
  names(l) <- x[[1]]
  class(l) <- c("rgo_var_list", "rgo_var", "list")
  return(l)
}

is_hidden <- function(x) !is.null(attr(x, "hide"))
is_temp   <- function(x) isTRUE(attr(x, "temp"))

maybe_selection <- function(x, env, ...){
  n <- length(x)
  if (n == 0) return(NULL)
  if (n == 1) return(as_var(env$parent$data[[x]], x, env = env, ...))
  rgo_selection(x, env)
}

model_env <- function(parent, ...){
  env           <- new.env(parent = parent)
  env$parent    <- parent
  env$dummies   <- list()
  env$models    <- list()
  env$new       <- structure(list(), env = env, class = c("rgo_var_list", "rgo_var", "list"))
  class(env)    <- "rgo_var_env"
  return(env)
}

#' @export
`$.rgo_var_env` <- function(x, name)
  get0(name, x, inherits = TRUE, ifnotfound = NULL)
#' @export
`[[.rgo_var_env` <- function(x, i, ...)
  get0(i, x, inherits = TRUE, ifnotfound = NULL)

rename_var <- function(x, new_name, ...) UseMethod("rename_var")
#' @exportS3Method rename_var default
rename_var.default <- function(x, new_name, ...) as_var(x, new_name, ...)
#' @exportS3Method rename_var rgo_var
rename_var.rgo_var <- function(x, new_name, rename_cols = TRUE, ...){
  old_name <- get_name(x)
  if (is.character(new_name) || is.name(new_name)){
    new_name <- as.character(new_name)
    attr(x, "label") <- new_name
    if (rename_cols && is.matrix(x)) colnames(x) <- gsub(old_name, new_name, colnames(x), fixed = TRUE)
    return(x)
  }
  new_name <- as_lambda(new_name)
  if (is.function(new_name)){
    attr(x, "label") <- new_name(old_name)
    if (rename_cols && is.matrix(x)) colnames(x) <- sapply(colnames(x), new_name)
  } else { stop(sprintf("Type Error: object on the left-hand side of ':=' is class %s and cannot be used to name variables.", class(new_name)[1])) }
  return(x)
}

rgo_selection <- function(cols, env)
  structure(
    list(columns = cols,
         env     = env),
    class = c("rgo_selection", "rgo_var", "list")
  )

rm_rgo_var <- function(x, env, ...) UseMethod("rm_rgo_var")
#' @exportS3Method rm_rgo_var default
rm_rgo_var.default <- function(x, env, ...) NULL
#' @exportS3Method rm_rgo_var call
rm_rgo_var.call    <- function(x, env, ...)
  rm_rgo_var(reg_select_var.call(x, env, ..., initial = FALSE), env, ...)
#' @exportS3Method rm_rgo_var character
rm_rgo_var.character <- function(x, env, ...){
  if (length(env$new) == 0)
    return(rgo_selection(colnames(env$data)[!(colnames(env$parent$data) %in% x)], env))
  to_rm <- sapply(env$new, function(var) get_name(var) == as.character(x))
  if (any(to_rm)){
    new_list              <- env$new[!to_rm]
    attr(new_list, "env") <- env
    class(new_list)       <- c("rgo_var_list", "rgo_var", "list")
    env$new               <- new_list
  }
  return(NULL)
}
#' @exportS3Method rm_rgo_var name
rm_rgo_var.name <- function(x, env, ...)
  return(rm_rgo_var.character(as.character(x), env))
#' @exportS3Method rm_rgo_var NULL
rm_rgo_var.NULL <- function(x, env, ...) NULL
#' @exportS3Method rm_rgo_var numeric
rm_rgo_var.numeric <- function(x, env, ...){
  if (all(x == 1)) env$intercept <- FALSE
  return(rm_rgo_var(env$intercept_name, env, ...))
}
#' @exportS3Method rm_rgo_var rgo_selection
rm_rgo_var.rgo_selection <- function(x, env, ...)
  rm_rgo_var(x[[1]], env, ...)
#' @exportS3Method rm_rgo_var rgo_var
rm_rgo_var.rgo_var <- function(x, env, ...)
  rm_rgo_var(get_name(x), env, ...)
#' @exportS3Method rm_rgo_var rgo_var_list
rm_rgo_var.rgo_var_list <- function(x, env, ...)
  rm_rgo_var(sapply(x, get_name), env, ...)


select_formula <- function(x, env, ...) UseMethod("select_formula")
#' @exportS3Method select_formula default
select_formula.default <- function(x, env,...) reg_select_var(x, env, ...)
#' @exportS3Method select_formula call
select_formula.call <- function(x, env, ...){
  first <- as.character(x[[1]])
  if (first == "+")
    return(c(select_formula(x[[2]], env, ...), select_formula(x[[3]], env, ...)))
  if (first == ":")
    return(select_formula(x[[2]], env, ...) * select_formula(x[[3]], env, ...))
  if (first == "*"){
    lhs <- select_formula(x[[2]], env, ...)
    rhs <- select_formula(x[[3]], env, ...)
    return(c(lhs, rhs, lhs * rhs))
  }
  ### Other operators???
  if (first == "I") return(reg_select_var(x[[2]], env, ...))
  return(reg_select_var.call(x, env, ...))
}
#' @exportS3Method select_formula name
select_formula.name <- function(x, env, ...) find_var(as.character(x), env, ...)
