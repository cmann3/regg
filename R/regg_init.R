# ============================================================================ #
# CONTENTS                                                                     #
#   - init_regg : primary, internal function for 'regg'                        #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - stat_n  -> stat_common.R                                                 #
# ============================================================================ #

#' @importFrom rlang quo eval_tidy
#' @importFrom stats na.omit
init_regg <- function(reg_obj,
                       ...,
                       class         = NULL,
                       data          = NULL,
                       default_se    = NULL,
                       default_stats = list(),
                       default_tests = list(),
                       hash          = TRUE,
                       intercept     = TRUE,
                       label         = "",
                       method        = NULL,
                       na.action     = NULL,
                       post_fit      = list(),
                       quoted_terms  = NULL,
                       response      = 1,
                       se            = NULL,
                       stats         = list(),
                       subset        = NULL,
                       tests         = list())
{

  ### Initialize Model Object
  n   <- length(reg_obj$models)
  if (n == 0){parent <- reg_obj
  } else if (length(reg_obj$models[[n]]$models) > 0){
    model_temp <- reg_obj$models[[n]]
    parent     <- model_temp$models[[length(model_temp$models)]]
  } else {parent <- reg_obj$models[[n]]}
  mod <- structure(
    new.env(hash = hash, parent = parent),
    class = c(class, "rgo_model", "rgo_m", "rgo")
  )
  mod$is_fitted           <- FALSE
  mod$response            <- response
  reg_obj$models[[n + 1]] <- mod
  if (n == 0){names(reg_obj$models)      <- label
  }   else {  names(reg_obj$models)[n+1] <- label}


  ### Store Data & Subset Rows
  if (!is.null(data)){
    mod$data <- quo(data)
    mod$nrow <- nrow(data)
  }
  sub <- substitute(subset)
  if (!is.null(sub)){
    mod$subset_call <- sub
    sub_values     <- reg_select(eval_tidy(mod$data), model = mod, quoted = list(sub),
                                 numeric = FALSE, response = FALSE)
    if (NCOL(sub_values) != 1 && NROW(sub_values) != mod$nrow)
      stop(sprintf("Dimension Mismatch Error: Expression (nrow = %d, ncol = %d) must be a vector or matrix with one column and rows equal to the data set (nrow = %d).",
                   NCOL(sub_values), NROW(sub_values), mod$nrow))
    mod$subset     <- sub_values
  }
  mod$na.action <- na.action

  ### Split dots into named versus unnamed, named
  dots <- eval(substitute(alist(...)))
  if (length(dots) > 0){
    if (is.null(names(dots))){
      mod$selection <- dots
    } else {
      unnamed <- names(dots) == ""
      if (length(dots[unnamed]) > 0) mod$selection <- dots[unnamed]
      named   <- lapply(dots[!unnamed], function(obj){
        rgo_select(get_data(mod), quoted = list(obj), numeric = FALSE, response = FALSE, drop = TRUE)
      })
      list2env(named, envir = mod)
    }
  }
  if (!is.null(quoted_terms)) mod$selection <- quoted_terms

  ### Method & SE
  meth <- substitute(method)
  if (!is.null(method)){
    if (!is.function(method)) stop("Type Error: Regression method must be a function.")
    mod$method      <- method
    mod$method_name <- as.character(meth)
  }


  if (!is.null(se) || !is.null(default_se)){
    if (is.null(se)) se <- default_se
    if (!is.function(se)) stop("Type Error: Regress standard errors must be calculated via a function.")
    mod$se <- se
  }

  ### Post Estimation Functions
  if (!is.list(post_fit)) post_fit <- list(post_fit)
  if (!all(sapply(post_fit, is.function))) stop("Type Error: Everything supplied to 'post_fit' must be a function.")
  mod$post_fit <- post_fit

  ### Get Tests & Stats
  mod$tests         <- list()
  mod$default_tests <- lapply(default_tests, function(t){
    if (is.character(t)) t <- get0(paste0("test_", t), mode = "function")
    if (!is.function(t)) stop("Type Error: All supplied tests must be functions.")
    return(t)
  })
  if (!is.list(tests)){
    if (is.character(tests)){tests <- as.list(tests)
    } else {tests <- list(tests)}
  }
  tests <- lapply(tests, function(t){
    if (is.character(t)) t <- get0(paste0("test_", t), mode = "function")
    if (!is.function(t)) stop("Type Error: All supplied tests must be functions.")
    return(t)
  })
  mod$test_fns <- unique(c(parent.env(mod)$test_fns, tests))
  w_tests      <- !(mod$test_fns %in% mod$default_tests)
  mod$test_fns <- mod$test_fns[w_tests]


  mod$stats          <- list()
  mod$stats_coef     <- list()
  mod$default_stats  <- c(list(stat_n), lapply(default_stats, function(t){
    if (is.character(t)) t <- get0(paste0("stat_", t), mode = "function")
    if (!is.function(t)) stop("Type Error: All supplied statistics must be functions.")
    return(t)
  }))
  if (!is.list(stats)){
    if (is.character(stats)){stats <- as.list(stats)
    } else {stats <- list(stats)}
  }
  stats <- lapply(stats, function(t){
    if (is.character(t)) t <- get0(paste0("stat_", t), mode = "function")
    if (!is.function(t)) stop("Type Error: All supplied statistics must be functions.")
    return(t)
  })
  mod$stat_fns   <- unique(c(parent.env(mod)$stat_fns, stats))
  w_stats        <- !(mod$stat_fns %in% mod$default_stats)
  mod$stat_fns   <- mod$stat_fns[w_stats]

  ### Return
  return(reg_obj)
}
