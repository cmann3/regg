# ============================================================================ #
# CONTENTS                                                                     #
#   - glance : create a table where each column is a regression statistic      #
#   - tidy   : create a coefficient table from a regg, rgo object              #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
# ============================================================================ #

#' Methods for Augmenting Data
#'
#' These methods extract information about the regression object and place into a
#' \code{\link[tibble]{tibble}}. \code{tidy} extracts the regression coefficient
#' table, along with model names if appropriate. Each column \code{glance} represents
#' a regression statistic such as the R-squared. \code{augment} adds information
#' related to residuals, fitted values, and so forth to the data set.
#'
#' @param x object of class \code{regg} or \code{rgo_model}
#' @param label default label of the model when multiple models are present.
#' @param bind should the results be bound into a single tibble data.frame?
#' @param type character vector describing the type of results to be returned -
#' coefficient table or tests.
#' @param ... objects passed to methods
#'
#' @return \code{tibble} object
#' @rdname tidy
#' @name tidy
#'
NULL


#' @rdname tidy
#' @exportS3Method augment rgo_m
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom generics augment components
augment.rgo_m <- function(x, ...){
  comps <- components(x, bind = TRUE, ...)
  if (is.null(comps$model)) return(bind_cols(get_data(x), comps))
  models <- unique(comps$model)
  dat    <- get_data(x)
  bind_rows(lapply(models, function(m) mutate(dat, model = m))) |>
    left_join(comps, by = "model")
}


#' @rdname tidy
#' @exportS3Method components rgo_model
#' @importFrom generics components
#' @importFrom dplyr bind_rows
components.rgo_model <- function(x, label = NULL, bind = TRUE, objects = c("resids", "fitted", "hat", "cooks"), ...){
  fit(x, ...)
  lab2 <- x[["label"]]
  if (!is.null(lab2) && lab2 != "") label <- lab2

  if (isTRUE(x[["multi_models"]])){
    mods <- x[["models"]]
    dfs  <- lapply(seq_along(mods), function(i){
      env <- mods[[i]]
      if (is.null(env[["label"]]) || env[["label"]] == ""){
        if (is.null(label)){label <- i
        } else {label <- paste0(label, ".", i)}
      } else {label <- env[["label"]]}
      components(mods[[i]], label = label, bind = bind, objects = objects, ...)
    })
    if (bind) return(bind_rows(dfs))
    return(dfs)
  }

  res <- lapply(objects, function(i){
    fn <- get0(sprintf("get_%s", i), mode = "function")
    if (is.null(fn)) return(NULL)
    fn(x)
  })
  names(res) <- sprintf(".%s", objects)
  if (!bind) return(res)
  res <- bind_rows(res)
  if (!is.null(label)){
    res$model <- label
    res       <- select(model, everything())
  }
  return(res)
}

#' @rdname tidy
#' @exportS3Method components regg
#' @importFrom dplyr bind_rows
#' @importFrom generics components
components.regg <- function(x, bind = TRUE, ...){
  mods     <- x[["models"]]
  multiple <- length(mods) > 1
  if (length(mods) > 1){
    dfs    <- lapply(seq_along(mods), function(i)
      components(mods[[i]], label = i, bind = bind, ...)
    )
  } else { dfs <- lapply(mods, components, ...) }
  if (bind) return(bind_rows(dfs))
  return(dfs)
}


#' @rdname tidy
#' @exportS3Method glance rgo_model
#' @importFrom dplyr bind_rows select transmute
#' @importFrom generics glance
#' @importFrom tibble as_tibble
glance.rgo_model <- function(x, label = NULL, bind = TRUE, ...){
  fit(x, ...)
  lab2  <- x[["label"]]
  if (!is.null(lab2) && lab2 != "") label <- lab2

  if (isTRUE(x[["multi_models"]])){
    mods <- x[["models"]]
    dfs  <- lapply(seq_along(mods), function(i){
      env <- mods[[i]]
      if (is.null(env[["label"]]) || env[["label"]] == ""){
        if (is.null(label)){label <- i
        } else {label <- paste0(label, ".", i)}
      } else {label <- env[["label"]]}
      glance(mods[[i]], label = label, bind = bind, ...)
    })
    if (bind) return(bind_rows(dfs))
    return(dfs)
  }

  if (is.null(label)){tb <- list()
  } else {tb <- list(model = label)}

  stats <- x$stats
  for (i in seq_along(stats)){
    if (is_stat(stats[[i]]) && !is.null(get_name(stats[[i]]))){
      nm <- get_name(stats[[i]])
    } else {nm <- names(stats)[i]}
    tb[[nm]] <- stats[[i]]
  }
  return(as_tibble(tb))
}

#' @rdname tidy
#' @exportS3Method glance regg
#' @importFrom dplyr bind_rows
#' @importFrom generics glance
glance.regg <- function(x, bind = TRUE, ...){
  mods     <- x[["models"]]
  multiple <- length(mods) > 1
  if (length(mods) > 1){
    dfs    <- lapply(seq_along(mods), function(i)
      glance(mods[[i]], label = i, bind = bind, ...)
    )
  } else { dfs <- lapply(mods, glance, ...) }
  if (bind) return(bind_rows(dfs))
  return(dfs)
}



#' @rdname tidy
#' @exportS3Method tidy rgo_model
#' @importFrom dplyr bind_rows
#' @importFrom generics tidy
#' @importFrom tibble tibble
tidy.rgo_model <- function(x, label = NULL, bind = TRUE, type = c("coef", "test", "stat", "full"), add_type = FALSE, ...){
  fit(x, ...)
  lab2 <- x[["label"]]
  if (!is.null(lab2) && lab2 != "") label <- lab2

  if (isTRUE(x[["multi_models"]])){
    mods <- x[["models"]]
    dfs  <- lapply(seq_along(mods), function(i){
      env <- mods[[i]]
      if (is.null(env[["label"]]) || env[["label"]] == ""){
        if (is.null(label)){label <- i
        } else {label <- paste0(label, ".", i)}
      } else {label <- env[["label"]]}
      tidy(mods[[i]], label = label, bind = bind, type = type, ...)
    })
    if (bind) return(bind_rows(dfs))
    return(dfs)
  }

  type <- type[1]
  if (type == "full"){
    tlist <- lapply(c("coef", "test", "stat"), function(i){
      tidy(x, label = label, bind = bind, type = i, add_type = TRUE, ...)
    })
    flist <- bind_rows(tlist)
    attr(flist, "hide") <- Reduce(rbind, lapply(tlist, function(i) attr(i, "hide")))
    return(flist)
  }

  stars <- x$star_fn
  if (is.null(stars)) stars <- stars_default()

  if (type == "coef"){
    cf <- coefficients(x)
    tb <- tibble(
      term     = names(cf),
      estimate = cf
    )
    if (!is.null(label)){
      tb$model <- label
      tb       <- select(tb, model, everything())
    }
    stats_coef <- x$stats_coef
    for (i in seq_along(stats_coef)){
      val <- stats_coef[[i]]
      if (!is.null(get_name(val))){nm <- get_name(val)
      } else {nm <- names(stats_coef)[i]}
      tb[[nm]] <- as.numeric(val)
      if (names(stats_coef)[i] == "pval") tb[["signif"]] <- stars(tb[[nm]])
    }

    if (!is.null(x$hide)){
      hidden_names  <- character(0)
      hidden_length <- numeric(0)
      for (grp in x$hide){
        tb <- tb[!(tb$term %in% grp[[2]]),]
        hidden_names  <- c(hidden_names, grp[[1]])
        hidden_length <- c(hidden_length, length(grp[[2]]))
      }
      hide_df <- data.frame(term     = hidden_names,
                            value    = "Yes",
                            estimate = hidden_length)
      if (!is.null(label)) hide_df$model <- label
      attr(tb, "hide") <- hide_df
    }
  } else if (type == "test"){
    if (length(x$tests) == 0) return(NULL)
    tb <- as_tibble(Reduce(rbind, lapply(seq_along(x$tests), function(i){
      val <- x$tests[[i]]
      if (is_test(val)){name <- get_name(val)
      } else {name <- names(x$test)[i]}
      data.frame(term = name, estimate = val$stat, pval = val$pval)
    })))
    if (!is.null(label)) tb <- transmute(tb, model = label, term, estimate, pval)
    tb$signif <- stars(tb$pval)
  } else if (type == "stat"){
    if (length(x$stats) == 0) return(NULL)
    tb <- as_tibble(Reduce(rbind, lapply(seq_along(x$stats), function(i){
      val <- x$stats[[i]]
      if (is_stat(val)){name <- get_name(val)
      } else {name <- names(x$stats[i])}
      data.frame(term = name, estimate = as.numeric(val))
    })))
    if (!is.null(label)) tb <- transmute(tb, model = label, term, estimate)
  }
  if (add_type) tb$type <- type
  return(tb)
}

#' @rdname tidy
#' @exportS3Method tidy regg
#' @importFrom dplyr bind_rows
#' @importFrom generics tidy
tidy.regg <- function(x, bind = TRUE, ...){
  mods     <- x[["models"]]
  multiple <- length(mods) > 1
  if (length(mods) > 1){
    dfs    <- lapply(seq_along(mods), function(i)
      tidy(mods[[i]], label = i, ...)
    )
  } else { dfs <- lapply(mods, tidy, ...) }
  if (bind){
    df2 <- bind_rows(dfs)
    if (!is.null(df2$type)){
      w <- c(which(df2$type == "coef"), which(df2$type == "test"), which(df2$type == "stat"))
      df2 <- df2[w,]
    }
    attr(df2, "hide") <- Reduce(rbind, lapply(dfs, function(i) attr(i, "hide")))
    return(df2)
  }
  return(dfs)
}
