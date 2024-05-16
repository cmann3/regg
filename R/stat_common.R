# ============================================================================ #
# CONTENTS                                                                     #
#   - stat_adj_r2 : include adjusted R-squared                                 #
#   - stat_conf   : include confidence intervals around coefficients           #
#   - stat_mae    : include Mean Absolute Error                                #
#   - stat_n      : include number of observations                             #
#   - stat_r2     : include R-squared                                          #
#   - stat_rmse   : include Root Mean Squared Error                            #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - add_stat                  -> stat_add.R                                  #
#   - get_mss, get_rss, get_se  -> get_reg_stats.R                             #
# ============================================================================ #

#' Calculate and Add Common Regression Statistics
#'
#' Common statistics include the number of observations \code{(N)}, the R-squared
#' \code{(R2)} and adjusted R-squared \code{(adj_R2)}. Each are included in the
#' \code{"stats"} field of each \code{rgo_model}.
#'
#' @param x \code{regg} or \code{rgo_model} object
#' @param ... objects passed to methods
#'
#' @return numeric value of statistic
#' @rdname stats_common
#' @name stats_common
#'
NULL


#' @export
#' @rdname stats_common
stat_adj_r2 <- function(x, ...) UseMethod("stat_adj_r2")

#' @exportS3Method stat_adj_r2 rgo_model
stat_adj_r2.rgo_model <- function(x, ...){
  r2 <- x[["stats"]]$R2
  if (is.null(r2)) r2 <- stat_r2(x, ...)
  x$stats$adj.r.squared <- as_stat(1 - (1 - r2) * ((x$nrow - as.integer(has_intercept(x)))/get_df(x)), label = "Adj. R2")
  return(x$stats$adj.r.squared)
}

#' @exportS3Method stat_adj_r2 regg
stat_adj_r2.regg <- function(x, ...) add_stat(x, stat_adj_r2, ...)

#' @exportS3Method stat_adj_r2 lm
stat_adj_r2.lm <- function(x, ...) summary(x)$adj.r.squared



#' @export
#' @rdname stats_common
stat_conf <- function(x, ...) UseMethod("stat_conf")

#' @exportS3Method stat_conf rgo_model
#' @importFrom stats coef
stat_conf.rgo_model <- function(x, p = 0.95, ...){
  coeff <- coef(x)
  intvl <- qt(1 - (1-p)/2, get_df(x), lower.tail = TRUE) * get_se(x)
  vals  <- list(as_stat(coeff - intvl, paste0("Lower ", p*100, "%")),
                as_stat(coeff + intvl, paste0("Upper ", p*100, "%")))
  x$stats_coef[[paste0("lower_", p*100)]] <- vals[[1]]
  x$stats_coef[[paste0("upper_", p*100)]] <- vals[[2]]
  return(vals)
}

#' @exportS3Method stat_conf regg
stat_conf.regg <- function(x, ...) add_stat(x, stat_conf, ...)



#' @export
#' @rdname stats_common
stat_mae <- function(x, ...) UseMethod("stat_mae")

#' @exportS3Method stat_mae rgo_model
#' @importFrom stats resid
stat_mae.rgo_model <- function(x, p = 0.95, ...){
  val          <- as_stat(mean(abs(resid(x))), "RMSE")
  x$stats$mae  <- val
  return(val)
}

#' @exportS3Method stat_rmse regg
stat_mae.regg <- function(x, ...) add_stat(x, stat_mae, ...)



#' @export
#' @rdname stats_common
stat_n <- function(x, ...) UseMethod("stat_n")

#' @exportS3Method stat_n rgo_model
stat_n.rgo_model <- function(x, ...){
  n <- find_else(x, "nrow", nrow(get_x(x)), write = FALSE)
  x$stats$N <- as_stat(n, "N")
  return(n)
}

#' @exportS3Method stat_n regg
stat_n.regg <- function(x, ...) add_stat(x, stat_n, ...)

#' @exportS3Method stat_n lm
stat_n.lm <- function(x, ...) NROW(x$qr$qr)



#' @export
#' @rdname stats_common
stat_r2 <- function(x, ...) UseMethod("stat_r2")

#' @exportS3Method stat_r2 rgo_model
stat_r2.rgo_model <- function(x, ...){
  mss <- get_mss(x)
  x$stats$r.squared <- as_stat(mss / (mss + get_rss(x)), label = "R2")
  return(x$stats$r.squared)
}

#' @exportS3Method stat_r2 regg
stat_r2.regg <- function(x, ...) add_stat(x, stat_r2, ...)

#' @exportS3Method stat_r2 lm
stat_r2.lm <- function(x, ...) summary(x)$r.squared



#' @export
#' @rdname stats_common
stat_rmse <- function(x, ...) UseMethod("stat_rmse")

#' @exportS3Method stat_rmse rgo_model
#' @importFrom stats resid
stat_rmse.rgo_model <- function(x, p = 0.95, ...){
  val          <- as_stat(sqrt(mean(resid(x)^2)), "RMSE")
  x$stats$rmse <- val
  return(val)
}

#' @exportS3Method stat_rmse regg
stat_rmse.regg <- function(x, ...) add_stat(x, stat_rmse, ...)




