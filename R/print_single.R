# ============================================================================ #
# CONTENTS                                                                     #
#   - print_single_model : called to print a regg object with only 1 model     #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - alter_names, make_dbl_line, make_line      -> print_helpers.R            #
#   - get_name                                   -> reg_select_helpers.R       #
#   - is_stat                                    -> stat_helpers.R             #
#   - stars_default                              -> stars.R                    #
# ============================================================================ #

### NOTES: Strategy, get negative indices  (adding 1) for each. Then change to red
###        after made to list. Same with those that are not significant ...
### ALSO:  Need to format stats to specific digits!


#' @importFrom dplyr bind_rows
#' @importFrom stats coef
#' @importFrom tidyr pivot_longer pivot_wider
print_single_model <- function(x,
                               digits        = 4,
                               styling       = TRUE,
                               name_list     = NULL,
                               trunc_dummies = TRUE,
                               ...)
{
  x  <- find_single_model(x)

  ## Determine whether in R Markdown
  if (in_knitr()) styling <- FALSE

  ## Extract Formatting from 'x'
  form <- x$format

  ## Get List to Alter Names
  if (is.null(name_list)) name_list <- list(se = "Std. Error", tstat = "t Stat", pval = "P(>|_|)")

  ## Get Significance Markers
  stars <- x$star_fn
  if (is.null(stars)) stars <- stars_default()

  ## Set Location Vector
  loc_num <- numeric(0)
  loc_chr <- character(0)

  ## Coeff Table
  cf <- coef(x)
  dt <- data.frame(Term         = names(cf),
                   Estimate     = cf)
  for (i in seq_along(x$stats_coef)) {
    st <- x$stats_coef[[i]]
    if (is_stat(st) && !is.null(get_name(st))){
      dt[[get_name(st)]] <- st
    } else {dt[[names(x$stats_coef)[i]]] <- st}
  }

  if (!is.null(x$hide)){
    hidden <- Reduce(c, sapply(x$hide, function(i) i[[2]]))
    dt     <- dt[!(dt$Term %in% hidden), ]
    hide_msg <- paste0("Not Shown: ", paste(Reduce(c, sapply(x$hide, function(i) paste0(i[[1]], " [", length(i[[2]]), "]"))), collapse = ", "), "\n")
  } else {hide_msg <- ""}
  if (trunc_dummies && !is.null(x$dummies)){
    dummies <- x$dummies
    for (d in seq_along(dummies)){
       dums <- dummies[[d]]
       w_d  <- which(dt$Term %in% dums)
       if (length(w_d) > 1){
          dt$Term[w_d] <- sapply(dt$Term[w_d], function(i) gsub("^[^_]+_", "  ", i))
          loc_num      <- c(loc_num, min(w_d))
          loc_chr      <- c(loc_chr, paste0("\x1b[4m", names(dummies)[d], "\x1b[0m"))
       }
    }
  }
  ncoef  <- nrow(dt)
  values <- dt$Estimate
  if (!is.null(dt$pval)){
    dt$Signif <- stars(dt$pval)
    dt_b <- pivot_longer(dt, cols = -c(Term, Signif))
  } else {dt_b <- pivot_longer(dt, cols = -Term)}
  dt_b$value <- format(dt_b$value, justify = "right", digits = digits)


  ## Test Table
  if (length(x$tests) > 0){
    dt2 <- Reduce(rbind, lapply(seq_along(x$tests), function(i){
      test <- x$tests[[i]]
      if (is_test(test) && !is.null(get_name(test))){name <- get_name(test)
      } else {name <- names(x$tests)[i]}
      data.frame(Term = name, Estimate = test$stat, pval = test$pval)
    }))
    ntest     <- nrow(dt2)
    values    <- c(values, dt2$Estimate)
    if (!is.null(dt2$pval)){
      dt2$Signif <- stars(dt2$pval)
      dt2_b <- pivot_longer(dt2, cols = -c(Term, Signif))
    } else {dt2_b <- pivot_longer(dt2, cols = -Term)}
    dt2_b$value <- format(dt2_b$value, justify = "right", digits = digits)
  } else {
    dt2   <- data.frame()
    dt2_b <- dt2
    ntest <- 0
  }



  ## Stats Table
  if (length(x$stats) > 0){
    dt3 <- Reduce(rbind, lapply(seq_along(x$stats), function(i){
      stat <- x$stats[[i]]
      if (is_stat(stat) && !is.null(get_name(stat))){name <- get_name(stat)
      } else {name <- names(x$stats)[i]}
      data.frame(Term = name, name = "Estimate", value = stat[[1]], Signif = " ")
    }))
    wn <- which(dt3$Term == "N")
    if (length(wn) > 0){
      dt_n       <- dt3[wn, ]
      dt_o       <- dt3[-wn, ]
      values     <- c(values, dt_n$value, dt_o$value)
      dt_n$value <- format(dt_n$value, digits = digits, big.mark = ",", justify = "right")
      dt_o$value <- format(dt_o$value, digits = digits, justify = "right")
      dt3_b      <- rbind(dt_n, dt_o)
    } else {
      dt3_b      <- dt3
      values     <- c(values, dt3$value)
    }
  } else {
    dt3   <- data.frame()
    dt3_b <- dt3_b
  }
  nstat <- nrow(dt3)


  ## Combine
  df        <- bind_rows(dt_b, dt2_b, dt3_b)
  df$value  <- format(df$value, justify = "right", ...)
  nchar_df  <- nchar(df$value[1])
  df        <- pivot_wider(df, values_fill = " ")
  signif    <- df$Signif
  df$Signif <- NULL
  ndf       <- nrow(df) + 1
  just      <- c("left", rep("right", ncol(df)-1))
  df_list   <- lapply(seq_along(df), function(i){
    format(c(alter_names(colnames(df)[i], list = name_list), df[[i]]),
           justify = just[i])
  })
  ncols    <- length(df_list)
  df_list[[ncols + 1]]  <- format(c(" ", signif))

  ## Get Special Locations
  width    <- sum(sapply(df_list, function(i) nchar(i[[1]])) + 1) - 1
  mid_line <- make_line(width)
  top_line <- make_hvy_line(width)
  line_at  <- cumsum(c(1, ncoef, ntest))

  ## Color
  if (styling){
    dt_a      <- bind_rows(dt, dt2)

    ## Alter Fonts
    if (is.null(form) || is.null(form$font)){
      font_list <- list(
        rgo_font_formatter(function(i) if (is.numeric(i)) i < 0 else rep(FALSE, length(i)), fg = "red"),
        rgo_font_formatter(function(i) !is.na(i) & grepl("^[[:blank:]]+$", i),
                    fg = 240, at = "it", at_else = "bold",
                    cols = "Signif", cols_on = "Term")
      )
    } else {font_list <- form$font}
    for (f in font_list){
      fn_ansi <- as_ansi(f$font)
      fe_ansi <- as_ansi(f$font_else)
      if (is.null(f$cols)){
        cols    <- seq_along(dt_a)
        cols_on <- cols
      } else {
        cols    <- which(colnames(dt_a) %in% f$cols)
        if (is.null(f$cols_on)){cols_on <- cols
        } else {cols_on <- which(colnames(dt_a) %in% f$cols_on)}
      }
      for (i in seq_along(cols)){
        col        <- cols[i]
        con        <- cols_on[i]
        meets_cond <- f$cond(dt_a[[col]])
        w_cond     <- which(meets_cond) + 1
        if (length(w_cond) > 0){
          df_list[[con]][w_cond] <- paste0(fn_ansi, df_list[[con]][w_cond], "\x1b[0m")
          if (!is.null(fe_ansi)){
            w_else <- which(!meets_cond) + 1
            df_list[[con]][w_else] <- paste0(fe_ansi, df_list[[con]][w_else], "\x1b[0m")
          }
        }
      }
    }
  }


  #if (styling){
   # w_neg <- which(values < 0)
    #if (length(w_neg) > 0){
     # w_neg <- w_neg + 1
      #df_list[[2]][w_neg] <- make_red(df_list[[2]][w_neg])
    #}
    #w_insig <- grep("^[[:blank:]]+$", signif[1:(ncoef + ntest)])
    #if (length(w_insig) > 0){
    #  df_list[[1]][w_insig+1] <- make_faint(df_list[[1]][w_insig+1])
    #  w_sig <- setdiff(1:(ncoef+ntest) + 1, w_insig + 1)
    #  df_list[[1]][w_sig] <- make_bold(df_list[[1]][w_sig])
    #}
  #}

  ## Print Name
  y_name <- x$yvar

  ## Print Table
  cat("\n")
  if (!is.null(y_name)) cat("Dependent Variable: ", paste(y_name, collapse = ", "), "\n")
  cat(top_line, "\n")
  for (i in 1:ndf){
    for (j in 1:(ncols+1)) cat(df_list[[j]][i], "")
    cat("\n")
    if (i %in% line_at) cat(mid_line, "\n")
    if (i %in% loc_num) cat(loc_chr[which(loc_num == i)], "\n")
  }
  cat(top_line, "\n", hide_msg, sep = "")


  ## Print Stars
  if (!is.null(attr(stars, "signif_levels"))){
    star_levels <- attr(stars, "signif_levels")
    cat(paste(c("0", paste0("'", names(star_levels), "' ", star_levels), "' ' 1"), collapse = " "), "\n")
  }
  cat("\n")
}

