# ============================================================================ #
# CONTENTS                                                                     #
#   - print   : methods for printing rgo objects                               #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - tidy, glance -> tidy.R                                                   #
# ============================================================================ #

#' @exportS3Method print regg
#' @importFrom generics fit tidy glance
print.regg <- function(x, format = "%estimate%signif\n(%se)", ...){
  fit(x)
  n <- length_models(x)

  ## Get Data Frame to Print
  df      <- tidy(x, type = "full")
  df_hide <- attr(df, "hide")

  ## Styling
  font_list <- list(
    rgo_font_formatter(function(i) if (is.numeric(i)) i < 0 else rep(FALSE, length(i)), fg = "red"),
    rgo_font_formatter(function(i) i == "",
                       fg = 240, at = "it", at_else = "bold",
                       cols = "signif", cols_on = "term")
  )
  style = get_table_style(x, ...)

  ## Print
  if (n == 1){
    if (!is.null(df_hide)){
      below_text <- sprintf(
        "Not Shown: %s",
        paste(sprintf("%s [%d]", df_hide$term, df_hide$estimate), collapse = ", "))
    } else {below_text <- NULL}
    new_names <- list(se = "std. err", tstat = "t stat", pval = "p(>|.|)", signif = " ")
    reg_table_print(df, name_list = new_names, table_by = "type", rows = "term",
                    do_format = FALSE, below = below_text, style = style, ...)
  } else {
    if (!is.null(df_hide)){
      hide2 <- data.frame(term = unique(df_hide$term))
      nterm <- nrow(hide2)
      for (i in unique(df$model)){
        hide_i <- df_hide[df_hide$model == i,]
        if (NROW(hide_i) == 0){hide2[[paste0("mod", i)]] <- rep("No", nterm)
        } else {
          hide2[[paste0("mod", i)]] <- ifelse(hide2$term %in% hide_i$term, "Yes", "No")
        }
      }
      hide_list <- list(coef = hide2)
    } else {hide_list <- NULL}
    reg_table_print(df, table_by = "type", spread_by = "model", rows = "term",
                    do_format = TRUE, insert_tab = hide_list, style = style, ...)
  }
}

#' @exportS3Method print rgo_model
#' @importFrom generics fit tidy glance
print.rgo_model <- function(x, ...){
  fit(x)
  n <- length_models(x)
  if (n == 1) return( print_single_model(x, ...) )

  tdf <- tidy(x, ...)
  print(tdf, ...)

  gdf <- glance(x, ...)
  print(gdf, ...)
}
