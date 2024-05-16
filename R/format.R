




#' @export
reg_font_numeric <- function(x, cond = NULL, ...){
  x <- find_regg(x)
  if (is.null(x$format)){x$format <- list(font_if = list())
  } else if (is.null(x$format$font_if)){x$format$font_if <- list()}
  if (is.null(cond)){cond_new <- function(x){
    n <- length(x)
    ifelse(is.numeric(x), rep(TRUE, n), rep(FALSE, n))
  }} else {
    cond     <- as_lambda(cond)
    cond_new <- function(x){
      if (is.numeric(x)) return(cond(i))
      return(rep(FALSE, length(x)))
    }
  }
  x$format$font_if[[length(x$format$font_if)+1]] <- rgo_font_formatter(cond = cond_new, ...)
  return(x)
}

#' @export
reg_font_if <- function(x, cond, ...){
  x <- find_regg(x)
  if (is.null(x$format)){x$format <- list(font_if = list())
  } else if (is.null(x$format$font_if)){x$format$font_if <- list()}
  x$format$font_if[[length(x$format$font_if)+1]] <- rgo_font_formatter(cond = cond, ...)
  return(x)
}


#' @export
rgo_font <- function(family = NULL, fg = NULL, bg = NULL, at = NULL, ...)
  structure(
    list(family = family, fg = fg, bg = bg, at = at, ...),
    class = c("rgo_font", "list")
  )

#' @export
rgo_font_formatter <- function(cond = NULL, cols = NULL, cols_on = NULL,
                               font = NULL, font_else = NULL,
                               family = NULL, fg = NULL, bg = NULL, at = NULL,
                               family_else = NULL, fg_else = NULL, bg_else = NULL, at_else = NULL,
                               ...){
  if (!is.null(cond)) cond <- as_lambda(cond)
  if (is.null(font))  font <- rgo_font(family = family, fg = fg, bg = bg, at = at)
  if (is.null(font_else)) font_else <- rgo_font(family = family_else, fg = fg_else,
                                                bg = bg_else, at = at_else)
  structure(
    list(cond = cond, cols = cols, cols_on = cols_on, font = font, font_else = font_else, ...),
    class = c("rgo_font_formatter", "list")
  )
}
