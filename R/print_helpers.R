
adjust_to_width <- function(char, width, nl = TRUE){
  if (is.null(char)) return("")
  if (char == "") return("")
  if (nchar(char) == 1) return(paste0(paste(rep(char, width), collapse = ""), "\n"))
  paste0(paste(rep_len(strsplit(char, "")[[1]], length.out = width), collapse = ""), "\n")
}

alter_names <- function(x, list){
  sapply(x, function(i){
    val <- list[[i]]
    if (is.null(val)) return(i)
    return(val)
  })
}

find_single_model <- function(x){
  if (exists("models", envir = x, inherits = FALSE) && length(x[["models"]]) > 0)
    return(find_single_model(x[["models"]][[1]]))
  return(x)
}

get_table_style <- function(x, ...){
  dots        <- list(...)
  style_names <- c("bottom_sep", "digits", "font_fns", "format", "header_sep",
                   "justify", "styling", "table_sep", "top_sep")
  opts        <- getOption("regg")
  styl <- lapply(style_names, function(i){
    val <- dots[[i]]
    if (is.null(val)){
      val <- x$style[[i]]
      if (is.null(val)) val <- opts[[i]]
    }
    return(val)
  })
  names(styl) <- style_names
  return(styl)
}


in_knitr <- function() isTRUE(getOption("knitr.in.progress"))

blanks        <- function(n) sapply(n, function(i) paste(rep(" ", i), collapse = ""))
make_hvy_line <- function(n) paste(rep("\u2501", n), collapse = "")
make_line     <- function(n) paste(rep("\u2500", n), collapse = "")

make_bold  <- function(x) paste0("\x1b[1m", x, "\x1b[0m")
make_faint <- function(x) paste0("\x1b[3;38;5;240m", x, "\x1b[0m")
make_red   <- function(x) paste0("\x1b[31m", x, "\x1b[0m")
