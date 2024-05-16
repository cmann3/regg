# ============================================================================ #
# CONTENTS                                                                     #
#   - ansi_at : check font attributes, return ANSI code sequence               #
#   - ansi_bg : check font foreground color, return ANSI code sequence         #
#   - ansi_fg : check font background color, return ANSI code sequence         #
#   - as_ansi : convert rgo_font to ANSI code sequence                         #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
# ============================================================================ #


ansi_at <- function(x){
  if (is.null(x)) return("")
  attrs <- sapply(x, function(i){
    if (is.character(i)){
      switch(
        i,
        "reset"     = "\x1b[0m",
        "bold"      = "\x1b[1m",
        "strong"    = "\x1b[1m",
        "dim"       = "\x1b[2m",
        "faint"     = "\x1b[3m",
        "it"        = "\x1b[3m",
        "italic"    = "\x1b[3m",
        "emph"      = "\x1b[3m",
        "under"     = "\x1b[4m",
        "underline" = "\x1b[4m",
        "reverse"   = "\x1b[7m",
        "strike"    = "\x1b[9m",
        ""
      )
    } else {
      stop(sprintf("TYPE ERROR: attribute input [class=%s] not supported.", class(i)[1]))
    }
  })
}


ansi_bg <- function(x){
  if (is.null(x)) return("")
  colors <- sapply(x, function(i){
    if (is.character(i)){
      switch(
        i,
        "black"   = "\x1b[40m",
        "red"     = "\x1b[41m",
        "green"   = "\x1b[42m",
        "yellow"  = "\x1b[43m",
        "blue"    = "\x1b[44m",
        "magenta" = "\x1b[45m",
        "cyan"    = "\x1b[46m",
        "white"   = "\x1b[47m",
        "default" = "\x1b[49m",
        ""
      )
    } else if (is.numeric(i)){
      if (i >= 0 && i < 256){sprintf("\x1b[48;5;%dm", as.integer(i))
      } else {stop("BOUNDS ERROR: color input '%d' must lie between 0 and 255.", i)}
    } else {
      stop(sprintf("TYPE ERROR: color input [class=%s] not supported.", class(i)[1]))
    }
  })
  paste(colors, collapse = "")
}

ansi_fg <- function(x){
  if (is.null(x)) return("")
  colors <- sapply(x, function(i){
    if (is.character(i)){
      switch(
        i,
        "black"   = "\x1b[30m",
        "red"     = "\x1b[31m",
        "green"   = "\x1b[32m",
        "yellow"  = "\x1b[33m",
        "blue"    = "\x1b[34m",
        "magenta" = "\x1b[35m",
        "cyan"    = "\x1b[36m",
        "white"   = "\x1b[37m",
        "default" = "\x1b[39m",
        ""
      )
    } else if (is.numeric(i)){
      if (i >= 0 && i < 256){sprintf("\x1b[38;5;%dm", as.integer(i))
      } else {stop("BOUNDS ERROR: color input '%d' must lie between 0 and 255.", i)}
    } else {
      stop(sprintf("TYPE ERROR: color input [class=%s] not supported.", class(i)[1]))
    }
  })
  paste(colors, collapse = "")
}

as_ansi <- function(x){
  if (is.null(x)) return(NULL)
  paste0(ansi_fg(x$fg), ansi_bg(x$bg), ansi_at(x$at))
}
