# ============================================================================ #
# CONTENTS                                                                     #
#   - print_table   : internal function for printing result tables             #
#                                                                              #
# INTERNAL DEPENDENCIES                                                        #
#   - adjust_to_width, blanks -> print_helpers.R                               #
# ============================================================================ #

#' @importFrom dplyr distinct
reg_table_print <- function(
  x,                            # Table (data.frame) to print
  above      = NULL,            # Should anything be printed above the table?
  below      = NULL,            # Should anything be printed below the table?
  big.mark   = ",",             # Mark for large numbers (N)
  bottom_sep = "\u2501",        # What should be printed at the bottom of the table?
  col_sep    = "  ",            # How should columns be separated?
  digits     = 4,               # Significant digits
  do_format  = TRUE,            # Should format be applied?
  font_fns   = NULL,            # list of rgo_font_formatter objects
  format     = NULL,            # Character string describing how to format data
  format_n   = TRUE,            # Should stat 'N' have a different format if table_by?
  header_sep = "\u2500",        # How should the table header be separated?
  insert_tab = NULL,            # List containing tables to insert (name describes location after the specific table.), first column is 'rows'
  just_num   = "right",         # Justification for numeric values
  just_chr   = "left",          # Justification for character
  just_fmt   = "center",        # Justification when formatted
  just_rows  = "left",          # Justification for row names
  name_list  = NULL,            # List of containing column name conversions
  rows       = NULL,            # Which column contains name of rows (ignore formatting)
  spread_by  = NULL,            # Column in table determining columns in final result
  style      = list(),          # List of styles passed from print
  styling    = TRUE,            # Should font styling be used (only if not Knitr)
  table_by   = NULL,            # Column representing different tables (for lines)
  table_sep  = "\u2500",        # How should tables be separated?
  titles     = NULL,            # Character vector to replace titles!
  top_sep    = "\u2501"         # What should be printed at the top of the table?
){
  ## Copy Table -> 1 for testing, 1 for printing
  tab <- x

  ignore_cols <- which(colnames(x) %in% c(rows, spread_by, table_by))

  ## Go through Table to Format Numbers, remove NA
  is_number              <- sapply(x, is.numeric)
  is_number[ignore_cols] <- FALSE
  for (i in which(is_number))
    tab[, i] <- format(tab[, i, drop = TRUE], digits = style$digits, trim = TRUE)
  tab[sapply(tab, function(i) grepl("^NA$", i))] <- ""
  tab[is.na(tab)] <- ""

  ## Get Nchars from insert_tab
  n_ins_rows <- 0
  if (!is.null(insert_tab)){
    insert_nchars <- sapply(seq_along(insert_tab[[1]]), function(col){
      max(sapply(insert_tab, function(tab) max(nchar(tab[[col]]))))
    })
    if (!is.null(rows)){
      n_ins_rows    <- insert_nchars[1]
      insert_nchars <- insert_nchars[-1]
    }
  }

  ## Adjust Row Column
  if (!is.null(rows)){
    nchar_rows  <- max(c(nchar(tab[[rows]]), n_ins_rows))
    tab[[rows]] <- format(tab[[rows]], width = nchar_rows, justify = just_rows)
    if (!is.null(insert_tab))
      for (i in seq_along(insert_tab))
        insert_tab[[i]][[rows]] <- format(insert_tab[[i]][[rows]], width = nchar_rows, justify = just_rows)
    blank_rows  <- paste(rep(" ", nchar_rows), collapse = "")
  }


  ## Change Format for "N"
  if (format_n && !is.null(table_by) && !is.null(rows)){
    w_n <- which(x[[rows]] == "N" & x[[table_by]] == "stat")
    if (length(w_n) > 0) tab[["estimate"]][w_n] <- format(x[["estimate"]][w_n], big.mark = big.mark)
  }

  ## Get Nchars
  nchars <- lapply(tab[,-which(colnames(tab) %in% c(rows, spread_by, table_by))], nchar)

  ## Apply Styling
  if (style$styling && !in_knitr()){
    if (!is.null(style$font_fns)){
      for (f in style$font_fns){
        fn_ansi <- as_ansi(f$font)
        fe_ansi <- as_ansi(f$font_else)
        if (is.null(f$cols)){
          cols    <- seq_along(x)[-(ignore_cols)]
          cols_on <- cols
        } else {
          cols    <- which(colnames(x) %in% f$cols)
          if (is.null(f$cols_on)){cols_on <- cols
          } else {cols_on <- which(colnames(x) %in% f$cols_on)}
        }
        for (i in seq_along(cols)){
          col        <- cols[i]
          con        <- cols_on[i]
          meets_cond <- f$cond(x[[col]])
          w_cond     <- which( meets_cond & !is.na(x[[col]]))
          w_else     <- which(!meets_cond & !is.na(x[[col]]))
          if (length(w_cond) > 0 && !is.null(fn_ansi))
            tab[[con]][w_cond]   <- paste0(fn_ansi, tab[[con]][w_cond], "\x1b[0m")
          if (length(w_else) > 0 && !is.null(fe_ansi))
            tab[[con]][w_else] <- paste0(fe_ansi, tab[[con]][w_else], "\x1b[0m")
        }
      }
    }
    ## TODO: [MAYBE] Other Styling
  }


  ## Formatting
  if (do_format){
    ## example: "%estimate%signif\n(%se)"
    format_split <- strsplit(style$format, "\n")[[1]]
    nrows        <- length(format_split)
    regex_pnames <- gregexpr("%[a-zA-Z]+[a-zA-Z0-9]*", format_split)
    nchar_format <- nchar(gsub("%[a-zA-Z]+[a-zA-Z0-9]*", "", format_split))
    if (all(sapply(regex_pnames, function(i) i[1] == -1))) stop("INPUT ERROR: columns in 'format' not recognized.")
    formatted <- lapply(seq_along(regex_pnames), function(j){
      p    <- regex_pnames[[j]]
      len  <- attr(p, "match.length")
      pnam <- sapply(seq_along(p), function(i) substr(format_split[j], p[i], p[i] + len[i] - 1))
      inam <- sapply(pnam, function(nm) substring(nm, 2))

      apply_form <- lapply(1:nrow(tab), function(i){
        val  <- format_split[j]
        nch  <- nchar_format[j]
        for (k in seq_along(pnam)){
          val <- gsub(pnam[k], tab[[inam[k]]][i], val)
          nch <- nch + nchars[[inam[k]]][i]
        }
        if (nch == nchar_format[j]) return(list(NA, 0))
        return(list(val, nch))
      })

      new_names <- sapply(apply_form, function(i) i[[1]])
      new_nch   <- sapply(apply_form, function(i) i[[2]])
      return(list(names = new_names, nchars = new_nch))
    })

    ## Initialize Long Data Frame
    new_tab <- tab[, c(table_by, spread_by, rows)]
    new_tab <- Reduce(rbind, lapply(1:nrow(tab), function(i){
      dfi <- new_tab[i,]
      Reduce(rbind, lapply(seq_along(formatted), function(j){
        dfi$value       <- formatted[[j]]$names[i]
        dfi$n           <- formatted[[j]]$nchars[i]
        dfi$form_number <- j
        return(dfi)
      }))
    }))

    ## Widen by 'spready_by' Column
    if (!is.null(spread_by)){
      spread_cols <- unique(x[[spread_by]])
      if (is.numeric(spread_cols)){spread_names <- paste0("(", spread_cols, ")")
      } else {spread_names <- spread_cols}
      core_tab <- distinct(tab[, c(table_by, rows)])
      tab_list <- lapply(1:nrow(core_tab), function(i){
        df_i <- core_tab[i, ]
        n_i  <- data.frame(temp_col = 1:nrows)
        df   <- new_tab[Reduce(intersect, lapply(seq_along(df_i), function(i){
          which(new_tab[[colnames(df_i)[i]]] == df_i[[i]])
        })),]
        if (nrows > 1) df_i <- Reduce(rbind, lapply(1:nrows, function(i){
          if (i > 1 & !is.null(rows)) df_i[[rows]] <- blank_rows
          return(df_i)
        }))

        ## df should now be restricted to the appropriate row, table, & df_i has correct number of rows!!
        ## from here, go through spread_by and
        for (k in seq_along(spread_cols)){
          df_temp  <- df[df[[spread_by]] == spread_cols[k],]
          col_name <- spread_names[k]
          if (nrow(df_temp) == 0){
            df_i[[col_name]] <- rep("", nrows)
            n_i[[col_name]]  <- rep(0, nrows)
          } else {
            df_i[[col_name]] <- df_temp$value
            n_i[[col_name]]  <- df_temp$n
          }
        }
        w_0 <- sapply(1:nrow(n_i), function(i) all(n_i[i,-1,drop = TRUE] == 0))
        list(df_i[!w_0,], n_i[!w_0, -1])
      })
      new_tab <- Reduce(rbind, lapply(tab_list, function(i) i[[1]]))
      nchars  <- as.list(Reduce(rbind, lapply(tab_list, function(i) i[[2]])))
    }

    tab <- new_tab
  } else {
    if (!is.null(name_list))
      for (i in seq_along(name_list)){
        w_col    <- which(colnames(tab) == names(name_list)[i])
        if (length(w_col) > 0) colnames(tab)[w_col[1]] <- name_list[[i]]
      }
  }

  ## Adjust character Width
  if (is.null(titles)) titles  <- colnames(tab)[-(which(colnames(tab) %in% c(rows, spread_by, table_by)))]
  n_title <- sapply(titles, nchar)
  maxn    <- sapply(seq_along(nchars), function(i){
    m <- max(nchars[[i]])
    if (!is.null(insert_tab)) m <- max(m, insert_nchars[i])
    if (n_title[i] > m) return(n_title[i])
    return(m)
  })

  to_add  <- lapply(seq_along(nchars), function(i) maxn[i] - nchars[[i]])

  to_widen <- which(!(colnames(tab) %in% c(rows, spread_by, table_by)))
  if (do_format){
    just <- rep(just_fmt, length(nchars))
  } else {
    just <- sapply(to_widen, function(i){
      col <- x[[i]]
      if (is.numeric(col)) return(just_num)
      return(just_chr)
    })
  }
  for (i in seq_along(nchars)){
    justi  <- just[i]
    nchari <- to_add[[i]]
    coli   <- tab[[to_widen[i]]]

    if (justi != "center"){
      blankt <- blanks(maxn[i] - n_title[i])
      blanks <- sapply(nchari, function(j) blanks(j))
      if (justi == "left"){
        tab[[to_widen[i]]] <- paste0(coli, blanks)
        titles[i]          <- paste0(titles[i], blankt)
        for (ins in seq_along(insert_tab)){
          w <- i + !is.null(rows)
          inster_tab[[ins]][[w]] <- sapply(insert_tab[[ins]][[w]], function(val) paste0(val, blanks(maxn[i] - nchar(val))))
        }
      } else {
        tab[[to_widen[i]]] <- paste0(blanks, coli)
        titles[i]          <- paste0(blankt, titles[i])
        for (ins in seq_along(insert_tab)){
          w <- i + !is.null(rows)
          insert_tab[[ins]][[w]] <- sapply(insert_tab[[ins]][[w]], function(val) paste0(blanks(maxn[i] - nchar(val)), val))
        }
      }
    } else {
      nleft  <- floor(nchari / 2)
      nright <- nchari - nleft
      tab[[to_widen[i]]] <-paste0(blanks(nleft), coli, blanks(nright))
      nl_tit <- floor((maxn[i]-n_title[i])/2)
      nr_tit <- maxn[i] - n_title[i] - nl_tit
      titles[i] <- paste0(blanks(nl_tit), titles[i], blanks(nr_tit))
      for (ins in seq_along(insert_tab)){
        w      <- i + !is.null(rows)
        iadd   <- maxn[i] - nchar(insert_tab[[ins]][[w]])
        ileft  <- floor(iadd/2)
        iright <- iadd - ileft
        insert_tab[[ins]][[w]] <- paste0(blanks(ileft), insert_tab[[ins]][[w]], blanks(iright))
      }
    }
  }

  ## Determine Number of Characters Needed & Adjust as Necessary!
  if (!is.null(rows)){
    titles <- c(blank_rows, titles)
    maxn <- c(nchar_rows, maxn)
  }
  n_col_sep   <- nchar(col_sep)
  total_width <- sum(maxn + n_col_sep) - n_col_sep
  top_line    <- adjust_to_width(style$top_sep[1], total_width)
  bot_line    <- adjust_to_width(style$bottom_sep[1], total_width)
  head_line   <- adjust_to_width(style$header_sep[1], total_width)
  tab_line    <- adjust_to_width(style$table_sep[1], total_width)

  if (!is.null(table_by)){
    table_vals <- unique(tab[[table_by]])
    table_breaks <- sapply(unique(tab[[table_by]]), function(i) min(which(tab[[table_by]] == i)) - 1)
    if (!is.null(insert_tab)){
      ins_breaks <- sapply(names(insert_tab), function(n) max(which(tab[[table_by]] == n)))
    } else {ins_breaks <- 0}
  } else {table_breaks <- ins_breaks <- 0}

  if (!is.null(rows)){
    to_widen <- c(which(colnames(tab) == rows), to_widen)
  }

  ## Print
  cat("\n")
  if (!is.null(above)) for (ab in above) cat(ab, "\n")
  cat(top_line)
  cat(paste(titles, collapse = col_sep), "\n")
  cat(head_line)
  for (i in 1:nrow(tab)){
    cat(paste(tab[i, to_widen, drop = TRUE], collapse = col_sep), "\n")
    if (i %in% table_breaks) cat(tab_line)
    if (i %in% ins_breaks){
      temp_tab <- insert_tab[[which(i == ins_breaks)]]
      for (i in 1:nrow(temp_tab)) cat(paste(temp_tab[i,], collapse = col_sep), "\n")
      cat(tab_line)
    }
  }
  cat(bot_line)
  if (!is.null(below)) for (be in below) cat(be, "\n")
  cat("\n")

}
