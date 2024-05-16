
.onLoad <- function(libname, pkgname){

  options("regg" = list(
    "bottom_sep"  = if (.Platform$OS.type == "windows") "\u2500" else "\u2501",
    "digits"      = 4,
    "font_fns"    = list(
      rgo_font_formatter(function(i) if (is.numeric(i)) i < 0 else rep(FALSE, length(i)), fg = "red"),
      rgo_font_formatter(function(i) i == "",
                         fg = 240, at = "it", at_else = "bold",
                         cols = "signif", cols_on = "term")
    ),
    "format"      = "%estimate%signif\n(%se)",
    "header_sep"  = "\u2500",
    "justify"     = list(
      "character" = "left",
      "fmt"       = "center",
      "numeric"   = "right",
      "rows"      = "left"
    ),
    "styling"     = TRUE,
    "table_sep"   = "\u2500",
    "top_sep"     = if (.Platform$OS.type == "windows") "\u2500" else "\u2501"
  ))


}
