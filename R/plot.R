
#' @export
#' @importFrom ggplot2 ggplot aes geom_hline geom_point geom_smooth xlab ylab
plot.rgo_m <- function(x, ...){
  dots    <- list(...)
  models  <- find_models(x)
  gg_mods <- lapply(models, function(mod){
    comps <- components(mod)
    gg    <- ggplot(comps, aes(x = .fitted, y = .resids)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = 2) +
      geom_smooth(se = FALSE, na.rm = TRUE) +
      xlab("Fitted Values") +
      ylab("Residuals")
    for (g in dots) gg <- gg + g
    return(gg)
  })
  if (length(gg_mods) == 1){print(gg_mods[[1]])
  } else {}
}
