#' Big plot of main measures
#' 
#' @param aov ANOVAs
#' 
#' @details
#' Needs {patchwork} to be loaded
#' 
#' 
#' @export
big_plot <- function(aov) {
  with(aov, {
    (HAMD_tot$plot | HAMA_tot$plot) /
      (SDS$plot  | BAI$plot) /
      (QIDS$plot | PSS$plot) +
      patchwork::plot_annotation(tag_levels = "A") +
      patchwork::plot_layout(guides = "collect") &
      ggplot2::theme(legend.position = "bottom") &
      ggplot2::xlab(NULL)
  })
}
