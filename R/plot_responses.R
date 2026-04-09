#' Make response/remission plots
#' 
#' @param data Input data
#' 
#' @returns List of plots
#' 
#' @details
#' Needs {patchwork} to be loaded
#' 
#' 
#' @export
plot_responses <- function(data) {
  
  pars <-
    data.frame(
      y = sub("_response", "", names(data)[grepl("response",names(data))])
    ) |>
    dplyr::mutate(
      y0 = sub("T1|T2", "T0", y),
      threshold = dplyr::case_when(
        grepl("SDS" , y) ~ 49,
        grepl("PSS" , y) ~ 13,
        grepl("BAI" , y) ~  7,
        grepl("QIDS", y) ~  5,
        grepl("HAMA", y) ~ 17,
        grepl("HAMD", y) ~  7
      )
    )
  
  plts <- lapply(rlang::set_names(seq_len(nrow(pars)), pars$y), function(i) {
    data |>
      ggplot2::ggplot() +
      ggplot2::aes(x = get(pars$y0[i]), y = get(pars$y[i]), colour = treatment) +
      ggplot2::labs(x = pars$y0[i], y = pars$y[i]) +
      ggplot2::geom_point(size = 2.33) +
      ggplot2::geom_abline(
        intercept = .5,
        slope = .5,
        colour = "black",
        linewidth = .8
      ) +
      ggplot2::geom_abline(
        intercept = .5,
        slope = 1 ,
        colour = "black",
        linewidth = .8,
        linetype = "dotted"
      ) +
      ggplot2::geom_abline(
        intercept = pars$threshold[i] + .5,
        slope = 0,
        colour = "red4",
        linewidth = .7,
        linetype = "dashed"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::scale_colour_manual(values = c("#64CDCC","#F9A729"))
  })
  
  with(plts, list(
    depression = (SDS_T1 | SDS_T2) / (QIDS_T1 | QIDS_T2) / (HAMD_tot_T1 | HAMD_tot_T2) +
      patchwork::plot_annotation(tag_levels = "A") +
      patchwork::plot_layout(guides = "collect") &
      ggplot2::theme(legend.position = "bottom"),
    
    anxiety = (BAI_T1 | BAI_T2) / (PSS_T1 | PSS_T2) / (HAMA_tot_T1 | HAMA_tot_T2) +
      patchwork::plot_annotation(tag_levels = "A") +
      patchwork::plot_layout(guides = "collect") &
      ggplot2::theme(legend.position = "bottom")
  ))
}

