#' Conduct classical statistical tests
#' 
#' @param data Input data
#' @param outcome Outcome variables
#' @param labels Labels for plotting
#' @param plt_aov Numeric denoting type of plot
#' 
#' @returns List of outcomes
#' 
#' @export
conduct_mixed_anova <- function(
    data,
    outcome,
    labels,
    plt_aov = 2
) {
  data <-
    extract_per_protocol_data(data) |>
    dplyr::select(tidyselect::all_of(c("STUDY_ID", "occasion", "treatment", outcome))) |>
    dplyr::mutate(occasion = factor(x = occasion, ordered = FALSE))
  
  aov <- rstatix::anova_test(
    data = data,
    dv = tidyselect::all_of(outcome),
    wid = "STUDY_ID",
    within = "occasion",
    between = "treatment",
    effect.size = "ges"
  )
  
  aov_tab <- rstatix::get_anova_table(x = aov, correction = "GG")
  
  treat_diffs <- test_paired_differences(
    input = data |> dplyr::group_by(occasion),
    grouping = "occasion",
    predictor = "treatment",
    reference = "HF-rTMS",
    y = outcome,
    dependent = FALSE
  )
  
  occas_aov <- rstatix::anova_test(
    data = data |> dplyr::group_by(treatment),
    dv = outcome,
    wid = "STUDY_ID",
    within = "occasion",
    effect.size = "ges"
  ) |>
    rstatix::get_anova_table(correction = "GG")
  
  occas_diffs <- test_paired_differences(
    input = data |> dplyr::group_by(treatment),
    grouping = "treatment",
    predictor = "occasion",
    reference = NULL,
    y = outcome,
    dependent = TRUE
  )
  
  occas_main <- test_paired_differences(
    input = data,
    grouping = NULL,
    predictor = "occasion",
    reference = NULL,
    y = outcome,
    dependent = TRUE
  )
  
  sum_stats <- data |>
    dplyr::group_by(occasion, treatment) |>
    dplyr::summarise(
      M = mean(get(outcome)),
      SD = sd(get(outcome)),
      N = sum(!is.na( get(outcome)))
    ) |>
    dplyr::mutate(
      SEM = SD / sqrt(N),
      conf.low = M - qt(.975, df = N-1) * SEM,
      conf.high = M + qt(.975, df = N-1) * SEM
    ) |>
    dplyr::ungroup() |>
    preformat_data()
  
  plt <- data |>
    preformat_data() |>
    ggplot2::ggplot() +
    ggplot2::aes(x = Occasion, y = get(outcome), fill = `Treatment:`) +
    ggplot2::geom_violin(
      ggplot2::aes(colour = `Treatment:`),
      position = ggplot2::position_dodge(.6),
      width = .6,
      linewidth = 1.2,
      alpha = 0
    ) +
    ggplot2::geom_boxplot(
      alpha = .5,
      position = ggplot2::position_dodge(.6),
      linewidth = .5,
      outlier.size = 2,
      notch = TRUE,
      width = .25
    ) +
    ggplot2::geom_point(
      data = sum_stats,
      ggplot2::aes(x = Occasion, y = M, group = `Treatment:`),
      colour = "red3",
      size = 2.5,
      position = ggplot2::position_dodge(.6)
    ) +
    ggplot2::geom_linerange(
      data = sum_stats,
      ggplot2::aes(x = Occasion, y = M, ymin = conf.low, ymax = conf.high, group = `Treatment:`),
      colour = "red3",
      linewidth = .5,
      position = ggplot2::position_dodge(.6)
    ) +
    ggplot2::geom_line(
      data = sum_stats,
      ggplot2::aes(x = Occasion, y = M, linetype = `Treatment:`, group = `Treatment:`),
      colour = "red3",
      linewidth = .4,
      position = ggplot2::position_dodge(.6)
    ) +
    ggplot2::labs(y = subset(labels, var == outcome)$sub_abre) +
    ggplot2::scale_fill_manual(values = c("#64CDCC","#F9A729")) +
    ggplot2::scale_colour_manual(values = c("#64CDCC","#F9A729")) +
    ggplot2::theme_bw(base_size = 8) +
    ggplot2::theme(legend.position = "bottom")
  
  if (is.numeric(plt_aov)) {
    plt <- plt +
      ggplot2::labs(
        subtitle = rstatix::get_test_label(
          aov,
          row = plt_aov,
          detailed = TRUE,
          type = "expression"
        )
      )
  }
  
  list(
    ANOVA_model     = aov,
    ANOVA_table     = aov_tab,
    occas_main_comp = occas_main,  # pairwise comparisons between occasions averaged over levels of treatment
    occas_simp_anov = occas_aov,   # ANOVAs evaluating simple main effects of occasion conditional on the level of treatment
    occas_simp_comp = occas_diffs, # pairwise comparisons between occasions conditional on the level of treatment
    treat_simp_comp = treat_diffs, # pairwise comparisons between treatment levels conditional on the occasion
    plot            = plt
  )
}

#' Conduct classical testing for each outcome
#' 
#' @param data Input data
#' @param labs Labels for plotting
#' @param show_stats Numeric pushed to \code{conduct_mixed_anova}
#' 
#' @returns List of lists of results
#' 
#' @export
conduct_anova_loop <- function(data, labs, show_stats = 2) {
  nms <- rlang::set_names(x = seq_len(nrow(labs)), nm = labs$var)
  
  lapply(nms, function(y) {
    conduct_mixed_anova(
      data,
      outcome = labs[y, "var"],
      labels = labs,
      plt_aov = show_stats
    )
  })
}

#' Prepare a big ANOVA table
#' 
#' @param anovas ANOVA results
#' @param labs Labels
#' @param tit Table title
#' 
#' @returns gt table
#' 
#' @export
print_anova_table <- function(anovas, labs, tit = " ") {
  
  purrr::map_dfr(seq_len(nrow(labs)), function(y) {
    with(labs, {
      anovas[[var[y]]]$ANOVA_table |>
        tibble::as_tibble() |>
        dplyr::mutate(
          y = sub_long[[y]],
          Effect = dplyr::case_when(
            Effect == "treatment" ~ "Treatment",
            Effect == "occasion" ~ "Occasion",
            Effect == "treatment:occasion" ~ "Treatment * Occasion"
          )
        )
    })
  }) |>
    
    dplyr::mutate(
      q = p.adjust(p = p, method = "BH"),
      `sig.` = if_else(q < .05, "*", ""),
      `F` = rprint(`F`, decimals = 3),
      DFn = rprint(DFn, decimals = 2),
      DFd = rprint(DFd, decimals = 2),
      p = pprint(p, dec = 3, text = FALSE),
      q = pprint(q, dec = 3, text = FALSE),
      ges = rprint(ges, decimals = 3)
    ) |>
    dplyr::select(y, Effect, `F`, DFn, DFd, p, q, `sig.`, ges) |>
    
    gt_apa(grp = "y", title = tit) |>
    cols_label(
      `F` ~ "{{*F*}}",
      DFn ~ "{{*df*_*n*}}",
      DFd ~ "{{*df*_*d*}}",
      p   ~ "{{*p*}}",
      q   ~ "{{*q*}}",
      ges ~ "{{:eta:^2}}"
    ) |>
    gt::tab_source_note(
      source_note = gt::md("*F*: F statistic from a mixed analysis of variance; *df*~*n*~: numerator degrees
    of freedom of the F statistic; *df*~*d*~: denominator (residual) degrees of freedom the F statistic;
    *p*: nominal p-value; *q*: false discovery rate (FDR) adjusted p-value (i.e., the q-value); sig.:
    statistically significant effects after adjusting for 5% FDR; η^2^: generalised η-squared measure
    of effect size.")
    )
}