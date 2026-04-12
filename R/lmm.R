#' Fit vanilla LMM
#' 
#' @param d0 Input data
#' @param outcome Outcome variable
#' @param labels Labels
#' @param type Type of analysis
#' 
#' @details
#' `type` can be:
#' - `"vanilla"` for classical `outcome ~ treatment * occasion + (1 | STUDY_ID)`
#' - `"baseline"` for baseline-adjusted `outcome ~ treatment * occasion + baseline + (1 | STUDY_ID)`
#' - `"baseline2"` for baseline-adjusted `outcome ~ (treatment + baseline) * occasion + (1 | STUDY_ID)`
#' - `"baseline_pp"` for baseline-adjusted `outcome ~ treatment * occasion + baseline + (1 | STUDY_ID)`
#'   on per protocol sample
#' - `"baseline_pp2"` for baseline-adjusted `outcome ~ (treatment + baseline) * occasion + (1 | STUDY_ID)`
#'   on per protocol sample
#' - `"random1"` for random slopes `outcome ~ treatment * occasion + (1 + occasion | STUDY_ID)`
#' - `"random2"` for random slopes `outcome ~ treatment * occasion + (1 + occasion  || STUDY_ID)`
#' 
#' For baseline-adjusted analyses, T0 results are treated as predictor (`pre`)
#' instead of outcome and `occasion` contains two levels (T1, T2).
#' 
#' @returns List of results
#' 
#' @export
conduct_lmer <- function(d0, outcome, labels, type = "vanilla") {
  
  df <- d0 |>
    dplyr::mutate(occasion = factor(x = occasion, ordered = FALSE)) |>
    dplyr::select(tidyselect::all_of(
      c("STUDY_ID", "occasion", "treatment", "completed", outcome)
    ))
  
  if (grepl("baseline", type)) {
    df <- df |>
      tidyr::pivot_wider(
        names_from = occasion,
        values_from = tidyselect::all_of(outcome)
      ) |>
      dplyr::rename(baseline = T0) |>
      tidyr::pivot_longer(
        c(T1, T2),
        names_to = "occasion",
        names_transform = factor,
        values_to = outcome
      )
  }
  
  if (grepl("_pp", type)) {
    df <- df |>
      dplyr::filter(completed == 1)
  }
  
  if (type == "cLDA") {
    df$occasion <- relevel(df$occasion, ref = "T0")
    
    dummy_occas <- paste0("occasion", levels(df$occasion)[-1], "treatmentiTBS")
    for (o in dummy_occas) {
      i <- stringr::str_replace_all(o, c(occasion = "", treatmentiTBS = ""))
      df[[o]] <- as.numeric(df$treatment == "iTBS" & df$occasion == i)
    }
  }
  
  contrasts(df$occasion) <- contr.treatment
  contrasts(df$treatment) <- contr.treatment
  
  lab <- labels[labels$var == outcome, "sub_abre"]
  
  form <- c(
    "vanilla"      = paste0(outcome, " ~ treatment * occasion + (1 | STUDY_ID)"),
    "baseline"     = paste0(outcome, " ~ treatment * occasion + baseline + (1 | STUDY_ID)"),
    "baseline2"    = paste0(outcome, " ~ (treatment + baseline) * occasion + (1 | STUDY_ID)"),
    "baseline_pp"  = paste0(outcome, " ~ treatment * occasion + baseline + (1 | STUDY_ID)"),
    "baseline_pp2" = paste0(outcome, " ~ (treatment + baseline) * occasion + (1 | STUDY_ID)"),
    #"cLDA"         = paste0(outcome, " ~ occasion + occasionT1treatmentiTBS + occasionT2treatmentiTBS + (1 | STUDY_ID)"),
    "random1"      = paste0(outcome, " ~ treatment * occasion + (1 + occasion | STUDY_ID)"),
    "random2"      = paste0(outcome, " ~ treatment * occasion + (1 + occasion || STUDY_ID)"),
    NULL
  )
  
  fit <- lmerTest::lmer(
    formula = as.formula(form[[type]]),
    data = df,
    REML = TRUE 
  )
  
  check <- performance::check_model(fit)
  
  plt <-
    emmeans::emmip(fit, treatment ~ occasion, CIs = TRUE, PIs = TRUE) +
    ggplot2::labs(x = "Occasion", y = lab) +
    ggplot2::scale_colour_manual(values = c("#64CDCC", "#F9A729"))
  
  aov <- anova(fit) |>
    tibble::as_tibble(rownames = NA) |>
    tibble::rownames_to_column("Term")
  
  occas_main <- compute_pairwise_comparisons(fit, lab, form = "~ occasion")
  treat_main <- compute_pairwise_comparisons(fit, lab, form = "~ treatment")
  if (grepl("baseline", type)) {
    occas_simp <- compute_pairwise_comparisons(fit, lab, type = "ancova", base = mean(df$baseline, na.rm = TRUE))
  } else {
    occas_simp <- compute_pairwise_comparisons(fit, lab, form = "~ occasion | treatment")
  }
  treat_simp <- compute_pairwise_comparisons(fit, lab, form = "~ treatment | occasion")
  
  list(
    LMER_model     = fit,
    diagnostics    = check,
    ANOVA_table    = aov,
    pwc_occas_main = occas_main, # pairwise comparisons between occasions averaged over treatments
    pwc_occas_simp = occas_simp, # pairwise comparisons between occasions conditional on the treatment
    pwc_treat_main = treat_main, # pairwise comparisons between treatments averaged over levels of occasion
    pwc_treat_simp = treat_simp, # pairwise comparisons between treatments conditional on the occasion
    plot           = plt
  )
}

#' Compute LMM for each outcome
#' 
#' @param data Input data
#' @param labs Labels
#' @param type Type of analysis
#' 
#' @seealso [conduct_lmer()]
#' 
#' @returns List of lists of results
#' 
#' @export
conduct_lmer_loop <- function(data, labs, type = "vanilla") {
  nms <- rlang::set_names(seq_len(nrow(labs)), nm = labs$var)
  lapply(nms, function(y) {
    conduct_lmer(
      d0      = data,
      outcome = labs[y, "var"],
      labels  = labs,
      type    = type 
    )
  })
}

#' Prepare LMM ANOVA table
#' 
#' @param lmers Models
#' @param labs Labels
#' @param tit Table title
#' 
#' @returns gt table
#' 
#' @export
print_lmer_anova <- function(lmers, labs, tit = " ") {
  
  purrr::map_dfr(seq_len(nrow(labs)), function(y) {
    with(labs, {
      lmers[[var[y]]]$ANOVA_table |>
        tibble::as_tibble() |>
        dplyr::mutate(
          y = sub_long[[y]],
          Effect = dplyr::case_when(
            Term == "treatment" ~ "Treatment",
            Term == "occasion" ~ "Occasion",
            Term == "treatment:occasion" ~ "Treatment * Occasion"
          )
        )
    })
  })  |>
    
    dplyr::mutate(
      q      = p.adjust(p = `Pr(>F)`, method = "BH"),
      `sig.` = if_else(q < .05, "*", ""),
      `F`    = rprint(`F value`, decimals = 3),
      DFn    = rprint(NumDF, decimals = 2),
      DFd    = rprint(DenDF, decimals = 2),
      p      = pprint(`Pr(>F)`, dec = 3, text = FALSE),
      q      = pprint(q, dec = 3, text = FALSE)
    ) |>
    dplyr::select(y, Effect, `F`, DFn, DFd, p, q, `sig.`) |>
    
    gt_apa(grp = "y", title = tit) |>
    gt::cols_label(
      `F` ~ "{{*F*}}",
      DFn ~ "{{*df*_*n*}}",
      DFd ~ "{{*df*_*d*}}",
       p  ~ "{{*p*}}",
       q  ~ "{{*q*}}"
    ) |>
    gt::tab_source_note(
      source_note = gt::md("*F*: F statistic from a mixed analysis of variance; *df*~*n*~: numerator degrees
    of freedom of the F statistic; *df*~*d*~: denominator (residual) degrees of freedom the F statistic;
    *p*: nominal p-value; *q*: false discovery rate (FDR) adjusted p-value (i.e., the q-value); sig.:
    statistically significant effects after adjusting for 5% FDR.")
    )
}

#' Prepare table of LMM paired comparisons
#' 
#' @param comps Pre-computed comparisons
#' @param labs Labels
#' @param x Predictor
#' @param type Effect type
#' @param tit Table title
#' 
#' @returns gt table
#' 
#' @export
print_lmer_pwc <- function(
    comps,
    labs,
    x = "occas",
    type = "main",
    tit = " "
) {
  d0 <- purrr::map_dfr(seq_len(nrow(labs)), function(y) {
    with(labs, {
      comps[[var[y]]][[paste0("pwc_", x, "_", type)]] |>
        tibble::as_tibble() |>
        dplyr::mutate(y = sub_abre[[y]])
    })
  }) |>
    
    dplyr::mutate(
      t   = rprint(t.ratio, 3),
      df  = rprint(df , 2),
      p   = sapply(seq_len(dplyr::n()), \(i) pprint(p.value[i], dec = 3, text = FALSE)),
      est = paste0(rprint(estimate, 2), " [", rprint(lower.CL, 2), ", ", rprint(upper.CL, 2),"]"),
      contrast = gsub(" ", "", contrast)
    )
  
  if (x == "occas" & type == "main") {
    d0$condition <- "none"
  } else if(x == "occas" & type == "simp") {
    d0$condition <- d0$treatment
  } else if(x == "treat") {
    d0$condition <- d0$occasion
  }
  
  df <- d0 |>
    dplyr::select(outcome, contrast, condition, est, t, df, p) |>
    tidyr::pivot_wider(
      id_cols = outcome,
      values_from = c(est, t, df, p),
      names_from = c(condition, contrast)
    )
  
  nms <- names(df)
  
  if (x == "occas" & type == "main") {
    tab <- df |>
      setNames(sub("none_", "", nms)) |>
      dplyr::relocate(`t_T1-T0` , .after = `est_T1-T0`) |> # T1-minus-T0
      dplyr::relocate(`df_T1-T0`, .after = `t_T1-T0`  ) |>
      dplyr::relocate(`p_T1-T0` , .after = `df_T1-T0` ) |>
      dplyr::relocate(`t_T2-T0` , .after = `est_T2-T0`) |> # T2-minus-T0
      dplyr::relocate(`df_T2-T0`, .after = `t_T2-T0`  ) |> 
      dplyr::relocate(`p_T2-T0` , .after = `df_T2-T0` ) |>
      
      gt_apa(title = tit) |>
      
      gt::tab_spanner(columns = tidyselect::ends_with("T1-T0"), label = "T1-minus-T0", gather = FALSE) |>
      gt::tab_spanner(columns = tidyselect::ends_with("T2-T0"), label = "T2-minus-T0", gather = FALSE) |>
      gt::tab_spanner(columns = tidyselect::ends_with("T2-T1"), label = "T2-minus-T1", gather = FALSE) |>
      
      gt::cols_label(
        outcome ~ "Outcome",
        tidyselect::starts_with("est_") ~ "Estimate [95% CI]",
        tidyselect::starts_with("t_") ~ "{{*t*}}",
        tidyselect::starts_with("df_") ~ "df",
        tidyselect::starts_with("p_") ~ "{{*p*}}"
      )
  } else if (x == "occas" & type == "simp") {
    tab <- df |>
      
      dplyr::relocate(`t_HF-rTMS_T1-T0` , .after = `est_HF-rTMS_T1-T0`) |> # T1-minus-T0 in HF-rTMS
      dplyr::relocate(`df_HF-rTMS_T1-T0`, .after = `t_HF-rTMS_T1-T0`  ) |>
      dplyr::relocate(`p_HF-rTMS_T1-T0` , .after = `df_HF-rTMS_T1-T0` ) |>
      
      dplyr::relocate(`est_iTBS_T1-T0`, .after = `p_HF-rTMS_T1-T0`) |> # T1-minus-T0 in iTBS
      dplyr::relocate(`t_iTBS_T1-T0`  , .after = `est_iTBS_T1-T0` ) |> 
      dplyr::relocate(`df_iTBS_T1-T0` , .after = `t_iTBS_T1-T0`   ) |>
      dplyr::relocate(`p_iTBS_T1-T0`  , .after = `df_iTBS_T1-T0`  ) |>
      
      dplyr::relocate(`est_HF-rTMS_T2-T0`, .after = `p_iTBS_T1-T0`     ) |> # T2-minus-T0 in HF-rTMS
      dplyr::relocate(`t_HF-rTMS_T2-T0`  , .after = `est_HF-rTMS_T2-T0`) |>
      dplyr::relocate(`df_HF-rTMS_T2-T0` , .after = `t_HF-rTMS_T2-T0`  ) |> 
      dplyr::relocate(`p_HF-rTMS_T2-T0`  , .after = `df_HF-rTMS_T2-T0` ) |>
      
      dplyr::relocate(`est_iTBS_T2-T0`, .after = `p_HF-rTMS_T2-T0`) |> # T2-minus-T0 in iTBS
      dplyr::relocate(`t_iTBS_T2-T0`  , .after = `est_iTBS_T2-T0` ) |> 
      dplyr::relocate(`df_iTBS_T2-T0` , .after = `t_iTBS_T2-T0`   ) |>
      dplyr::relocate(`p_iTBS_T2-T0`  , .after = `df_iTBS_T2-T0`  ) |>
      
      dplyr::relocate(`t_HF-rTMS_T2-T1` , .after = `est_HF-rTMS_T2-T1`) |> # T2-minus-T1 in HF-rTMS
      dplyr::relocate(`df_HF-rTMS_T2-T1`, .after = `t_HF-rTMS_T2-T1` ) |> 
      dplyr::relocate(`p_HF-rTMS_T2-T1` , .after = `df_HF-rTMS_T2-T1`) |>
      
      gt_apa(title = tit) |>
      
      gt::tab_spanner(columns = tidyselect::contains("HF-rTMS"), label = "HF-rTMS"    , gather = FALSE) |>
      gt::tab_spanner(columns = tidyselect::contains("iTBS")   , label = "iTBS"       , gather = FALSE) |>
      gt::tab_spanner(columns = tidyselect::ends_with("T1-T0") , label = "T1-minus-T0", gather = FALSE) |>
      gt::tab_spanner(columns = tidyselect::ends_with("T2-T0") , label = "T2-minus-T0", gather = FALSE) |>
      gt::tab_spanner(columns = tidyselect::ends_with("T2-T1") , label = "T2-minus-T1", gather = FALSE) |>
      
      gt::cols_label(
        outcome ~ "Outcome",
        tidyselect::starts_with("est_") ~ "Estimate [95% CI]",
        tidyselect::starts_with("t_") ~ "{{*t*}}",
        tidyselect::starts_with("df_") ~ "df",
        tidyselect::starts_with("p_") ~ "{{*p*}}"
      )
    
  } else if (x == "treat") {
    tab <- df |>
      
      dplyr::relocate(`t_T0_iTBS-(HF-rTMS)` , .after = `est_T0_iTBS-(HF-rTMS)`) |> # iTBS-minus-HF-rTMS in T0
      dplyr::relocate(`df_T0_iTBS-(HF-rTMS)`, .after = `t_T0_iTBS-(HF-rTMS)`  ) |> 
      dplyr::relocate(`p_T0_iTBS-(HF-rTMS)` , .after = `df_T0_iTBS-(HF-rTMS)` ) |>
      dplyr::relocate(`t_T1_iTBS-(HF-rTMS)` , .after = `est_T1_iTBS-(HF-rTMS)`) |> # iTBS-minus-HF-rTMS in T1
      dplyr::relocate(`df_T1_iTBS-(HF-rTMS)`, .after = `t_T1_iTBS-(HF-rTMS)`  ) |>
      dplyr::relocate(`p_T1_iTBS-(HF-rTMS)` , .after = `df_T1_iTBS-(HF-rTMS)` ) |>
      
      gt_apa(title = tit) |>
      
      gt::tab_spanner(columns = tidyselect::contains("T0"), label = "T0", gather = FALSE) |>
      gt::tab_spanner(columns = tidyselect::contains("T1"), label = "T1", gather = FALSE) |>
      gt::tab_spanner(columns = tidyselect::contains("T2"), label = "T2", gather = FALSE) |>
      
      gt::cols_label(
        outcome ~ "Outcome",
        tidyselect::starts_with("est_") ~ "Estimate [95% CI]",
        tidyselect::starts_with("t_") ~ "{{*t*}}",
        tidyselect::starts_with("df_") ~ "df",
        tidyselect::starts_with("p_") ~ "{{*p*}}"
      )
  }
  
  tab |> gt::tab_source_note(
    source_note = gt::md("HAMA: Hamilton's Depression Inventory; SDS: Self-rating Depression Scale, QIDS:
      Quick Inventory of Depressive Symptomatology; HAMA: Hamilton's Anxiety Inventory, BAI: Beck's 
      Anxiety Scale; PSS: Perceived Stress Scale; HF-rTMS: high frequency repetitive transcranial magnetic
      stimulation; iTBS: intermittent theta burst stimulation; *Estimate [95% CI]*: Estimated difference with
      its 95% confidence interval; *t*: test statistic; df: degrees of freedom; *p*: p-value after applying
      rowwise Tukey adjustment for multiple comparisons.")
  )
}
