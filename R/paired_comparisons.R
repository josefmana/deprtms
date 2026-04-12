#' Conduct a series of t-tests
#' 
#' @param input Input data
#' @param grouping Groups to be compared
#' @param predictor Predictor variable
#' @param reference Reference group
#' @param y Outcome
#' @param adjust Multiple testing adjustment
#' @param dependent Paired (`TRUE`) or independent (`FALSE`)
#' 
#' @returns Tibble
#' 
#' @export
test_paired_differences <- function(
    input,
    grouping,
    predictor,
    reference,
    y,
    adjust = "holm",
    dependent = TRUE
) {
  lapply(c("t_test", "cohens_d"), function(fun) {
    fn <- getFromNamespace(fun, "rstatix")
    
    argument <- list(
      data = input,
      formula = as.formula(paste(y, predictor, sep = " ~ ")),
      ref.group = reference,
      paired = dependent,
      var.equal = FALSE,
      conf.level = .95,
      hedges.correction = TRUE,
      ci = TRUE
    )
    
    if (grepl("t_test", fun)) {
      argument$hedges.correction <- NULL
      argument$ci <- NULL
    }
    
    set.seed(87542)
    tab <- do.call(fn, args = argument)
    
    if (grepl("t_test", fun)) {
      tab <- tab |>
        dplyr::ungroup() |>
        rstatix::adjust_pvalue(method = adjust)
    }
    
    tab
  }) |>
    purrr::reduce(
      dplyr::left_join,
      by = c(grouping, ".y.", "group1", "group2", "n1", "n2")
    )
}

#' Compute pairwise comparisons from LMM
#' 
#' @param mod Model
#' @param lab Labels
#' @param adj Multiple comparisons adjustment
#' @param form Formula (as character)
#' @param contr Contrast to {emmeans}
#' 
#' @returns data.frame
#' 
#' @export
compute_pairwise_comparisons <- function(
    mod,
    lab,
    adj = "tukey",
    form = "~ occasion",
    contr = "revpairwise"
) {
  emm <-
    emmeans::emmeans(mod, as.formula(form)) |>
    emmeans::contrast(method = contr, adjust = adj) |>
    tibble::as_tibble() |>
    dplyr::mutate(outcome = lab, .before = 1)
  
  ci <-
    emmeans::emmeans(mod, as.formula(form)) |>
    emmeans::contrast(method = contr, adjust = adj) |>
    confint() |>
    tibble::as_tibble() |>
    dplyr::select(tidyselect::ends_with(".CL"))
  
  cbind.data.frame(emm, ci)
}

#' Extract table of paired comparisons
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
print_paired_comparisons <- function(
    comps,
    labs,
    x = "occas",
    type = "main",
    tit = " "
) {
  
  df <- purrr::map_dfr(seq_len(nrow(labs)), function(y) {
    with(labs, {
      comps[[var[y]]][[paste0(x, "_", type, "_comp")]] |>
        tibble::as_tibble() |>
        dplyr::mutate(y = sub_abre[[y]])
    })
  }) |>
    dplyr::mutate(
      comparison = paste(group2, group1, sep = "-"),
      t = rprint(statistic, 3),
      df = rprint(df, 0),
      p = sapply(seq_len(dplyr::n()), \(i) pprint(p.adj[i], dec = 3, text = FALSE)),
      d = paste0(rprint(effsize, 2)," [", rprint(conf.low, 2),", ", rprint(conf.high, 2), "]")
    )
  
  if (x == "occas" & type == "main") {
    df$condition <- "none"
  } else if(x == "occas" & type == "simp") {
    df$condition <- df$treatment
  } else if(x == "treat") {
    df$condition <- df$occasion
  }
  
  df <- df |>
    dplyr::select(y, condition, comparison, t, df, p, d, magnitude) |>
    tidyr::pivot_wider(
      id_cols = y,
      values_from = c(t, df, p, d, magnitude),
      names_from = c(condition, comparison)
    )
  
  nms <- names(df)
  
  if (x == "occas" & type == "main") {
    tab <- df |>
      setNames(sub("none_", "", nms)) |>
      dplyr::relocate(`df_T1-T0`       , .after = `t_T1-T0` ) |> # T1-minus-T0
      dplyr::relocate(`p_T1-T0`        , .after = `df_T1-T0`) |>
      dplyr::relocate(`d_T1-T0`        , .after = `p_T1-T0` ) |>
      dplyr::relocate(`magnitude_T1-T0`, .after = `d_T1-T0` ) |>
      dplyr::relocate(`df_T2-T0`       , .after = `t_T2-T0` ) |> # T2-minus-T0
      dplyr::relocate(`p_T2-T0`        , .after = `df_T2-T0`) |>
      dplyr::relocate(`d_T2-T0`        , .after = `p_T2-T0` ) |>
      dplyr::relocate(`magnitude_T2-T0`, .after = `d_T2-T0` ) |>
      
      gt_apa(title = tit) |>
      
      gt::tab_spanner(columns = tidyselect::ends_with("T1-T0"), label = "T1-minus-T0", gather = FALSE) |>
      gt::tab_spanner(columns = tidyselect::ends_with("T2-T0"), label = "T2-minus-T0", gather = FALSE) |>
      gt::tab_spanner(columns = tidyselect::ends_with("T2-T1"), label = "T2-minus-T1", gather = FALSE) |>
      
      gt::cols_label(
        y ~ "Outcome",
        tidyselect::starts_with("t_") ~ "{{*t*}}",
        tidyselect::starts_with("df_") ~ "df",
        tidyselect::starts_with("p_") ~ "{{*p*}}",
        tidyselect::starts_with("d_") ~ "{{*d*}}",
        tidyselect::starts_with("magnitude_") ~ "ES"
      )
  } else if (x == "occas" & type == "simp") {
    tab <- df |>
      
      dplyr::relocate(`df_HF-rTMS_T1-T0`       , .after = `t_HF-rTMS_T1-T0` ) |> # T1-minus-T0 in HF-rTMS
      dplyr::relocate(`p_HF-rTMS_T1-T0`        , .after = `df_HF-rTMS_T1-T0`) |>
      dplyr::relocate(`d_HF-rTMS_T1-T0`        , .after = `p_HF-rTMS_T1-T0` ) |>
      dplyr::relocate(`magnitude_HF-rTMS_T1-T0`, .after = `d_HF-rTMS_T1-T0` ) |>
      
      dplyr::relocate(`t_iTBS_T1-T0`        , .after = `magnitude_HF-rTMS_T1-T0` ) |> # T1-minus-T0 in iTBS
      dplyr::relocate(`df_iTBS_T1-T0`       , .after = `t_iTBS_T1-T0`            ) |>
      dplyr::relocate(`p_iTBS_T1-T0`        , .after = `df_iTBS_T1-T0`           ) |>
      dplyr::relocate(`d_iTBS_T1-T0`        , .after = `p_iTBS_T1-T0`            ) |>
      dplyr::relocate(`magnitude_iTBS_T1-T0`, .after = `d_iTBS_T1-T0`            ) |>
      
      dplyr::relocate(`df_HF-rTMS_T2-T0`       , .after = `t_HF-rTMS_T2-T0` ) |> # T2-minus-T0 in HF-rTMS
      dplyr::relocate(`p_HF-rTMS_T2-T0`        , .after = `df_HF-rTMS_T2-T0`) |>
      dplyr::relocate(`d_HF-rTMS_T2-T0`        , .after = `p_HF-rTMS_T2-T0` ) |>
      dplyr::relocate(`magnitude_HF-rTMS_T2-T0`, .after = `d_HF-rTMS_T2-T0` ) |>
      
      dplyr::relocate(`t_iTBS_T2-T0`        , .after = `magnitude_HF-rTMS_T2-T0` ) |> # T2-minus-T0 in iTBS
      dplyr::relocate(`df_iTBS_T2-T0`       , .after = `t_iTBS_T2-T0`            ) |>
      dplyr::relocate(`p_iTBS_T2-T0`        , .after = `df_iTBS_T2-T0`           ) |>
      dplyr::relocate(`d_iTBS_T2-T0`        , .after = `p_iTBS_T2-T0`            ) |>
      dplyr::relocate(`magnitude_iTBS_T2-T0`, .after = `d_iTBS_T2-T0`            ) |>
      
      dplyr::relocate(`df_HF-rTMS_T2-T1`       , .after = `t_HF-rTMS_T2-T1` ) |>
      dplyr::relocate(`p_HF-rTMS_T2-T1`        , .after = `df_HF-rTMS_T2-T1`) |>
      dplyr::relocate(`d_HF-rTMS_T2-T1`        , .after = `p_HF-rTMS_T2-T1` ) |>
      dplyr::relocate(`magnitude_HF-rTMS_T2-T1`, .after = `d_HF-rTMS_T2-T1` ) |>
      
      gt_apa(title = tit) |>
      
      gt::tab_spanner(columns = tidyselect::contains("HF-rTMS"), label = "HF-rTMS"    , gather = FALSE) |>
      gt::tab_spanner(columns = tidyselect::contains("iTBS")   , label = "iTBS"       , gather = FALSE) |>
      gt::tab_spanner(columns = tidyselect::ends_with("T1-T0") , label = "T1-minus-T0", gather = FALSE) |>
      gt::tab_spanner(columns = tidyselect::ends_with("T2-T0") , label = "T2-minus-T0", gather = FALSE) |>
      gt::tab_spanner(columns = tidyselect::ends_with("T2-T1") , label = "T2-minus-T1", gather = FALSE) |>
      
      gt::cols_label(
        y ~ "Outcome",
        tidyselect::starts_with("t_") ~ "{{*t*}}",
        tidyselect::starts_with("df_") ~ "df",
        tidyselect::starts_with("p_") ~ "{{*p*}}",
        tidyselect::starts_with("d_") ~ "{{*d*}}",
        tidyselect::starts_with("magnitude_") ~ "ES"
      )
    
  } else if (x == "treat") {
    tab <- df |>
      
      dplyr::relocate(`df_T0_iTBS-HF-rTMS`       , .after = `t_T0_iTBS-HF-rTMS` ) |> # iTBS-minus-HF-rTMS in T0
      dplyr::relocate(`p_T0_iTBS-HF-rTMS`        , .after = `df_T0_iTBS-HF-rTMS`) |>
      dplyr::relocate(`d_T0_iTBS-HF-rTMS`        , .after = `p_T0_iTBS-HF-rTMS` ) |>
      dplyr::relocate(`magnitude_T0_iTBS-HF-rTMS`, .after = `d_T0_iTBS-HF-rTMS` ) |>
      dplyr::relocate(`df_T1_iTBS-HF-rTMS`       , .after = `t_T1_iTBS-HF-rTMS` ) |> # iTBS-minus-HF-rTMS in T1
      dplyr::relocate(`p_T1_iTBS-HF-rTMS`        , .after = `df_T1_iTBS-HF-rTMS`) |>
      dplyr::relocate(`d_T1_iTBS-HF-rTMS`        , .after = `p_T1_iTBS-HF-rTMS` ) |>
      dplyr::relocate(`magnitude_T1_iTBS-HF-rTMS`, .after = `d_T1_iTBS-HF-rTMS` ) |>
      
      gt_apa(title = tit) |>
      
      gt::tab_spanner(columns = tidyselect::contains("T0"), label = "T0", gather = FALSE) |>
      gt::tab_spanner(columns = tidyselect::contains("T1"), label = "T1", gather = FALSE) |>
      gt::tab_spanner(columns = tidyselect::contains("T2"), label = "T2", gather = FALSE) |>
      
      gt::cols_label(
        y ~ "Outcome",
        tidyselect::starts_with("t_") ~ "{{*t*}}",
        tidyselect::starts_with("df_") ~ "df",
        tidyselect::starts_with("p_") ~ "{{*p*}}",
        tidyselect::starts_with("d_") ~ "{{*d*}}",
        tidyselect::starts_with("magnitude_") ~ "ES"
      )
  }
  
  tab |> gt::tab_source_note(
    source_note = gt::md("HAMA: Hamilton's Depression Inventory; SDS: Self-rating Depression Scale, QIDS:
      Quick Inventory of Depressive Symptomatology; HAMA: Hamilton's Anxiety Inventory, BAI: Beck's 
      Anxiety Scale; PSS: Perceived Stress Scale; HF-rTMS: high frequency repetitive transcranial magnetic
      stimulation; iTBS: intermittent theta burst stimulation; *t*: test statistic; df: degrees of freedom;
      *p*: p-value after applying rowwise Holm's multiple correction adjustment for multiple comparisons;
      *d*: Cohen's d with Hedges correction with its 95% confidence interval;
      ES: verbal classification of the effect size point estimate.")
  )
}
