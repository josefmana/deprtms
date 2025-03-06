#
# This is a script used to analyse data via intention-to-treat analysis
#

#
# PREPARE DATA FOR MULTILEVEL MODELS ----
prepare_LMM_data <- function(d0) {
  
  # unorder occasion
  df <- d0 %>% mutate(occasion = factor(x = occasion, ordered = F))
  
  # set contrasts
  contrasts(df$occasion)  <- bayestestR::contr.equalprior(n = length(levels(df$occasion)))
  contrasts(df$treatment) <- bayestestR::contr.equalprior(n = length(levels(df$treatment)))
 
  # return the data
  df 
  
}


# Goals:
#   1. lmerTest 'anova tables' and emmeans-based PWCs comparable to per-protocol ANOVAs

#
# GET PAIRWISE COMPARISONS ----
compute_pairwise_comparisons <- function(mod, lab, adj = "tukey", form = "~ occasion", contr = "revpairwise") {
  
  # compute expected marginalised means
  emm <-
    emmeans(mod, as.formula(form)) %>%
    contrast(method = contr, adjust = adj) %>%
    as_tibble() %>%
    mutate(outcome = lab, .before = 1)
  
  # add CIs (adjusted for multiple comparisons as specified)
  ci <-
    emmeans(mod, as.formula(form)) %>%
    contrast(method = contr, adjust = adj) %>%
    confint() %>%
    as_tibble() %>%
    select(ends_with(".CL"))
  
  # bind them   
  cbind.data.frame(emm, ci)
  
}

#
# FIT A VANILLA LMM ----
conduct_LMER <- function(d0, outcome, labels) {
  
  # keep only variables of interest in the data
  df <- d0 %>% select(all_of(c("STUDY_ID", "occasion", "treatment", outcome)))
  
  # get label to use in graphs and tables
  lab <- labels[labels$var == outcome, "sub_abre"]
  
  # fit model
  fit <- lmer(

    formula = as.formula(paste0(outcome," ~ treatment * occasion + (1 | STUDY_ID)")),
    data    = df,
    REML    = T 

  )
  
  # model check
  check <- performance::check_model(fit)
  
  # emmip plot
  plt <-
    emmip(fit, treatment ~ occasion, CIs = T, PIs = T) +
    labs(x = "Occasion", y = lab) +
    scale_colour_manual(values = c("#64CDCC","#F9A729"))
  
  # stats
  aov <- anova(fit) %>% as_tibble(rownames = NA) %>% rownames_to_column("Term")         # fixed effects ANOVA
  occas_main <- compute_pairwise_comparisons(fit, lab, form = "~ occasion")             # main effect of occasion
  treat_main <- compute_pairwise_comparisons(fit, lab, form = "~ treatment")            # main effect of treatment
  occas_simp <- compute_pairwise_comparisons(fit, lab, form = "~ occasion | treatment") # simple main effect of occasion
  treat_simp <- compute_pairwise_comparisons(fit, lab, form = "~ treatment | occasion") # simple main effect of treatment
  
  # return a list with all components
  list(
    LMER_model     = fit,
    diagnostics    = check,
    ANOVA_table    = aov,
    pwc_occas_main = occas_main, # pairwise comparisons between occasions averaged over treatments
    pwc_occas_simp = occas_simp, # pairwise comparisons between occasions conditional on the treatment
    pwc_occas_main = treat_main, # pairwise comparisons between treatments averaged over levels of occasion
    pwc_treat_simp = treat_simp, # pairwise comparisons between treatments conditional on the occasion
    plot           = plt
  )
  
}

#
# CONDUCT LMER FOR EACH OUTCOME VARIABLE ----
conduct_LMER_loop <- function(data, labs) lapply(
  
  set_names(x = 1:nrow(labs), nm = labs$var),
  function(y) conduct_LMER(
    
    d0      = data,
    outcome = labs[y, "var"],
    labels  = labs
    
  )
)


#
# PREPARE A BIG ANOVA TABLE ----
print_lmer_ANOVA <- function(lmers, labs, tit = " ") lapply(
  
  1:nrow(labs), # re-format each ANOVA table and add outcome column to it
  function(y) with(
    
    labs,
    lmers[[var[y]]]$ANOVA_table %>%
      as_tibble() %>%
      mutate(
        y = sub_long[[y]],
        Effect = case_when(
          Term == "treatment" ~ "Treatment",
          Term == "occasion" ~ "Occasion",
          Term == "treatment:occasion" ~ "Treatment * Occasion"
        )
      )
  )
  
) %>%
  
  # prepare variables to suitable format
  reduce(full_join) %>%
  mutate(
    q      = p.adjust(p = `Pr(>F)`, method = "BH"),
    `sig.` = if_else(q < .05, "*", ""),
    `F`    = rprint(.x = `F value`, .decimals = 3),
    DFn    = rprint(.x = NumDF, .decimals = 2),
    DFd    = rprint(.x = DenDF, .decimals = 2),
    p      = pprint(.p = `Pr(>F)`, .dec = 3, text = F),
    q      = pprint(.p = q, .dec = 3, text = F)
  ) %>%
  select(y, Effect, `F`, DFn, DFd, p, q, `sig.`) %>%
  
  # do table formatting
  gt_apa(grp = "y", title = tit) %>%
  cols_label(
    `F` ~ "{{*F*}}",
    DFn ~ "{{*df*_*n*}}",
    DFd ~ "{{*df*_*d*}}",
    p   ~ "{{*p*}}",
    q   ~ "{{*q*}}"
  ) %>%
  tab_source_note(
    source_note = md("*F*: F statistic from a mixed analysis of variance; *df*~*n*~: numerator degrees
    of freedom of the F statistic; *df*~*d*~: denominator (residual) degrees of freedom the F statistic;
    *p*: nominal p-value; *q*: false discovery rate (FDR) adjusted p-value (i.e., the q-value); sig.:
    statistically significant effects after adjusting for 5% FDR.")
  )

#
# EXTRACT TABLE OF PAIRED COMPARISONS ----
print_lmer_pwc <- function(comps, labs, x = "occas", type = "main", tit = " ") {
  
  ## ---- prepare a data.frame for later gt() formatting ----
  d0 <- lapply(
    
    1:nrow(labs), # re-format each paired comparisons table and add outcome column to it
    function(y) with(
      
      labs,
      comps[[var[y]]][[paste0("pwc_",x,"_",type)]] %>%
        as_tibble %>%
        mutate(y = sub_abre[[y]])
    )
    
  ) %>%
    
    # prepare variables to suitable format
    reduce(full_join) %>%
    mutate(
      t   = rprint(t.ratio, 3),
      df  = rprint(df     , 2),
      p   = sapply(seq_len(nrow(.)), function(i) pprint(.p = p.value[i], .dec = 3, text = F)),
      est = paste0(rprint(estimate, 2)," [", rprint(lower.CL, 2),", ",rprint(upper.CL, 2),"]"),
      contrast = gsub(" ", "", contrast)
    )
  
  ## ---- add the condition variable (that's being conditioned on) ----
  if (x == "occas" & type == "main") {
    d0$condition <- "none"
  } else if(x == "occas" & type == "simp") {
    d0$condition <- d0$treatment
  } else if(x == "treat") {
    d0$condition <- d0$occasion
  }
  
  ## ---- keep only columns of interest ----
  df <- d0 %>%
    
    select(outcome, contrast, condition, est, t, df, p) %>%
    pivot_wider(id_cols = outcome, values_from = c(est, t, df, p), names_from = c(condition, contrast) )
  
  ## ---- prepare a gt() file ----
  if (x == "occas" & type == "main") {
    
    tab <- df %>%

      # arrange columns
      setNames( sub("none_", "", names(.) ) ) %>%
      relocate(`t_T1-T0` , .after = `est_T1-T0`) %>% # T1-minus-T0
      relocate(`df_T1-T0`, .after = `t_T1-T0`  ) %>%
      relocate(`p_T1-T0` , .after = `df_T1-T0` ) %>%
      relocate(`t_T2-T0` , .after = `est_T2-T0`) %>% # T2-minus-T0
      relocate(`df_T2-T0`, .after = `t_T2-T0`  ) %>% 
      relocate(`p_T2-T0` , .after = `df_T2-T0` ) %>%
      
      gt_apa(title = tit) %>%
      
      tab_spanner(columns = ends_with("T1-T0"),  label = "T1-minus-T0", gather = F) %>%
      tab_spanner(columns = ends_with("T2-T0"),  label = "T2-minus-T0", gather = F) %>%
      tab_spanner(columns = ends_with("T2-T1"),  label = "T2-minus-T1", gather = F) %>%
      
      cols_label(
        outcome             ~ "Outcome",
        starts_with("est_") ~ "Estimate [95% CI]",
        starts_with("t_")   ~ "{{*t*}}",
        starts_with("df_")  ~ "df",
        starts_with("p_")   ~ "{{*p*}}"
      )

  } else if (x == "occas" & type == "simp") {
    
    tab <- df %>%
      
      # arrange columns
      relocate(`t_HF-rTMS_T1-T0` , .after = `est_HF-rTMS_T1-T0`) %>% # T1-minus-T0 in HF-rTMS
      relocate(`df_HF-rTMS_T1-T0`, .after = `t_HF-rTMS_T1-T0`  ) %>%
      relocate(`p_HF-rTMS_T1-T0` , .after = `df_HF-rTMS_T1-T0` ) %>%
      
      relocate(`est_iTBS_T1-T0`, .after = `p_HF-rTMS_T1-T0`) %>% # T1-minus-T0 in iTBS
      relocate(`t_iTBS_T1-T0`  , .after = `est_iTBS_T1-T0` ) %>% 
      relocate(`df_iTBS_T1-T0` , .after = `t_iTBS_T1-T0`   ) %>%
      relocate(`p_iTBS_T1-T0`  , .after = `df_iTBS_T1-T0`  ) %>%
      
      relocate(`est_HF-rTMS_T2-T0`, .after = `p_iTBS_T1-T0`     ) %>% # T2-minus-T0 in HF-rTMS
      relocate(`t_HF-rTMS_T2-T0`  , .after = `est_HF-rTMS_T2-T0`) %>%
      relocate(`df_HF-rTMS_T2-T0` , .after = `t_HF-rTMS_T2-T0`  ) %>% 
      relocate(`p_HF-rTMS_T2-T0`  , .after = `df_HF-rTMS_T2-T0` ) %>%
      
      relocate(`est_iTBS_T2-T0`, .after = `p_HF-rTMS_T2-T0`) %>% # T2-minus-T0 in iTBS
      relocate(`t_iTBS_T2-T0`  , .after = `est_iTBS_T2-T0` ) %>% 
      relocate(`df_iTBS_T2-T0` , .after = `t_iTBS_T2-T0`   ) %>%
      relocate(`p_iTBS_T2-T0`  , .after = `df_iTBS_T2-T0`  ) %>%
      
      relocate(`t_HF-rTMS_T2-T1` , .after = `est_HF-rTMS_T2-T1`) %>% # T2-minus-T1 in HF-rTMS
      relocate(`df_HF-rTMS_T2-T1`, .after = `t_HF-rTMS_T2-T1` ) %>% 
      relocate(`p_HF-rTMS_T2-T1` , .after = `df_HF-rTMS_T2-T1`) %>%
      
      gt_apa(title = tit) %>%
      
      tab_spanner(columns = contains("HF-rTMS"), label = "HF-rTMS"    , gather = F) %>%
      tab_spanner(columns = contains("iTBS")   , label = "iTBS"       , gather = F) %>%
      tab_spanner(columns = ends_with("T1-T0") , label = "T1-minus-T0", gather = F) %>%
      tab_spanner(columns = ends_with("T2-T0") , label = "T2-minus-T0", gather = F) %>%
      tab_spanner(columns = ends_with("T2-T1") , label = "T2-minus-T1", gather = F) %>%
      
      cols_label(
        outcome             ~ "Outcome",
        starts_with("est_") ~ "Estimate [95% CI]",
        starts_with("t_")   ~ "{{*t*}}",
        starts_with("df_")  ~ "df",
        starts_with("p_")   ~ "{{*p*}}"
      )

  } else if (x == "treat") {
    
    tab <- df %>%
      
      # arrange columns
      relocate(`t_T0_iTBS-(HF-rTMS)` , .after = `est_T0_iTBS-(HF-rTMS)`) %>% # iTBS-minus-HF-rTMS in T0
      relocate(`df_T0_iTBS-(HF-rTMS)`, .after = `t_T0_iTBS-(HF-rTMS)`  ) %>% 
      relocate(`p_T0_iTBS-(HF-rTMS)` , .after = `df_T0_iTBS-(HF-rTMS)` ) %>%
      relocate(`t_T1_iTBS-(HF-rTMS)` , .after = `est_T1_iTBS-(HF-rTMS)`) %>% # iTBS-minus-HF-rTMS in T1
      relocate(`df_T1_iTBS-(HF-rTMS)`, .after = `t_T1_iTBS-(HF-rTMS)`  ) %>%
      relocate(`p_T1_iTBS-(HF-rTMS)` , .after = `df_T1_iTBS-(HF-rTMS)` ) %>%
      
      gt_apa(title = tit) %>%

      tab_spanner(columns = contains("T0"), label = "T0", gather = F) %>%
      tab_spanner(columns = contains("T1"), label = "T1", gather = F) %>%
      tab_spanner(columns = contains("T2"), label = "T2", gather = F) %>%
      
      cols_label(
        outcome             ~ "Outcome",
        starts_with("est_") ~ "Estimate [95% CI]",
        starts_with("t_")   ~ "{{*t*}}",
        starts_with("df_")  ~ "df",
        starts_with("p_")   ~ "{{*p*}}"
      )
    
  }
  
  ## ---- finish it ----
  tab %>% tab_source_note(
    
    source_note = md("HAMA: Hamilton's Depression Inventory; SDS: Self-rating Depression Scale, QIDS:
      Quick Inventory of Depressive Symptomatology; HAMA: Hamilton's Anxiety Inventory, BAI: Beck's 
      Anxiety Scale; PSS: Perceived Stress Scale; HF-rTMS: high frequency repetitive transcranial magnetic
      stimulation; iTBS: intermittent theta burst stimulation; *Estimate [95% CI]*: Estimated difference with
      its 95% confidence interval; *t*: test statistic; df: degrees of freedom; *p*: unadjusted p-value.")
    
  )
}





#
# Goals:
#   2. rstanarm models equivalent to the lmer4 ones + others with added patient-specific effects
#   3. brms multivariate models with patient-specific effects and covariance between measures
#
