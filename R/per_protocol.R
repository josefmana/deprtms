#
# This is a script used to analyse data via per-protocol analysis
#

#
# EXTRACT DATA FOR PER PROTOCOL ANALYSIS ----
extract_per_protocol_data <- function(.data) subset(x = .data, completed == 1)

# PRE-FORMAT DATA NEEDED FOR PLOTTING ----
preformat_data <- function(input) mutate(
  
  .data = input,
  `Treatment:` = treatment,
  Occasion = factor(
    x = occasion,
    levels = paste0("T",0:2),
    labels = c("Pre-treatment", "Post-treatment", "Follow-up"),
    ordered = T
  )
  
)

#
# CONDUCT A SERIES OF T-TEST WITH ADDED EFFECT SIZE CALCULATION ----
test_paired_differences <- function(input, grouping, predictor, reference, y, dependent = T) lapply(
  
  c("t_test", "cohens_d"), # functions to be applied
  function(fun) {
    
    set.seed(87542) # set seed for reproducibility
    do.call( # call t-test or effect size function
      
      what = fun,
      args = list(
        
        data = input,
        formula = as.formula( paste(y, predictor, sep = " ~ ") ),
        ref.group = reference,
        paired = dependent,
        var.equal = F,
        conf.level = .95,
        hedges.correction = T, # for correction of Cohen's d
        ci = T # return CIs for Cohen's d
        
      )[if (fun == "t_test") 1:6 else 1:8] # keep only arguments 1:6 for t-test, all for effect size
      
    )
    
  }
  
) %>% reduce( # glue t-test and effect sizes together
  
  left_join,
  by = c(grouping,".y.","group1","group2","n1","n2")
  
)

#
# CONDUCT CLASSICAL STATISTICAL TESTS ----
conduct_mixed_ANOVA <- function(.data, outcome, labels, plt_aov = 2) {
  
  #
  ## ---- prepare data ----
  #
  data <-
    extract_per_protocol_data(.data) %>%
    select( all_of( c("STUDY_ID", "occasion", "treatment", outcome) ) ) %>%
    mutate( occasion = factor(x = occasion, ordered = F) ) # unorder occasion for ANOVAs
  
  #
  ## ---- omnibus analysis ----
  #
  # calculate rm-ANOVA
  aov <- anova_test(
  
    data = data,
    dv = all_of(outcome),
    wid = "STUDY_ID",
    within = "occasion",
    between = "treatment",
    effect.size = "ges" # generalised eta squared as a measure of effect size

  )
  
  # extract ANOVA table
  aov_tab <- get_anova_table(
    
    x = aov,
    correction = "GG" # apply Greenhouse-Geisser adjustment to all within-subjects factors irrespective of (non-)sphericity
    
  )
  
  #
  ## ---- simple main effects ----
  #
  # treatment differences per occasion
  treat_diffs <- test_paired_differences(
    
    input = group_by(.data = data, occasion),
    grouping = "occasion",
    predictor = "treatment",
    reference = "HF-rTMS",
    y = outcome,
    dependent = F
    
  )
  
  # ANOVA of occasion changes per treatment level
  occas_aov <- anova_test(
    
    data = group_by(.data = data, treatment),
    dv = outcome,
    wid = "STUDY_ID",
    within = "occasion",
    effect.size = "ges"
    
  ) %>% get_anova_table(correction = "GG")
  
  
  # paired differences between occasions per treatment level
  occas_diffs <- test_paired_differences(
    
    input = group_by(.data = data, treatment),
    grouping = "treatment",
    predictor = "occasion",
    reference = NULL,
    y = outcome,
    dependent = T
    
  )
  
  #
  ## ---- main effects ----
  #
  # main effect of treatment not computed
  # (would need hierarchical specification and ain't relevant in most cases
  # for decision making in a 'classical' analysis due to a lack of p < .05
  # for treatment main effects)
  
  # main effect pairwise comparisons of occasion
  occas_main <- test_paired_differences(
    
    input = data,
    grouping = NULL,
    predictor = "occasion",
    reference = NULL,
    y = outcome,
    dependent = T
    
  )
  
  #
  ## prepare a plot ----
  
  # calculate mean and 95% CIs for each treatment/occassion combination
  sum_stats <- data %>%
    
    group_by(occasion, treatment) %>%
    summarise( # summary statistics of the (sub-)sample(s)
      M = mean( get(outcome) ),
      SD = sd( get(outcome) ),
      N = sum( !is.na( get(outcome) ) )
    ) %>%
    mutate(
      SEM = SD / sqrt(N), # standard error of the mean
      conf.low = M - qt(.975, df = N-1) * SEM, # lower confidence interval end point for a mean with unknown variance
      conf.high = M + qt(.975, df = N-1) * SEM # upper confidence interval end point for a mean with unknown variance
    ) %>%
    ungroup() %>%
    preformat_data()
  
  # plot the data & results
  plt <-
    
    # prepare data for plotting
    data %>%
    preformat_data() %>%
    
    # plotting proper
    ggplot() +
    aes(x = Occasion, y = get(outcome), fill = `Treatment:`) +
    
    geom_violin( # violin plot to represent distribution of the data
      aes(colour = `Treatment:`),
      position = position_dodge(.6),
      width = .6,
      linewidth = 1.2,
      alpha = 0
    ) +
    
    geom_boxplot( # boxplot to represent summaries of the data
      alpha = .5,
      position = position_dodge(.6),
      linewidth = .8,
      outlier.size = 4,
      notch = T,
      width = .25
    ) +
    
    geom_point( # points to represent averages
      data = sum_stats,
      aes(x = Occasion, y = M, group = `Treatment:`),
      colour = "red3",
      size = 5,
      position = position_dodge(.6)
    ) +
    
    geom_linerange( # whiskers to represent compatibility intervals
      data = sum_stats,
      aes(x = Occasion, y = M, ymin = conf.low, ymax = conf.high, group = `Treatment:`),
      colour = "red3",
      linewidth = 1,
      position = position_dodge(.6)
    ) +
    
    geom_line( # lines to represent changes in means
      data = sum_stats,
      aes(x = Occasion, y = M, linetype = `Treatment:`, group = `Treatment:`),
      colour = "red3",
      linewidth = .8,
      position = position_dodge(.6)
    ) +
    
    # remaining formatting
    labs(y = subset(labels, var == outcome)$sub_abre) +
    scale_fill_manual( values = c("#64CDCC","#F9A729") ) +
    scale_colour_manual( values = c("#64CDCC","#F9A729") ) +
    theme_bw(base_size = 16) +
    theme(legend.position = "bottom")
  
  # add labels regarding effects to the plot if called for
  if ( is.numeric(plt_aov) ) plt <-
    
    plt +
    labs( subtitle = get_test_label(aov, row = plt_aov, detailed = T, type = "expression") )
  
  # return a list with all results
  return(
    list(
      ANOVA_model = aov,
      ANOVA_table = aov_tab,
      occas_main_comp = occas_main,  # pairwise comparisons between occasions averaged over levels of treatment
      occas_simp_anov = occas_aov,   # ANOVAs evaluating simple main effects of occasion conditional on the level of treatment
      occas_simp_comp = occas_diffs, # pairwise comparisons between occasions conditional on the level of treatment
      treat_simp_comp = treat_diffs, # pairwise comparisons between treatment levels conditional on the occasion
      plot = plt
    )
  )
  
}

#
# CONDUCT CLASSICAL TESTING FOR EACH OUTCOME VARIABLE ----
conduct_ANOVA_loop <- function(.data, labs, show_stats = 2) lapply(
  
  set_names(x = 1:nrow(labs), nm = labs$var),
  function(y) conduct_mixed_ANOVA(
    
    .data = .data,
    outcome = labs[y, "var"],
    labels = labs,
    plt_aov = show_stats
    
  )
)

#
# PREPARE A BIG ANOVA TABLE ----
print_ANOVA_table <- function(anovas, labs, tab_no = 1) lapply(
  
  1:nrow(labs), # re-format each ANOVA table and add outcome column to it
  function(y) with(
    
    labs,
    anovas[[var[y]]]$ANOVA_table %>%
      as_tibble %>%
      mutate(
        y = sub_long[[y]],
        Effect = case_when(
          Effect == "treatment" ~ "Treatment",
          Effect == "occasion" ~ "Occasion",
          Effect == "treatment:occasion" ~ "Treatment * Occasion"
        )
      )
  )
  
) %>%
  
  # prepare variables to suitable format
  reduce(full_join) %>%
  mutate(
    q = p.adjust(p = p, method = "BH"),
    `sig.` = if_else(q < .05, "*", ""),
    `F` = rprint(.x = `F`, .decimals = 3),
    DFn = rprint(.x = DFn, .decimals = 2),
    DFd = rprint(.x = DFd, .decimals = 2),
    p = pprint(.p = p, .dec = 3, text = F),
    q = pprint(.p = q, .dec = 3, text = F),
    ges = rprint(.x = ges, .decimals = 3)
  ) %>%
  select(y, Effect, `F`, DFn, DFd, p, q, `sig.`, ges) %>%
  
  # do table formatting
  gt_apa(
    grp = "y",
    title = paste0(
      "<b>Table ", tab_no,
      "</b><br><i>A series of univariate mixed ANOVAs testing for null effects in a per-protocol analysis.</i>"
    )
  ) %>%
  cols_label(
    `F` ~ "{{*F*}}",
    DFn ~ "{{*df*_*n*}}",
    DFd ~ "{{*df*_*d*}}",
    p ~ "{{*p*}}",
    q ~ "{{*q*}}",
    ges ~ "{{:eta:^2}}"
  ) %>%
  tab_source_note(
    source_note = md("*F*: F statistic from a mixed analysis of variance; *df*~*n*~: numerator degrees
    of freedom of the F statistic; *df*~*d*~: denominator (residual) degrees of freedom the F statistic;
    *p*: nominal p-value; *q*: false discovery rate (FDR) adjusted p-value (i.e., the q-value); sig.:
    statistically significant effects after adjusting for 5% FDR; η^2^: generalised η-squared measure
    of effect size.")
  )

#
# EXTRACT TABLE OF PAIRED COMPARISONS ----
print_paired_comparisons <- function(comps, labs, x = "occas", type = "main", tab_no = "A1") {
  
  ## ---- prepare a data.frame for later gt() formatting ----
  df <- lapply(
    
    1:nrow(labs), # re-format each paired comparions table and add outcome column to it
    function(y) with(
      
      labs,
      comps[[var[y]]][[paste0(x,"_",type,"_comp")]] %>%
        as_tibble %>%
        mutate(y = sub_abre[[y]])
    )
    
  ) %>%
    
    # prepare variables to suitable format
    reduce(full_join) %>%
    mutate(
      comparison = paste0(group2,"-",group1),
      t = rprint(statistic, 3),
      df = rprint(df, 0),
      p = sapply( 1:nrow(.), function(i) pprint(.p = p[i], .dec = 3, text = F) ),
      d = paste0(rprint(effsize, 2)," [", rprint(conf.low,2),", ",rprint(conf.high,2),"]")
    )
  
  ## ---- add the condition variable (that's being conditioned on) ----
  if (x == "occas" & type == "main") {
    df$condition <- "none"
  } else if(x == "occas" & type == "simp") {
    df$condition <- df$treatment
  } else if(x == "treat") {
    df$condition <- df$occasion
  }
  #if     (x == "occas" & type == "main") df$condition <- "none"
  #else if(x == "occas" & type == "simp") df$condition <- df$treatment
  #else if(x == "treat"                 ) df$condition <- df$occasion
  
  ## ---- keep only columns of interest ----
  df <-
    df %>%
    select(y, condition, comparison, t, df, p, d, magnitude) %>%
    pivot_wider(id_cols = y, values_from = c(t, df, p, d, magnitude), names_from = c(condition, comparison) )
  
  ## ---- prepare a gt() file ----
  if (x == "occas" & type == "main") {
    
    tab <- df %>%
      
      # arrange columns
      setNames( sub("none_", "", names(.) ) ) %>%
      relocate(`df_T1-T0`       , .after = `t_T1-T0` ) %>% # T1-minus-T0
      relocate(`p_T1-T0`        , .after = `df_T1-T0`) %>%
      relocate(`d_T1-T0`        , .after = `p_T1-T0` ) %>%
      relocate(`magnitude_T1-T0`, .after = `d_T1-T0` ) %>%
      relocate(`df_T2-T0`       , .after = `t_T2-T0` ) %>% # T2-minus-T0
      relocate(`p_T2-T0`        , .after = `df_T2-T0`) %>%
      relocate(`d_T2-T0`        , .after = `p_T2-T0` ) %>%
      relocate(`magnitude_T2-T0`, .after = `d_T2-T0` ) %>%
      
      gt_apa(
        title = paste0(
          "<b>Table ", tab_no,
          "</b><br><i>Paired comaparisons regarding main effects of the Occasion variable in a per-protocol analysis.</i>"
        )
      ) %>%
      
      tab_spanner(columns = ends_with("T1-T0"),  label = "T1-minus-T0", gather = F) %>%
      tab_spanner(columns = ends_with("T2-T0"),  label = "T2-minus-T0", gather = F) %>%
      tab_spanner(columns = ends_with("T2-T1"),  label = "T2-minus-T1", gather = F) %>%
      
      cols_label(
        y                         ~ "Outcome",
        starts_with("t_")         ~ "{{*t*}}",
        starts_with("df_")        ~ "df",
        starts_with("p_")         ~ "{{*p*}}",
        starts_with("d_")         ~ "{{*d*}}",
        starts_with("magnitude_") ~ "ES"
      )
    
      
  } else if (x == "occas" & type == "simp") {
    
    tab <- df %>%
      
      # arrange columns
      relocate(`df_HF-rTMS_T1-T0`       , .after = `t_HF-rTMS_T1-T0` ) %>% # T1-minus-T0 in HF-rTMS
      relocate(`p_HF-rTMS_T1-T0`        , .after = `df_HF-rTMS_T1-T0`) %>%
      relocate(`d_HF-rTMS_T1-T0`        , .after = `p_HF-rTMS_T1-T0` ) %>%
      relocate(`magnitude_HF-rTMS_T1-T0`, .after = `d_HF-rTMS_T1-T0` ) %>%
      
      relocate(`t_TBS_T1-T0`        , .after = `magnitude_HF-rTMS_T1-T0` ) %>% # T1-minus-T0 in TBS
      relocate(`df_TBS_T1-T0`       , .after = `t_TBS_T1-T0`             ) %>%
      relocate(`p_TBS_T1-T0`        , .after = `df_TBS_T1-T0`            ) %>%
      relocate(`d_TBS_T1-T0`        , .after = `p_TBS_T1-T0`             ) %>%
      relocate(`magnitude_TBS_T1-T0`, .after = `d_TBS_T1-T0`             ) %>%
      
      relocate(`df_HF-rTMS_T2-T0`       , .after = `t_HF-rTMS_T2-T0` ) %>% # T2-minus-T0 in HF-rTMS
      relocate(`p_HF-rTMS_T2-T0`        , .after = `df_HF-rTMS_T2-T0`) %>%
      relocate(`d_HF-rTMS_T2-T0`        , .after = `p_HF-rTMS_T2-T0` ) %>%
      relocate(`magnitude_HF-rTMS_T2-T0`, .after = `d_HF-rTMS_T2-T0` ) %>%
      
      relocate(`t_TBS_T2-T0`        , .after = `magnitude_HF-rTMS_T2-T0` ) %>% # T2-minus-T0 in TBS
      relocate(`df_TBS_T2-T0`       , .after = `t_TBS_T2-T0`             ) %>%
      relocate(`p_TBS_T2-T0`        , .after = `df_TBS_T2-T0`            ) %>%
      relocate(`d_TBS_T2-T0`        , .after = `p_TBS_T2-T0`             ) %>%
      relocate(`magnitude_TBS_T2-T0`, .after = `d_TBS_T2-T0`             ) %>%
      
      relocate(`df_HF-rTMS_T2-T1`       , .after = `t_HF-rTMS_T2-T1` ) %>%
      relocate(`p_HF-rTMS_T2-T1`        , .after = `df_HF-rTMS_T2-T1`) %>%
      relocate(`d_HF-rTMS_T2-T1`        , .after = `p_HF-rTMS_T2-T1` ) %>%
      relocate(`magnitude_HF-rTMS_T2-T1`, .after = `d_HF-rTMS_T2-T1` ) %>%
      
      gt_apa(
        title = paste0(
          "<b>Table ", tab_no,
          "</b><br><i>Paired comaparisons regarding simple main effects of the Occasion variable in a per-protocol analysis.</i>"
        )
      ) %>%
      
      tab_spanner(columns = contains("HF-rTMS"), label = "HF-rTMS",     gather = F) %>%
      tab_spanner(columns = contains("TBS"),     label = "TBS",         gather = F) %>%
      tab_spanner(columns = ends_with("T1-T0"),  label = "T1-minus-T0", gather = F) %>%
      tab_spanner(columns = ends_with("T2-T0"),  label = "T2-minus-T0", gather = F) %>%
      tab_spanner(columns = ends_with("T2-T1"),  label = "T2-minus-T1", gather = F) %>%
      
      cols_label(
        y                         ~ "Outcome",
        starts_with("t_")         ~ "{{*t*}}",
        starts_with("df_")        ~ "df",
        starts_with("p_")         ~ "{{*p*}}",
        starts_with("d_")         ~ "{{*d*}}",
        starts_with("magnitude_") ~ "ES"
      )
      
  } else if (x == "treat") {
    
    tab <- df %>%
      
      # arrange columns
      relocate(`df_T0_TBS-HF-rTMS`       , .after = `t_T0_TBS-HF-rTMS`  ) %>% # TBS-minus-HF-rTMS in T0
      relocate(`p_T0_TBS-HF-rTMS`        , .after = `df_T0_TBS-HF-rTMS` ) %>%
      relocate(`d_T0_TBS-HF-rTMS`        , .after = `p_T0_TBS-HF-rTMS`  ) %>%
      relocate(`magnitude_T0_TBS-HF-rTMS`, .after = `d_T0_TBS-HF-rTMS`  ) %>%
      relocate(`df_T1_TBS-HF-rTMS`       , .after = `t_T1_TBS-HF-rTMS`  ) %>% # TBS-minus-HF-rTMS in T1
      relocate(`p_T1_TBS-HF-rTMS`        , .after = `df_T1_TBS-HF-rTMS` ) %>%
      relocate(`d_T1_TBS-HF-rTMS`        , .after = `p_T1_TBS-HF-rTMS`  ) %>%
      relocate(`magnitude_T1_TBS-HF-rTMS`, .after = `d_T1_TBS-HF-rTMS`  ) %>%
      
      
      gt_apa(
        title = paste0(
          "<b>Table ", tab_no,
          "</b><br><i>Paired comaparisons regarding simple main effects of the Treatment variable in a per-protocol analysis.</i>"
        )
      ) %>%
      
      tab_spanner(columns = contains("T0"), label = "T0",     gather = F) %>%
      tab_spanner(columns = contains("T1"), label = "T1",     gather = F) %>%
      tab_spanner(columns = contains("T2"), label = "T2",     gather = F) %>%
      
      cols_label(
        y                         ~ "Outcome",
        starts_with("t_")         ~ "{{*t*}}",
        starts_with("df_")        ~ "df",
        starts_with("p_")         ~ "{{*p*}}",
        starts_with("d_")         ~ "{{*d*}}",
        starts_with("magnitude_") ~ "ES"
      )
    
  }
  
  ## ---- finish it ----
  tab <-
    tab %>%
    tab_source_note(
      source_note = md("HAMA: Hamilton's Depression Inventory; SDS: Self-rating Depression Scale, QIDS:
      Quick Inventory of Depressive Symptomatology; HAMA: Hamilton's Anxiety Inventory, BAI: Beck's 
      Anxiety Scale; PSS: Perceived Stress Scale; HF-rTMS: high frequency repetitive transcranial magnetic
      stimulation; TBS: intermittent theta burst stimulation; *t*: test statistic; df: degrees of freedom;
      *p*: unadjusted p-value; *d*: Cohen's d with Hedges correction with its 95% confidence interval;
      ES: verbal classification of the effect size point estimate.")
    )

  ## return it
  return(tab)
  
  
}