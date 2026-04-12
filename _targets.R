library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(packages = c(
  "here",       # path listing
  "tidyverse",  # data wrangling
  "readxl",     # reading Excel
  "ggdag",      # DAG drawing
  "ggraph",     # advanced DAG/ggplot operations
  "gt",         # nice tables
  "ggpubr",     # easier time with boxplots
  "patchwork",  # putting plots together
  "rstatix",    # repeated measures ANOVAs
  "patchwork",  # arranging plots
  "lmerTest",   # frequentist LMMs
  "emmeans",    # marginal means and contrast of frequentist LMMs
  "bayestestR", # contrasts
  "performance" # model diagnostics
))

tar_source()

list(
  ## CAUSAL ASSUMPTIONS ----
  tar_target(
    name    = DAG, # base directed acyclic graph representing causal structure of the problem
    command = make_dag(plot = TRUE)
  ),

  ## DATA FILES ----
  tar_target(
    name    = desc_path, # path to variables' description
    command = here::here("data-raw", "data_descript_rct_rtms.csv"),
    format  = "file"
  ),
  tar_target(
    name    = desc_file, # data set with variables' description
    command = read_file(path = desc_path, separator = ";")
  ),
  tar_target(
    name    = data_path, # path to raw data
    command = here::here("data-raw", "data_rtms_rct_clean_MEDIKACE.xlsx"),
    format  = "file"
  ),
  tar_target(
    name    = data_file, # data set with all observations
    command = read_file(path = data_path, separator = NULL)
  ),
  
  ## DATA IMPORT ----
  tar_target(
    name    = data_wide, # data in a wide-format
    command = import_data(file = data_file, format = "wide")
  ),
  tar_target( # data in long-format w.r.t. time-point, wide-format w.r.t. to outcomes, i.e., half-wide/half-long
    name    = data_half,
    command = import_data(file = data_file, format = "long")
  ),
  tar_target( # pre-vs-post data for baseline adjustment
    name    = data_prepost,
    command = import_data(file = data_file, format = "prepost")
  ),
  tar_target(
    name    = data_long, # data in long-format w.r.t. time-point and w.r.t. to outcomes
    command = import_data(file = data_file, format = "longer")
  ),
  tar_target(
    name    = outcomes, # table containing labelling conventions regarding outcomes 
    command = list_outcomes()
  ),
  
  ## DESCRIPTION OF THE SAMPLE ----
  tar_target(
    name    = sample_description, # prepare a table with demographics
    command = decribe_demographics(
      data  = data_wide,
      tit   = "<b>Table 1<br>Subject demographics.</b> Demographics for the two groups after randomization."
    )
  ),
  tar_target(
    name    = response_rates, # prepare a table with reponse/remission rates
    command = describe_responses(
      data   = data_wide,
      labels = outcomes,
      tit    = "<b>Table 4<br>Response and remission rates.</b> Frequency and rate of subjects' response and remissions per group."
    )
  ),
  tar_target(
    name    = response_plots, # plot 
    command = plot_responses(data = data_wide)
  ),
  
  ## PER PROTOCOL ANALYSIS ----
  tar_target(
    name    = per_protocol_descriptives, # descriptive table for the per protocol analysis
    command = describe_outcomes(
      data    = data_long,
      include = 1,
      decs    = 2,
      tit     = "<b>Table 2<br>Outcomes description.</b> Outcomes' descriptive statistics for the per protocol analysis for the two groups after randomization."
    )
  ),
  tar_target(
    name    = per_protocol_ANOVAs, # mixed ANOVAs as the first approximation per protocol analysis
    command = conduct_anova_loop(data = data_half, labs = outcomes, show_stats = 2)
  ),
  tar_target(
    name    = per_protocol_ANOVA_table, # extract a big (set of) ANOVAs table(s)
    command = print_anova_table(
      anovas = per_protocol_ANOVAs,
      labs   = outcomes,
      tit   = "<b>Table 3<br>Analyses of variance.</b> A series of univariate mixed ANOVAs testing for null effects in a per-protocol analysis."
    )
  ),
  tar_target(
    name    = per_protocol_pairwise_occasion_main, # pairwise comparisons for Occasion main effects
    command = print_paired_comparisons(
      comps = per_protocol_ANOVAs,
      labs  = outcomes,
      x     = "occas",
      type  = "main",
      tit   = "<b>Table 6<br>Paired comparisons.</b> Main effects of the Occasion variable in a per-protocol analysis."
    )
  ),
  tar_target(
    name    = per_protocol_pairwise_occasion_simple, # pairwise comparisons for Occasion simple main effects
    command = print_paired_comparisons(
      comps = per_protocol_ANOVAs,
      labs  = outcomes,
      x     = "occas",
      type  = "simp",
      tit   = "<b>Table 7<br>Paired comparisons.</b> Simple main effects of the Occasion variable in a per-protocol analysis."
    )
  ),
  tar_target(
    name    = per_protocol_pairwise_treatment_simple, # pairwise comparisons for Treatment simple main effects
    command = print_paired_comparisons(
      comps = per_protocol_ANOVAs,
      labs  = outcomes,
      x     = "treat",
      type  = "simp",
      tit   = "<b>Table 8<br>Paired comparisons.</b> Simple main effects of the Treatment variable in a per-protocol analysis."
    )
  ),
  tar_target(
    name    = per_protocol_figure, # figure including the primary outcomes
    command = big_plot(aov = per_protocol_ANOVAs)
  ),
  
  ## INTENTION-TO-TREAT ANALYSIS ----
  tar_target(
    name    = intention_to_treat_descriptives, # descriptive table for the intention-to-treat analysis
    command = describe_outcomes(
      data    = data_long,
      include = c(0, 1),
      decs    = 2,
      tit     = "<b>Table 5<br>Outcomes description.</b> Outcomes' descriptive statistics for the intention-to-treat analysis for the two groups after randomization."
    )
  ),
  tar_target(
    name    = intention_to_treat_LMERs, # LMMs as the first approximation intention to treat analysis
    command = conduct_lmer_loop(data = data_half, labs = outcomes)
  ),
  tar_target(
    name    = intention_to_treat_ANOVA_table, # extract a big (set of) ANOVA table(s)
    command = print_lmer_anova(
      lmers = intention_to_treat_LMERs,
      labs  = outcomes,
      tit   = "<b>Table 9<br>Analyses of variance.</b> A series of univariate ANOVAs testing for null effects in an intention-to-treat analysis."
    )
  ),
  tar_target(
    name    = intention_to_treat_pwc_occasion_main, # pairwise comparisons for Occasion main effects
    command = print_lmer_pwc(
      comps = intention_to_treat_LMERs,
      labs  = outcomes,
      x     = "occas",
      type  = "main",
      tit   = "<b>Table 10<br>Paired comparisons.</b> Main effects of the Occasion variable in an intention-to-treat analysis."
    )
  ),
  tar_target(
    name    = intention_to_treat_pwc_occasion_simple, # pairwise comparisons for Occasion simple main effects
    command = print_lmer_pwc(
      comps = intention_to_treat_LMERs,
      labs  = outcomes,
      x     = "occas",
      type  = "simp",
      tit   = "<b>Table 11<br>Paired comparisons.</b> Simple main effects of the Occasion variable in an intention-to-treat analysis."
    )
  ),
  tar_target(
    name    = intention_to_treat_pwc_treatment_simple, # pairwise comparisons for Treatment simple main effects
    command = print_lmer_pwc(
      comps = intention_to_treat_LMERs,
      labs  = outcomes,
      x     = "treat",
      type  = "simp",
      tit   = "<b>Table 12<br>Paired comparisons.</b> Simple main effects of the Treatment variable in an intention-to-treat analysis."
    )
  ),
  
  ## SENSITIVITY ANALYSES ----
  tar_target(
    name    = per_protocol_ANCOVAs,
    command = conduct_lmer_loop(
      data = data_half,
      labs = outcomes,
      type = "baseline_pp"
    )
  ),
  tar_target(
    name    = per_protocol_ANCOVAs_interaction,
    command = conduct_lmer_loop(
      data = data_half,
      labs = outcomes,
      type = "baseline_pp2"
    )
  ),
  tar_target(
    name    = intention_to_treat_ANCOVAs,
    command = conduct_lmer_loop(
      data = data_half,
      labs = outcomes,
      type = "baseline"
    )
  ),
  tar_target(
    name    = intention_to_treat_ANCOVAs_interaction,
    command = conduct_lmer_loop(
      data = data_half,
      labs = outcomes,
      type = "baseline2"
    )
  ),
  tar_target(
    name    = per_protocol_baseline_sensitivity,
    command = print_baseline_adjustment_comparisons_table(
      vanilla = per_protocol_ANOVAs,
      ancova1 = per_protocol_ANCOVAs,
      ancova2 = per_protocol_ANCOVAs_interaction,
      pp      = TRUE,
      labs    = outcomes,
      tit     = "<b>Table 13<br>Sensitivity analysis.</b> Comparison between the primary and baseline-adjusted simple main effect estimates of the Occasion variable in the per-protocol analysis."
    )
  ),
  tar_target(
    name    = intention_to_treat_baseline_sensitivity,
    command = print_baseline_adjustment_comparisons_table(
      vanilla = intention_to_treat_LMERs,
      ancova1 = intention_to_treat_ANCOVAs,
      ancova2 = intention_to_treat_ANCOVAs_interaction,
      pp      = FALSE,
      labs    = outcomes,
      tit     = "<b>Table 14<br>Sensitivity analysis.</b> Comparison between the primary and baseline-adjusted simple main effect estimates of the Occasion variable in the intention-to-treat analysis."
    )
  ),
  
  ## STRUCTURE FOR SAVING THE OUTCOMES ----
  tar_target(
    name    = tables_structure, # structure for saving tables
    command = tabs_struct()
  ),
  
  ## REPORT ----
  tar_quarto(
    name  = report, # render a Quarto report
    path  = "report.qmd",
    quiet = FALSE
  )
)

