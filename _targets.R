#
# This is a script defining targets pipeline of the rTMS clinical trial for depression project.
#

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set( packages = c(
  
  "here",      # for path listing
  "tidyverse", # for data wrangling
  "readxl",    # for reading Excel
  "ggdag",     # for DAG drawing
  "ggraph",    # for advanced DAG/ggplot operations
  "gt",        # for nice tables
  "ggpubr",    # for easier time with boxplots
  "patchwork", # for putting plots together
  "rstatix",   # for repeated measures ANOVAs
  "patchwork", # for arranging plots
  "lmerTest",  # for frequentist LMMs
  "rstanarm"   # for Bayesian LMMs
  #"ggtext" # for adding text to plots
  
) )

# Load all in-house functions:
tar_source()

# List the targets:
list(

  ## CAUSAL ASSUMPTIONS ----
  tar_target(
    name    = DAG, # base directed acyclic graph representing causal structure of the problem
    command = make_dag(plot = T)
  ),

  ## DATA FILES ----
  tar_target(
    name    = desc_path, # path to variables' description
    command = here("_raw", "data_descript_rct_rtms.csv"),
    format  = "file"
  ),
  tar_target(
    name    = desc_file, # data set with variables' description
    command = read_file(path = desc_path, separator = ";")
  ),
  tar_target(
    name    = data_path, # path to raw data
    command = here("_raw", "data_rtms_rct_clean_MEDIKACE.xlsx"),
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
      .data = data_wide,
      tit   = "<b>Table 1<br>Subject demographics.</b> Demographics for the two groups after randomization."
    )
  ),
  tar_target(
    name    = response_rates, # prepare a table with reponse/remission rates
    command = describe_responses(
      .data  = data_wide,
      labels = outcomes,
      tit    = "<b>Table 2<br>Response and remission rates.</b> Frequency and rate of subjects' response and remissions per group."
    )
  ),
  tar_target(
    name    = response_plots, # plot 
    command = plot_responses(.data = data_wide)
  ),
  
  ## PER PROTOCOL ANALYSIS ----
  tar_target(
    name    = per_protocol_descriptives, # descriptive table for the per protocol analysiss
    command = describe_outcomes(
      .data   = data_long,
      include = 1,
      decs    = 2,
      tit     = "<b>Table 3<br>Outcomes description.</b> Outcomes' descriptive statistics for the per protocol analysis for the two groups after randomization."
    )
  ),
  tar_target(
    name    = per_protocol_ANOVAs, # mixed ANOVAs as the first approximation per protocol analysis
    command = conduct_ANOVA_loop(.data = data_half, labs = outcomes, show_stats = 2)
  ),
  tar_target(
    name    = per_protocol_ANOVA_table, # extract a big (set of) ANOVAs table(s)
    command = print_ANOVA_table(
      anovas = per_protocol_ANOVAs,
      labs   = outcomes,
      tit   = "<b>Table 4<br>Analyses of variance.</b> A series of univariate mixed ANOVAs testing for null effects in a per-protocol analysis."
    )
  ),
  tar_target(
    name    = per_protocol_pairwise_occasion_main, # pairwise comparisons for Occasion main effects
    command = print_paired_comparisons(
      comps = per_protocol_ANOVAs,
      labs  = outcomes,
      x     = "occas",
      type  = "main",
      tit   = "<b>Table 5<br>Paired comaparisons.</b> Main effects of the Occasion variable in a per-protocol analysis."
    )
  ),
  tar_target(
    name    = per_protocol_pairwise_occasion_simple, # pairwise comparisons for Occasion simple main effects
    command = print_paired_comparisons(
      comps = per_protocol_ANOVAs,
      labs  = outcomes,
      x     = "occas",
      type  = "simp",
      tit   = "<b>Table 6<br>Paired comaparisons.</b> Simple main effects of the Occasion variable in a per-protocol analysis."
    )
  ),
  tar_target(
    name    = per_protocol_pairwise_treatment_simple, # pairwise comparisons for Treatment simple main effects
    command = print_paired_comparisons(
      comps = per_protocol_ANOVAs,
      labs  = outcomes,
      x     = "treat",
      type  = "simp",
      tit   = "<b>Table 7<br>Paired comaparisons.</b> Simple main effects of the Treatment variable in a per-protocol analysis."
    )
  ),
  tar_target(
    name    = per_protocol_figure, # figure including the primary outcomes
    command = big_plot(aov = per_protocol_ANOVAs)
  ),
  
  ## INTENTION-TO-TREAT ANALYSIS ----
  tar_target(
    name    = intetion_to_treat_descriptives, # descriptive table for the intention-to-treat analysis
    command = describe_outcomes(
      .data   = data_long,
      include = c(0,1),
      decs    = 2,
      tit     = "<b>Table 8<br>Outcomes description.</b> Outcomes' descriptive statistics for the intention-to-treat analysis for the two groups after randomization."
    )
  ),
  
  ## STRUCTURE FOR SAVING THE OUTCOMES ----
  tar_target(
    name    = tables_structure, # structure for saving tables
    command = tabs_struct()
  )
  
)

