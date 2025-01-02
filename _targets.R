#
# This is a script running targets pipeline of the rTMS clinical trial for depression project.
#

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set( packages = c(
  
  "here",      # for path listing
  "tidyverse", # for data wrangling
  "ggdag",     # for DAG drawing
  "ggraph",    # for advanced DAG/ggplot operations
  "gt",        # for nice tables
  "ggpubr",    # for easier time with boxplots
  "rstatix",   # for repeated measures ANOVAs
  "patchwork"  # for arranging plots
  #"ggtext" # for adding text to plots
  
) )

# Load all in-house functions:
tar_source()

# List the targets:
list(

  ## CAUSAL ASSUMPTIONS ----
  tar_target(
    name = DAG, # base directed acyclic graph representing causal structure of the problem
    command = make_dag(plot = T)
  ),

  ## DATA FILES ----
  tar_target(
    name = desc_path, # path to variables' description
    command = list_path(folder = "_raw", file = "data_descript_rct_rtms.csv"),
    format = "file"
  ),
  tar_target(
    name = desc_file, # data set with variables' description
    command = read_file(path = desc_path, separator = ";")
  ),
  tar_target(
    name = data_path, # path to raw data
    command = list_path(folder = "_raw", file = "data_rtms_rct_clean.csv"),
    format = "file"
  ),
  tar_target(
    name = data_file, # data set with all observations
    command = read_file(path = data_path, separator = ";")
  ),
  
  ## DATA IMPORT ----
  tar_target(
    name = data_wide, # data in a wide-format
    command = import_data(file = data_file, format = "wide")
  ),
  tar_target( # data in long-format w.r.t. time-point, wide-format w.r.t. to outcomes, i.e., half-wide/half-long
    name = data_half,
    command = import_data(file = data_file, format = "long")
  ),
  tar_target(
    name = data_long, # data in long-format w.r.t. time-point and w.r.t. to outcomes
    command = import_data(file = data_file, format = "longer")
  ),
  tar_target(
    name = outcomes, # table containing labelling conventions regarding outcomes 
    command = list_outcomes()
  ),
  
  ## DATA DESCRIPTION ----
  tar_target(
    name = table_description_ITT, # descriptive table for the intention-to-treat analysis
    command = description_table(.data = data_long, include = c(0,1), decs = 2)
  ),
  tar_target(
    name = table_description_PP, # descriptive table for the per protocol analysiss
    command = description_table(.data = data_long, include = 1, decs = 2)
  )
  
)
