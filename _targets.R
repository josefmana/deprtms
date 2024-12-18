#
# This is a script running targets pipeline of the rTMS clinical trial for depression project.
#

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set( packages = c(
  
  "here", # for path listing
  "tidyverse", # for data wrangling
  "ggdag", # for DAG drawing
  "ggraph" # for advance DAG/ggplot operations
  #"ggtext", # for adding text to plots
  #"patchwork" # for arranging plots
  
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
    name = data_path,
    command = list_path(folder = "_raw", file = "data_rtms_rct_clean.csv"),
    format = "file"
  ),
  
  tar_target(
    name = data_file, # data set with all observations
    command = read_file(path = data_path, separator = ";")
  )
  
)
