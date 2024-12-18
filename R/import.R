#
# This is a script used to import the data
#

#
# LIST PATH TO A FILE----
list_path <- function(folder, file) here(folder, file)

#
# READ DATA FROM CSV ----
read_file <- function(path, separator) {
  
  not_all_na <- function(x) !all( is.na(x) ) # in-house function for selecting columns with at least one non-NA entry
  read.csv(path, sep = separator) %>% select_if(not_all_na) %>% return() # read and drop all-NA columns
  
}

#
# IMPORT DATA ----
import_data <- function(file, format = "long") {
  
  
  
}
