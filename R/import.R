#
# This is a script used to import the data
#

#
# LIST PATH TO A FILE ----
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
  
  # prepare a list for long- and wide-format data
  # using list such that later call for return is more straightforward
  # compared to two data frames
  d0 <- list(long = NULL, longer = NULL, wide = NULL)
  
  # extract column names of repeatedly measured outcomes
  outs <- names(file)[ grepl( "_T", names(file) ) ] # scale_time-point variables
  scls <- unique( sub("_[^_]+$", "", outs) ) # scale irrespective of the time-point
  
  # prepare the long format data first
  d0$longer <-
    
    file %>%
    pivot_longer( # change data to a long format 
      cols = all_of(outs),
      names_to = c("scale", "subscale","occasion"),
      names_sep = "_",
      values_to = "score"
    ) %>%
    mutate( # re-code rows where there is no subscale
      occasion = if_else( is.na(occasion), subscale, occasion),
      subscale = if_else( subscale == occasion, NA, subscale)
    ) %>%
    mutate( # formatting proper
      STUDY_ID = factor(x = STUDY_ID),
      completed = if_else( # manually re-code completion for subject #130
        condition = STUDY_ID == 130,
        true = 0,
        false = completed
      ),
      sex = factor(
        x = case_when(sex == "f" ~ 0, sex == "m" ~ 1),
        levels = 0:1,
        labels = c("female", "male"),
        ordered = F
      ),
      laterality = factor(
        x = case_when(laterality %in% c("P","p") ~ 0, laterality %in% c("L","l") ~ 1),
        levels = 0:1,
        labels = c("right", "left"),
        ordered = F
      ),
      dg = factor(x = dg),
      stim_type = factor(
        x = case_when(stim_type == "HF" ~ 0, stim_type == "TBS" ~ 1),
        levels = 0:1,
        labels = c("HF-rTMS", "TBS"),
        ordered = F
      ),
      across(
        .cols = contains("termination"),
        .fns = ~ if_else(.x == "", NA, .x)
      ),
      occasion = factor(
        x = sub("T", "", occasion),
        levels = 0:2,
        labels = paste0("T",0:2),
        ordered = T
      )
    ) %>%
    rename("treatment" = "stim_type")
  
  # do the first pivoting to wider
  d0$long <-
    
    d0$longer %>%
    mutate( col = if_else(!is.na(subscale), paste0(scale,"_",subscale), scale) ) %>% # prepare column names
    select( -ends_with("scale") ) %>% # get rid of scale names to allow for proper pivot
    pivot_wider(names_from = col, values_from = score)
  
  # do the second pivoting to the widest (i.e., the original but formatted)
  d0$wide <-
    
    d0$long %>%
    pivot_wider(values_from = all_of(scls), names_from = occasion)
  
  # check whether there is a difference between original and the widest data (there should bo none)
  if (max(abs(file[ , outs] - d0$wide[ , outs] ), na.rm = T) > 0) {
    
    return("Imported data do not agree with the original file!")
    
  } else return(d0[[format]])
  
}

#
# LIST OUTCOME VARIABLES WITH THEIR MEANINGS ----
list_outcomes <- function() data.frame(
  
  var = c( # variable name in the half-long data format
    paste0( "HAMD_", c("tot","anxF1","weightF2","thoughtF3","circF4","retarF5","sleepF6") ),
    "SDS",
    "QIDS",
    paste0( "HAMA_", c("tot","somF1","psychF2") ),
    "BAI",
    "PSS",
    "Chron"
  ),
  scl_long = c( # long form of scale's name
    rep("Hamilton's Depression Inventory", 7),
    "Self-rating Depression Scale",
    "Quick Inventory of Depressive Symptomatology",
    rep("Hamilton's Anxiety Inventory", 3),
    "Beck's Anxiety Scale",
    "Perceived Stress Scale",
    "Chronotype Inventory"
  ),
  scl_abre = c( # abbreviation of scale's name
    rep("HAMD", 7),
    "SDS",
    "QIDS",
    rep("HAMA", 3),
    "BAI",
    "PSS",
    "Chronotype"
  ),
  sub_long = c( # long form of subscale's label
    paste0(
      "Hamilton's Depression Inventory - ", c(
        "Total score",
        "Anxiety",
        "Weight Loss",
        "Thought Disorders",
        "Disturbed Circadian Rhythm",
        "Retardation (Slowness)",
        "Sleep Disorders"
      )
    ),
    "Self-rating Depression Scale",
    "Quick Inventory of Depressive Symptomatology",
    paste0(
      "Hamilton's Anxiety Inventory - ", c(
        "Total score",
        "Somatization",
        "Psychological Anxiety"
      )
    ),
    "Beck's Anxiety Scale",
    "Perceived Stress Scale",
    "Chronotype Inventory"
  ),
  sub_abre = c( # abbreviation of subscale's name
    paste0(
      "HAMD (", c(
        "Total score",
        "Anxiety",
        "Weight Loss",
        "Thought Disorders",
        "Disturbed Circadian Rhythm",
        "Retardation (Slowness)",
        "Sleep Disorders"
      ), ")"
    ),
    "SDS",
    "QIDS",
    paste0(
      "HAMA (", c(
        "Beck's Anxiety Scale",
        "Perceived Stress Scale",
        "Chronotype Inventory"
      ), ")"
    ),
    "BAI",
    "PSS",
    "Chronotype"
  ),
  rater = c( # who was the evaluator
    rep("clinician", 7), # HAMD
    rep("self", 2), # SDS & QIDS
    rep("clinician", 3), # HAMA
    rep("self", 3) # BAI, PSS & Chronotype Inventory
  )
)
