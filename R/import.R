#
# This is a script used to import the data
#

#
# READ DATA FROM CSV ----
read_file <- function(path, separator = NULL) {
  
  not_all_na <- function(x) !all( is.na(x) ) # in-house function for selecting columns with at least one non-NA entry
  
  # read and drop all-NA columns
  if ( is.null(separator) ) read_excel(path) %>% select_if(not_all_na) %>% return()
  else read.csv(path, sep = separator) %>% select_if(not_all_na) %>% return()
  
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
  scls <- unique( sub("_[^_]+$", "", outs) )        # scale irrespective of the time-point
  resp <- scls[c(1:4, 6, 9)]                        # scales for response rates calculations (no subscales)
  
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
      occasion = if_else(is.na(occasion), subscale, occasion),
      subscale = if_else(subscale == occasion, NA, subscale)
    ) %>%
    mutate( # formatting proper
      STUDY_ID = factor(x = STUDY_ID),
      completed = if_else( # manually re-code completion for subjects #130 and #148
        condition = STUDY_ID %in% c(130, 148),
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
        labels = c("HF-rTMS", "iTBS"),
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
      ),
      termination_phase = factor(
        x = case_when(
          is.na(termination_phase)  ~ 0,
          termination_phase == "T1" ~ 1,
          termination_phase == "T2" ~ 2
        ),
        levels = 0:2,
        labels = c("none","T1","T2"),
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
  
  # add response rates
  for (i in resp) for (j in c("T1","T2")) {
    
    # response rate
    rr <- (d0$wide[ , paste0(i,"_",j)] - d0$wide[ , paste0(i,"_T0")]) / d0$wide[ , paste0(i,"_T0")]
    
    # response (i.e., at least 50% reduction compared to baseline)
    d0$wide[ , paste0(i,"_",j,"_perc_decl")] <- -100 * rr
    d0$wide[ , paste0(i,"_",j,"_response")]  <- if_else(condition = rr < -0.5, true = 1, false = 0)
    
  }
  
  # manually add remissions
  d0$wide <- d0$wide %>% mutate(

    across( all_of( paste0( "SDS_"     , c("T1","T2") ) ), ~ if_else(.x <= 49, 1, 0), .names = "{.col}_remission" ),
    across( all_of( paste0( "PSS_"     , c("T1","T2") ) ), ~ if_else(.x <= 13, 1, 0), .names = "{.col}_remission" ),
    across( all_of( paste0( "BAI_"     , c("T1","T2") ) ), ~ if_else(.x <=  7, 1, 0), .names = "{.col}_remission" ),
    across( all_of( paste0( "QIDS_"    , c("T1","T2") ) ), ~ if_else(.x <=  5, 1, 0), .names = "{.col}_remission" ),
    across( all_of( paste0( "HAMA_tot_", c("T1","T2") ) ), ~ if_else(.x <= 17, 1, 0), .names = "{.col}_remission" ),
    across( all_of( paste0( "HAMD_tot_", c("T1","T2") ) ), ~ if_else(.x <=  7, 1, 0), .names = "{.col}_remission" )

  )
  
  # check whether there is a difference between original and the widest data (there should bo none)
  if (max(abs(file[ , outs] - d0$wide[ , outs] ), na.rm = T) > 0) {
    
    return("Imported data do not agree with the original file!")
    
  } else return(d0[[format]])
  
}
