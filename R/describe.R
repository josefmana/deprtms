#
# This is a script used to describe the data
#

#
# PREPARE A DESCRIPTIVE TABLE ----
description_table <- function(.data, include = 1, decs = 2) {
 
  # prepare the table with descriptive numbers
  tab0 <- .data %>%
    
    filter(completed %in% include) %>%
    mutate( # scale all factors for printing
      scale = factor(
        x = scale,
        levels = c("HAMD", "SDS", "QIDS", "HAMA", "BAI", "PSS", "Chron"),
        labels = c(
          "Hamilton's Depression Inventory",
          "Self-rating Depression Scale",
          "Quick Inventory of Depressive Symptomatology",
          "Hamilton's Anxiety Inventory",
          "Beck's Anxiety Scale",
          "Perceived Stress Scale",
          "Chronotype Inventory"
        ),
        ordered = T
      ),
      subscale = factor(
        x = if_else(is.na(subscale), "tot", subscale),
        levels = c("tot","somF1","psychF2","anxF1","weightF2","thoughtF3","circF4","retarF5","sleepF6"),
        labels = c(
          "Total score",
          "Somatization",
          "Psychological Anxiety",
          "Anxiety",
          "Weight Loss",
          "Thought Disorders",
          "Disturbed Circadian Rhythm",
          "Retardation (Slowness)",
          "Sleep Disorders"
        ),
        ordered = T
      )
    ) %>%
    group_by(treatment, scale, subscale, occasion) %>%
    summarise( N = sum( !is.na(score) ), Score = cenvar(score, .dec = decs) ) %>%
    ungroup() %>%
    pivot_wider(values_from = c("N","Score"), names_from = c("treatment","occasion") ) %>%
    
    relocate(`Score_HF-rTMS_T0`, .after = `N_HF-rTMS_T0`) %>%
    relocate(`Score_HF-rTMS_T1`, .after = `N_HF-rTMS_T1`) %>%
    relocate(`Score_HF-rTMS_T2`, .after = `N_HF-rTMS_T2`) %>%
    relocate(Score_TBS_T0, .after = N_TBS_T0) %>%
    relocate(Score_TBS_T1, .after = N_TBS_T1)
  
  # do post-processing specific for per-protocol analysis (i.e., for include == 1)
  if ( isTRUE(include == 1) ) {
    
    N <- c( HF = unique(tab0$`N_HF-rTMS_T0`), TBS = unique(tab0$N_TBS_T0) ) # number of subjects in each group
    tab0 <- tab0 %>% select( -starts_with("N") ) # drop rows with number of observations
    note <- sapply( N, function(n) paste0("N = ", n) ) # prepare notes with group numbers
    
  }
  
  # prepare the common aspects of the table via gt()
  tab <- tab0 %>%
    
    gt_apa(grp = "scale", nms = "subscale") %>%
    cols_align(columns = 1:2, align = "left") %>%
    cols_align(columns = -(1:2), align = "center")
  
  # add analysis-type (PP vs ITT) specific formatting
  if ( isTRUE(include == 1) ) {
    
    tab <- tab %>%
      
      tab_spanner(columns = contains("HF-rTMS"), label = "HF-rTMS", gather = F) %>%
      tab_spanner(columns = contains("TBS"), label = "TBS", gather = F) %>%
      
      cols_label(
        ends_with("T0") ~ "Pre-intervention",
        ends_with("T1") ~ "Post-intervention",
        ends_with("T2") ~ "Follow-up"
      ) %>%
      
      tab_footnote( locations = cells_column_spanners("HF-rTMS"), footnote = note["HF"] ) %>%
      tab_footnote( locations = cells_column_spanners("TBS"), footnote = note["TBS"] )
    
  } else {
    
    tab <- tab %>%
      
      tab_spanner(columns = ends_with("T0"), label = "Pre-intervention", gather = F) %>%
      tab_spanner(columns = ends_with("T1"), label = "Post-intervention", gather = F) %>%
      tab_spanner(columns = ends_with("T2"), label = "Follow-up", gather = F) %>%
      tab_spanner(columns = contains("HF"), label = "HF-rTMS", gather = F) %>%
      tab_spanner(columns = contains("TBS"), label = "TBS", gather = F) %>%
      
      cols_label(
        starts_with("N") ~ "N",
        starts_with("Score") ~ "Score"
      )
    
  }
  
  # return the result
  return(tab)
  
}


# NEXT FORMAT THE TABLE VIA gt()
#