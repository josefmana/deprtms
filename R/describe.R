#
# This is a script used to describe the data
#

#
# PREPARE A DESCRIPTIVE TABLE OF DEMOGRAPHICS ----
decribe_demographics <- function(.data, tit = "<b>Table 1</b>") {
  
  # list variables to be described
  vars <- data.frame(
    var = c(
      "age",
      "education",
      "device_energy",
      "sex",
      "dg",
      "completed",
      "termination_phase",
      "Antidepressants",
      "Antipsychotics",
      "Benzodiazepines",
      "Mood stabilizers",
      "Non-benzodiazepine anxiolitics/Hypnotics",
      "Stimulants"
    ),
    lab = c(
      "Age (years)",
      "Education (years)",
      "Device energy (% of maximum capacity)",
      "Sex",
      "Diagnosis",
      "Completed",
      "Termination phase",
      "Antidepressants",
      "Antipsychotics",
      "Benzodiazepines",
      "Mood stabilizers",
      "Non-benzodiazepine anxiolitics/Hypnotics",
      "Stimulants"
    ),
    type = c(
      rep("continuous" , 3),
      rep("categorical", 4),
      rep("medication",  6)
    )
  )
  
  # prepare descriptions
  tab <- with(
    
    vars, list(
      
      # continuous data
      .data %>%
        group_by(treatment) %>%
        summarise( across( all_of(var[type == "continuous"]), cenvar) ) %>%
        pivot_longer(cols = -treatment, names_to = "Variable") %>%
        pivot_wider(names_from = "treatment", values_from = "value") %>%
        mutate(Variable = lab[var == Variable], Level = NA, .before = 2),
      
      # categorical data
      lapply(
        var[type == "categorical"], 
        function(x)
          freqperc(.x = .data[[x]], .y = .data$treatment, dec = 1) %>%
          mutate(Variable = lab[var == x], .before = 1)
      ) %>% reduce(full_join),
      
      # medication data
      .data %>%
        group_by(treatment) %>%
        summarise(
          across(
            all_of(var[type == "medication"]),
            ~ paste0(sum(.x), " (", rprint(100 * mean(.x, na.rm = T), 1), "%)")
          )
        ) %>%
        pivot_longer(cols = -treatment, names_to = "Level") %>%
        pivot_wider(names_from = "treatment", values_from = "value") %>%
        mutate(Variable = "Medication", .before = 1)

    )
  ) %>% reduce(full_join)

  # do statistical tests to compare groups
  stats <- with(

    vars, sapply(
      
      var, function(y) {
        
        # do appropriate tests
        if (type[var == y] == "continuous") res <- t.test(
          
          formula    = as.formula( paste0(y," ~ treatment") ),
          data       = .data,
          altrnative = "two.sided",
          var.equal  = F

        ) else res <- chisq.test( table(.data[[y]], .data$treatment) )
        
        # extract text
        txt <- ifelse(
          
          test = type[var == y] == "continuous",
          yes  = paste0( "t(" , rprint(res$parameter, 2),") = ", rprint(res$statistic, 2),", p ", pprint(res$p.value, 3, T) ),
          no   = paste0( "χ2(", rprint(res$parameter, 0),") = ", rprint(res$statistic, 2),", p ", pprint(res$p.value, 3, T) )
        )
        
        # select row for categorical variables
        if (type[var == y] == "categorical") lvl <- unlist(subset(tab, Variable == lab[var == y])[1, "Level"], use.names = F)
        else lvl <- NA
        
        # add variable name to it
        if (type[var == y] != "medication") return( c(Variable = lab[var == y], Level = lvl, Stats = txt) )
        else return(c(Variable = "Medication", Level = lab[var == y], Stats = txt) )
        
      }
    )
  ) %>% t() %>% as.data.frame()
  
  # add stats to the table & format it
  left_join(
    x  = tab,
    y  = stats,
    by = c("Variable", "Level")
  ) %>%
    
    mutate_all( ~ if_else(is.na(.x), "", .x) ) %>%
    gt_apa(grp = "Variable", nms = "Level", title = tit) %>%
    return()
  
}

#
# PREPARE A DESCRIPTIVE TABLE OF THE OUTCOMES ----
describe_outcomes <- function(.data, include = 1, decs = 2, tit = " ") {
 
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
    
    gt_apa(grp = "scale", nms = "subscale", title = tit) %>%
    cols_align(columns = 1:2.  , align = "left"  ) %>%
    cols_align(columns = -(1:2), align = "center")
  
  # add analysis-type (PP vs ITT) specific formatting
  if ( isTRUE(include == 1) ) {
    
    tab <- tab %>%
      
      tab_spanner(columns = contains("HF-rTMS"), label = "HF-rTMS", gather = F) %>%
      tab_spanner(columns = contains("TBS")    , label = "TBS"    , gather = F) %>%
      
      cols_label(
        ends_with("T0") ~ "Pre-intervention",
        ends_with("T1") ~ "Post-intervention",
        ends_with("T2") ~ "Follow-up"
      ) %>%
      
      tab_footnote( locations = cells_column_spanners("HF-rTMS"), footnote = note["HF"] ) %>%
      tab_footnote( locations = cells_column_spanners("TBS"), footnote = note["TBS"] )
    
  } else {
    
    tab <- tab %>%
      
      tab_spanner(columns = ends_with("T0"), label = "Pre-intervention" , gather = F) %>%
      tab_spanner(columns = ends_with("T1"), label = "Post-intervention", gather = F) %>%
      tab_spanner(columns = ends_with("T2"), label = "Follow-up"        , gather = F) %>%
      tab_spanner(columns = contains("HF") , label = "HF-rTMS"          , gather = F) %>%
      tab_spanner(columns = contains("TBS"), label = "TBS"              , gather = F) %>%
      
      cols_label(
        starts_with("N") ~ "N",
        starts_with("Score") ~ "Score"
      )
    
  }
  
  # return the result
  return(tab)
  
}

#
# EXTRACT RESPONSE/REMISSION TABLE ----
extract_responses <- function(.data, which = "response") .data %>%
  
  group_by(treatment) %>%
  summarise(
    across(
      .cols = ends_with( paste0("_",which) ),
      .fns = ~ paste0(sum(.x, na.rm = T), " (", rprint(100 * sum(.x, na.rm = T) / sum( !is.na(.x) ), 1), "%)")
    )
  ) %>%
  pivot_longer(cols = -treatment) %>%
  pivot_wider(names_from = treatment, values_from = value) %>%
  mutate(
    name     = sub( paste0("_",which), "", name),
    occasion = substr(name, nchar(name)-1, nchar(name)),
    scale    = gsub("_[^_]*$" , "", name)
  ) %>%
  select(-name) %>%
  pivot_wider(values_from = c(`HF-rTMS`, TBS), names_from = occasion) %>%
  relocate(TBS_T1, .after = `HF-rTMS_T1`)

#
# RESPONSE & REMISSION RATES ----
describe_responses <- function(.data, labels, tit = " ") {
  
  # prepare tables for responses and remissions
  tab <- lapply(

    c("response","remission"),
    function(i)
      extract_responses(.data, i) %>%
      mutate(measure = i, .before = 1)

  ) %>%
    reduce(full_join)
  
  # calculate chi-square tests
  stats <- lapply(
    
    set_names( unique(tab$measure) ),
    function(i) lapply(
      
      set_names( unique(tab$scale) ),
      function(y) lapply(
        
        set_names( c("T1", "T2") ),
        function(t)
          chisq.test( table(.data[[paste0(y,"_",t,"_",i)]], .data$treatment) )
        
      )
    )
  )
  
  # format it
  tab %>%
    
    mutate(
      stats_T1 = unlist(
        sapply(
          1:nrow(.),
          function(i) with(
            stats[[measure[i]]][[scale[i]]]$T1,
            paste0( "χ2(", rprint(parameter, 0),") = ", rprint(statistic, 2),", p ", pprint(p.value, 3, T) )
          )
        )
      ),
      stats_T2 = unlist(
        sapply(
          1:nrow(.),
          function(i) with(
            stats[[measure[i]]][[scale[i]]]$T2,
            paste0( "χ2(", rprint(parameter, 0),") = ", rprint(statistic, 2),", p ", pprint(p.value, 3, T) )
          )
        )
      ),
      measure  = paste0(sub("r", "R", measure), " Rate"),
      scale    = sub(
        pattern     = " (Total score)", # messy way to print HAMD and HAMA better
        replacement = "",
        x           = unlist(sapply(1:nrow(.), function(i) labels$sub_abre[labels$var == scale[i]]), use.names = F),
        fixed       = T
      )
    ) %>%
    relocate(stats_T1, .after = TBS_T1) %>%
    
    gt_apa(grp = "measure", nms = "scale", title = tit) %>%
    tab_spanner(columns = ends_with("T1"), label = "Post-intervention", gather = F) %>%
    tab_spanner(columns = ends_with("T2"), label = "Follow-up"        , gather = F) %>%
    cols_label(
      starts_with("HF-rTMS") ~ "HF-rTMS",
      starts_with("TBS")     ~ "TBS",
      starts_with("stats")   ~ "Stat. test"
    )
  
}
