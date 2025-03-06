#
# This is a script used to describe the data
#

#
# PREPARE A DESCRIPTIVE TABLE OF DEMOGRAPHICS ----
decribe_demographics <- function(.data, tit = "<b>Table 1</b>") {
  
  # set-up order of variables
  ord <- c(
    "Sex",
    "Age (years)",
    "Education (years)",
    "Medication",
    "Completed",
    "Termination phase",
    "Device energy (% of maximum capacity)"
  )

  # list variables to be described
  vars <- data.frame(
    var = c(
      "sex",
      "age",
      "education",
      "Antidepressants",
      "Antipsychotics",
      "Benzodiazepines",
      "Mood stabilizers",
      "Non-benzodiazepine anxiolitics/Hypnotics",
      "Stimulants",
      "completed",
      "termination_phase",
      "device_energy"
    ),
    lab = c(
      "Sex",
      "Age (years)",
      "Education (years)",
      "Antidepressants",
      "Antipsychotics",
      "Benzodiazepines",
      "Mood stabilizers",
      "Non-benzodiazepine anxiolitics/Hypnotics",
      "Stimulants",
      "Completed",
      "Termination phase",
      "Device energy (% of maximum capacity)"
    ),
    type = c(
      "categorical",
      rep("continuous" , 2),
      rep("medication",  6),
      rep("categorical", 2),
      "continuous"
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
        mutate(Variable = sapply(seq_len(n()), function(i) lab[var == Variable[i]]), Level = NA, .before = 2),
      
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
  ) %>%
    reduce(full_join)

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
    mutate(Variable = factor(Variable, levels = ord, ordered = T)) %>%
    arrange(Variable) %>%
    gt_apa(grp = "Variable", nms = "Level", title = tit)
  
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
        levels = c("SDS", "QIDS", "BAI", "PSS", "Chron", "HAMD", "HAMA"),
        labels = c(
          "Self-rating Depression Scale",
          "Quick Inventory of Depressive Symptomatology",
          "Beck's Anxiety Scale",
          "Perceived Stress Scale",
          "Chronotype Inventory",
          "Hamilton's Depression Inventory",
          "Hamilton's Anxiety Inventory"
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
    relocate(Score_iTBS_T0     , .after = N_iTBS_T0     ) %>%
    relocate(Score_iTBS_T1     , .after = N_iTBS_T1     )
  
  # do post-processing specific for per-protocol analysis (i.e., for include == 1)
  if ( isTRUE(include == 1) ) {
    N    <- c(HF = unique(tab0$`N_HF-rTMS_T0`), iTBS = unique(tab0$N_iTBS_T0)) # number of subjects in each group
    tab0 <- tab0 %>% select(-starts_with("N"))                                 # drop rows with number of observations
    note <- sapply(N, function(n) paste0("N = ", n))                           # prepare notes with group numbers
  }
  
  # prepare the common aspects of the table via gt()
  tab <-
    tab0 %>%
    gt_apa(grp = "scale", nms = "subscale", title = tit) %>%
    cols_align(columns =   1:2 , align = "left"        ) %>%
    cols_align(columns = -(1:2), align = "center"      )

  # add analysis-type (PP vs ITT) specific formatting
  if ( isTRUE(include == 1) ) {

    tab <- tab %>%

      tab_spanner(columns = contains("HF-rTMS"), label = "HF-rTMS", gather = F) %>%
      tab_spanner(columns = contains("iTBS")   , label = "iTBS"    , gather = F) %>%
      
      cols_label(
        ends_with("T0") ~ "Pre-intervention",
        ends_with("T1") ~ "Post-intervention",
        ends_with("T2") ~ "Follow-up"
      ) %>%
      
      tab_footnote( locations = cells_column_spanners("HF-rTMS"), footnote = note["HF"] ) %>%
      tab_footnote( locations = cells_column_spanners("iTBS"), footnote = note["iTBS"] )
    
  } else {
    
    tab <- tab %>%
      
      tab_spanner(columns = ends_with("T0") , label = "Pre-intervention" , gather = F) %>%
      tab_spanner(columns = ends_with("T1") , label = "Post-intervention", gather = F) %>%
      tab_spanner(columns = ends_with("T2") , label = "Follow-up"        , gather = F) %>%
      tab_spanner(columns = contains("HF")  , label = "HF-rTMS"          , gather = F) %>%
      tab_spanner(columns = contains("iTBS"), label = "iTBS"             , gather = F) %>%
      
      cols_label(
        starts_with("N") ~ "N",
        starts_with("Score") ~ "Score"
      )
    
  }
  
  # return the result
  tab
  
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
  pivot_wider(values_from = c(`HF-rTMS`, iTBS), names_from = occasion) %>%
  relocate(iTBS_T1, .after = `HF-rTMS_T1`)

#
# RESPONSE & REMISSION RATES ----
describe_responses <- function(.data, labels, tit = " ") {
  
  # keep per protocol data only
  data <- .data %>% filter(completed == 1)
  
  # summarise percentage changes
  perc <- sapply(
    levels(data$treatment),
    function(i) apply(
      subset(data, treatment == i)[ , names(data)[grepl("_perc",names(data))]],
      2,
      cenvar
    )
  ) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    mutate(
      rowname = sub("_perc_decl", "", rowname),
      scale   = sub("_[^_]*$", "", rowname),
      event   = sub(".*\\_", "", rowname),
      measure = "response"
    ) %>%
    select(measure, scale, event, `HF-rTMS`, iTBS) %>%
    pivot_wider(values_from = c(`HF-rTMS`, iTBS), names_from = event, names_prefix = "perc")
  
  # prepare tables for responses and remissions
  tab <- lapply(
    c("response","remission"),
    function(i)
      extract_responses(data, i) %>%
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
          chisq.test(table(.data[[paste0(y,"_",t,"_",i)]], .data$treatment))
      )
    )
  )

  # format it
  full_join(perc, tab, by = c("measure", "scale")) %>%
    
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
    mutate_all(~if_else(is.na(.),"-",.)) %>%
    relocate(stats_T1   , .after = iTBS_T1         ) %>%
    relocate(iTBS_percT1, .after = `HF-rTMS_percT1`) %>%
    
    gt_apa(grp = "measure", nms = "scale", title = tit) %>%
    tab_spanner(columns = ends_with("T1"), label = "T1", gather = F) %>%
    tab_spanner(columns = ends_with("T2"), label = "T2", gather = F) %>%
    tab_spanner(columns = contains("perc"), label = "Percentage change", gather = F) %>%
    cols_label(
      starts_with("HF-rTMS") ~ "HF-rTMS",
      starts_with("iTBS")    ~ "iTBS",
      starts_with("stats")   ~ "Stat. test"
    )
  
}

#
# RESPONSE/REMISSION PLOTS ----
plot_responses <- function(.data) {
  
  # extract all post- variables of interest and set threshold for remissions
  pars <-
    data.frame( y = sub( "_response", "", names(.data)[grepl("response",names(.data))] ) ) %>%
    mutate(
      y0 = sub("T1|T2", "T0", y),
      threshold = case_when(
        grepl("SDS" , y) ~ 49,
        grepl("PSS" , y) ~ 13,
        grepl("BAI" , y) ~  7,
        grepl("QIDS", y) ~  5,
        grepl("HAMA", y) ~ 17,
        grepl("HAMD", y) ~  7
      )
    )
  
  # do (all) the plotting
  plts <- lapply(
    
    set_names(x = 1:nrow(pars), nm = pars$y),
    function(i)
      .data %>%
      ggplot() +
      aes(x = get(pars$y0[i]), y = get(pars$y[i]), colour = treatment) +
      labs(x = pars$y0[i], y = pars$y[i]) +
      geom_point(size = 2.33) +
      geom_abline(intercept = .5, slope = .5, colour = "black", linewidth = .8) +
      geom_abline(intercept = .5, slope = 1 , colour = "black", linewidth = .8, linetype = "dotted") +
      geom_abline(intercept = pars$threshold[i] + .5, slope = 0 , colour = "red4", linewidth = .7, linetype = "dashed") +
      theme_minimal(base_size = 12) +
      scale_colour_manual(values = c("#64CDCC","#F9A729"))
  )
  
  # set-up 'depression' & 'anxiety' big plots
  with(
    
    plts, list(
      
      # depression
      depression =
        (SDS_T1 | SDS_T2) / (QIDS_T1 | QIDS_T2) / (HAMD_tot_T1 | HAMD_tot_T2) +
        plot_annotation(tag_levels = "A") +
        plot_layout(guides = "collect") &
        theme(legend.position = "bottom"),

      # anxiety/stress
      anxiety =
        (BAI_T1 | BAI_T2) / (PSS_T1 | PSS_T2) / (HAMA_tot_T1 | HAMA_tot_T2) +
        plot_annotation(tag_levels = "A") +
        plot_layout(guides = "collect") &
        theme(legend.position = "bottom")

    )
  )
  
}

