#
# This is a script used to analyse data via per-protocol analysis
#

#
# EXTRACT DATA FOR PER PROTOCOL ANALYSIS ----
extract_per_protocol_data <- function(.data) subset(x = .data, completed == 1)

#
# CONDUCT CLASSICAL STATISTICAL TESTS ----
conduct_mixed_ANOVA <- function(.data, outcome, labels) {
  
  #
  ## ---- in-house function for t-test / Cohen's combo ----
  #
  diff_test <- function(input, grouping, predictor, reference, dependent = T) lapply(
    
    c("t_test", "cohens_d"), # functions to be applied
    function(fun) {
      
      set.seed(87542) # set seed for reproducibility
      do.call( # call t-test or effect size function
        
        what = fun,
        args = list(
          
          data = input,
          formula = as.formula( paste(outcome, predictor, sep = " ~ ") ),
          ref.group = reference,
          paired = dependent,
          var.equal = F,
          conf.level = .95,
          hedges.correction = T, # for correction of Cohen's d
          ci = T # return CIs for Cohen's d
          
        )[if (fun == "t_test") 1:6 else 1:8] # keep only arguments 1:6 for t-test, all for effect size
        
      )
      
    }
    
  ) %>% reduce( # glue t-test and effect sizes together
    
    left_join,
    by = c(grouping,".y.","group1","group2","n1","n2")
    
  )
  
  #
  ## ---- prepare data ----
  #
  data <-
    extract_per_protocol_data(.data) %>%
    select( all_of( c("STUDY_ID", "occasion", "treatment", outcome) ) ) %>%
    mutate( occasion = factor(x = occasion, ordered = F) ) # unorder occasion for ANOVAs
  
  #
  ## ---- omnibus analysis ----
  #
  # calculate rm-ANOVA
  aov <- anova_test(
  
    data = data,
    dv = outcome,
    wid = "STUDY_ID",
    within = "occasion",
    between = "treatment",
    effect.size = "ges" # generalised eta squared as a measure of effect size

  )
  
  # extract ANOVA table
  aov_tab <- get_anova_table(
    
    x = aov,
    correction = "GG" # apply Greenhouse-Geisser adjustment to all within-subjects factors irrespective of (non-)sphericity
    
  )
  
  #
  ## ---- simple main effects ----
  #
  # treatment differences per occasion
  treat_diffs <- diff_test(
    
    input = group_by(.data = data, occasion),
    grouping = "occasion",
    predictor = "treatment",
    reference = "HF-rTMS",
    dependent = F
    
  )
  
  # ANOVA of occasion changes per treatment level
  occas_aov <- anova_test(
    
    data = group_by(.data = data, treatment),
    dv = outcome,
    wid = "STUDY_ID",
    within = "occasion",
    effect.size = "ges"
    
  ) %>% get_anova_table(correction = "GG")
  
  
  # paired differences between occasions per treatment level
  occas_diffs <- diff_test(
    
    input = group_by(.data = data, treatment),
    grouping = "treatment",
    predictor = "occasion",
    reference = "T0",
    dependent = T
    
  )
  
  #
  ## ---- main effects ----
  #
  # main effect of treatment not computed
  # (would need hierarchical specification and ain't relevant in most cases
  # for decision making in a 'classical' analysis due to a lack of p < .05
  # for treatment main effects)
  
  # main effect pairwise comparisons of occasion
  occas_main <- diff_test(
    
    input = data,
    grouping = NULL,
    predictor = "occasion",
    reference = "T0",
    dependent = T
    
  )
  
  #
  ## prepare a plot ----
  #
  # pre-format data files needed for plotting function
  prep_data <- function(input) mutate(
    
    .data = input,
    `Treatment:` = treatment,
    Occasion = factor(
      x = occasion,
      levels = paste0("T",0:2),
      labels = c("Pre-treatment", "Post-treatment", "Follow-up"),
      ordered = T
    )
    
  )
  
  # calculate mean and 95% CIs for each treatment/occassion combination
  sum_stats <- data %>%
    
    group_by(occasion, treatment) %>%
    summarise( # summary statistics of the (sub-)sample(s)
      M = mean( get(outcome) ),
      SD = sd( get(outcome) ),
      N = sum( !is.na( get(outcome) ) )
    ) %>%
    mutate(
      SEM = SD / sqrt(N), # standard error of the mean
      conf.low = M - qt(.975, df = N-1) * SEM, # lower confidence interval end point for a mean with unknown variance
      conf.high = M + qt(.975, df = N-1) * SEM # upper confidence interval end point for a mean with unknown variance
    ) %>%
    ungroup() %>%
    prep_data()
  
  # plot the data & results
  plt <-
    
    # prepare data for plotting
    data %>%
    prep_data() %>%
    
    # plotting proper
    ggplot() +
    aes(x = Occasion, y = get(outcome), fill = `Treatment:`) +
    
    geom_violin( # violin plot to represent distribution of the data
      aes(colour = `Treatment:`),
      position = position_dodge(.6),
      width = .6,
      linewidth = 1.2,
      alpha = 0
    ) +
    
    geom_boxplot( # boxplot to represent summaries of the data
      alpha = .5,
      position = position_dodge(.6),
      linewidth = .8,
      outlier.size = 4,
      notch = T,
      width = .25
    ) +
    
    geom_point( # points to represent averages
      data = sum_stats,
      aes(x = Occasion, y = M, group = `Treatment:`),
      colour = "red3",
      size = 5,
      position = position_dodge(.6)
    ) +
    
    geom_linerange( # whiskers to represent compatibility intervals
      data = sum_stats,
      aes(x = Occasion, y = M, ymin = conf.low, ymax = conf.high, group = `Treatment:`),
      colour = "red3",
      linewidth = 1,
      position = position_dodge(.6)
    ) +
    
    geom_line( # lines to represent changes in means
      data = sum_stats,
      aes(x = Occasion, y = M, linetype = `Treatment:`, group = `Treatment:`),
      colour = "red3",
      linewidth = .8,
      position = position_dodge(.6)
    ) +
    
    # remaining formatting
    labs(y = subset(labels, var == outcome)$sub_abre) +
    scale_fill_manual( values = c("#64CDCC","#F9A729") ) +
    scale_colour_manual( values = c("#64CDCC","#F9A729") ) +
    theme_bw(base_size = 16) +
    theme(legend.position = "bottom")
  
  # return a list with all results
  return(
    list(
      ANOVA_model = aov,
      ANOVA_table = aov_tab,
      occas_main_comp = occas_main,  # pairwise comparisons between occasions averaged over levels of treatment
      occas_simp_anov = occas_aov,   # ANOVAs evaluating simple main effects of occasion conditional on the level of treatment
      occas_simp_comp = occas_diffs, # pairwise comparisons between occasions conditional on the level of treatment
      treat_simp_comp = treat_diffs, # pairwise comparisons between treatment levels conditional on the occasion
      plot = plt
    )
  )
  
}

#
# CONDUCT CLASSICAL TESTING FOR EACH OUTCOME VARIABLE ----
conduct_ANOVA_loop <- function(.data, labs) lapply(
  
  set_names(x = 1:nrow(labs), nm = labs$var),
  function(y) conduct_mixed_ANOVA(
    
    .data = .data,
    outcome = labs[y, "var"],
    labels = labs
    
  )
)


# IN THE NEXT FUNCTION:
# ADD ANOVA RESULTS TO PLOTS (SEE https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/)
# AND ARRANGE THEM.


