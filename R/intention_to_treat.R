#
# This is a script used to analyse data via intention-to-treat analysis
#

#
# PREPARE DATA FOR MULTILEVEL MODELS ----
prepare_LMM_data <- function(d0) {
  
  # unorder occasion
  df <- d0 %>% mutate(occasion = factor(x = occasion, ordered = F))
  
  # set contrasts
  contrasts(df$occasion)  <- bayestestR::contr.equalprior(n = length(levels(df$occasion)))
  contrasts(df$treatment) <- bayestestR::contr.equalprior(n = length(levels(df$treatment)))
 
  # return the data
  df 
  
}


# Goals:
#   1. lmerTest 'anova tables' and emmeans-based PWCs comparable to per-protocol ANOVAs

#
# GET PAIRWISE COMPARISONS ----
compute_pairwise_comparisons <- function(mod, lab, form = "~ occasion", contr = "revpairwise") {
  
  emmeans(mod, as.formula(form)) %>%
    contrast(method = contr) %>%
    as_tibble() %>%
    mutate(outcome = lab, .before = 1)
  
}

#
# FIT A VANILLA LMM ----
conduct_LMER <- function(d0, outcome, labels) {
  
  # keep only variables of interest in the data
  df <- d0 %>% select(all_of(c("STUDY_ID", "occasion", "treatment", outcome)))
  
  # get label to use in graphs and tables
  lab <- labels[labels$var == outcome, "sub_abre"]
  
  # fit model
  fit <- lmer(

    formula = as.formula(paste0(outcome," ~ occasion * treatment + (1 | STUDY_ID)")),
    data    = df,
    REML    = T 

  )
  
  # model check
  check <- performance::check_model(fit)
  
  # emmip plot
  plt <-
    emmip(fit, treatment ~ occasion, CIs = T, PIs = T) +
    labs(x = "Occasion", y = lab) +
    scale_colour_manual(values = c("#64CDCC","#F9A729"))
  
  # stats
  aov <- anova(fit) %>% as_tibble(rownames = NA) %>% rownames_to_column("Term")         # fixed effects ANOVA
  occas_main <- compute_pairwise_comparisons(fit, lab, form = "~ occasion")             # main effect of occasion
  treat_main <- compute_pairwise_comparisons(fit, lab, form = "~ treatment")            # main effect of treatment
  occas_simp <- compute_pairwise_comparisons(fit, lab, form = "~ occasion | treatment") # simple main effect of occasion
  treat_simp <- compute_pairwise_comparisons(fit, lab, form = "~ treatment | occasion") # simple main effect of treatment
  
  # return a list with all components
  list(
    LMER_model     = fit,
    diagnostics    = check,
    ANOVA_table    = aov,
    pwc_occas_main = occas_main, # pairwise comparisons between occasions averaged over treatments
    pwc_occas_simp = occas_simp, # pairwise comparisons between occasions conditional on the treatment
    pwc_occas_main = treat_main, # pairwise comparisons between treatments averaged over levels of occasion
    pwc_treat_simp = treat_simp, # pairwise comparisons between treatments conditional on the occasion
    plot           = plt
  )
  
}

#
# CONDUCT LMER FOR EACH OUTCOME VARIABLE ----
conduct_LMER_loop <- function(data, labs) lapply(
  
  set_names(x = 1:nrow(labs), nm = labs$var),
  function(y) conduct_LMER(
    
    d0      = data,
    outcome = labs[y, "var"],
    labels  = labs
    
  )
)


#
# Goals:
#   2. rstanarm models equivalent to the lmer4 ones + others with added patient-specific effects
#   3. brms multivariate models with patient-specific effects and covariance between measures
#
