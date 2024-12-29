#
# This is a script used to represent causal assumptions
# for deducing correct adjustment sets
# for a subsequent statistical analysis
#

#
# PREPARE & SAVE A DAG ----
make_dag <- function(plot = T) {
  
  # set-up a data frame with node labels and coordinates
  nms <- data.frame(

    name = c(
      paste0("SDS_T", 0:2),      # observed depressive symptoms variables (self-rating)
      paste0("HAMD_tot_T", 0:2), # observed depressive symptoms variables (clinician-rating)
      paste0("Depr_T", 0:2),     # latent depression state variables
      paste0("ed_T", 0:2),       # unobserved error scores for depression (self-rating)
      paste0("td_T", 0:2),       # unobserved error scores for depression (clinician-rating)
      "Stim",                    # intervention/treatment variable
      paste0("BAI_T", 0:2),      # observed anxiety symptoms variables (self-rating)
      paste0("HAMA_tot_T", 0:2), # observed anxiety symptoms variables (clinician-rating)
      paste0("Anx_T", 0:2),      # latent anxiety state variables
      paste0("ea_T", 0:2),       # unobserved error scores for anxiety (self-rating)
      paste0("ta_T", 0:2)        # unobserved error scores for anxiety (clinician-rating)
    ),
    label = c(
      "SDS[T0]", "SDS[T1]", "SDS[T2]",                      # observed depressive symptoms variables (self-rating)
      "HAMD[T0]", "HAMD[T1]", "HAMD[T2]",                   # observed depressive symptoms variables (clinician-rating)
      "Depr[T0]", "Depr[T1]", "Depr[T2]",                   # latent depression state variables
      "epsilon[D[T0]]", "epsilon[D[T1]]", "epsilon[D[T2]]", # unobserved error scores for depression self-rating
      "tau[D[T0]]", "tau[D[T1]]", "tau[D[T2]]",             # unobserved error scores for depression clinician-rating
      "TMS",                                                # intervention/treatment variable
      "BAI[T0]", "BAI[T1]", "BAI[T2]",                      # observed anxiety symptoms variables (self-rating)
      "HAMA[T0]", "HAMA[T1]", "HAMA[T2]",                   # observed anxiety symptoms variables (clinician-rating)
      "Anx[T0]", "Anx[T1]", "Anx[T2]",                      # latent anxiety state variables
      "epsilon[A[T0]]", "epsilon[A[T1]]", "epsilon[A[T2]]", # unobserved error scores for anxiety (self-rating)
      "tau[A[T0]]", "tau[A[T1]]", "tau[A[T2]]"              # unobserved error scores for anxiety (clinician-rating)
    ),
    x = c(
      1:3 - 0.25, # observed depressive symptoms variables (self-rating)
      1:3 + 0.25, # observed depressive symptoms variables (clinician-rating)
      1:3,       # latent depression state variables
      1:3 - 0.25, # unobserved error scores for depression (self-rating)
      1:3 + 0.25, # unobserved error scores for depression (clinician-rating)
      1.75,      # intervention/treatment variable
      1:3 - 0.25, # observed anxiety symptoms variables (self-rating)
      1:3 + 0.25, # observed anxiety symptoms variables (clinician-rating)
      1:3,       # latent anxiety state variables
      1:3 - 0.25, # unobserved error scores for anxiety (self-rating)
      1:3 + 0.25  # unobserved error scores for anxiety (clinician-rating)
    ),
    y = c(
      rep(2,3),  # observed depressive symptoms variables (self-rating)
      rep(2,3),  # observed depressive symptoms variables (clinician-rating)
      rep(1,3),  # latent depression state variables
      rep(3,3),  # unobserved error scores for depression (self-rating)
      rep(3,3),  # unobserved error scores for depression (clinician-rating)
      0,         # intervention/treatment variable
      rep(-2,3), # observed anxiety symptoms variables (self-rating)
      rep(-2,3), # observed anxiety symptoms variables (clinician-rating)
      rep(-1,3), # latent anxiety state variables
      rep(-3,3), # unobserved error scores for anxiety (self-rating)
      rep(-3,3)  # unobserved error scores for anxiety (clinician-rating)
    )

  )
  
  # prepare the DAG
  dag <- dagify(
    
    # observed scores
    SDS_T0 ~ Depr_T0 + ed_T0,
    SDS_T1 ~ Depr_T1 + ed_T1,
    SDS_T2 ~ Depr_T2 + ed_T2,
    
    HAMD_tot_T0 ~ Depr_T0 + td_T0,
    HAMD_tot_T1 ~ Depr_T1 + td_T1,
    HAMD_tot_T2 ~ Depr_T2 + td_T2,
    
    BAI_T0 ~ Anx_T0 + ea_T0,
    BAI_T1 ~ Anx_T1 + ea_T1,
    BAI_T2 ~ Anx_T2 + ea_T2,
    
    HAMA_tot_T0 ~ Anx_T0 + ta_T0,
    HAMA_tot_T1 ~ Anx_T1 + ta_T1,
    HAMA_tot_T2 ~ Anx_T2 + ta_T2,
    
    # latent variables
    Depr_T1 ~ Depr_T0 + Stim,
    Depr_T2 ~ Depr_T1 + Stim,
    Anx_T1 ~ Anx_T0 + Stim,
    Anx_T2 ~ Anx_T1 + Stim,
    
    latent = c(
      paste0("Depr_T", 0:2), paste0("ed_T", 0:2), paste0("td_T", 0:2),
      paste0("Anx_T", 0:2), paste0("ea_T", 0:2), paste0("ta_T", 0:2)
    ),
    coords = nms
    
  ) %>%
    
    tidy_dagitty() %>%
    arrange("name")
  
  # plot the DAG
  dag_plt <- dag %>%
    
    # prepare the plot
    ggplot() +
    aes(x = x, y = y, xend = xend, yend = yend) +
    
    # add nodes and edges
    geom_dag_point(size = 30, colour = "white") +
    scale_colour_manual(values = "white") +
    geom_dag_edges(
      arrow_directed = grid::arrow(length = grid::unit(8, "pt"), type = "open"),
      start_cap = ggraph::ellipsis(13, 9, 'mm'),
      end_cap = ggraph::ellipsis(13, 9,'mm')
    ) +
    geom_dag_text(
      label = arrange(nms, name)$label,
      colour = "black",
      size = 5.83,
      parse = T
    ) +
    
    # finishing touches
    theme_dag() +
    theme(legend.position = "none")
  
  # return it
  if(plot == T) return(dag_plt) else return(dag)

}
