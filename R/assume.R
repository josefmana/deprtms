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
      paste0("HAMD_tot_T", 0:2), # observed depressive symptoms variables
      paste0("Depr_T", 0:2), # latent depression state variables
      paste0("ed_T", 0:2), # unobserved error scores for depression
      "Stim" # intervention/treatment variable
    ),
    label = c(
      "Depr[T0]", "Depr[T1]", "Depr[T2]", # latent depression state variables
      "HAMD[T0]", "HAMD[T1]", "HAMD[T2]", # observed depressive symptoms variables
      "e[D[T0]]", "e[D[T1]]", "e[D[T2]]", # unobserved error scores for depression
      "X" # intervention/treatment variable
    ),
    x = c(rep(1:3, 3), 1.75),
    y = c(rep(2,3), rep(1,3), rep(3,3), 0)

  )
  
  # prepare the DAG
  dag <- dagify(
    
    # observed scores
    HAMD_tot_T0 ~ Depr_T0 + ed_T0,
    HAMD_tot_T1 ~ Depr_T1 + ed_T1,
    HAMD_tot_T2 ~ Depr_T2 + ed_T2,
    
    # latent variables
    Depr_T1 ~ Depr_T0 + Stim,
    Depr_T2 ~ Depr_T1 + Stim,
    
    latent = c( paste0("Depr_T", 0:2), paste0("ed_T", 0:2) ),
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
