#
# This is a script used to define more generalised functions to be used
# at various points throught the processing pipeline.
#

#
# PRINT ROUNDED NUMBER ----
rprint <- function(.x, .decimals = 2) sprintf( paste0("%.",.decimals,"f"), round(.x, .decimals) )

#
# GIVE CENTRAL TENDENCY ± VARIABILITY ----
cenvar <- function(.y, .dec = 2, cen = "mean", var = "sd", sep = " ± ") sapply(
  
  c(cen, var),
  function(fun) do.call(
    
    fun,
    list(.y, na.rm = T)
    
  ) %>% rprint(.decimals = .dec)
  
) %>% paste(collapse = sep)

#
# FORMAT TABLE TO APA STYLE ----
gt_apa <- function(x, grp = NULL, nms = NULL, title = " ") x %>%
  
  gt(groupname_col = grp, rowname_col = nms) %>%
  tab_options(
    table.border.top.color = "white",
    heading.title.font.size = px(16),
    column_labels.border.top.width = 3,
    column_labels.border.top.color = "black",
    column_labels.border.bottom.width = 3,
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black",
    table.border.bottom.color = "white",
    table.width = pct(100),
    table.background.color = "white"
  ) %>%
  cols_align(align="center") %>%
  tab_style(
    style = list(
      cell_borders(
        sides = c("top", "bottom"),
        color = "white",
        weight = px(1)
      ),
      cell_text(
        align="center"
      ),
      cell_fill(color = "white", alpha = NULL)
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )
  ) %>%
  tab_header( # title setup
    title = html("<i>", title, "</i>")
  ) %>%
  opt_align_table_header(align = "left")

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
