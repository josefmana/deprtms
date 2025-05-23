#
# This is a script used to define more general functions to be used
# at various points through the processing pipeline.
#

#
# PREPARE A FOLDER ----
new_folder <- function(name) if (!dir.exists(name)) dir.create(name)

#
# PRINT ROUNDED NUMBER ----
rprint <- function(.x, .decimals = 2) sprintf( paste0("%.",.decimals,"f"), round(.x, .decimals) )

#
# GET RID OF LEADING ZERO ----
zerolead <- function(x, dec = 3) sub("0.", ".", rprint(x, dec), fixed = T)

#
# PRINT P-VALUE ----
pprint <- function(.p, .dec = 3, text = F) ifelse(
  
  test = text == T,
  yes  = ifelse( .p < .001, "< .001", paste0( "= ", zerolead(.p, .dec) ) ),
  no   = ifelse( .p < .001, "< .001",               zerolead(.p, .dec)   )
  
)

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
# GIVE FREQUENCY (COLUMN PERCENTAGE) ----
freqperc <- function(.x, .y, dec) left_join(
  
  x = table(.x, .y) %>%
    as.data.frame() %>%
    pivot_wider(names_from = .y, values_from = Freq),
  
  y = prop.table(table(.x, .y), margin = 2) %>%
    as.data.frame() %>%
    mutate( Perc = paste0(rprint(100*Freq, dec),"%") ) %>%
    select(-Freq) %>%
    pivot_wider(names_from = .y, values_from = Perc),
  
  by     = ".x",
  suffix = c("_x", "_y")
  
) %>%
  
  mutate( # .y needs to be treatment in this code
    `HF-rTMS` = paste0(`HF-rTMS_x`," (",`HF-rTMS_y`,")"),
    iTBS      = paste0(iTBS_x     ," (", iTBS_y    ,")")
  ) %>%
  select( -contains("_") ) %>%
  rename("Level" = ".x")

#
# FORMAT TABLE TO APA STYLE ----
gt_apa <- function(x, grp = NULL, nms = NULL, title = " ") x %>%
  
  gt(groupname_col = grp, rowname_col = nms) %>%
  tab_options(
    table.border.top.color            = "white",
    heading.title.font.size           = px(16),
    column_labels.border.top.width    = 3,
    column_labels.border.top.color    = "black",
    column_labels.border.bottom.width = 3,
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color    = "black",
    table.border.bottom.color         = "white",
    table.width                       = pct(100),
    table.background.color            = "white"
  ) %>%
  cols_align(align = "center") %>%
  tab_style(
    style = list(
      cell_borders(
        sides  = c("top", "bottom"),
        color  = "white",
        weight = px(1)
      ),
      cell_text(
        align="center"
      ),
      cell_fill(color = "white", alpha = NULL)
    ),
    locations = cells_body(
      columns = everything(),
      rows    = everything()
    )
  ) %>%
  tab_header( # title setup
    title = html(title)
  ) %>%
  opt_align_table_header(align = "left")

#
# LIST OUTCOME VARIABLES WITH THEIR MEANINGS ----
list_outcomes <- function() data.frame(
  
  var = c( # variable name in the half-long data format
    
    "SDS",
    "QIDS",
    "BAI",
    "PSS",
    "Chron",
    paste0( "HAMD_", c("tot","anxF1","weightF2","thoughtF3","circF4","retarF5","sleepF6") ),
    paste0( "HAMA_", c("tot","somF1","psychF2") )
  ),
  scl_long = c( # long form of scale's name
    
    "Self-rating Depression Scale",
    "Quick Inventory of Depressive Symptomatology",
    "Beck's Anxiety Scale",
    "Perceived Stress Scale",
    "Chronotype Inventory",
    rep("Hamilton's Depression Inventory", 7),
    rep("Hamilton's Anxiety Inventory", 3)
  ),
  scl_abre = c( # abbreviation of scale's name
    "SDS",
    "QIDS",
    "BAI",
    "PSS",
    "Chronotype",
    rep("HAMD", 7),
    rep("HAMA", 3)
  ),
  sub_long = c( # long form of subscale's label
    "Self-rating Depression Scale",
    "Quick Inventory of Depressive Symptomatology",
    "Beck's Anxiety Scale",
    "Perceived Stress Scale",
    "Chronotype Inventory",
    paste0(
      "Hamilton's Depression Inventory - ", c(
        "Total score",
        "Anxiety",
        "Weight Loss",
        "Thought Disorders",
        "Disturbed Circadian Rhythm",
        "Retardation",
        "Sleep Disorders"
      )
    ),
    paste0(
      "Hamilton's Anxiety Inventory - ", c(
        "Total score",
        "Somatization",
        "Psychological Anxiety"
      )
    )
  ),
  sub_abre = c( # abbreviation of subscale's name
    "SDS",
    "QIDS",
    "BAI",
    "PSS",
    "Chronotype",
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
    paste0(
      "HAMA (", c(
        "Total score",
        "Somatization",
        "Psychological Anxiety"
      ), ")"
    )
  ),
  rater = c( # who was the evaluator
    rep("self"     , 5 ), # SDS, QIDS, BAI, PSS & Chronotype Inventory
    rep("clinician", 10)  # HAMD & HAMA
  )
)

#
# PRE-ALLOCATE TABLE NAMES ----
tabs_struct <- function() data.frame(
  
  tar  = c(
    "sample_description",
    "response_rates",
    "per_protocol_descriptives",
    "per_protocol_ANOVA_table",
    "per_protocol_pairwise_occasion_main",
    "per_protocol_pairwise_occasion_simple",
    "per_protocol_pairwise_treatment_simple",
    "intention_to_treat_descriptives",
    "intention_to_treat_ANOVA_table",
    "intention_to_treat_pwc_occasion_main",
    "intention_to_treat_pwc_occasion_simple",
    "intention_to_treat_pwc_treatment_simple"
    
  ),
  path = paste0(
    "_tables/", c(
      "sample_description",
      "responses_remissions",
      "per_protocol_descriptives",
      "per_protocol_ANOVAs",
      "per_protocol_PWCs_occasion_main",
      "per_protocol_PWCs_occasion_simple_main",
      "per_protocol_PWCs_treatment_simple_main",
      "intetion_to_treat_descriptives",
      "intention_to_treat_ANOVAs",
      "intention_to_treat_PWCs_occasion_main",
      "intention_to_treat_PWCs_occasion_simple_main",
      "intention_to_treat_PWCs_treatment_simple_main"
    )
  )
  
)

#
# SAVE TABLES ----
save_tables <- function(struct) {
  
  # prepare a folder if it does not exist yet
  new_folder("_tables")
  
  # loop through tables and save them
  for (i in seq_len(nrow(struct))) with(
    
    struct, {
      cat(paste0("Writing ",path[i], " ... \n"))
      gt::gtsave( tar_read_raw(tar[i]), paste0(path[i],".html") )
      gt::gtsave( tar_read_raw(tar[i]), paste0(path[i],".docx") )
    }
  )
  
}
