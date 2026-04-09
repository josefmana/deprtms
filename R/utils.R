#' Prepare a folder
#' 
#' @param name Folder name
#' 
#' @export
new_folder <- function(name) {
  if (!dir.exists(name)) {
    dir.create(name)
  }
}

#' Print rounded number
#' 
#' @param x Number
#' @param decimals Decimals
#' 
#' @export
rprint <- function(x, decimals = 2) {
  sprintf(paste0("%.", decimals, "f"), round(x, decimals))
}

#' Get rid of leading zero
#' 
#' @param x Number
#' @param dec Decimals
#' 
#' @export
zerolead <- function(x, dec = 3) {
  sub("0.", ".", rprint(x, dec), fixed = TRUE)
}

#' Print p-value
#' 
#' @param p Raw p-value
#' @param dec Decimals
#' @param text Use for in-text (`TRUE`) or table (`FALSE`)?
#' 
#' @export
pprint <- function(p, dec = 3, text = FALSE) {
  if (text) {
    dplyr::if_else(p < .001, "< .001", paste0("= ", zerolead(p, dec)))
  } else {
    dplyr::if_else(p < .001, "< .001", zerolead(p, dec))
  }
}

#' Give central tendecny ± variability
#' 
#' @param y Vector
#' @param dec Decimals
#' @param cen Central tendency measure
#' @param var Variability measure
#' @param sep Separator
#' 
#' @export
cenvar <- function(y, dec = 2, cen = "mean", var = "sd", sep = " ± ") {
  sapply(c(cen, var), function(fun) {
    do.call(fun, list(y, na.rm = TRUE)) |>
      rprint(decimals = dec)
  }) |>
    paste(collapse = sep)
}

#' Give frequency (column percentage)
#' 
#' @param x Vector
#' @param y Vector
#' @param dec decimals
#' 
#' @export
freqperc <- function(x, y, dec) {
  dplyr::left_join(
    x = table(x, y) |>
      as.data.frame() |>
      tidyr::pivot_wider(names_from = y, values_from = Freq),
    
    y = prop.table(table(x, y), margin = 2) |>
      as.data.frame() |>
      dplyr::mutate(Perc = paste0(rprint(100 * Freq, dec), "%")) |>
      dplyr::select(-Freq) |>
      tidyr::pivot_wider(names_from = y, values_from = Perc),

    by = "x",
    suffix = c("_x", "_y")
  ) |>
    dplyr::mutate( # y needs to be treatment in this code
      `HF-rTMS` = paste0(`HF-rTMS_x`, " (", `HF-rTMS_y`, ")"),
      iTBS      = paste0(iTBS_x     , " (",  iTBS_y    , ")")
    ) %>%
    dplyr::select(-contains("_")) |>
    dplyr::rename("Level" = "x")
}

#' Format table to APA style
#' 
#' @param x Table
#' @param grp Grouping variable
#' @param nms Names
#' @param title Table title
#' 
#' @export
gt_apa <- function(x, grp = NULL, nms = NULL, title = " ") {
  x |>
    gt::gt(groupname_col = grp, rowname_col = nms) |>
    gt::tab_options(
      table.border.top.color            = "white",
      heading.title.font.size           = gt::px(16),
      column_labels.border.top.width    = 3,
      column_labels.border.top.color    = "black",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color    = "black",
      table.border.bottom.color         = "white",
      table.width                       = gt::pct(100),
      table.background.color            = "white"
    ) |>
    gt::cols_align(align = "center") |>
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides  = c("top", "bottom"),
          color  = "white",
          weight = gt::px(1)
        ),
        gt::cell_text(
          align="center"
        ),
        gt::cell_fill(color = "white", alpha = NULL)
      ),
      locations = gt::cells_body(
        columns = tidyselect::everything(),
        rows    = tidyselect::everything()
      )
    ) |>
    gt::tab_header( # title setup
      title = gt::html(title)
    ) |>
    gt::opt_align_table_header(align = "left")
} 

#' List outcome variables with their meanings
#' 
#' @export
list_outcomes <- function() {
  data.frame(
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
}

#' Pre-allocate table names
#' 
#' @export
tabs_struct <- function() {
  data.frame(
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
      "tables/", c(
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
}

#' Save tables
#' 
#' @param struct Some structure
#' 
#' @export
save_tables <- function(struct) {
  new_folder("tables")
  
  for (i in seq_len(nrow(struct))) {
    with(struct, {
      cat(paste0("Writing ",path[i], " ... \n"))
      gt::gtsave(targets::tar_read_raw(tar[i]), paste0(path[i],".html"))
      gt::gtsave(targets::tar_read_raw(tar[i]), paste0(path[i],".docx"))
    })
  }
}

#' Extract data for per protocol analysis
#' 
#' @param data Input data
#' 
#' @export
extract_per_protocol_data <- function(data) {
  subset(x = data, completed == 1)
}

#' Pre-format data for plotting
#' 
#' @param input Input data
#' 
#' @export
preformat_data <- function(input) {
  input |>
    mutate(
      `Treatment:` = treatment,
      Occasion = factor(
        x = occasion,
        levels = paste0("T", 0:2),
        labels = c("Pre-treatment", "Post-treatment", "Follow-up"),
        ordered = TRUE
      )
    )
}
