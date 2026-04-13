#' Describe demographics
#' 
#' @param data Input data
#' @param tit Table title
#' 
#' @returns gt table
#' 
#' @export
decribe_demographics <- function(data, tit = "<b>Table 1</b>") {
  
  # Variables order:
  ord <- c(
    "Sex",
    "Age (years)",
    "Education (years)",
    "Medication",
    "Completed",
    "Termination phase",
    "Device energy (% of maximum capacity)"
  )
  
  # Variables to be described:
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
  
  tab <- with(vars, list(
    # Continuous:
    data |>
      dplyr::group_by(treatment) |>
      dplyr::summarise(
        dplyr::across(tidyselect::all_of(var[type == "continuous"]), cenvar)
      ) |>
      tidyr::pivot_longer(
        cols = -treatment,
        names_to = "Variable"
      ) |>
      tidyr::pivot_wider(
        names_from = "treatment",
        values_from = "value"
      ) |>
      dplyr::mutate(
        Variable = sapply(seq_len(dplyr::n()), \(i) lab[var == Variable[i]]),
        Level = NA,
        .before = 2
      ),
    
    # Categorical:
    purrr::map_dfr(var[type == "categorical"], function(x) {
      freqperc(x = data[[x]], y = data$treatment, dec = 1) |>
        dplyr::mutate(
          Variable = lab[var == x],
          .before = 1
        )
    }),
    
    # Medication:
    data |>
      dplyr::group_by(treatment) |>
      dplyr::summarise(
        dplyr::across(
          .cols = tidyselect::all_of(var[type == "medication"]),
          .fns = \(x) paste0(sum(x), " (", rprint(100 * mean(x, na.rm = TRUE), 1), "%)")
        )
      ) |>
      tidyr::pivot_longer(
        cols = -treatment,
        names_to = "Level"
      ) |>
      tidyr::pivot_wider(
        names_from = "treatment",
        values_from = "value"
      ) |>
      dplyr::mutate(
        Variable = "Medication",
        .before = 1
      )
  )) |>
    purrr::reduce(dplyr::full_join)
  
  # Statistical tests:
  stats <- with(vars, sapply(
    var, function(y) {
      if (type[var == y] == "continuous") {
        res <- t.test(
          formula    = as.formula(paste0(y, " ~ treatment")),
          data       = data,
          altrnative = "two.sided",
          var.equal  = FALSE
        ) 
      } else {
        res <- chisq.test(table(data[[y]], data$treatment))
      }
      
      txt <- ifelse( 
        type[var == y] == "continuous",
        paste0("t(" , rprint(res$parameter, 2), ") = ", rprint(res$statistic, 2),", p ", pprint(res$p.value, 3, TRUE)),
        paste0("χ2(", rprint(res$parameter, 0), ") = ", rprint(res$statistic, 2),", p ", pprint(res$p.value, 3, TRUE))
      )
      
      # Select rows for categorical variables:
      if (type[var == y] == "categorical") {
        lvl <- unlist(subset(tab, Variable == lab[var == y])[1, "Level"], use.names = FALSE)
      } else {
        lvl <- NA
      }
      
      # Add variable names
      if (type[var == y] != "medication") {
        c(Variable = lab[var == y], Level = lvl, Stats = txt)
      }
      else {
        c(Variable = "Medication", Level = lab[var == y], Stats = txt)
      }
    }
  )) |>
    t() |>
    as.data.frame()
  
  dplyr::left_join(
    x  = tab,
    y  = stats,
    by = c("Variable", "Level")
  ) |>
    dplyr::mutate_all(\(x) dplyr::if_else(is.na(x), "", x)) |>
    dplyr::mutate(Variable = factor(Variable, levels = ord, ordered = TRUE)) |>
    dplyr::arrange(Variable) |>
    gt_apa(grp = "Variable", nms = "Level", title = tit)
}

#' Prepare descriptive table of the outcomes
#' 
#' @param data Input data
#' @param include Numeric
#' @param decs Decimals
#' @param tit Table title
#' 
#' @export
describe_outcomes <- function(data, include = 1, decs = 2, tit = " ") {
  
  tab0 <- data |>
    dplyr::filter(completed %in% include) |>
    dplyr::mutate(
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
        ordered = TRUE
      ),
      subscale = factor(
        x = dplyr::if_else(is.na(subscale), "tot", subscale),
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
        ordered = TRUE
      )
    ) |>
    dplyr::group_by(treatment, scale, subscale, occasion) |>
    dplyr::summarise(
      N = sum(!is.na(score)),
      Score = cenvar(score, dec = decs),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      values_from = c("N","Score"),
      names_from = c("treatment","occasion")
    ) |>
    dplyr::relocate(`Score_HF-rTMS_T0`, .after = `N_HF-rTMS_T0`) |>
    dplyr::relocate(`Score_HF-rTMS_T1`, .after = `N_HF-rTMS_T1`) |>
    dplyr::relocate(`Score_HF-rTMS_T2`, .after = `N_HF-rTMS_T2`) |>
    dplyr::relocate( Score_iTBS_T0    , .after =  N_iTBS_T0    ) |>
    dplyr::relocate( Score_iTBS_T1    , .after =  N_iTBS_T1    )
  
  if (isTRUE(include == 1)) {
    N <- c(HF = unique(tab0$`N_HF-rTMS_T0`), iTBS = unique(tab0$N_iTBS_T0)) # number of subjects in each group
    tab0 <- tab0 |>
      dplyr::select(-tidyselect::starts_with("N"))
    note <- sapply(N, \(n) paste0("N = ", n))
  }
  
  tab <- tab0 |>
    gt_apa(grp = "scale", nms = "subscale", title = tit) |>
    gt::cols_align(columns = 1:2 , align = "left") |>
    gt::cols_align(columns = -(1:2), align = "center")
  
  # Analysis-type (PP vs ITT) specific formatting:
  if (isTRUE(include == 1)) {
    tab |>
      gt::tab_spanner(
        columns = tidyselect::contains("HF-rTMS"),
        label = "HF-rTMS",
        gather = FALSE
      ) |>
      gt::tab_spanner(
        columns = tidyselect::contains("iTBS"),
        label = "iTBS",
        gather = FALSE
      ) |>
      gt::cols_label(
        tidyselect::ends_with("T0") ~ "Pre-intervention",
        tidyselect::ends_with("T1") ~ "Post-intervention",
        tidyselect::ends_with("T2") ~ "Follow-up"
      ) |>
      gt::tab_footnote(
        locations = gt::cells_column_spanners("HF-rTMS"),
        footnote = note["HF"]
      ) |>
      gt::tab_footnote(
        locations = gt::cells_column_spanners("iTBS"),
        footnote = note["iTBS"]
      )
  } else {
    tab |>
      gt::tab_spanner(
        columns = tidyselect::ends_with("T0"),
        label = "Pre-intervention",
        gather = FALSE
      ) |>
      gt::tab_spanner(
        columns = tidyselect::ends_with("T1"),
        label = "Post-intervention",
        gather = FALSE
      ) |>
      gt::tab_spanner(
        columns = tidyselect::ends_with("T2"),
        label = "Follow-up",
        gather = FALSE
      ) |>
      gt::tab_spanner(
        columns = tidyselect::contains("HF"),
        label = "HF-rTMS",
        gather = FALSE
      ) |>
      gt::tab_spanner(
        columns = tidyselect::contains("iTBS"),
        label = "iTBS",
        gather = FALSE
      ) |>
      gt::cols_label(
        tidyselect::starts_with("N") ~ "N",
        tidyselect::starts_with("Score") ~ "Score"
      )
  }
}

#' Table response and remission rates
#' 
#' @param data Input data
#' @param labels Labels
#' @param tit Table title
#' 
#' @returns gt table
#' 
#' @export
describe_responses <- function(data, labels, tit = " ") {
  
  data <- data |>
    dplyr::filter(completed == 1)
  
  perc <- sapply(levels(data$treatment), function(i) {
    d0 <- subset(data, treatment == i)[, names(data)[grepl("_perc", names(data))]]
    apply(d0, 2, cenvar)
  }) |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    dplyr::mutate(
      rowname = sub("_perc_decl", "", rowname),
      scale   = sub("_[^_]*$", "", rowname),
      event   = sub(".*\\_", "", rowname),
      measure = "response"
    ) |>
    dplyr::select(measure, scale, event, `HF-rTMS`, iTBS) |>
    tidyr::pivot_wider(
      values_from = c(`HF-rTMS`, iTBS),
      names_from = event,
      names_prefix = "perc"
    )
  
  tab <- purrr::map_dfr(c("response","remission"), function(i) {
    extract_responses(data, i) |>
      dplyr::mutate(measure = i, .before = 1)
  })
  
  stats <- lapply(rlang::set_names(unique(tab$measure)), function(i) {
    lapply(rlang::set_names(unique(tab$scale)), function(y) {
      lapply(rlang::set_names(c("T1", "T2")), function(t) {
        out <- factor(data[[paste0(y, "_", t, "_", i)]], levels = c(0, 1))
        #chisq.test(table(data[[paste0(y, "_", t,"_", i)]], data$treatment))
        fisher.test(table(out, data$treatment))
      })
    })
  })
  
  dplyr::full_join(perc, tab, by = c("measure", "scale")) |>
    dplyr::mutate(
      stats_T1 = unlist(
        sapply(seq_len(dplyr::n()), function(i) with(
          stats[[measure[i]]][[scale[i]]]$T1,
          paste0(
            #"χ2(", rprint(parameter, 0),") = ",
            #rprint(statistic, 2),
            "OR = ", rprint(estimate, 2),
            " [", paste(rprint(conf.int, 2), collapse = ", "), "]",
            ", p ", pprint(p.value, 3, TRUE)
          )
        ))
      ),
      stats_T2 = unlist(
        sapply(seq_len(dplyr::n()), function(i) with(
          stats[[measure[i]]][[scale[i]]]$T2,
          paste0(
            #"χ2(", rprint(parameter, 0),") = ",
            #rprint(statistic, 2),
            "OR = ", rprint(estimate, 2),
            " [", paste(rprint(conf.int, 2), collapse = ", "), "]",
            ", p ", pprint(p.value, 3, TRUE)
          )
        ))
      ),
      measure = paste0(sub("r", "R", measure), " Rate"),
      scale = sub(
        pattern = " (Total score)", # messy way to print HAMD and HAMA better
        replacement = "",
        x = unlist(sapply(seq_len(dplyr::n()), function(i) {
          labels$sub_abre[labels$var == scale[i]]
        }), use.names = FALSE),
        fixed = TRUE
      )
    ) |>
    dplyr::mutate_all(\(x) dplyr::if_else(is.na(x), "-", x)) |>
    dplyr::relocate(stats_T1, .after = iTBS_T1 ) |>
    dplyr::relocate(iTBS_percT1, .after = `HF-rTMS_percT1`) |>
    
    gt_apa(grp = "measure", nms = "scale", title = tit) |>
    gt::tab_spanner(
      columns = tidyselect::ends_with("T1"),
      label = "T1",
      gather = FALSE
    ) |>
    gt::tab_spanner(
      columns = tidyselect::ends_with("T2"),
      label = "T2",
      gather = FALSE
    ) |>
    gt::tab_spanner(
      columns = tidyselect::contains("perc"),
      label = "Percentage change",
      gather = FALSE
    ) |>
    gt::cols_label(
      tidyselect::starts_with("HF-rTMS") ~ "HF-rTMS",
      tidyselect::starts_with("iTBS") ~ "iTBS",
      tidyselect::starts_with("stats") ~ "Fisher's test"
    ) |>
    gt::tab_source_note(
      source_note = "OR: odds ratio and its 95% confidence interval; p: unadjusted p-value;
      none of the presented comparisons were statistically significant after adjusting for
      5% False Discovery Rate across all comaprisons presented in the table."
    )
}
