#' Extract response/remission table
#' 
#' @param data Input data
#' @param which `"response"` or `"remissions"`
#' 
#' @returns Tibble
#' 
#' @export
extract_responses <- function(data, which = "response") {
  data |>
    dplyr::group_by(treatment) |>
    dplyr::summarise(
      dplyr::across(
        .cols = tidyselect::ends_with( paste0("_", which) ),
        .fns = function(x) {
          paste0(
            sum(x, na.rm = TRUE),
            " (", rprint(100 * sum(x, na.rm = TRUE) / sum(!is.na(x)), 1), "%)"
          )
        }
      )
    ) |>
    tidyr::pivot_longer(cols = -treatment) |>
    tidyr::pivot_wider(
      names_from = treatment,
      values_from = value
    ) |>
    dplyr::mutate(
      name     = sub(paste0("_", which), "", name),
      occasion = substr(name, nchar(name) - 1, nchar(name)),
      scale    = gsub("_[^_]*$" , "", name)
    ) |>
    dplyr::select(-name) |>
    tidyr::pivot_wider(
      values_from = c(`HF-rTMS`, iTBS),
      names_from = occasion
    ) |>
    dplyr::relocate(iTBS_T1, .after = `HF-rTMS_T1`)
}