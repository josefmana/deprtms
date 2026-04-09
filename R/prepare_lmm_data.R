#' Prepare data for multilevel models
#' 
#' @param d0 Input data
#' 
#' @returns data.frame with contrasts
#' 
#' @export
prepare_lmm_data <- function(d0) {
  df <- d0 |>
    dplyr::mutate(occasion = factor(x = occasion, ordered = FALSE))
  
  contrasts(df$occasion)  <- bayestestR::contr.equalprior(n = length(levels(df$occasion)))
  contrasts(df$treatment) <- bayestestR::contr.equalprior(n = length(levels(df$treatment)))
  
  df 
}
