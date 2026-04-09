#' Read data from CSV or Excel
#' 
#' @param path Path to where raw data lives
#' @param separator CSV separator. If `NULL`,
#'   Excel file assumed
#' 
#' @returns Tibble
#' 
#' @export
read_file <- function(path, separator = NULL) {
  
  not_all_na <- \(x) !all(is.na(x))
  
  if (is.null(separator)) {
    readxl::read_excel(path) |>
      dplyr::select_if(not_all_na)
  } else {
    read.csv(path, sep = separator) |>
      dplyr::select_if(not_all_na)
  }
}