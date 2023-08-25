#' Process the raw px lines that start with the DATA keyword
#'
#' @importFrom dplyr %>%
#'
#' @param data_lines raw data lines
#'
#' @return data_numeric vector: data as numeric values
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' vectorize_data(
#'   c('DATA=1102 \"...\" \"...\"', '370 607 125')
#' )
#' should return:
#' c(1102, NA, NA, 370, 607, 125)
#' }
vectorize_data <- function(data_lines) {
  data_lines %>%
    trimws() %>%
    stringr::str_remove("DATA=") %>%
    stringr::str_remove(";") %>%
    strsplit(split = " ") %>%
    unlist() %>%
    stringr::str_replace("^[:punct:]*$", "NA") -> data_as_strings
  suppressWarnings(data_numeric <- as.numeric(data_as_strings))
  return(data_numeric)
}
