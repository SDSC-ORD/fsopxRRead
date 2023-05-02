#' Process the raw px lines that start with the DATA keyword
#'
#' @importFrom dplyr %>%
#'
#' @param lines raw data lines
#'
#' @return vectorized data as numericals
#' @noRd
#'
#' @examples vectorize_px_data(
#' c('DATA=1102 \"...\" \"...\"', '370 607 125', ' '))
vectorize_px_data <- function(
  lines
  ) {
  lines[1] <- stringr::str_replace(lines[1], "DATA=", "")
  lines <- lines[lines != ""]
  data <- vectorize_data(lines)
  return(data)
}

#' Turn raw px data lines into a numeric vector
#'
#' @importFrom dplyr %>%
#'
#' @param rows scanned rows of a px file
#'
#' @return rows as numeric
#' @noRd
#'
#' @examples vectorize_data(c('1102 \"...\" \"...\"', '370 607 125'))
vectorize_data <- function(
  lines
  ) {
  values <- trimws(lines) %>% strsplit(split = " ") %>% unlist()
  suppressWarnings(values <- as.numeric(values))
  return(values)
}
