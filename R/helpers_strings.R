#' Get the values from a group of px lines
#'
#' The lines consist of a px key keyword and content for
#' that keyword. The content can contain one or several values
#' These values are then returned as a list
#'
#' @importFrom dplyr %>%
#' @importFrom utils tail
#'
#' @param lines vector: scanned lines that belong to the same px key
#'
#' @return vector of values for this keyword
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' get_values_from_lines(
#'   'LANGUAGE="de"'
#' )
#' should return:
#' "de"
#' }
get_values_from_lines <- function(lines) {
  values <- lines %>%
    stringr::str_flatten() %>%
    stringr::str_split_1("=") %>%
    tail(1) %>%
    stringr::str_split_1('\",\"') %>%
    stringr::str_replace_all('\"', "") %>%
    stringr::str_remove("(,|;)$")
  return(values)
}
