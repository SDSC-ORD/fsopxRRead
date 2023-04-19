#' Derive key value pairs from px raw lines.
#'
#' The px_key is split off as a name.
#' Values are seprated into lists.
#'
#' @importFrom dplyr %>%
#'
#' @param lines scanned lines as raw input for a px row
#'
#' @return named vector with keywords as names for values
#' @noRd
#'
#' @example get_keyword_values_pair('LANGUAGE="de"')
get_keyword_values_pair <- function(
  lines
  ) {
  line_parts <- strsplit(lines[1], split = "\\=") %>% unlist()

  # get keyword from the first line: before the equal sign
  keyword <- head(line_parts, 1)

  # adjust the lines, after the keyword has been removed
  line_end <- tail(line_parts, 1)
  if (line_end != line_parts[1]) {
    lines[1] <- line_end
  } else {
    lines <- lines[-1]
  }

  # combine the lines into one line
  lines <- paste(lines, collapse = "")

  # split the line into values
  px_row <- clean_keyword_strings(lines) %>%
    list()

  # the keyword is used as name of the content
  names(px_row) <- keyword
  return(px_row)
}

#' Vectorize scanned lines into value vectors:
#' 
#' "value1","value2","value3" is turned into a
#' vector c("value1", "value2", "value3")
#'
#' @importFrom dplyr %>%
#'
#' @param lines scanned px_lines as strings
#'
#' @return scanned lines as vector of values
#' @noRd
#'
#' @example clean_keyword_strings(c('\"Betriebe\",\"Zimmer\"', '\"Ankünfte\",\"Zimmernächte\"'))
clean_keyword_strings <- function(
  lines
  ) {
  lines <- strsplit(lines, split = '\",\"') %>% unlist()
  values <- stringr::str_replace_all(lines, '\"', "")
  return(values)
}
