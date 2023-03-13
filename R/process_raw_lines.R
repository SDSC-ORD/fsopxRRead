#' Parse the lines of a px file and get the px metadata records and data
#'
#' The lines in the px file are separated by ';'.
#' After scanning there are groups of lines that together
#' form a px record. The px record consists of a key that
#' is contained in the first line and usually separated
#' by an '=' sign from the content of the px record.
#'
#' @param scanned_lines all scanned line of the px file
#' @param supported_keywords array of supported px keywords
#'
#' @return named list of px_rows and data
#' @noRd
#'
#' @examples parse_px_lines(c('CHARSET=\"ANSI\"', "", 'AXIS-VERSION=\"2010\"', "", "DATA= 1 2 3", ""),
#'                          c('CHARSET', 'AXIS-VERSION', 'DATA'))
parse_px_lines <- function(scanned_lines, supported_keywords) {
  # make sure the last scanned line is an empty line
  scanned_lines <- append(scanned_lines, "")
  px_rows <- list()
  px_Line_group <- c()
  for (line in scanned_lines) {

    # the empty line separate the px key value blocks after scanning
    if (line != "") {
      px_Line_group <- append(px_Line_group, line)

    } else {
      if (length(px_Line_group) > 0) {

        # the key is contained in the first row of the line group
        px_key <- get_keywords_from_lines(px_Line_group[1])

        if (px_key == "DATA") {
          # process the actual data
          data <- vectorize_px_data(px_Line_group)

        } else if (grepl('[A-Z-]', px_key)) {

          # make sure the line is not empty
          if (is_supported_px_key(px_key, supported_keywords)) {
            px_rows <- append(px_rows, get_keyword_values_pair(px_Line_group))
          } else {
            print(paste("unsupported keyword detected", px_key))
          }
        }
        px_Line_group <- c()
      }
    }
  }
  return(list(metadata=px_rows, data=data))
}

#' Derive key value px row from raw px lines
#'
#' @importFrom dplyr %>%
#'
#' @param lines scanned lines as raw input for a px row
#'
#' @return named vector with keywords as names for values
#' @noRd
#'
#' @example get_keyword_values_pair('LANGUAGE="de"')
get_keyword_values_pair <- function (lines) {
  key_value_pair <- strsplit(lines[1], split='\\=') %>% unlist()
  keyword <- head(key_value_pair, 1)
  lines[1] <- tail(key_value_pair, 1)
  px_row <- paste(lines, collapse = "") %>%
    vectorize_raw_input() %>%
    list()
  names(px_row) = keyword
  return(px_row)
}

#' Vectorize scanned lines into value vectors
#'
#' @importFrom dplyr %>%
#'
#' @param lines scanned px_lines as strings
#'
#' @return scanned lines as vector of values
#' @noRd
#'
#' @example vectorize_raw_input(c('\"Betriebe\",\"Zimmer\"', '\"Ankünfte\",\"Zimmernächte\"'))
vectorize_raw_input <- function(lines) {
  lines <- strsplit(lines, split='\",\"') %>% unlist()
  values <- stringr::str_replace_all(lines, '\"', "")
  return(values)
}

#' Get keyword from lines of a px file
#'
#' @param lines as raw string scanned from the px file
#'
#' @return keywords
#' @noRd
#'
#' @example get_keywords_from_lines('LANGUAGE="de"')
get_keywords_from_lines <- function (lines) {
  line_parts <- strsplit(lines, split='\\=')
  keywords <- sapply(line_parts, getElement, 1)
  return(keywords)
}

#' Get languages for a keywords
#'
#' @param lines as raw string scanned from the px file
#' @param keyword LANGUAGES or LANGUAGE
#'
#' @return keyword
#' @noRd
#'
#' @example get_languages_for_keyword('LANGUAGES="de","en"', 'LANGUAGES')
get_languages_for_keyword <- function (lines, keyword) {
  keyword_id <- paste0(keyword, '=')
  languages <- lines[startsWith(lines, keyword_id)] %>%
    stringr::str_replace(keyword_id, '') %>%
    stringr::str_extract_all("[[:lower:]]{2}") %>%
    unlist()
  return(languages)
}

#' Process the raw px lines that start with the DATA keyword
#'
#' @importFrom dplyr %>%
#'
#' @param lines raw data lines
#'
#' @return vectorized data as numericals
#' @noRd
#'
#' @examples vectorize_px_data(c('DATA=1102 \"...\" \"...\"', '370 607 125', ' '))
vectorize_px_data <- function (lines) {
  lines[1] <- stringr::str_replace(lines[1], 'DATA=', '')
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
vectorize_data <- function(lines) {
  values <- trimws(lines) %>% strsplit(split=' ') %>% unlist()
  suppressWarnings(values <- as.numeric(values))
  return(values)
}

is_supported_px_key <- function(px_key, supported_keywords) {
  keyword_start <- stringr::str_extract(px_key, '^([:upper:]|-|[:digit:])*')
  if (px_key %in% supported_keywords) {
    return(TRUE)
  } else if (keyword_start %in% supported_keywords) {
    return(TRUE)
  }
  return(FALSE)
}
