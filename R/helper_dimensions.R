#' Checks whether a px key depends on a dimension
#'
#' @param key px key
#' @param dimension_patterh pattern to recognize dimensions
#'
#' @return boolean value that indicates whether the px key relates to a dimension
#' @noRd
#'
#' @examples belongs_to_dimensions('PRECISION("Indikator","Scheidungen")', "(Jahr|Indikator)")
belongs_to_dimensions <- function(key, dimension_pattern) {
  keyword_start <- stringr::str_extract(key, '^([:upper:]|-|[:digit:])*')
  if (keyword_start %in% c("VALUES", "CODES", "PRECISION", "ELEMINATION")) {
    return(TRUE)
  }
  if (grepl(pattern=dimension_pattern, x=key)) {
    return(TRUE)
  }
  return(FALSE)
}

#' Split px key that includes dimensions into parts
#'
#' @param px_key such as "Jahr"
#'
#' @return key in parts with dimensions
#' @noRd
#'
#' @examples get_key_parts_from_dimension_key('PRECISION("Indikator","Scheidungen")')
get_key_parts_from_dimension_key <- function(px_key) {
  px_key <- stringr::str_split(px_key, '[\\(,]') %>%
    unlist() %>%
    stringr::str_replace('\\)', '')
  px_key <- gsub("\\\"","", px_key)
  return(px_key)
}

#' Get all dimsnions with translations for a px file
#'
#' @param px_rows px records
#' @param default_language default language of the px file
#' @param languages languages in which translations exist
#'
#' @return dimensions in all languages as an array
#' @noRd
#'
#' @examples get_dimension_with_translations(list('STUB'=c('Jahr'), "de", c("de", "en"))
get_dimension_with_translations <- function(px_rows, default_language, languages) {
  dimensions <- c(px_rows$STUB, px_rows$HEADING)
  for (lang in languages) {
    if (lang != default_language) {
      stub_locale <- paste0("STUB", '[', lang, ']')
      heading_locale <- paste0("HEADING", '[', lang, ']')
      dimensions <- c(dimensions, c(px_rows[[stub_locale]],
                                    px_rows[[heading_locale]]))
    }
  }
  return(dimensions)
}

#' Transform px dimension into a px dimension values key
#'
#' @param dimensions array of dimensions
#'
#' @return regex pattern to recognize dimensions in px keys
#' @noRd
#'
#' @examples get_dimension_pattern(c("Jahr", "Indikator", "Year", "Indicator"))
get_dimension_pattern <- function(dimensions) {
  grepl('\\(("Jahr"|"More and More")\\)', 'VALUES("Jahr")')
  dimension_pattern <- paste0('(', paste(dimensions, collapse = '|'), ')')
  return(dimension_pattern)
}

#' Transform px dimension into a px dimension values key
#'
#' @param dimension name such as "Jahr"
#'
#' @return VALUES px key such as VALUES("Jahr")
#' @noRd
#'
#' @examples get_dimension_key("Jahr")
get_dimension_key <- function(dimension_name) {
  dimension_key <- paste0('VALUES("', dimension_name, '")')
  return(dimension_key)
}

#' Get dimension values for a px_variable such as STUB or HEADING
#'
#' @importFrom dplyr %>%
#'
#' @param px_rows as key value pairs of a px file
#' @param dimension_name dimension name such as "Jahr"
#'
#' @return values for the px dimension key
#' @noRd
#'
#' @examples get_dimension_values(list('VALUES("Jahr")'= c('1876','1877','1878'), 'STUB'='Jahr'), 'STUB')
get_dimension_values <- function(px_rows, px_variable) {
  dimension_names <- px_rows[[px_variable]]
  dimensions <- list()
  for (dimension_name in dimension_names) {
    values_key <- get_dimension_key(dimension_name)
    dimension_values <- px_rows[[values_key]]
    dimension <- list()
    dimension[[dimension_name]] <- dimension_values
    dimensions <- append(dimensions, dimension)
  }
  return(dimensions)
}
