#' Get dimension of a px key
#'
#' @param key px key
#' @param dimension_names array of dimension names
#'
#' @return list: a boolean value that indicates whether the px key
#'         relates to a dimension, the dimension that it relates to
#' @noRd
#'
#' @examples get_dimension('PRECISION("Indikator","Scheidungen")',
#'                         c("Jahr", "Indikator"))
dimension_detector <- function(
  key,
  dimension_names
  ) {
  for (dimension_name in dimension_names) {
    pattern <- paste0('\"', dimension_name, '\"')
    if (stringr::str_detect(key, pattern)) {
      return(list(belongs_to_dimension = TRUE, dimension_name = dimension_name))
    }
  }
  return(list(belongs_to_dimension = FALSE))
}

#' Get locale key
#'
#' @param px_key px key
#' @param locale language code
#'
#' @return px_key for the translation to the locale
#' @noRd
#'
#' @examples get_locale_key("TITLE",
#'                          "en")
get_locale_key <- function(
  px_key,
  locale
  ) {
  pattern_locale <- paste0("[", locale, "]")
  return(paste0(px_key, pattern_locale))
}

#' Transform px dimension name and locale into a px values key
#'
#' @param dimension_name name such as "Jahr"
#' @param locale locale language of a translation
#'
#' @return VALUES px key such as VALUES("Jahr")
#' @noRd
#'
#' @examples get_dimension_values_key("Year", "en")
get_dimension_values_key <- function(
  dimension_name,
  locale = NULL
  ) {
  values_key <- "VALUES"
  if (!is.null(locale)) {
    values_key <- get_locale_key(values_key, locale)
  }
  dimension_key <- paste0(values_key, '("', dimension_name, '")')
  return(dimension_key)
}

#' Get dimension values for a px_variable such as STUB or HEADING
#'
#' @importFrom dplyr %>%
#'
#' @param px_rows as key value pairs of a px file
#' @param px_key px_key such as STUB or HEADING
#'
#' @return values for the px dimension key
#' @noRd
#'
#' @examples get_dimension_values(list('VALUES("Jahr")'=
#'           c('1876','1877','1878'), 'STUB'='Jahr'), 'STUB')
get_dimension_values <- function(
  px_rows,
  px_key,
  locale = NULL
  ) {
  if (!is.null(locale)) {
    px_key <- get_locale_key(px_key, locale)
  }
  dimension_names <- px_rows[[px_key]]
  dimensions <- list()
  for (dimension_name in dimension_names) {
    values_key <- get_dimension_values_key(dimension_name, locale)
    dimension_values <- px_rows[[values_key]]
    dimension <- list()
    dimension[[dimension_name]] <- dimension_values
    dimensions <- append(dimensions, dimension)
  }
  return(dimensions)
}

#' Get dimensions metadata
#'
#' @importFrom dplyr %>%
#'
#' @param px_key as key value pairs of a px file
#' @param px_value dimension name such as "Jahr"
#' @param dimension_as_list dimension name such as "Jahr"
#'
#' @return dimension key as list of keyword with variable and value
#'         example: list(PRECISION = list("Indikator" = list("Indikator 2" = 1)
#' @noRd
#'
#' @examples get_dimension_metadata('PRECISION("Indikator","Indikator 2")',
#'                                  1,
#'                                  list("Indikator" = c("Indikator 1",
#'                                       "Indikator 2", "Indikator 3")))
get_dimension_dependent_struct <- function(
  px_key,
  px_value,
  dimension_name,
  dimension_values
  ) {
  keyword <- get_keyword_from_px_key(px_key)
  value <- list()
  for (dimension_value in dimension_values) {
    pattern <- paste0('\"', dimension_value, '\"')
    if (stringr::str_detect(px_key, pattern)) {
      value[[dimension_name]][[dimension_value]] <- px_value
      output <- list(keyword = keyword,
                     is_dimension_value = TRUE,
                     dimension_value = dimension_value)
      return(output)
    }
  }
  value[[dimension_name]] <- px_value
  output <- list(keyword = keyword,
                 is_dimension_value = FALSE)
  return(output)
}
