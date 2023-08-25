#' Get locale key
#'
#' @param px_key px key
#' @param locale language code
#'
#' @return px_key for the translation to the locale
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' get_locale_key("TITLE", "en")
#' # should return: "TITLE[en]"
#' }
get_locale_key <- function(
  px_key,
  locale
  ) {
  pattern_locale <- paste0("[", locale, "]")
  return(paste0(px_key, pattern_locale))
}

#' Transform px dimension name and locale into a px values key
#'
#' @param dimension_name string: such as "Year"
#' @param locale string: locale for a translation
#'
#' @return dimension_values_key string: VALUES px key such as VALUES("Year")
#'
#' @noRd
#'
#' @examples get_dimension_values_key("Year", "en")
#' \dontrun{
#' get_dimension_values_key("Year", "en")
#' should return: "VALUES[en]("Year")"
#' }
get_dimension_values_key <- function(
  dimension_name,
  locale = NULL
  ) {
  values_key <- "VALUES"
  if (!is.null(locale)) {
    values_key <- get_locale_key(values_key, locale)
  }
  dimension_values_key <- paste0(values_key, '("', dimension_name, '")')
  return(dimension_values_key)
}

#' Build dimensions
#'
#' Build a list of all dimensions with their values
#'
#' @param dimension_names vector: dimension variables as it occurs in STUB or
#'                       HEADING such as "Year", "Indicator"
#' @param lines vector: lines of a px file
#' @param idx tibble: index of the lines with keyword, keyword_line,
#'                     px_row_start and px_row_end as columns#'
#' @param locale string: locale as a language code to localize to
#'
#' @return dimensions_with_values list: a named list of dimensions and their
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' get_dimension_values_key(
#'   c("Year", "Indikator"), lines, idx, "en"
#' )
#' should return: the dimensions with a list of their values
#' }
build_dimensions <- function(dimension_names, lines, idx, locale = NULL) {
  dimensions_with_values <- list()
  for (dimension_name in dimension_names) {
    px_key <- get_dimension_values_key(dimension_name, locale)
    dimension_lines <- get_dimension_lines(lines, idx, dimension_name, px_key)
    dimension_values <- get_values_from_lines(dimension_lines)
    dimensions_with_values[[dimension_name]] <- dimension_values
  }
  return(dimensions_with_values)
}


#' Get dimension lines
#'
#' get lines of the VALUES for a dimension such as all lines that belong to
#' VALUES[en]("Year") for example
#'
#' @param lines vector: lines of a px file
#' @param idx tibble: index of the lines with keyword, keyword_line,
#'                     px_row_start and px_row_end as columns
#'
#' @param dimension_name string: dimension variable as it occurs in STUB or
#'                       HEADING such as "Year"
#' @param px_key string: the corresponding VALUES key, such as VALUES("Year")
#'                       or "VALUES[en]("Year")
#'
#' @return lines_for_values vector: all the lines that belong to a
#'                                  dimension variable
#' @noRd
#'
#' @examples
#' \dontrun{
#' get_dimension_lines(
#'   lines, idx, "Year", 'VALUES[en]("Year")'
#' )
#'.should return: c("1991", "1992", "1993") or similar: the list
#' of values for the dimension
#' }
get_dimension_lines <- function(lines, idx, dimension_name, px_key) {
  filtered_idx <- idx %>%
    dplyr::filter(stringr::str_detect(keyword, "VALUES")) %>%
    dplyr::filter(startsWith(keyword_line, px_key)) %>%
    dplyr::select(px_row_start, px_row_end)
  lines_for_values <- lines[filtered_idx$px_row_start:filtered_idx$px_row_end]
  return(lines_for_values)
}
