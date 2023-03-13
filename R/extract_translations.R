#' Extract translations from px rows
#'
#' For each language string translations are extracted from the px file
#' The translations are returned as lists of string translations.
#' The process of translating is done by going through px_rows
#' The default row comes always first: there translatable strings are
#' extracted and memorized. The next rows marked as translations will
#' then bring the translations.
#'
#' @param px_rows as px key value pairs
#' @param languages languages for that translations exist
#' @param default_language default language of the px file
#' @param language_pattern regex pattern to detect translations in px_keys
#'
#' @return lists of string translations per language
#' @noRd
#'
#' @examples extract_translations(list('DESCRIPTION'='Heiraten seit 1876', 'DESCRIPTION[en]'='Marriages since 1876'),
#' c("de", "en"), "de", "[[](de|en)[]]")
extract_translations <- function(px_rows, languages, default_language, language_pattern) {
  translations <- list()
  # prepare translations as empty lists
  for (lang in languages) {
    if (lang != default_language) {
      translations[[lang]] = c(list())
    }
  }
  # get all dimensions and their translations as a list
  dimensions <- get_dimension_with_translations(
    px_rows = px_rows,
    default_language = default_language,
    languages = languages)
  # from the dimension a pattern is derived that helps to recognize the in the px keys
  dimension_pattern <- get_dimension_pattern(dimensions)
  # go through metadata keys in the default language
  # (these are the rows where the language is not specified)
  for (i in 1:length(px_rows)) {
    key <- names(px_rows)[i]
    # check whether the key is a default language key
    key_in_default_language <- !is_language_specific(key, language_pattern)
    # also check whether the key includes a dimension
    key_without_dimensions <- !belongs_to_dimensions(key, dimension_pattern)
    # STUB and HEADING are the px keys that include the dimensions
    if (startsWith(key, "STUB") || startsWith(key, "HEADING")) {
      if (key_in_default_language) {
        default_key <- key
        default_values <- px_rows[[default_key]] }
      else {
        values <- px_rows[[key]]
        for (i in 1:length(values)) {
          # translate the dimensions with string translation
          translations[[lang]][[default_values[i]]] <- values[i]
        }
      }
    } else if (key_in_default_language && key_without_dimensions) {
      # keys in the default language are just stored so that they can
      # be used as tranlation targets in the rows that follow
      default_key <- key
      default_value <- px_rows[[default_key]]
    } else if (key_without_dimensions) {
      # if the key has not dimension part, then it is just translated
      # the language is also marked in the px key
      value <- px_rows[[key]]
      lang <- get_translation_language_from_key(key)
      if (default_value != value) {
        translations[[lang]][[default_value]] <- value
      }
    } else if (startsWith(key, 'VALUES')) {
      if (key_in_default_language) {
        default_key <- key
        default_values <- px_rows[[default_key]] %>% unlist()
      } else {
        values <- px_rows[[key]] %>% unlist()
        if (length(setdiff(default_values, values)) != 0) {
          lang <- stringr::str_extract(key, "[[:lower:]]{2}") %>% unlist()
          for (i in 1:length(default_values)) {
            to_translate <- default_values[i]
            translation <- values[i]
            translations[[lang]][[to_translate]] <- translation
          }
        }
      }
    }
  }
  return(translations)
}
