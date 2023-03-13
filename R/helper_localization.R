#' Translate string: if a string translation exist it is returned
#'
#' if no translation exist the string is returned as is
#'
#' @param to_translate string to translate
#' @param locale locale to translate to
#' @param translation_keys all strings that have translations
#' @param translations list of all string translations
#'
#' @return translated string
#' @noRd
#'
#' @examples translate_string("Jahr", "en", c("Jahr"), list("en"=list("Jahr"="Year")))
translate_string <- function(to_translate, locale, translation_keys, translations) {
  if (to_translate %in% translation_keys) {
    translated_string <- translations[[locale]][[to_translate]]
    return(translated_string)
  } else {
    return(to_translate)
  }
}

#' get language of a px translation record from it's key
#'
#' @param key a px key of a translation record
#'
#' @return language
#' @noRd
#'
#' @examples get_translation_language_from_key('TITLE[en]')
get_translation_language_from_key <- function(key) {
  language <- stringr::str_extract(key, "[[:lower:]]{2}") %>% unlist()
  return(language)
}

#' Get pattern to recognize translation records
#'
#' @param languages all languages that have translations
#'
#' @return regex language pattern to recognize these languages
#' @noRd
#'
#' @examples get_language_pattern(c("de", "en"))
get_language_pattern <- function(languages) {
  language_pattern <- paste0("[[](", paste(languages, collapse = "|"), ")[]]")
  return(language_pattern)
}

#' Check px key whether it is a basic key or belongs to a translation
#'
#' @param key one px key
#' @param language_pattern pattern to recognize px keys for translations
#'
#' @return bool that indicates whether the key is a basic key
#' @noRd
#'
#' @examples is_language_specific('TITLE[fr]', "(de|en)")
is_language_specific <- function(key, language_pattern) {
  key_contains_language <- grepl(pattern=language_pattern, x=key)
  return(key_contains_language)
}
