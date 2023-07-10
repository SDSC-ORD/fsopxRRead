#' Get supported px keywords
#'
#' The keywords are read from a file
#' Some px keywords are mostly for internal use
#' Others are defined in a complicated way
#' the list of supported keywords is meant to be extended when
#' the need for an extension arises
#'
#' @noRd
#'
#' @examples get_supported_px_keywords()
get_supported_px_keywords <- function(
  ) {
  keywords <- c(
    "CHARSET",
    "AXIS-VERSION",
    "CODEPAGE",
    "LANGUAGE",
    "LANGUAGES",
    "CREATION-DATE",
    "UPDATE-FREQUENCY",
    "DECIMALS",
    "SHOWDECIMALS",
    "MATRIX",
    "SUBJECT-CODE",
    "SUBJECT-AREA",
    "COPYRIGHT",
    "DESCRIPTION",
    "TITLE",
    "DESCRIPTIONDEFAULT",
    "CONTENTS",
    "UNITS",
    "STUB",
    "HEADING",
    "VALUES",
    "CODES",
    "MAP",
    "ELIMINATION",
    "LAST-UPDATED",
    "CONTACT",
    "REFPERIOD",
    "DATABASE",
    "SOURCE",
    "SURVEY",
    "LINK",
    "NOTE",
    "VALUENOTE",
    "DATASYMBOL1",
    "DATASYMBOL2",
    "DATASYMBOL3",
    "DATASYMBOL4",
    "DATASYMBOL5",
    "DATASYMBOL6",
    "PRECISION",
    "DATA"
  )
  print(keywords)
  return(keywords)
}
#' Get keyword from lines of a px file
#'
#' @param lines as raw string scanned from the px file
#'
#' @return keywords
#' @noRd
#'
#' @example get_keywords_from_lines('LANGUAGE="de"')
get_keywords_from_lines <- function(
  lines
  ) {
  line_parts <- strsplit(lines, split = "\\=")
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
get_languages_for_keyword <- function(
  lines,
  keyword
  ) {
  keyword_id <- paste0(keyword, "=")
  languages <- lines[startsWith(lines, keyword_id)] %>%
    stringr::str_replace(keyword_id, "") %>%
    stringr::str_extract_all("[[:lower:]]{2}") %>%
    unlist()
  return(languages)
}

#' Check whether px key is supported by the parser
#'
#' @param px_key px key to check
#' @param supported_keywords array of supported keywords
#'
#' @return bool value that indicated whether px key is supported
#'         by the parser
#' @noRd
#'
#' @example is_supported_px_key('VALUES("Jahr")', c('LANGUAGES', 'VALUES')
is_supported_px_key <- function(
  px_key,
  supported_keywords
  ) {
  keyword_start <- stringr::str_extract(px_key, "^([:upper:]|-|[:digit:])*")
  if (px_key %in% supported_keywords) {
    return(TRUE)
  } else if (keyword_start %in% supported_keywords) {
    return(TRUE)
  }
  return(FALSE)
}

#' Get px keys that relate to a translation
#'
#' these are all keys that belong to the locale or
#' that are not translateable
#'
#' @param px_keys all parsed px keys
#' @param px_default_keys px keys that belong to the default
#' @param dimension_dep_default_keys px default keys that relate to a dimension
#' @param locale locale to localize to
#'
#' @return px_locale_keys keys that belong to that locale
#' @noRd
get_px_locale_keys <- function(
  px_keys,
  px_default_keys,
  dimension_dep_default_keys,
  locale
  ) {
  px_locale_keys <- c()
  search_pattern_locale <- get_language_pattern_for_keys(locale)
  key_pattern_locale <- get_key_pattern_locale(locale)

  for (px_key in px_keys) {
    if (is_language_specific_key(px_key, search_pattern_locale)) {
      px_locale_keys <- c(px_locale_keys, px_key)
    } else {
      is_default_key <- px_key %in% px_default_keys
      is_dimension_dependent <- px_key %in% dimension_dep_default_keys
      if (is_default_key && !is_dimension_dependent) {
        translated_key <- paste0(px_key, key_pattern_locale)
        if (!translated_key %in% px_keys) {
          px_locale_keys <- c(px_locale_keys, px_key)
        }
      }
    }
  }
  return(px_locale_keys)
}

#' Get keyword from px key
#'
#' @param px_key px key
#'
#' @return keyword keyword of the px_key
#' @noRd
#'
#' @example get_keyword_from_px_key('VALUES("Jahr")')
get_keyword_from_px_key <- function(
  px_key
  ) {
  keyword <- stringr::str_extract(px_key, "^([:upper:]|-|[:digit:])*")
  return(keyword)
}

#' Get locale version of not dimension dependent px_key
#'
#' @param px_key px key
#' @param locale language code of a translation
#'
#' @return px_key_locale px_key for the translation
#' @noRd
#'
#' @example localize_px_key('TITLE', "en")
localize_px_key <- function(
  px_key,
  locale
  ) {
  if (is.null(locale)) {
    return(px_key)
  }
  key_pattern_locale <- get_key_pattern_locale(locale)
  px_key_locale <- paste0(px_key, key_pattern_locale)
  return(px_key_locale)
}
#' Get pattern to recognize translation records
#'
#' @param languages all languages that have translations
#'
#' @return regex language pattern to recognize these languages
#' @noRd
#'
#' @examples get_language_pattern_for_keys(c("de", "en"))
get_language_pattern_for_keys <- function(
  languages
  ) {
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
#' @examples is_language_specific_key('TITLE[fr]', "(de|en)")
is_language_specific_key <- function(
  key,
  language_pattern
  ) {
  key_contains_language <- grepl(pattern = language_pattern,
                                x = key)
  return(key_contains_language)
}

#' Get key pattern that is added to the key for translations
#'
#' @param locale language of the translations
#'
#' @return pattern with locale to add to the key of tranlations
#' @noRd
#'
#' @examples get_key_pattern_locale ("en")
get_key_pattern_locale <- function(
  locale
  ) {
  return(paste0("[", locale, "]"))
}
