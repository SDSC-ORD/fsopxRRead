#' Get keyword pattern for the metadata keywords that are
#' currently supported
#'
#' When a locale is specified the patterns targets the localized
#' versions of the px metadata keys
#'
#' The function is very simple, but it still makes sense to have
#' a function for that, so that only that function needs to get
#' adapted in case other keywords should be picked up in the
#' future.
#'
#' @param locale default is NULL
#'
#' @return keyword_pattern string: a regex pattern to detect supported
#'                                 px_keys
#'                                 (pattern for the keywords that are
#'                                 parsed after the initial check of the
#'                                 file has finished)
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' get_keywords_from_lines(
#'   'LANGUAGE="de"'
#' )
#' should return:
#' LANGUAGE
#' }
get_localized_keyword_pattern <- function(locale = NULL) {
  if (is.null(locale)) {
    return("^(DATA=|STUB=|HEADING=|TITLE=|DESCRIPTION=|UNITS=|VALUES[(]{1})")
  }
  language_pattern <- paste0("[:punct:]", locale, "[:punct:]")
  keyword_pattern <- paste0(
    "^(DATA=|STUB", language_pattern, "|",
    "HEADING", language_pattern, "|",
    "TITLE", language_pattern, "|",
    "DESCRIPTION", language_pattern, "|",
    "UNITS", language_pattern, "|",
    "VALUES", language_pattern, ")"
  )
  return(keyword_pattern)
}
