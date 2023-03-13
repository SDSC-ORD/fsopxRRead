#' Localize metadata for a px cube by using metadata and string translations
#'
#' Combine metadata that have already been collected in the default language
#' with the string translations that have also been collected. The metadata
#' are copied into the metadata locale, Afterward the collected string
#' translations are used to translate all strings that are translateable
#'
#' @param metadata metadata in the default language
#' @param translations list of string translations per language
#' @param locale locale to translate the metadata to
#'
#' @return metadata translated into the locale
#' @noRd
#'
#' @examples localize_metadata(list("TITLE"="Scheidungen"))
#'                             list(en=list(Scheidungen="Divorces")),
#'                             "en")
localize_metadata <- function(metadata, translations, locale) {
  # the default language has already been prepared as metadata
  if (!locale %in% names(translations)) {
    return(metadata)
  }

  # collect string translations for the given locale
  translation_keys <- names(translations[[locale]])

  # copy metadata into the locale
  metadata_locale <- metadata

  # go through the metadata and translate all strings where translations exist
  for (key in names(metadata)) {
    row <- metadata[[key]]
    if (class(row) == "character" && length(row) == 1) {
      # simple strings are translated
      if (row %in% translation_keys) {
        metadata_locale[[key]] <- translations[[locale]][[row]]
      }
    } else if (key == "LANGUAGES") {
      # the language key just contains language codes
      next
    } else if (class(row) == "list") {
      # in lists all values have to be checked for translations independently
      # some lists have a second level of values
      for (i in 1:length(row)) {
        name <- names(row)[i]
        names(metadata_locale[[key]])[i] <- translate_string(
          name, locale, translation_keys, translations)
        name_locale <- names(metadata_locale[[key]])[i]
        for (j in 1:length(row[[name]])) {
          metadata_locale[[key]][[name_locale]][[j]] <- translate_string(
            row[[name]][[j]], locale, translation_keys, translations)
          if (!is.null(names(row[[name]][j]))) {
            names(metadata_locale[[key]][[name_locale]])[j] <- translate_string(
              names(row[[name]][j]), locale, translation_keys, translations)
          }
        }
      }
    }
  }
  # return localized metadata for further processing
  return(metadata_locale)
}
