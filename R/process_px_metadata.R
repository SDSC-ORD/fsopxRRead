#' Extract metadata from px rows for the default language
#'
#' Only px rows that don't contain translations are considered
#'
#' @param px_rows as px key value pairs
#' @param is_multilingual bool value to indicate the px cube has translations
#' @param language_pattern regex pattern to detect translations in px_keys
#'
#' @return metadata and also write it to a json file
#' @noRd
#'
#' @examples process_px_metadata(list('DESCRIPTION'='Heiraten seit 1876'),
#'                               TRUE, "[[](de|en)[]]")
process_px_metadata <- function(px_rows, is_multilingual, language_pattern) {
  px_keys <- names(px_rows)
  if (is_multilingual) {
    px_keys <- px_keys[!is_language_specific(px_keys, language_pattern)]
  }

  dimensions <- c(px_rows$STUB, px_rows$HEADING)

  # pattern to recognize dimensions being part of px keys
  dimension_pattern <- get_dimension_pattern(dimensions)

  metadata <- list()
  for (px_key in px_keys) {
    if (px_key %in% c('STUB', 'HEADING')) {
      # the dimensions are collected together with their values:
      # the values are stored in seperate px_rows
      metadata[[px_key]] = get_dimension_values(px_rows, px_key)

    } else if (belongs_to_dimensions(px_key, dimension_pattern) ) {
      # some px keys are composed of a px key word and dimensions and values
      key_parts <- get_key_parts_from_dimension_key(px_key)
      if (key_parts[1] == "VALUES") {
        next
      }
      if (length(key_parts) == 2) {
        metadata[[key_parts[1]]][[key_parts[2]]] <- px_rows[[px_key]]
      } else if (length(key_parts) == 3) {
        metadata[[key_parts[1]]][[key_parts[2]]][[key_parts[3]]]<- px_rows[[px_key]]
      }
    } else {
      # digest a simple metadata key
      metadata[[px_key]] <- px_rows[[px_key]]
    }
  }
  return(metadata)
}
