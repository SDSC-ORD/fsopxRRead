#' Extract metadata from px rows for the default language
#'
#' Only px rows that don't contain translations are considered
#'
#' @param px_rows as px key value pairs
#' @param px_keys default px_keys
#'
#' @return metadata and also write it to a json file
#' @noRd
#'
#' @examples process_px_metadata(list('DESCRIPTION'='Heiraten seit 1876'), TRUE, "[[](de|en)[]]")
process_px_metadata <- function(
  px_rows,
  px_keys,
  locale = NULL
  ) {
  # metadata is prepared as empty list
  metadata <- list()

  # dimension names are needed to correctly detect px_keys
  # as dimensions are often worked into px keys such as
  # PRECISION("Jahr")="some value" where the px_key is
  # PRECISION in the json output, but the original px key
  # is PRECISION("Jahr")
  dim_key_stub <- localize_px_key("STUB", locale)
  dim_key_heading <- localize_px_key("HEADING", locale)
  dimension_names <- c(px_rows[[dim_key_stub]], px_rows[[dim_key_heading]])

  # also dimension values lists are needed for the same reason
  # and will be filled during the looping
  dimensions_with_values <- list()
  dimension_dependent_keys <- list()
  for (px_key in px_keys) {

    # all dimensions are in either STUB or HEADING
    if (px_key %in% c(dim_key_stub, dim_key_heading)) {
      keyword <- get_keyword_from_px_key(px_key)
      # set dimension metadata
      metadata[[keyword]] <- get_dimension_values(px_rows, keyword, locale)
      # the dimensions are collected together with their values
      dimensions_with_values <- c(dimensions_with_values, metadata[[keyword]])

    } else {
      # check whether the key depends on a dimension
      dimension_detector <- dimension_detector(px_key, dimension_names)
      if (dimension_detector$belongs_to_dimension) {
        dimension_dependent_keys <- c(dimension_dependent_keys, px_key)
        dimension_name <- dimension_detector$dimension_name
        px_value <- px_rows[[px_key]]
        px_structure <- get_dimension_dependent_struct(
          px_key,
          px_value,
          dimension_name,
          dimensions_with_values[[dimension_name]]
        )
        keyword <- px_structure$keyword
        if (keyword == "VALUES") {
          # VALUES keys are not taken, as the dimension values have
          # already been collected together with the dimensions
          next
        } else if (!px_structure$is_dimension_value) {
          # keys that depend on both dimension and dimension value
          # such as PRECISION("Indikator", "Indikator 1")
          metadata[[keyword]][[dimension_name]] <- px_value
        } else {
          # keys that depend on only on a dimension
          # such as ELEMINATION("Indikator")
          dimension_value <- px_structure$dimension_value
          metadata[[keyword]][[dimension_name]][[dimension_value]] <- px_value
        }
      } else {
        # all other px keys
        keyword <- get_keyword_from_px_key(px_key)
        metadata[[keyword]] <- px_rows[[px_key]]
      }
    }
  }
  return(list(metadata = metadata,
              dimension_dependent_keys = dimension_dependent_keys))
}
