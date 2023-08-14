#' Scan a px cube file and return px data and metadata
#'
#' The Px file can be requested for a locale.
#' The output contains the metadata as json files.
#' The data is returned as a dataframe but also as csv file
#' The output will be stored in an output directory of choice.
#'
#' @param file_or_url url or file path to px cube
#'                    if the url gets a timeout: it is best to download
#'                    the file first and pass it as a file to the
#'                    function.
#' @param locale language for localizing the output (optional)
#'        if not provided all possible translations in the px cube
#'        are skipped
#' @param encoding chose encoding as either UTF-8 or latin1
#'        default is latin1
#' @param reverse_stub default FALSE: decided whether the sequence
#'                     of the data is according to the order of the
#'                     dimensions in the stub or in the revered order
#' @param output_dir directory for writing the output files (optional)
#'        if not provided the output will not be offered as files
#'
#' @return output contains of 2 parts: metadata and dataframe
#'         additionally the output may be written to files
#'         the files consist of a csv file with the data and
#'         a json file with the metadata. In case a locale is
#'         requested, the default metadata is provided along
#'         with the translated metadata
#' @export
#'
#' @examples scan_px_file("px-x-0102020203_110.px",
#'                        locale="en",
#'                        encoding="UTF-8",
#'                        reverse_stub = FALSE,
#'                        output_dir="/tmp/")
scan_px_file <- function(
  file_or_url,
  locale = "default",
  encoding = "latin1",
  reverse_stub = FALSE,
  output_dir = NULL) {
  tryCatch(
    {
      # check px file format and get back default language and available locales
      file <- check_file_or_url(file_or_url, locale, encoding)
      check_output_dir(output_dir)
      assertthat::assert_that(encoding %in% c("UTF-8", "latin1"))

      supported_keywords <- get_supported_px_keywords()

      scanned_lines <- scan(
        file_or_url,
        what = "list",
        sep = ";",
        quote = NULL,
        quiet = TRUE,
        encoding = encoding,
        multi.line = TRUE)
    },
    error = function(error_message) {
      stop(error_message)
    }
  )

  # group px data by px keywords
  px_cube <- parse_px_lines(
    scanned_lines,
    supported_keywords)

  # get px_keys for metadata in default language
  px_keys <- names(px_cube$metadata)
  if (file$is_multilingual) {
    px_default_keys <- px_keys[!is_language_specific_key(px_keys,
                                                         file$language_pattern)]
  } else {
    px_default_keys <- px_keys
  }

  # process metadata in default language and write json file of default language
  px_default <- process_px_metadata(px_cube$metadata,
                                    px_default_keys)
  metadata_default <- px_default$metadata
  if (!is.null(output_dir)) {
    write_json_output(metadata_default,
                      output_dir,
                      "metadata.json")
  }

  if (file$is_multilingual && (file$locale != file$default_language)) {
    # get px_keys for locale
    px_locale_keys <- get_px_locale_keys(px_keys,
                                         px_default_keys,
                                         px_default$dimension_dependent_keys,
                                         locale)
    # process metadata locale
    px_locale <- process_px_metadata(px_cube$metadata,
                                     px_locale_keys,
                                     file$locale)
    metadata_output <- px_locale$metadata
    if (!is.null(output_dir)) {
      write_json_output(metadata_output, output_dir,
                        paste0("metadata-", file$locale, ".json"))
    }
  } else {
    metadata_output <- metadata_default
  }

  # order dimensions
  dimension_order <- c(metadata_output$HEADING, metadata_output$STUB)
  if (reverse_stub) {
    dimension_order <- c(metadata_output$HEADING, rev(metadata_output$STUB))
  }

  # gather output as localized dataframe
  df <- expand.grid(dimension_order)
  data_col_name <- paste0("data[", metadata_output$UNIT, "]")
  df[, data_col_name] <- px_cube$data
  output <- list("metadata" = metadata_output,
                 "dataframe" = tibble::as_tibble(df))

  if (!is.null(output_dir)) {
    # also write dataframe to csv file
    if (file$is_multilingual) {
      data_file_name <- paste0("data-", file$locale)
    } else {
      data_file_name <- "data"
    }
    file_path <- paste0(output_dir, data_file_name, ".csv")
    write.table(output$dataframe, file = file_path, sep = ";",
                row.names = FALSE)
  }
  return(output)
}
