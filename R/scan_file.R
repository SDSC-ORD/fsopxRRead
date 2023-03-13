#' Scan a px cube file and return px data and metadata
#'
#' The Px file can be requested for a locale
#' The output contains the metadata as json files. Also the translations
#' are provided as json files that contain string tranlations
#' The data is returned as a dataframe but also as csv file
#' The output will be stored in an output directory of choice
#'
#' @param file_or_url url or file path to px cube
#' @param locale language for localizing the output
#' @param output_dir directory for writing the output files
#'
#' @return output is offered as json files and the data as csv file or dataframe
#' @export
#'
#' @examples scan_px_file("px-x-0102020203_110.px", locale="en", output_dir="/tmp/")
  scan_px_file <- function (file_or_url, locale="default", output_dir="default") {
  tryCatch(
    {
      # check px file format and get back default language and available locales
      multilingual <- check_file_or_url(file_or_url, locale)
      is_multilingual <- multilingual$flag
      languages <- multilingual$languages
      default_language <- multilingual$default_language
      language_pattern <- multilingual$language_pattern
      locale <- multilingual$locale

      output_settings <- check_output_dir(output_dir)

      supported_keywords <- get_supported_px_keywords()

      # scan px file with ASCII encoding
      scanned_lines <- scan(file_or_url, what = "list", sep = ";", quote = NULL,
                            quiet = TRUE, encoding = "latin1", multi.line = TRUE)
    },
    error = function(error_message) {
      stop(error_message)
    }
  )
  # group px data by px keywords
  px_rows <- parse_px_lines(scanned_lines, supported_keywords)
  data <- px_rows$data
  metadata_all_languages <- px_rows$metadata

  # process metadata in default language and write json file of default language
  metadata_default <- process_px_metadata(metadata_all_languages,
                                          is_multilingual,
                                          language_pattern)
  if (output_settings$flag) {
    write_json_output(metadata_default,
                      output_settings$dir,
                      "metadata.json")
  }

  if (multilingual$flag) {
    # process translation records in px file and store as string translations
    translations <- extract_translations(metadata_all_languages,
                                         languages,
                                         default_language,
                                         language_pattern)
    if (output_settings$flag) {
      # write translations to json files
      for (language in languages) {
        if (language != default_language) {
          write_json_output(translations[[language]],
                            output_settings$dir,
                            paste0(language, ".json"))
        }
      }
    }

    # localize metadata by applying string translation
    metadata_output <- localize_metadata(metadata_default,
                                         translations,
                                         locale)
    if (output_settings$flag) {
      write_json_output(metadata_output, output_settings$dir,
                        paste0("metadata-", locale, ".json"))
    }
  } else {
    metadata_output <- metadata_default
  }

  # gather output as localized dataframe
  df <- expand.grid(c(metadata_output$HEADING, metadata_output$STUB))
  data_col_name <- paste0('data[', metadata_output$UNIT ,']')
  df[, data_col_name] = px_rows$data
  output <- list('metadata' = metadata_output,
                 'dataframe' = tibble::as_tibble(df))

  if (output_settings$flag) {
    # also write dataframe to csv file
    if (is_multilingual) {
      data_file_name <- paste0('data-', locale)
    } else {
      data_file_name <- 'data'
    }
    file_path <- paste0(output_settings$dir, data_file_name, '.csv')
    write.table(output$dataframe , file=file_path, sep=';', row.names = FALSE)
  }
  # return output
  return(output)
}
