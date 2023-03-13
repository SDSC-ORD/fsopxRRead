#' Check download url whether it is a px cube
#'
#' Also check the encoding and return the languages of
#' all translations it contains
#'
#' @importFrom dplyr %>%
#' @param file_or_url url to download a px cube
#' @param locale language to localize a multilingual file into
#'
#' @return file_is_px_axis_file
#' @noRd
#'
#' @examples check_px_cube_url('https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-0102020207_102',
#'                             "en")
check_file_or_url <- function(file_or_url, locale="default") {
  tryCatch(
    {
      lines <- readr::read_lines(file_or_url,  n_max = 10)
      keywords <- get_keywords_from_lines(lines)
      file_is_px_axis_file <- ('AXIS-VERSION' %in% keywords)
      if (!('AXIS-VERSION' %in% keywords )) {
        stop("File is not a px cube: could not find AXIS-VERSION statement")
      }
      if (!('CHARSET=\"ANSI\";' %in% lines)) {
        stop("File has not the expected ANSI encoding: file an issue and ask for an extensions to other encodings")
      }
      multilingual <- list()
      if ('LANGUAGES' %in% keywords) {
        languages=get_languages_for_keyword(lines, 'LANGUAGES')
        if ('LANGUAGE' %in% keywords) {
          default_language=language=get_languages_for_keyword(lines, 'LANGUAGE')
        } else {
          stop("File has translations but no default language.")
        }
        language_pattern=get_language_pattern(languages)
        multilingual$flag <- TRUE
        multilingual$languages <- languages
        multilingual$language_pattern <- language_pattern
        multilingual$default_language <- default_language
        if (locale == "default") {
          multilingual$locale <- multilingual$default_language
        } else if (locale %in% multilingual$languages) {
          multilingual$locale <- locale
        } else {
          stop("The requested locale does not exist for this px cube file")
        }
      } else {
        multilingual$flag <- FALSE
        if (locale != "default") {
          stop("This is not a multilingual px file. So the parameter 'locale' is not allowed.")
        }
      }
    },
    error = function(error_message) {
      stop(error_message)
    }
  )
  return(multilingual)
}

#' Check input parameter `output_dir`
#'
#' Raise an error if the output_dir is not a directory
#'
#' @param output_dir directory to collect the output files
#'
#' @noRd
#'
#' @examples check_output_dir('/tmp/my_output')
check_output_dir <- function(output_dir) {
  output_settings <- list()
  if (output_dir == "default") {
    output_settings$flag <- FALSE
  } else if (!dir.exists(output_dir)) {
    stop("The requested output_dir does not exist")
  } else {
    output_settings <- list(dir=output_dir)
    output_settings$flag <- TRUE
  }
  return(output_settings)
}

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
get_supported_px_keywords <- function() {
  key_path <- here::here("supported_keywords.csv")
  keywords <- read.csv(key_path)
  return(keywords[['supported']])
}
