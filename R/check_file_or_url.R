#' Check download url whether it is a px cube
#'
#' Also check the encoding and return the languages of
#' all translations it contains
#'
#' @importFrom dplyr %>%
#' @param file_or_url url to download a px cube
#' @param locale language to localize a multilingual file into
#' @param encoding encoding of the file
#'
#' @return file: list of attributes for the file,
#'         including a bool flag that indicates whether the file
#'         is multilingual
#' @noRd
#'
#' @examples check_px_cube_url(
#' "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-0102020207_102",
#' "en")
check_file_or_url <- function(
  file_or_url,
  locale = "default",
  encoding = "latin1"
  ) {
  tryCatch(
    {
      lines <- readLines(con = file_or_url, n = 10,
                encoding = encoding)
      keywords <- get_keywords_from_lines(lines)
      if (!("MATRIX" %in% keywords)) {
        stop("File is not a px cube: could not find mandatory MATRIX statement")
      }
      if ("LANGUAGES" %in% keywords) {
        languages <- get_languages_for_keyword(lines, "LANGUAGES")
        if ("LANGUAGE" %in% keywords) {
          default_language <- get_languages_for_keyword(lines, "LANGUAGE")
        } else {
          stop("File has translations but no default language.")
        }
        language_pattern <- get_language_pattern_for_keys(languages)
        file <- list(is_multilingual = TRUE,
                     languages = languages,
                     language_pattern = language_pattern,
                     default_language = default_language)
        if (locale == "default") {
          file$locale <- file$default_language
        } else if (locale %in% file$languages) {
          file$locale <- locale
        } else {
          stop("The requested locale does not exist for this px cube file")
        }
      } else {
        file <- list(is_multilingual = FALSE)
        if (locale != "default") {
          stop(paste("This is not a multilingual px file.",
                     "So the parameter 'locale' is not allowed."))
        }
      }
    },
    error = function(error_message) {
      stop(error_message)
    }
  )
  file$file_encoding <- get_file_encoding(lines)
  return(file)
}
