#' Check download url whether it is a px cube
#'
#' Also check the encoding and return the languages of
#' all translations it contains
#'
#' @importFrom dplyr %>%
#' @param lines lines from px file
#' @param locale language to localize a multilingual file into
#'
#' @return locale string: locale as language code or NULL
#'                        if the default language has been requested or
#'                        if the file is not multilingual
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' check_file_or_url(
#'   lines,
#'   locale="en",
#' )
#' returns locale if it exists for the file, otherwise it throws an error
#' an error is also thrown if the file is not a px file
#' }
check_file_or_url <- function(
  lines,
  locale = NULL
  ) {
  tryCatch(
    {
      if (!TRUE %in% grepl(x = lines, pattern = "MATRIX")) {
        stop("File is not a px cube: could not find mandatory MATRIX statement")
      }
      if (is.null(locale)) {
        return(locale)
      }
      languages_line <- grep(x = lines, pattern = "LANGUAGES=", value = TRUE)
      if (length(languages_line)) {
        languages <- get_values_from_lines(languages_line)
      }
      if (is.null(languages) || !locale %in% languages) {
        stop("The requested locale does not exist for this px cube file")
      }
      default_language_line <- grep(x = lines, pattern = "LANGUAGE=", value = TRUE)
      if (length(languages_line)) {
        default_language <- get_values_from_lines(default_language_line)
      }
      if (!is.null(default_language) && locale == default_language) {
        # the file does not need to get localized because the default values
        # can be used
        return(NULL)
      }
      return(locale)
    },
    error = function(error_message) {
      stop(error_message)
    }
  )
  return(file)
}

