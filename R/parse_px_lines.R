#' Parse the lines of a px file and get the px metadata records and data
#'
#' The lines in the px file are separated by ';'.
#' After scanning there are groups of lines that together
#' form a px record. The px record consists of a key that
#' is contained in the first line and usually separated
#' by an '=' sign from the content of the px record.
#'
#' @param scanned_lines all scanned line of the px file
#' @param supported_keywords array of supported px keywords
#'
#' @return named list of px_rows and data
#' @noRd
#'
#' @examples parse_px_lines(c('CHARSET=\"ANSI\"',
#'                            "",
#'                            'AXIS-VERSION=\"2010\"',
#'                             "",
#'                             "DATA= 1 2 3",
#'                             ""),
#'                          c('CHARSET', 'AXIS-VERSION', 'DATA'))
parse_px_lines <- function(
  scanned_lines,
  supported_keywords
  ) {
  # make sure the last scanned line is an empty line
  scanned_lines <- append(scanned_lines, "")
  px_rows <- list()
  px_line_group <- c()

  for (i in seq_along(scanned_lines)) {

    # the empty line separate the px key value blocks after scanning
    if (scanned_lines[i] != "") {
      px_line_group <- append(px_line_group, scanned_lines[i])

    } else {
      if (length(px_line_group) > 0) {

        # the key is contained in the first row of the line group
        px_key <- get_keywords_from_lines(px_line_group[1])

        if (px_key == "DATA") {
          # process the actual data
          data <- vectorize_px_data(px_line_group)

        } else if (grepl("[A-Z-]", px_key)) {

          # make sure the line is not empty
          if (is_supported_px_key(px_key, supported_keywords)) {
            px_rows <- append(px_rows, get_keyword_values_pair(px_line_group))
          }
        }
        px_line_group <- c()
      }
    }
  }
  return(list(metadata = px_rows, data = data))
}
