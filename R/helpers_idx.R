#' Get line index
#'
#' @param lines vector: lines of a px key file
#' @param px_key_pattern string: the pattern of keys to search for
#'
#' @return idx tibble: an index for the lines in a px file, that has
#'                     the lines on where a px row starts and ends
#'                     this is a tibble with the fields: keyword_line
#'                     keyword, px_row_start, px_row_end
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' get_line_index(
#'   lines,
#'   px_key_pattern
#' )
#' }
get_line_index <- function(lines, px_key_pattern) {
  # pattern to detect px row end of row
  px_row_end_pattern <- ";\\s?$"
  # get indexes of all row ends
  px_row_end_idx <- grep(x = lines,
                         pattern = px_row_end_pattern,
                         value = FALSE)

  # get indexes of all row starts
  px_row_start_idx <- get_px_row_start_idx(px_row_end_idx)

  # make a tibble for px row index information
  idx <- tibble::tibble(
    px_row_start = px_row_start_idx,
    px_row_end = px_row_end_idx
  ) %>%
    dplyr::mutate(
      keyword_line = lines[px_row_start]
    ) %>%
    dplyr::filter(
      stringr::str_detect(string = keyword_line, pattern = px_key_pattern)
    ) %>%
    dplyr::mutate(
      keyword = stringr::str_extract(keyword_line, "^[:upper:]*")
    )
  return(idx)
}

#' get px_row_start_index for each px_row_end_index
#'
#' this can be determined by sequencing through the px_row_end_idx
#' sequence: after a px_row ends the next px_row will start unless
#' it is the last px_row.
#'
#' @param px_row_end_idx vector: sequence of indexes for px_row ends
#'
#' @return px_row_start_idx vector: sequence of indexes for px_row starts
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' get_px_row_start_idx(
#'   c(3, 5, 6, 20, 102)
#' )
#' }
get_px_row_start_idx <- function(px_row_end_idx) {
  # get indexes of all row starts
  px_row_start_idx <- 1
  for (i in seq_along(px_row_end_idx)) {
    if (i == 1) {
      next
    }
    px_row_start_idx[i] <- px_row_end_idx[i - 1] + 1
  }
  return(px_row_start_idx)
}

#' Get the location from a px row from the index and return the lines
#'
#' The lines are returned for further processing
#'
#' @param lines vector the lines of a px file
#' @param idx tibble: the index of px keys and their
#'                    corresponding start and end line numbers
#'                    in lines
#' @param px_key string: a px_key word such as "LANGUAGES"
#'
#' @return lines_for_px_key vector: lines that correspond to a px_key
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' get_lines_for_px_key(
#'   lines,
#'   idx,
#'   "STUB"
#' )
#' returns the lines that belong to the provided px_key
#' }
get_lines_for_px_key <- function(lines, idx, px_key) {
  px_key_pattern <- paste0("^", px_key, "$")
  filtered_idx <- idx %>% dplyr::filter(stringr::str_detect(keyword, px_key_pattern)) %>%
    dplyr::select(px_row_start, px_row_end)
  lines_for_px_key <- lines[filtered_idx$px_row_start:filtered_idx$px_row_end]
  return(lines_for_px_key)
}
