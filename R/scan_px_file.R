#' Scan a px cube file and return px data and metadata
#'
#' The px file can be requested for a locale.
#' The data and metadata are both returned as dataframe
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
#'
#' @return output contains of 2 parts: metadata and dataframe
#'
#' @export
#'
#' @examples
#' scan_px_file(
#'   "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-1703010000_103",
#'   locale="fr"
#' )
scan_px_file <- function(
    file_or_url,
    locale = NULL,
    encoding = "latin1"
) {
  tryCatch(
    {
      assertthat::assert_that(encoding %in% c("UTF-8", "latin1"))

      suppressWarnings(lines <- readLines(
        con = file_or_url,
        encoding = encoding
      ))

      # check px file format and get back default language and available locales
      locale <- check_file_or_url(lines[1:10], locale)
    },
    error = function(error_message) {
      stop(error_message)
    }
  )

  # group px data by px keywords
  px_key_pattern <- get_localized_keyword_pattern(
    locale
  )
  idx <- get_line_index(
    lines,
    px_key_pattern
  )

  # get list of metadata
  metadata_list <- list()
  for (px_key in c("UNITS", "STUB", "HEADING", "TITLE")) {
    item_lines <- get_lines_for_px_key(lines, idx, px_key)
    values <- get_values_from_lines(item_lines)
    metadata_list[[px_key]] <- values
  }

  # metadata as a tibble
  metadata <- tibble::tibble(
    title = metadata_list$TITLE,
    units = list(metadata_list$UNITS),
    stub = list(metadata_list$STUB),
    heading = list(metadata_list$HEADING),
  )

  # get dimensions
  heading_dimension_names <- metadata$heading %>% unlist()
  stub_dimension_names <- metadata$stub %>% unlist()

  # order dimensions
  dimension_names <- c(stub_dimension_names, heading_dimension_names)
  dimensions_with_values <- build_dimensions(dimension_names, lines, idx, locale)

  # get the data
  data_lines <- get_lines_for_px_key(lines, idx, "DATA")
  suppressWarnings(data <- vectorize_data(data_lines))

  # make localized tibble with data and dimensions
  # Build the grid by iterating though the dimensions:
  # iterate through the innermost dimensions first: for this arrange_all is needed
  # since expand_grid iterates first through the outermost variables
  dataframe <- expand.grid(dimensions_with_values) %>%
    dplyr::arrange_all() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(data = data) %>%
    janitor::clean_names()

  return(list(dataframe=dataframe, metadata=metadata))
}

