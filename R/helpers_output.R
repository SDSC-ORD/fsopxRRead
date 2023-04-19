#' Check input parameter `output_dir`
#'
#' Raise an error if the output_dir is not a directory
#'
#' @param output_dir directory to collect the output files
#'
#' @noRd
#'
#' @examples check_output_dir('/tmp/my_output')
check_output_dir <- function(
  output_dir
  ) {
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      stop("The requested output_dir does not exist")
    }
  }
}

#' Write output to a json file
#'
#' @param output_data data to write to the json file
#' @param dir directory to write the output into
#' @param file_name name of the json file
#'
#' @noRd
#'
#' @examples write_json_output(c(list("a": "x"),
#'                             dir='/tmp/'),
#'                             'test.json'))
write_json_output <- function(
  output_data,
  dir,
  file_name
  ) {
  json_export <- jsonlite::toJSON(
    output_data,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  file_path <- paste0(dir, file_name)
  write(json_export, file_path)
}
