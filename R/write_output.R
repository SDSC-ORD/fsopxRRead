#' Write output to a json file
#'
#' @param output_data data to write to the json file
#' @param dir directory to write the output into
#' @param file_name name of the json file
#'
#' @noRd
#'
#' @examples write_json_output(c(list("a": "x"), dir='/tmp/'), 'test.json'))
write_json_output <- function(output_data, dir, file_name) {
  json_export <- jsonlite::toJSON(
    output_data,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  file_path <- paste0(dir, file_name)
  write(json_export, file_path)
}
