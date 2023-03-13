# Tests for write_json_output
# unit test to check whether output is written as expected
test_that("write_json_output case: write output file", {
  output_dir <- here::here("tests/testthat/output/test-output/")
  data <- c(list(a="apple", b="banana", c="rose"))
  write_json_output(data, dir=output_dir, 'test.json')
  expected_file_path <- paste0(output_dir, 'test.json')
  assertthat::assert_that(file.exists(expected_file_path))
  recovered_content <- jsonlite::read_json(expected_file_path)
  expect_equal(recovered_content, data)
  file.remove(expected_file_path)
})
