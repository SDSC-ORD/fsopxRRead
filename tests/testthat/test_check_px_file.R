# Tests for check_file_or_url:
# file not multilingual and locale not specified
test_that("check_file_or_url: case: for a not multiligual file", {
  path <- here::here("tests/testthat/data/2184.px")
  multilingual <- check_file_or_url(path)
  expect_equal(multilingual$flag, FALSE)
})

# error case: file not multilingual and locale is specified
test_that("check_locale case: not multilingual file and locale set", {
  path <- here::here("tests/testthat/data/2184.px")
  expect_error(check_file_or_url(path, "de"),
  "This is not a multilingual px file. So the parameter 'locale' is not allowed.")
})

# okay case: multilingual file and locale not specified
test_that("check_file_or_url: case: for a multiligual file", {
  path <- here::here("tests/testthat/data/px-x-0102020203_110.px")
  multilingual <- check_file_or_url(path)
  expect_equal(multilingual$flag, TRUE)
  expect_equal(multilingual$locale, "de")
  expect_equal(multilingual$languages, c("de", "fr", "it", "en"))
  expect_equal(multilingual$default_language, "de")
  expect_equal(multilingual$language_pattern, "[[](de|fr|it|en)[]]")
})

# okay case: file multilingual and locale is valid
test_that("check_locale case: file multilingual and locale is valid", {
  path <- here::here("tests/testthat/data/px-x-0102020203_110.px")
  output <- check_file_or_url(path, "en")
  expected_output <- list(flag=TRUE,
                          languages=c("de", "fr", "it", "en"), language_pattern="[[](de|fr|it|en)[]]",
                          default_language="de", locale="en")
  expect_equal(output, expected_output)
})

# error case: multilingual file and locale not in languages of the file
test_that("check_file_or_url: case: for a multiligual file", {
  path <- here::here("tests/testthat/data/px-x-0102020203_110.px")
  expect_error(check_locale(check_file_or_url(path, "es")))
})

# error case: invalid file format
test_that("check_file_or_url: case: unvalid file format", {
  url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=a"
  expect_error(check_file_or_url(url), "File is not a px cube: could not find AXIS-VERSION statement")
})

# Tests for check_output_dir:
# okay case: no output dir specified
test_that("check_output_dir case: default", {
  output_settings <- check_output_dir("default")
  expect_equal(output_settings$flag, FALSE)
})

# okay case: output dir specified
test_that("check_output_dir case: valid directory", {
  path <- here::here("tests/testthat/data/")
  output_settings <- check_output_dir(path)
  expect_equal(output_settings$flag, TRUE)
  expect_equal(output_settings$dir, path)
})
