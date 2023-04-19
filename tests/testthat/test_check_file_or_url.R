# Tests for check_file_or_url:
test_that("file not multilingual and locale not specified", {
  path <- here::here("tests/testthat/data/2184.px")
  file <- check_file_or_url(path)
  expect_equal(file$is_multilingual, FALSE)
})

test_that("error case: file not multilingual and locale is specified", {
  path <- here::here("tests/testthat/data/2184.px")
  expect_error(check_file_or_url(path, "de"),
  paste("This is not a multilingual px file.",
        "So the parameter 'locale' is not allowed."))
})

test_that("okay case: multilingual file and locale not specified", {
  path <- here::here("tests/testthat/data/px-x-0102020203_110.px")
  file <- check_file_or_url(path)
  expect_equal(file$is_multilingual, TRUE)
  expect_equal(file$locale, "de")
  expect_equal(file$languages, c("de", "fr", "it", "en"))
  expect_equal(file$default_language, "de")
  expect_equal(file$language_pattern, "[[](de|fr|it|en)[]]")
})

test_that("okay case: file multilingual and locale is valid", {
  path <- here::here("tests/testthat/data/px-x-0102020203_110.px")
  output <- check_file_or_url(path, "en")
  expected_output <- list(is_multilingual = TRUE,
                          languages = c("de", "fr", "it", "en"), 
                          language_pattern = "[[](de|fr|it|en)[]]",
                          default_language = "de", locale = "en")
  expect_equal(output, expected_output)
})

test_that("error case: multilingual file and locale not in languages of the file", {
  path <- here::here("tests/testthat/data/px-x-0102020203_110.px")
  expect_error(check_locale(check_file_or_url(path, "es")))
})

test_that("error case: invalid file format", {
  url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=a"
  expect_error(check_file_or_url(url),
               "File is not a px cube: could not find AXIS-VERSION statement")
})
