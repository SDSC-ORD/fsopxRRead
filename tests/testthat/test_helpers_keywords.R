# Tests for get_supported_px_keywords
get_supported_px_keywords()
test_that("make sure a list of supported keywords is available", {
  output <- get_supported_px_keywords()
  expect_equal("AXIS-VERSION" %in% output, TRUE)
})

# Tests for get_keywords_from_lines
test_that("get keyword from raw px lines", {
  px_line <- 'LANGUAGE="de"'
  keyword <- "LANGUAGE"
  expect_equal(get_keywords_from_lines(px_line), keyword)
})

# Tests for get_languages_for_keyword
test_that("test get languages from keywords", {
  lines <- c('LANGUAGES="de","en"')
  keyword <- "LANGUAGES"
  expected_output <- c("de", "en")
  output <- get_languages_for_keyword(lines, keyword)
  expect_equal(output, expected_output)
})

# Tests for is_supported_px_key
test_that("supported px keys are regcognized", {
  output <- is_supported_px_key('VALUES("Jahr")', c('LANGUAGES', 'VALUES'))
  expect_equal(output, TRUE)
})

# Tests for get_px_locale_keys
test_that("get all relevant px keys for a locale", {
  px_keys <- c("CHARSET",
               "TITLE",
               "TITLE[fr]",
               "TITLE[en]",
               "STUB",
               "STUB[fr]",
               "STUB[en]",
               "VALUES(\"Wirtschaftsabteilung\")",
               "VALUES[fr](\"Division économique\")",
               "VALUES[en](\"Economic division\")")
  px_default_keys <- c("CHARSET",
                       "TITLE",
                       "STUB",
                       "VALUES(\"Wirtschaftsabteilung\")")
  dimension_dep_default_keys <- list("VALUES(\"Wirtschaftsabteilung\")")
  locale <- "en"
  output <- get_px_locale_keys(px_keys,
                               px_default_keys,
                               dimension_dep_default_keys,
                               locale)
  expected_output <-  c("CHARSET",
                       "TITLE[en]",
                       "STUB[en]",
                       "VALUES[en](\"Economic division\")")
  expect_equal(output, expected_output)
})

# Tests for get_keyword_from_px_key
test_that("get keyword from px key", {
  output <- get_keyword_from_px_key('PRECISION[en]("Indikator")')
  expect_equal(output, "PRECISION")
})

# Tests for localize_px_key
test_that("localizing px key to get the key for the translation", {
  output <- localize_px_key("TITLE", "en")
  expect_equal(output, "TITLE[en]")
})

# Tests for get_language_pattern_for_keys
test_that("get_language_pattern for keys if languages are provided", {
  language_pattern <- get_language_pattern_for_keys(c("de", "en"))
  expect_equal(language_pattern, "[[](de|en)[]]")
})

# Tests for is_language_specific_key
test_that("is_language_specific: case: key that relates to translation", {
  expect_equal(is_language_specific_key("TITLE[fr]", "(de|fr|en)"), TRUE)
  expect_equal(is_language_specific_key("TITLE", "(de|fr|en)"), FALSE)
})

# Tests for get_key_pattern_locale
test_that("get key pattern to add to px keys for translations of locale", {
  expect_equal(get_key_pattern_locale("de"), "[de]")
})
