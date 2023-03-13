# Tests for translate_string:
# okay case: translated string is found
test_that("translate_string: okay case: translated string is found", {
  translation_keys <- c("Jahr", "Indikator")
  translations <- list(en=list(Jahr="Year", Indikator="Indikator"))
  translated_string <- translate_string("Jahr", "en", translation_keys, translations)
  expect_equal(translated_string, "Year")
})

# okay case: translated string is not found
test_that("translate_string: okay case: translated string is not found", {
  translation_keys <- c("Jahr", "Indikator")
  translations <- list(en=list(Jahr="Year", Indikator="Indikator"))
  translated_string <- translate_string("Kanton", "en", translation_keys, translations)
  expect_equal(translated_string, "Kanton")
})

# Tests for get_translation_language_from_key
# unit test for get_translation_language_from_key: key that marks a translations
test_that("get_translation_language_from_key: case: key that marks a translation", {
  language <- get_translation_language_from_key("TITLE[de]")
  expect_equal(language, "de")
})

# Tests for get_language_pattern
# unit test for get_language_pattern: valid languages are provided
test_that("get_language_pattern: case: languages are provided", {
  language_pattern <- get_language_pattern(c("de", "en"))
  expect_equal(language_pattern, "[[](de|en)[]]")
})

# Tests for is_language_specific
# unit test for is_language_specific: recognizes that a px key relates to a translation
test_that("is_language_specific: case: key that relates to translation", {
  expect_equal(is_language_specific('TITLE[fr]', "(de|fr|en)"), TRUE)
  expect_equal(is_language_specific('TITLE', "(de|fr|en)"), FALSE)
})
