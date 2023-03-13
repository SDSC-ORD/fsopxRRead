# Tests for localize_metadata:

# unit test of localize_metadata: case string translation
test_that("localize_metadata: translate metadata via string translations", {
  metadata <- list(TITLE="Scheidungen")
  translations <- list(en=list(Scheidungen="Divorces"))
  locale <- "en"
  output <- localize_metadata(metadata, translations, locale)
  expect_equal(output, list("TITLE"="Divorces"))
})

# unit test of localize_metadata: case locale is same as default language
test_that("localize_metadata: return metadata if locale has no translation", {
  metadata <- list(TITLE="Scheidungen")
  translations <- list(en=list(Scheidungen="Divorces"))
  locale <- "de"
  output <- localize_metadata(metadata, translations, locale)
  expect_equal(output, metadata)
})
