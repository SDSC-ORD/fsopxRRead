# Tests for extract_translations:

test_that("test extract tranlations", {
  raw_metadata <- list(TITLE='Scheidungen seit 1876', 'TITLE[en]'='Divorces since 1876',
                  STUB="Jahr", "STUB[en]"="Year",
                  "HEADING"="Indikator", "HEADING[en]"="Indicator", 'VALUES(\"Jahr\")'=c("2013","2014"),
                  'VALUES[en](\"Year\")'=c("2013","2014"), 'VALUES(\"Indikator\")'=c("Fall 1","Fall 2"),
                  'VALUES[en](\"Indicator\")'=c("Case 1","Case 2"))
  languages <- c("de", "en")
  default_language <- "de"
  language_pattern <- "[[](de|en)[]]"
  output <- extract_translations(raw_metadata, languages, default_language, language_pattern)
  expected_output <- list(en=list("Scheidungen seit 1876"="Divorces since 1876", "Jahr"="Year",
                                  "Indikator"="Indicator", "Fall 1"="Case 1", "Fall 2"="Case 2"))
  expect_equal(output, expected_output)
})