# Tests for process_px_metadata:
test_that("process metadata from px rows", {
  raw_metadata <- list(TITLE = "Scheidungen seit 1876",
                       "TITLE[en]" = "Divorces since 1876",
                       STUB = "Jahr",
                       "STUB[en]" = "Year",
                       "HEADING" = "Indikator",
                       "HEADING[en]" = "Indicator",
                       'VALUES(\"Jahr\")' = c("2013", "2014"),
                       'VALUES[en](\"Year\")' = c("2013", "2014"),
                       'VALUES(\"Indikator\")' = c("Fall 1", "Fall 2"),
                       'VALUES[en](\"Indicator\")' = c("Case 1", "Case 2"),
                       'PRECISION(\"Indikator\")' = 2,
                       'PRECISION[en](\"Indicator\")' = 2)
  px_keys <- c("TITLE",
               "STUB",
               "HEADING",
               'VALUES(\"Indikator\")',
               'PRECISION(\"Indikator\")')
  output <- process_px_metadata(raw_metadata, px_keys)
  expected_output <- list(
    metadata = list(
      "TITLE" = "Scheidungen seit 1876",
      "STUB" = list("Jahr" = c("2013", "2014")),
      "HEADING" = list("Indikator" = c("Fall 1", "Fall 2")),
      "PRECISION" = list("Indikator" = 2)),
    dimension_dependent_keys = list(
      'VALUES(\"Indikator\")',
      'PRECISION(\"Indikator\")')
  )
  expect_equal(output, expected_output)
})

test_that("process metadata from px rows with locale", {
  raw_metadata <- list(TITLE = "Scheidungen seit 1876",
                       "TITLE[en]" = "Divorces since 1876",
                       STUB = "Jahr",
                       "STUB[en]" = "Year",
                       "HEADING" = "Indikator",
                       "HEADING[en]" = "Indicator",
                       'VALUES(\"Jahr\")' = c("2013", "2014"),
                       'VALUES[en](\"Year\")' = c("2013", "2014"),
                       'VALUES(\"Indikator\")' = c("Fall 1", "Fall 2"),
                       'VALUES[en](\"Indicator\")' = c("Case 1", "Case 2"),
                       'PRECISION(\"Indikator\")' = 2,
                       'PRECISION[en](\"Indicator\")' = 2)
  locale <- "en"
  px_keys <- c("TITLE[en]",
               "STUB[en]",
               "HEADING[en]",
               'VALUES[en](\"Indicator\")',
               'PRECISION[en](\"Indicator\")')
  output <- process_px_metadata(raw_metadata, px_keys, locale)
  expected_output <- list(
    metadata = list(
      "TITLE" = "Divorces since 1876",
      "STUB" = list("Year" = c("2013", "2014")),
      "HEADING" = list("Indicator" = c("Case 1", "Case 2")),
      "PRECISION" = list("Indicator" = 2)),
    dimension_dependent_keys = list(
      'VALUES[en](\"Indicator\")',
      'PRECISION[en](\"Indicator\")')
  )
  expect_equal(output, expected_output)
})
