# Tests for get_localized_keyword_pattern
test_that("get_localized_keyword_pattern default", {
  output <- get_localized_keyword_pattern()
  expected_output <- "^(DATA=|STUB=|HEADING=|TITLE=|DESCRIPTION=|UNITS=|VALUES[(]{1})"
  expect_equal(output, expected_output)
})

test_that("get_localized_keyword_pattern for locale", {
  locale <- "fr"
  output <- get_localized_keyword_pattern(locale)
  expected_output <- "^(DATA=|STUB[:punct:]fr[:punct:]|HEADING[:punct:]fr[:punct:]|TITLE[:punct:]fr[:punct:]|DESCRIPTION[:punct:]fr[:punct:]|UNITS[:punct:]fr[:punct:]|VALUES[:punct:]fr[:punct:])"
  expect_equal(output, expected_output)
})

