# Tests for belongs_to_dimensions
# unit test for belongs_to_dimensions: recognizes that a px key relates to a translation
test_that("belongs to dimension: case: dimension dependent key", {
  expect_equal(belongs_to_dimensions('PRECISION("Indikator","Scheidungen")', "(Jahr|Indikator)"), TRUE)
  expect_equal(belongs_to_dimensions('TITLE[de]', "(Jahr|Indikator)"), FALSE)
})

# Tests for get_key_parts_from_dimension_key
# unit test: key parts of a dimension dependent key are identified
test_that("key parts of a px dimension dependent key", {
  output <- get_key_parts_from_dimension_key('PRECISION("Indikator","Scheidungen")')
  expected_output <- c("PRECISION", "Indikator", "Scheidungen")
  expect_equal(output, expected_output)
})

# Tests for get_dimension_with_translations
# get_dimension_with_translations(list('STUB'=c('Jahr'), 'STUB[en]'=c('Year')), "de", c("de", "en"))
# unit test: get dimensions with translations from px_rows
test_that("key parts of a px dimension dependent key", {
  px_rows <- list('STUB'=c('Jahr'), 'STUB[en]'=c('Year'))
  default_language <- 'de'
  languages <- c("de", "en")
  output <- get_dimension_with_translations(px_rows, default_language, languages)
  expected_output <- c("Jahr", "Year")
  expect_equal(output, expected_output)
})

# Tests for get_dimension_pattern
# unit test to make pattern for dimensions
test_that("get dimension px keyword for dimension name", {
  output <- get_dimension_pattern(c("Jahr", "Indikator", "Year", "Indicator"))
  expected_output <- "(Jahr|Indikator|Year|Indicator)"
  expect_equal(output, expected_output)
})

# Tests for get dimension values
# unit test for making a dimension values key for a dimension
test_that("get dimension values key for dimenstion", {
  expected_output <- 'VALUES("Jahr")'
  output <- get_dimension_key("Jahr")
  expect_equal(output, expected_output)
})

test_that("get dimension values for px variable", {
  px_variable <- "STUB"
  dimension_values <- c("1876","1877","1878")
  px_rows = list('VALUES("Jahr")' = dimension_values, "STUB"="Jahr")
  expected_output = list("Jahr" = dimension_values)
  values <- get_dimension_values(px_rows, px_variable)
  expect_equal(values, expected_output)
})
