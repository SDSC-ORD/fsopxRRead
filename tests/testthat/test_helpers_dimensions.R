# Tests for get_dimension_values_key
test_that("get dimension values key for dimenstion", {
  expected_output <- 'VALUES("Jahr")'
  output <- get_dimension_values_key("Jahr")
  expect_equal(output, expected_output)
})

test_that("get dimension values key for dimenstion and locale", {
  expected_output <- 'VALUES[en]("Year")'
  output <- get_dimension_values_key("Year",
                                     "en")
  expect_equal(output, expected_output)
})
