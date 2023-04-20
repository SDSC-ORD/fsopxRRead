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

# Tests for get_dimension_values
test_that("get dimension values for px variable", {
  px_variable <- "STUB"
  dimension_values <- c("1876", "1877", "1878")
  px_rows <- list('VALUES("Jahr")' = dimension_values,
                  "STUB" = "Jahr")
  expected_output <- list("Jahr" = dimension_values)
  values <- get_dimension_values(px_rows,
                                 px_variable)
  expect_equal(values, expected_output)
})

test_that("get dimension values for px variable", {
  px_variable <- "STUB"
  dimension_values <- c("1876", "1877", "1878")
  px_rows <- list('VALUES("Jahr")' = dimension_values,
                  "STUB" = "Jahr")
  expected_output <- list("Jahr" = dimension_values)
  values <- get_dimension_values(px_rows,
                                 px_variable)
  expect_equal(values, expected_output)
})

test_that("get dimension values for px variable: edge case", {
  px_variable <- "STUB"
  dimension_values <- c("1876", "1877", "1878")
  px_rows <- list('VALUES("Jahr")' = dimension_values,
                  "STUB" = "Jahr")
  expected_output <- list("Jahr" = dimension_values)
  values <- get_dimension_values(px_rows, px_variable)
  expect_equal(values, expected_output)
})

# Tests for dimension_detector
test_that("get dimension for dimension dependent key", {
  px_key <- 'PRECISION("Indikator","Scheidungen")'
  dimension_names <- c("Jahr", "Indikator")
  output <- dimension_detector(px_key, dimension_names)
  expected_output <- list(belongs_to_dimension = TRUE,
                          dimension_name = "Indikator")
  expect_equal(output, expected_output)
})

# Tests for dimension_detector
test_that("get dimension for dimension dependent key that is translated", {
  px_key <- 'PRECISION[en]("Indicator","Divorces")'
  dimension_names <- c("Year", "Indicator")
  output <- dimension_detector(px_key, dimension_names)
  expected_output <- list(belongs_to_dimension = TRUE,
                          dimension_name = "Indicator")
  expect_equal(output, expected_output)
})

# Tests for get_dimension_dependent_struct
test_that("get structure for dimension dependent key", {
  px_key <- 'PRECISION("Indikator","Indikator 2")'
  px_value <- "1"
  dimension_name <- "Indikator"
  dimension_values <- c("Indikator 1",
                        "Indikator 2",
                        "Indikator 3")
  output <- get_dimension_dependent_struct(px_key,
                                           px_value,
                                           dimension_name,
                                           dimension_values)
  expected_output <- list(keyword = "PRECISION",
                          is_dimension_value = TRUE,
                          dimension_value = "Indikator 2")
  expect_equal(output, expected_output)
})

test_that("get dimension metadata for dimension dependent key: edge case", {
  px_key <- 'ELIMINATION("Staatsangehörigkeit (Auswahl) des Ehemannes")'
  px_value <- "Staatsangehörigkeit des Ehemannes - Total"
  dimension_values <- c(
    "Staatsangehörigkeit des Ehemannes - Total",
    "Schweiz",
    "Dänemark")
  dimension_name <- "Staatsangehörigkeit (Auswahl) des Ehemannes"
  output <- get_dimension_dependent_struct(px_key,
                                           px_value,
                                           dimension_name,
                                           dimension_values)
  expected_output <- list(keyword = "ELIMINATION",
                          is_dimension_value = FALSE)
  expect_equal(output, expected_output)
})
