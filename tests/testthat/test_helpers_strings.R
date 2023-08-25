# tests for get_values_from_lines
test_that("get_values_from_lines with LANGUAGE", {
  lines <- 'LANGUAGE="de"'
  expected_output <- "de"
  output <- get_values_from_lines(lines)
  expect_equal(output, expected_output)
})

test_that("get_values_from_lines with TITLE[fr]", {
  lines <- "TITLE[fr]=\"Votations populaires (Nombre d'objets par thème depuis 1971)\";"
  expected_output <- "Votations populaires (Nombre d'objets par thème depuis 1971)"
  output <- get_values_from_lines(lines)
  expect_equal(output, expected_output)
})

test_that("get_values_from_lines with VALUES[fr]", {
  lines <- "VALUES[fr](\"Objet de votation (thème)\")=\"Ordre public\",\"Politique étrangère\","
  expected_output <- c("Ordre public", "Politique étrangère")
  output <- get_values_from_lines(lines)
  expect_equal(output, expected_output)
})

test_that("get_values_from_lines with VALUES[fr]", {
  lines <- 'LANGUAGES="de","en"'
  expected_output <- c("de", "en")
  output <- get_values_from_lines(lines)
  expect_equal(output, expected_output)
})

test_that("get_values_from_lines with multiple values", {
  lines <- '"HEADING=\"Índices y tasas\",\"Periodo\";"'
  expected_output <- c("Índices y tasas", "Periodo")
  output <- get_values_from_lines(lines)
  expect_equal(output, expected_output)
})
