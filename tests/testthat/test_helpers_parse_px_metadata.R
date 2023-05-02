# tests for get_keyword_values_pair
test_that("get px_row as keyword values pair from group of px lines", {
  lines <- 'LANGUAGE="de"'
  keyword <- "LANGUAGE"
  values <- "de"
  px_row_expected <- list(values)
  names(px_row_expected) <- keyword
  px_row <- get_keyword_values_pair(lines)
  expect_equal(px_row, px_row_expected)
  expect_equal(names(px_row_expected), keyword)
})

# tests for clean_keyword_strings
test_that("test vecorizing of px metadata lines", {
  lines <- c('\"Betriebe\",\"Zimmer\"', '\"Ankünfte\",\"Zimmernächte\"')
  expected_output <- c("Betriebe", "Zimmer", "Ankünfte", "Zimmernächte")
  output <- clean_keyword_strings(lines)
  expect_equal(output, expected_output)
})
