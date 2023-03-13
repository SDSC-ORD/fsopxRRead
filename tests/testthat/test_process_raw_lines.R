# Test for get_keywords_from_lines
test_that("get keyword from raw px lines", {
  px_line <- 'LANGUAGE="de"'
  keyword <- "LANGUAGE"
  expect_equal(get_keywords_from_lines(px_line), keyword)
})

# Tests for get_keyword_values_pair:
test_that("get px_row as keyword values pair from group of px lines", {
  lines <- 'LANGUAGE="de"'
  keyword <- "LANGUAGE"
  values <- "de"
  px_row_expected <- list(values)
  names(px_row_expected) = keyword
  px_row <- get_keyword_values_pair(lines)
  expect_equal(px_row, px_row_expected)
  expect_equal(names(px_row_expected), keyword)
})

# Tests for vectorize_data:
test_that("test vectorizing of px data lines", {
  lines <- c('1102 \"...\" \"...\"', '370 607 125')
  expected_output <- c(1102, NA, NA, 370, 607, 125)
  output <- vectorize_data(lines)
  expect_equal(output, expected_output)
})

# Tests for vectorize_px_data:
test_that("test vectorizing of px data lines", {
  lines <- c('DATA= 1102 \"...\" \"...\"', '370 607 125')
  expected_output <- c(1102, NA, NA, 370, 607, 125)
  output <- vectorize_px_data(lines)
  expect_equal(output, expected_output)
})

# Tests for vectorize_raw_input:
test_that("test vecorizing of px metadata lines", {
  lines <- c('\"Betriebe\",\"Zimmer\"', '\"Ankünfte\",\"Zimmernächte\"')
  expected_output <- c("Betriebe", "Zimmer", "Ankünfte", "Zimmernächte")
  output <- vectorize_raw_input(lines)
  expect_equal(output, expected_output)
})

# Tests for get_languages_for_keyword
test_that("test get languages from keywords", {
  lines <- c('LANGUAGES="de","en"')
  keyword <- 'LANGUAGES'
  expected_output <- c("de", "en")
  output <- get_languages_for_keyword(lines, keyword)
  expect_equal(output, expected_output)
})

# Tests for parse_px_lines:
test_that("test parsing of px raw lines", {
  lines <- c('CHARSET=\"ANSI\"', "", 'AXIS-VERSION=\"2010\"', "", "DATA= 1 2 3", "")
  supported_keywords <- c('CHARSET', 'AXIS-VERSION', 'DATA')
  expected_output <- list(metadata=list("CHARSET"='ANSI', "AXIS-VERSION"='2010'), data=c(1, 2, 3))
  output <- parse_px_lines(lines, supported_keywords)
  expect_equal(output, expected_output)
})
