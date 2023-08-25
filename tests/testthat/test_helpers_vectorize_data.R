# Tests for vectorize_data:
test_that("test vectorizing of px data lines", {
  lines <- c('DATA=1102 \"...\" \"...\"', '370 607 125')
  expected_output <- c(1102, NA, NA, 370, 607, 125)
  output <- vectorize_data(lines)
  expect_equal(output, expected_output)
})

