# tests for get_px_row_start_idx
test_that("get_px_row_start_idx", {
  px_row_end_idx <- c(3, 5, 6, 20, 102)
  expected_output <- c(1, 4, 6, 7, 21)
  output <- get_px_row_start_idx(px_row_end_idx)
  expect_equal(output, expected_output)
})
