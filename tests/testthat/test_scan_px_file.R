# Tests fo scan-px-file
test_that("scan px cube file: px-x-0102020203_110.px", {
  path <- here::here("tests/testthat/data/px-x-0102020203_110.px")
  output <- scan_px_file(path, locale = "en")
  expect_equal(length(output$metadata), 27)
  expect_equal(dim(output$dataframe), c(5402, 3))
  expect_vector(output$metadata)
})

test_that("scan px cube file: px-x-1003020000_201.px", {
  path <- here::here("tests/testthat/data/px-x-1003020000_201.px")
  output <- scan_px_file(path)
  expect_equal(length(output$metadata), 28)
  expect_equal(dim(output$dataframe), c(108160, 5))
  expect_vector(output$metadata)
})

test_that("scan px cube file: 2184.px", {
  url <- "http://www.ine.es/jaxiT3/files/t/es/px/2184.px"
  output <- scan_px_file(url, encoding = "latin1")
  expect_equal(length(output$metadata), 18)
  expect_equal(dim(output$dataframe), c(9600, 5))
  expect_vector(output$metadata)
})

test_that("error case: invalid file format", {
  url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=a"
  suppressWarnings({
    expect_error(scan_px_file(url),
                 "File is not a px cube: could not find AXIS-VERSION statement")
  })
})

test_that("unvalid file or url", {
  url <- "a"
  suppressWarnings({
    expect_error(scan_px_file(url))
  })
})

test_that("scan px cube file: 2184.px", {
  test_output_dir <- here::here("tests/testthat/output/test-output/")
  expected_output_dir <- here::here("tests/testthat/output/px-2184/")
  expected_metadata <- jsonlite::fromJSON(paste0(expected_output_dir,
                                          "metadata.json"))
  expected_data <- read.csv(paste0(expected_output_dir, "data.csv"))
  url <- "http://www.ine.es/jaxiT3/files/t/es/px/2184.px"
  result <- scan_px_file(url,
                         encoding = "latin1",
                         output_dir = test_output_dir)
  actual_path_metadata <- paste0(test_output_dir, "metadata.json")
  actual_path_data <- paste0(test_output_dir, "data.csv")
  actual_metadata <- jsonlite::fromJSON(actual_path_metadata)
  actual_data <- read.csv(actual_path_data)
  expect_equal(identical(expected_metadata, actual_metadata), TRUE)
  file.remove(actual_path_metadata)
  file.remove(actual_path_data)
})

test_that("scan px cube file: px-x-0102020203_110 and localize to English", {
  input_path <- here::here("tests/testthat/data/px-x-0102020203_110.px")
  test_output_dir <- here::here("tests/testthat/output/test-output/")
  expected_output_dir <- here::here(
    "tests/testthat/output/px-x-0102020203_110/")
  expected_metadata_en <- jsonlite::fromJSON(
    paste0(expected_output_dir, "metadata-en.json"))
  expected_metadata_default <- jsonlite::fromJSON(
    paste0(expected_output_dir, "metadata.json"))
  expected_data <- read.csv(
    paste0(expected_output_dir, "data-en.csv"))
  result <- scan_px_file(input_path,
                         locale = "en",
                         output_dir = test_output_dir)
  actual_path_metadata_en <- paste0(test_output_dir, "metadata-en.json")
  actual_path_metadata_default <- paste0(test_output_dir, "metadata.json")
  actual_path_data <- paste0(test_output_dir, "data-en.csv")
  actual_metadata_en <- jsonlite::fromJSON(actual_path_metadata_en)
  actual_metadata_default <- jsonlite::fromJSON(actual_path_metadata_default)
  actual_data <- read.csv(actual_path_data)
  expect_equal(identical(expected_metadata_en, actual_metadata_en), TRUE)
  expect_equal(identical(
    expected_metadata_default, actual_metadata_default), TRUE)
  expect_equal(identical(expected_data, actual_data), TRUE)
  file.remove(actual_path_metadata_en)
  file.remove(actual_path_metadata_default)
  file.remove(actual_path_data)
})

test_that("scan px cube file: px-x-0602000000_107 and localize to English", {
  input_path <- here::here("tests/testthat/data/px-x-0602000000_107.px")
  test_output_dir <- here::here("tests/testthat/output/test-output/")
  expected_output_dir <- here::here(
    "tests/testthat/output/px-x-0602000000_107/")
  expected_metadata_en <- jsonlite::fromJSON(
    paste0(expected_output_dir, "metadata-en.json"))
  expected_metadata_default <- jsonlite::fromJSON(
    paste0(expected_output_dir, "metadata.json"))
  expected_data <- read.csv(paste0(expected_output_dir, "data-en.csv"))
  result <- scan_px_file(input_path, locale = "en",
                         output_dir = test_output_dir)
  actual_path_metadata_en <- paste0(test_output_dir, "metadata-en.json")
  actual_path_metadata_default <- paste0(test_output_dir, "metadata.json")
  actual_path_data <- paste0(test_output_dir, "data-en.csv")
  actual_metadata_en <- jsonlite::fromJSON(actual_path_metadata_en)
  actual_metadata_default <- jsonlite::fromJSON(actual_path_metadata_default)
  actual_data <- read.csv(actual_path_data)
  expect_equal(identical(expected_metadata_en, actual_metadata_en), TRUE)
  expect_equal(identical(
    expected_metadata_default, actual_metadata_default), TRUE)
  expect_equal(identical(expected_data, actual_data), TRUE)
  file.remove(actual_path_metadata_en)
  file.remove(actual_path_metadata_default)
  file.remove(actual_path_data)
})

test_that("scan px cube file: px-x-1003020000_201 and localize to English", {
  input_path <- here::here("tests/testthat/data/px-x-1003020000_201.px")
  test_output_dir <- here::here("tests/testthat/output/test-output/")
  expected_output_dir <- here::here(
    "tests/testthat/output/px-x-1003020000_201/")
  expected_metadata_en <- jsonlite::fromJSON(paste0(expected_output_dir,
                                                    "metadata-en.json"))
  expected_metadata_default <- jsonlite::fromJSON(paste0(expected_output_dir,
                                                         "metadata.json"))
  expected_data <- read.csv(paste0(expected_output_dir, "data-en.csv"))
  result <- scan_px_file(input_path, locale="en", output_dir = test_output_dir)
  actual_path_metadata_en <- paste0(test_output_dir, "metadata-en.json")
  actual_path_metadata_default <- paste0(test_output_dir, "metadata.json")
  actual_path_data <- paste0(test_output_dir, "data-en.csv")
  actual_metadata_en <- jsonlite::fromJSON(actual_path_metadata_en)
  actual_metadata_default <- jsonlite::fromJSON(actual_path_metadata_default)
  actual_data <- read.csv(actual_path_data)
  expect_equal(identical(expected_metadata_en, actual_metadata_en), TRUE)
  expect_equal(identical(expected_metadata_default,
                         actual_metadata_default), TRUE)
  expect_equal(identical(expected_data, actual_data), TRUE)
  file.remove(actual_path_metadata_en)
  file.remove(actual_path_metadata_default)
  file.remove(actual_path_data)
})
