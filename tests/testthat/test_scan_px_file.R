# Tests for scan_px_file
test_that("scan_px_file of BFS dataset with locale", {
  url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-0204000000_106"
  ds <- scan_px_file(url, locale = "en")
  expect_equal(dim(ds$dataframe)[2], 5)
  expect_equal(colnames(ds$dataframe),
               c("unit_of_measure", "economy_and_households", "energy_product", "year", "data"))
})

test_that("scan_px_file of BFS with default locale", {
  url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-1903020100_101"
  ds <- scan_px_file(url, encoding = "UTF-8")
  expect_equal(dim(ds$dataframe)[2], 6)
  expect_equal(colnames(ds$dataframe),
               c("straftat", "kanton", "ausfuhrungsgrad", "aufklarungsgrad", "jahr", "data"))
})

test_that("scan_px_file unvalid file or url", {
  url <- "a"
  suppressWarnings({
    expect_error(scan_px_file(url))
  })
})

test_that("scan_px_file error case: invalid file format", {
  url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=a"
  suppressWarnings({
    expect_error(scan_px_file(url),
                 "File is not a px cube: could not find mandatory MATRIX statement")
  })
})

test_that("scan_px_file multilingual file and locale in languages of the file", {
  url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-1703010000_103"
  ds <- scan_px_file(url, locale = "fr")
  expect_equal(ds$metadata$units %>% unlist(), c("Objet de votation"))
})
