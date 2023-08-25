# --------------------------------------------------------------------------------
# Test Script for other px datasets
# --------------------------------------------------------------------------------
# This script is meant to test run some px datasets after changes in the code
# to manually check on them
# These datasets are also included in the tests, but not every output part
# is checked there

# Dataset url <- "http://www.ine.es/jaxiT3/files/t/es/px/2184.px"
# spanish statiistical office dataset
url <- "http://www.ine.es/jaxiT3/files/t/es/px/2184.px"
ds <- scan_px_file(url)
ds

lines <- '"MATRIX=\"2007_CC\";"'
lines
check_file_or_url(lines)
lines <- readLines(
  con = url,
  encoding = "latin1"
)
lines
locale <- NULL

# check px file format and get back default language and available locales
locale <- check_file_or_url(lines[1:10], NULL)
locale
