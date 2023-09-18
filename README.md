# pxRRead

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/SDSC-ORD/pxRRead/branch/main/graph/badge.svg)](https://app.codecov.io/gh/SDSC-ORD/pxRRead?branch=main)
[![R-CMD-check](https://github.com/SDSC-ORD/pxRRead/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SDSC-ORD/pxRRead/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## About

The px file format is a format for offering statistical tables
in an interactive way. It was introduced by the Statistics office of Sweden and is also used by statistical offices in other countries.

See here for a specification of this format: [px file format specification](https://www.scb.se/en/services/statistical-programs-for-px-files/px-file-format/)

The goal of pxRRead is parse px cube files:

* from a download url for a px cube file
* from a px cube file that has been downloaded previously

## Installation

You can install the development version of `pxRRead` with:

``` r
# install.packages("devtools")
devtools::install_github("SDSC-ORD/pxRRead")
```

## Usage

``` r
library(pxRRead)
scan_px_file('px-x-0102020203_110.px')
scan_px_file('https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-0602000000_107')
```

## Output

The goal of this package is to prepare the data and metadata for data science
applications. This package is especially meant to also support large 
multilingual px files and allow to localize them to a supported language.

The output consist of a list of data and metadata:

* `output$metadata` includes all metadata in the default language
* `output$dataframe` is a tibble with the length of the `DATA` of the px cube
* The dimensions in "STUB" and "HEADING" of the px cube are all turned into columns of the tibble

``` r
output <- scan_px_file('https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-0602000000_107')
output$metadata
output$dataframe
```

### Option to localize the output for multilingual px cubes

If the px cube is multilingual: a locale can be specified with `locale=<language code>` to localize the output: then the metadata and dataframe will use the translations for the given locale.

The language code must match the language code used in the px cube and specified with the `LANGUAGES` keyword. 

``` r
output <- scan_px_file(
  'https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-0602000000_107',
  locale="en"
)
output$metadata
output$dataframe
```

### Examples

In `scripts` directory there are examples to run. Most example are from 
the Swiss Federal Office of Statistics: `/scripts/bfs_px_files.R` but there 
is also a script `/scripts/other_px_files.R` for other px files. These
scripts include complete examples. They are meant for further exploration of the output of this package.

### Warnings and Restrictions

#### Encoding

Even though there is a `CODE_PAGE` specified in the px file the specified encoding does not always work to parse the file correctly with `scan` and `fileEncoding`. Therefor the encoding has been added as a parameter: `encoding`. The default is `latin1`. So if the file encoding seems incorrect it can be changed to `UTF-8`:

``` r
output <- scan_px_file(
  'https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-1002020000_101',
  locale = "de",
  encoding = "UTF-8")
```
