
# pxRRead

<!-- badges: start -->
<!-- badges: end -->

## About

The px file format is a format for offering statistical tables
in an interactive way. It was introducted by the Statistics office of Schweden and
is also used by statistical offices in other countries.

See here for a specification of this format: [px file format specification](https://www.scb.se/en/services/statistical-programs-for-px-files/px-file-format/)

The goal of pxRRead is parse px cube files:
- from a download url for a px cube file
- from a px cube file that has been downloaded previously

## Installation

You can install the development version of pxRRead from [GitHub](https://github.com/) with:

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

The goal of this package is to offer the metadata and data in a reduction free machine readable
way.

The output consist of a list of data and metadata:
- `output$metadata` includes all metadata in the default language
- `output$dataframe` is a tibble with the length of the `DATA` of the px cube 
- The dimensions in "STUB" and "HEADING" of the px cube are all turned into columns of the tibble

``` r
output <- scan_px_file('https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-0602000000_107')
output$metadata
output$dataframe
```

### Option to localize the output for multilingual px cubes

If the px cube is multilingual: a locale can be specified with `locale=<language code>` to
localize the output

The language code must match the language code used in the px cube and specified with the 
`LANGUAGES` keyword. 

``` r
output <- scan_px_file(
  'https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-0602000000_107',
  locale="en"
)
output$metadata
output$dataframe
```

In this case the metadata and dataframe will use the translations for the given locale.

### Option to write the output to json and csv files

The output is structure in such a way, that it can directly be written to json or csv files.
This is done by providing an output directory: 

``` r
output <- scan_px_file(
  'https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-0602000000_107',
  locale="en",
  output_dir = paste0(getwd(), '/output/'))
```

For multilingual files the output consists of the following files:

- `metadata.json` : this file stores the metadata redundance free in the default language
- `metadata-en.json`: a file where the metadata is translated to the given locale
- `data-en.csv`: a csv file with the data and the headers in the chosen locale

### Examples

In `tests/testthat` in the directories `data` and `output` example input and 
output files are kept for testing purposes. They might also be helpful in understanding 
how px cubes are parsed by the package.

### Warnings and Restrictions

#### Encoding

Even though there is a `CODE_PAGE` specified in the px file the specified encoding
does not always work to parse the file correctly with `scan` and `fileEncoding`.
Therefor the encoding has been added as a parameter: `encoding`. The default is 
`latin1`. So if the file encoding seems incorrect it can be changed to `UTF-8`:

``` r
output <- scan_px_file(
  'https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-1002020000_101',
  locale = "de",
  encoding = "UTF-8")
```

#### Keywords

Only keywords in the file `supported_keywords.csv` are currently supported.
If an unsupported keyword is detected in the parsed px cube, the user is informed about this.
