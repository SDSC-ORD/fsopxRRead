# Tests for parse_px_lines
test_that("test parsing of px lines", {
  lines <- c('CHARSET=\"ANSI\"',
             "",
             'AXIS-VERSION=\"2010\"',
             "",
             "DATA= 1 2 3",
             "")
  supported_keywords <- c("CHARSET", "AXIS-VERSION", "DATA")
  expected_output <- list(
    metadata = list("CHARSET" = "ANSI", "AXIS-VERSION" = "2010"),
    data = c(1, 2, 3))
  output <- parse_px_lines(lines, supported_keywords)
  expect_equal(output, expected_output)
})

test_that("test parsing of px lines", {
  lines <- c('STUB="Jahr","Staatsangehˆrigkeit (Auswahl) des Ehemannes"',
             "",
             'VALUES("Staatsangehˆrigkeit (Auswahl) des Ehemannes")="Staatsangehˆrigkeit des Ehemannes - Total","Schweiz"',
             "",
             "DATA= 1 2 3",
             "")
  supported_keywords <- c("STUB", "VALUES", "DATA")
  expected_output <- list(
    metadata = list("STUB" = c("Jahr", "Staatsangehˆrigkeit (Auswahl) des Ehemannes"),
                    'VALUES("Staatsangehˆrigkeit (Auswahl) des Ehemannes")' =
                    c("Staatsangehˆrigkeit des Ehemannes - Total", "Schweiz")),
    data = c(1, 2, 3))
  output <- parse_px_lines(lines, supported_keywords)
  expect_equal(output, expected_output)
})

test_that("test parsing of px lines", {
  lines <- c('STUB="Wirtschaftsabteilung","Ausbildungsniveau","Schwierigkeiten","Gewichtung"',
             "",
             'VALUES("Wirtschaftsabteilung")="5-96 Total","5-43 SEKTOR II"',
             "",
             "DATA= 1 2 3",
             "")
  supported_keywords <- c("STUB", "VALUES", "DATA")
  expected_output <- list(
    metadata = list("STUB" = c("Wirtschaftsabteilung", "Ausbildungsniveau", "Schwierigkeiten", "Gewichtung"),
                    'VALUES("Wirtschaftsabteilung")' = c("5-96 Total", "5-43 SEKTOR II")),
    data = c(1, 2, 3))
  output <- parse_px_lines(lines, supported_keywords)
  expect_equal(output, expected_output)
})

test_that("test parsing of px lines", {
  lines <- c('STUB="Jahr","Kanton (-) / Bezirk (>>) / Gemeinde (......)","Staatsangehörigkeit (Kategorie) des Ehemannes"',
             "",
             'VALUES(\"Staatsangehörigkeit (Kategorie) des Ehemannes\")=',
             '\"Staatsangehörigkeit des Ehemannes - Total\",\"Schweiz\",\"Ausland\"',
             "",
             "DATA= 1 2 3",
             "")
  supported_keywords <- c("STUB", "VALUES", "DATA")
  expected_output <- list(
    metadata = list("STUB" = c("Jahr", "Kanton (-) / Bezirk (>>) / Gemeinde (......)",
                               "Staatsangehörigkeit (Kategorie) des Ehemannes"),
                    'VALUES("Staatsangehörigkeit (Kategorie) des Ehemannes")' =
                    c("Staatsangehörigkeit des Ehemannes - Total", "Schweiz", "Ausland")),
    data = c(1, 2, 3))
  output <- parse_px_lines(lines, supported_keywords)
  expect_equal(output, expected_output)
})
