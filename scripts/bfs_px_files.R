# --------------------------------------------------------------------------------
# Test Script for BFS datasets
# --------------------------------------------------------------------------------
# This script is meant to test run some BFS datasets after changes in the code
# to manually check on them
# Some of these datasets are also included in the tests, but not all since
# this would cause the tests to receive a buffer overflow.

# Dataset https://www.bfs.admin.ch/asset/de/px-x-0204000000_106
# energieeinsatzkonten_haushalte_und_wirtschaft
url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-0204000000_106"
ds <- scan_px_file(url, locale = "en")
ds

# Dataset https://www.bfs.admin.ch/asset/de/px-x-1903020100_101
# straftaten_aufklaerung
url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-1903020100_101"
ds <- scan_px_file(url, locale = "fr", encoding = "UTF-8")
ds

# Dataset https://www.bfs.admin.ch/asset/de/px-x-0703030000_112
# pflanzungen_schweiz
url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-0703030000_112"
ds <- scan_px_file(url, locale = "de")
ds

# Dataset https://www.bfs.admin.ch/asset/de/px-x-1002020000_101
# tourism_economy_by_canton
url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-1002020000_101"
ds <- scan_px_file(url, locale = "de", encoding = "UTF-8")
ds

# Dataset https://www.bfs.admin.ch/asset/de/px-x-0302010000_101
# grenzgaenger_statistik
# parse error
url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-0302010000_101"
ds <- scan_px_file(url, locale = "de")
ds

# Dataset https://www.bfs.admin.ch/asset/de/px-x-1703030000_100
# volksabstimmung_nach_kanton_seit_1861
# parse error
url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-1703030000_100"
ds <- scan_px_file(url, locale = "fr")
ds

# Dataset https://www.bfs.admin.ch/asset/de/px-x-1703010000_103
# abstimmungsvorlagen_seit_1971
url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-1703010000_103"
ds <- scan_px_file(url, locale = "de")
ds

# Dataset https://www.bfs.admin.ch/asset/de/px-x-0702000000_101
# farming
url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-0702000000_101"
ds <- scan_px_file(url, locale = "de")
ds

# Dataset https://www.bfs.admin.ch/asset/de/px-x-1702020000_104
# nationalratswahlen
url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-1702020000_104"
ds <- scan_px_file(url, locale = "de")
ds

# Dataset https://www.bfs.admin.ch/asset/de/px-x-1404010100_101
# medizinisch_technische_infrastruktur
url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-1404010100_101"
ds <- scan_px_file(url, locale = "de")
ds

# Dataset https://www.bfs.admin.ch/asset/de/px-x-0904030000_107
# neu_erstellte_wohnungen
url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-0904030000_107"
ds <- scan_px_file(url, locale = "de")
ds

# Dataset A5 https://www.bfs.admin.ch/asset/de/px-x-1305000000_101
# anzahl_ahv_renten
url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-1305000000_101"
ds <- scan_px_file(url, locale = "de")
ds

# Dataset https://www.bfs.admin.ch/asset/de/px-x-1404050000_103
# arztpraxen_ambulante_zentren
url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-1404050000_103"
ds <- scan_px_file(url, locale = "de")
ds

# Dataset https://www.bfs.admin.ch/asset/de/px-x-0102020000_101
# demographic_balance_by_canton
url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-0102020000_101"
ds <- scan_px_file(url, locale = "de")
ds

# Dataset https://www.bfs.admin.ch/asset/de/px-x-1305000000_101
# employees_farmholdings_agricultural_area_livestock_per_canton
url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-1305000000_101"
ds <- scan_px_file(url, locale = "de")
ds

# Dataset https://www.bfs.admin.ch/asset/de/px-x-0904010000_202
# bauausgaben
url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-0904010000_202"
ds <- scan_px_file(url, locale = "de")
ds

# Dataset https://www.bfs.admin.ch/asset/de/px-x-0602000000_107
# Betriebe nach Schwierigkeiten bei der Personalrekrutierung
url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?file=px-x-0602000000_107"
ds <- scan_px_file(url, locale = "en")
