
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `googlepatents`

<!-- badges: start -->
<!-- badges: end -->

The goal of googlepatents is to download data from patents available in
[Google Patents](https://patents.google.com/) website.

## Installation

You can install the `googlepatents` package by:

``` r
devtools::install_github("roneyfraga/googlepatents")
```

## How To Use

The tables: `abstract.csv`, `claims.csv`, `cited_by.csv`,
`patent_citations.csv` and `ipc.csv` are saved in the
`rawfiles_pantents/` directory.

``` r
library(googlepatents)

patent_scraping(url = 'https://patents.google.com/patent/US10780391B2/en',
                directory = 'rawfiles_patents',
                tables = c('abstract', 'claims', 'cited_by', 'patent_citations', 'ipc'),
                show_progress = FALSE)

fs::dir_ls('rawfiles_patents/')
```

``` r
rawfiles_patents/abstract.csv
rawfiles_patents/cited_by.csv
rawfiles_patents/claims.csv
rawfiles_patents/id.csv
rawfiles_patents/ipc.csv
rawfiles_patents/patent_citations.csv
```
