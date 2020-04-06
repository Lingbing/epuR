
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epuR

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/Lingbing/epuR.svg?branch=master)](https://travis-ci.org/Lingbing/epuR)
<!-- badges: end -->

The goal of epuR is to provide a simple and consistent framework to
collect Economic Policy Uncertainty and related index data from their
official web locations in real time.

## Installation

You can install the released version of `epuR` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("epuR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Lingbing/epuR")
```

## Example

`epuR` functions adopts an **get\_XXX()** style to get the data, in
which ‘XXX’ refers to the index name. For example, to get EPU index, use
function `get_EPU()`:

``` r
library(epuR)
## get EPU data
epu_data <- get_EPU()
class(epu_data)
#> [1] "xts" "zoo"
```

Every `get` function returns an `xts` time series object so further data
manipulation and visualization is straightforward. To plot all regions
in the EPU data:

``` r
plot(epu_data)
```

<img src="man/figures/README-cars-1.png" width="100%" />

To plot some specific region:

``` r
plot(epu_data$Australia)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

Currently, the following indexes are supported:

## Supported Index

| Function  |         Index Data          | Default arguments |
| :-------: | :-------------------------: | ----------------- |
| `get_EPU` | Economic Policy Uncertainty | region = “all”    |
| `get_EMV` |  Equity Market Volatility   | all = T           |
| `get_FSI` | Financial Stress Indicator  | freq = “monthly”  |
| `get_GPR` |   Geopolitical Risk Index   | type = 1          |
| `get_IRI` |  Immigration Related Index  | region = “all”    |
| `get_TPU` |  Trade Policy Uncertainty   | region = “China”  |
| `get_WUI` |   World Uncertainty Index   | type = “F1”       |
