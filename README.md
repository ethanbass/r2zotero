
# r2zotero

<!-- badges: start -->
  [![Last commit](https://img.shields.io/github/last-commit/ethanbass/r2zotero)]()
  [![stability-experimental](https://img.shields.io/badge/stability-experimental-orange.svg)](https://github.com/emersion/stability-badges#experimental)
<!-- badges: end -->

The goal of **r2zotero** is to facilitate citation of R packages by importing citations from an 
R package's 'CITATION' file directly to a local [Zotero](https://www.zotero.org/) 
library using the [Zotero Web API](https://www.zotero.org/support/dev/web_api/v3/start).

## Installation

You can install the development version of r2zotero from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("ethanbass/r2zotero")
```

## Example

To send a citation to Zotero:

``` r
library(r2zotero)
r2zotero("lattice")
```

