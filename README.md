
<!-- README.md is generated from README.Rmd. Please edit that file -->

# scmlog

An R package for generating Word tables from the output of the PsN
command `scm`.

## Installation

To install from GitHub:

``` r
require(remotes)
remotes::install_github("benjaminrich/scmlog")
```

## Usage

``` r
library(scmlog)

# Make sure the working directory contains the file `scmlog.txt`, if not setwd(...)
process_scmlog()
```
