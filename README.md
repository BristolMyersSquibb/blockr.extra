
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blockr.extra

<!-- badges: start -->

[![ci](https://github.com/blockr-org/blockr.extra/actions/workflows/ci.yml/badge.svg)](https://github.com/blockr-org/blockr.extra/actions/workflows/ci.yml)
[![codecov](https://codecov.io/github/blockr-org/blockr.extra/graph/badge.svg?token=9AO88LK8FJ)](https://codecov.io/github/blockr-org/blockr)
<!-- badges: end -->

## Usage

``` r
library(blockr)
library(blockr.extra)

options(BLOCKR_DEV = TRUE)
pkgload::load_all(".")

stack <- new_stack(data_block, filter_expr_block)
serve_stack(stack)

# see admiral vingette for how to use admiral_dpc_block
stack <- new_stack(data_block, mutate_block, admiral_dpc_block)
serve_stack(stack)
```

## Installation

You can install the development version of blockr.extra from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("blockr-org/blockr.extra")
```
