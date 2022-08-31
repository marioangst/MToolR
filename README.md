
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MToolR

<!-- badges: start -->
<!-- badges: end -->

MToolR is a companion package for the Mental Model Mapping Tool M-Tool
(<https://www.m-tool.org/>). The package provides: a) functionality to
load and process data generated by M-Tool b) procedures for common
(basic) analytical tasks c) visualization functions

⚠️ This package is literally a couple of hours old - it’s under
development and use it at your own peril at the moment ⚠️

## Installation

You can install the development version of MToolR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("marioangst/MToolR")
```

## Example 1: Read in .csv file exported from M-Tool, aggregate and visualize

Say you have exported a file called “test_export.csv” from M-Tool and
have saved it in a folder data/. Here is how to read it into R and
create a first plot for a user.

``` r
library(MToolR)

mtool_data <-
  MToolR::parse_mtools_csv("data/test_export.csv", 
                           exclude_nonresponse = TRUE)

plot_user_model(mtool_data,
                user = mtool_data$User_ID[100])
```

<img src="man/figures/README-example1-1.png" width="100%" />

<!-- build this with devtools::build_readme() -->