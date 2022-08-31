
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

plot_user_model(mtool_data, layout = "circle",
                user = mtool_data$User_ID[50])
```

<img src="man/figures/README-example1-1.png" width="100%" />

## Example 2: Calculate overview stats for the aggregate model and a user model

These are functions operating on igraph objects, so a lot is possible.
At present, weighted betweenness and degrees are implemented.

Here for an aggregate example:

``` r
calculate_aggregate_stats(mtool_data)
#>                                  w_betweenness w_in_degree w_out_degree
#> Carbon capture and storage           3.0000000           4            2
#> Climate compensation                34.6666667           3           10
#> Electrics cars                       0.0000000           7            3
#> Energy efficient home appliances    14.0000000           9            3
#> Energy efficient houses              2.0000000          10            3
#> Energy saving                       17.0000000          11            4
#> Environmental education              5.5000000           3           10
#> Hydropower                           4.8333333           5            3
#> Nuclear power                        0.0000000           4            2
#> Public transportation                4.0000000           8            2
#> Regulations                          0.0000000           1           16
#> Science                             21.8333333           4           16
#> Solar panels                         4.0000000           5            7
#> Subsidies                            5.5000000           2           14
#> Walking and cycling                  0.3333333           6            2
#> Wind farms                           0.5000000           5            6
#> Energy transition                    0.0000000          16            0
#>                                  w_total_degree
#> Carbon capture and storage                    6
#> Climate compensation                         13
#> Electrics cars                               10
#> Energy efficient home appliances             12
#> Energy efficient houses                      13
#> Energy saving                                15
#> Environmental education                      13
#> Hydropower                                    8
#> Nuclear power                                 6
#> Public transportation                        10
#> Regulations                                  17
#> Science                                      20
#> Solar panels                                 12
#> Subsidies                                    16
#> Walking and cycling                           8
#> Wind farms                                   11
#> Energy transition                            16
```

And for a specific user:

``` r
calculate_user_stats(mtool_data, user = mtool_data$User_ID[100])
#>                                  w_betweenness w_in_degree w_out_degree
#> Nuclear power                              0.0           0            1
#> Carbon capture and storage                 1.0           2            1
#> Climate compensation                       0.0           0            4
#> Wind farms                                 0.0           3            1
#> Hydropower                                 1.0           1            1
#> Solar panels                               0.0           3            1
#> Energy saving                              2.0           2            1
#> Energy efficient houses                    1.0           2            1
#> Energy efficient home appliances           1.0           1            1
#> Electrics cars                             1.0           2            1
#> Public transportation                      0.5           2            1
#> Environmental education                    0.0           0            2
#> Walking and cycling                        0.5           1            1
#> Subsidies                                  0.0           0            3
#> Science                                    0.0           0            4
#> Regulations                                0.0           0            5
#> Energy transition                          0.0          10            0
#>                                  w_total_degree
#> Nuclear power                                 1
#> Carbon capture and storage                    3
#> Climate compensation                          4
#> Wind farms                                    4
#> Hydropower                                    2
#> Solar panels                                  4
#> Energy saving                                 3
#> Energy efficient houses                       3
#> Energy efficient home appliances              2
#> Electrics cars                                3
#> Public transportation                         3
#> Environmental education                       2
#> Walking and cycling                           2
#> Subsidies                                     3
#> Science                                       4
#> Regulations                                   5
#> Energy transition                            10
```

<!-- build this with devtools::build_readme() -->
