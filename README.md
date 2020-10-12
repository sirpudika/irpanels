
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sep

The goal of sep is to help collaborators and interested users to create
graphics as used in the Swiss Environmental Panel Reports.

## Installation

You can install a development version of `sep` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bonschorno/sep")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(sep)

#create a simple barplot with manually adjusted with of bars

simple_barplot(df = mtcars, var = gear, title = "Gears", width = 0.3)
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r
# creating a simple crosstable

crosstable(vars = c("gear", "am"), df = mtcars)
#>     am
#> gear    1
#>    4 0.62
#>    5 0.38
```
