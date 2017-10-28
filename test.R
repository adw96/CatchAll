### Amy Willis, October 2017

## A script to enable compiling, building and testing the CatchAll R package

## Create and set the working directory

directory <- "/Users/teres/Documents/GitHub/Catch All REAL/CatchAll" ## your local copy here
setwd(directory)

# Download some required packages
require(devtools)
require(roxygen2)
require(testthat)
require(knitr)
require(rstudioapi)
require(Rd2roxygen)
devtools::install_github("hadley/pkgdown")
library(pkgdown)
library(breakaway)

# to run CatchAll on the apples dataset from the package breakaway
build()
install()
library(CatchAll)
CatchAll(apples) 


## another test set


# 
create_package("CatchAll")
build()
install()
test_data_set_1 <- read.csv("data/10000_geo_.5_2.csv", header = F)
x <- CatchAll(test_data_set_1)
x[19:25, 1:5]
x[19:25, 12:14]

## To build the full site -- not yet!!
# roxygenise()
# roxygen_and_build(directory)
# install()
# build_site()
# check()
