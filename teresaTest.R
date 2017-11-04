### Amy Willis, October 2017

## A script to enable compiling, building and testing the CatchAll R package
## (locally)

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
library(tidyverse)


rm(list = ls(all = T))

# to run CatchAll on the apples dataset from the package breakaway
build()
4+4
install()
5+5
library(CatchAll)

#how to run it on test.csv? maybe i'm getting confused on workflow
#test_simple <- read.csv("test.csv", header = F)
#simple <- CatchAll(test_simple)

#CatchAll(apples) 
CatchAll(apples) 


## another test set

for (file in list.files("R/", full.names = T)) source(file)
print(file)
CatchAll(apples)
#CatchAll(simple)
2+2


#CatchAll:CatchAll(apples)
# 
create_package("CatchAll")
build()
install()
#rename to test.csv?2

test_data_set_1 <- read.csv("test.csv", header = F)
x <- CatchAll(test_data_set_1)
x[19:25, 1:5]
x[19:25, 12:14]

## To build the full site -- not yet!!
# roxygenise()
# roxygen_and_build(directory)
# install()
# build_site()
# check()