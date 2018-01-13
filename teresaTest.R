### Amy Willis, October 2017

## A script to enable compiling, building and testing the CatchAll R package
## (locally)

## Create and set the working directory

#C:\Users\teres\OneDrive\CatchAll
#"/Users/teres/Documents/GitHub/Catch All REAL/CatchAll
# directory <- "/Users/teres/OneDrive/CatchAll" ## your local copy here
directory <- "/Users/teres/Documents/GitHub/Catch All REAL/CatchAll"
setwd(directory)

# Download some required packages

library(devtools)
library(roxygen2)
library(testthat)
library(knitr)
library(rstudioapi)
library(Rd2roxygen)
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


testData <- read.csv("test_data.csv", header = F)


CatchAll(testData)
# apples1 <- read.csv("tests/apples.csv", header = F)
# CatchAll(apples1)
# butterfly1 <- read.csv("tests/butterfly.csv", header = F)
# CatchAll(butterfly1)
# #CatchAll(apples) 
# CatchAll(apples) 


## another test set

for (file in list.files("R/", full.names = T)) source(file)

print(file)
print("hi")

#CatchAll(apples)
CatchAll(testData)
2+2


#CatchAll:CatchAll(apples)
# 
create_package("CatchAll")
build()
install()

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
