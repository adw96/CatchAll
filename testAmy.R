### Amy Willis, October 2017

## A script to enable compiling, building and testing the CatchAll R package

## Intended for use only by Amy and Teresa as they build and test CatchAll

## Create and set the working directory
#directory <- "/Users/teres/Documents/GitHub/Catch All REAL/CatchAll" ## your local copy here
# directory <- "/Users/amy/Documents/software/CatchAll" ## your local copy here
directory <- "/Users/adwillis/software/CatchAll" ## your local copy here
setwd(directory)

# Download some required packages
library(devtools)
library(roxygen2)
library(testthat)
library(knitr)
library(rstudioapi)
library(Rd2roxygen)
# devtools::install_github("hadley/pkgdown")
library(pkgdown)
library(breakaway)
data(apples)



#### TODO: Fix CatchAll
roxygenise(directory)
document(directory)
load_all()
build(directory)
install(pkg = directory)
library(CatchAll)
data("butterfly")
suppressMessages({CatchAll(butterfly)})

check()
test(directory)

PoissonModel(apples) 
PoissonModel(apples, 10) 
source("R/CatchAll.R")
x <- CatchAll(apples)
x[,1]

rm(CatchAll)

#################################################################################################
###################################################################################
####################################


# to run CatchAll on the apples dataset from the package breakaway
build()
install()
library(CatchAll)
x <- CatchAll(apples) 
x

for (file in list.files("R/", full.names = T)) source(file)
CatchAll(apples) 

# unsource
rm(list =ls(all = T))
rm(list =ls())
CatchAll)

## another test set


# 
create_package("CatchAll")
build()
install()

#test_data_set_1 <- read.csv("test.csv", header = F)
#test_data_set_1 <- read.csv("data/10000_geo_.5_2.csv", header = F)

Å
x <- CatchAll(test_data_set_1)
x[19:25, 1:5]
x[19:25, 12:14]

## To build the full site -- not yet!!
# roxygenise()
# roxygen_and_build(directory)
# install()
# build_site()
# check()
