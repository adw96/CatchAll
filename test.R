### Amy Willis, October 2017

## A script to enable compiling, building and testing the CatchAll R package

## Create and set the working directory

directory <- "/Users/adw96/Documents/software/CatchAll" ## your local copy here
setwd(directory)

# Download some required packages
library(devtools)
library(roxygen2)
library(testthat)
library(knitr)
library(rstudioapi)
library(Rd2roxygen)
library(breakaway)
library(tidyverse)

rm(list = ls())

frequency_count <- apples
names(frequency_count) <- c("j", "f")
source("R/poisson.R")
PoissonModel(frequency_count, 4)
PoissonModel(frequency_count, 5)
PoissonModel(frequency_count, 6)
PoissonModel(frequency_count, 7)

# to run CatchAll on the apples dataset from the package breakaway
build(vignettes = T)
roxygenise()
install()
library(CatchAll)
PoissonModel(apples, 4)
PoissonModel(apples)

CatchAll(apples) 

test1 <- read.csv("tests/test_data.csv", header = F)
CatchAll(test1)

## another test set
for (file in list.files("R/", full.names = T)) source(file)
CatchAll(apples)


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

build()
install()
library(CatchAll)
PoissonModel0(apples)

## To build the full site -- not yet!!
# roxygenise()
# roxygen_and_build(directory)
# install()
# build_site()
# check()
