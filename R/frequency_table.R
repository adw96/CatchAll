setClass("FrequencyTable", 
         slots = c(table = "data.frame",
                   f1 = "integer",
                   n = "integer"))

setMethod("initialize", "FrequencyTable", 
          function(.Object, 
                   table = data.frame(), 
                   f1 = integer(0),
                   n = integer(0),
                   ...) {
            .Object <- callNextMethod(.Object, ...)
            .Object@table <- table[table[,2] > 0, ]
            .Object@f1 <- table[1, 2]
            .Object@n <- sum(table[ , 2])
            .Object
          })

validFrequencyTable <- function(object) {
  errors <- character()
  
  width_table <- dim(object@table)[2]
  if (!(width_table %in% c(0, 2))) {
    msg <- paste("Table has ", width_table, "columns.  Should be 0 or 2", sep = "")
    errors <- c(errors, msg)
  }
  
  ## check that is a frequency count table
  
  if (length(errors) == 0) TRUE else errors
}

setValidity("FrequencyTable", validFrequencyTable)
# 
# test_data_set_1 <- read.csv("/Users/adw96/Documents/Research w Bunge/CatchAll/10000_geo_.5_2.csv", header = F)
# ft1 <- new("FrequencyTable", table = test_data_set_1)
# ft1
# class(ft1)
# 
# wlrm_transformed(ft1)
