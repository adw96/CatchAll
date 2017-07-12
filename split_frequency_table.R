setClass("SplitFrequencyTable", 
         slots = c(table = "data.frame",
                   f1 = "integer",
                   n = "integer",
                   cutoff = "numeric", 
                   head = "data.frame",
                   tail = "data.frame"))

setMethod("initialize", "SplitFrequencyTable", 
          function(.Object, 
                   table = data.frame(), 
                   f1 = integer(0),
                   n = integer(0),
                   cutoff = Inf, 
                   head = data.frame(), 
                   tail = data.frame(),
                   ...) {
            .Object <- callNextMethod(.Object, ...)
            clean_table <- table[table[,2] > 0, ]
            .Object@table <- clean_table[clean_table[,2] > 0, ]
            .Object@f1 <- .Object@table[1, 2]
            .Object@n <- sum(.Object@table[ , 2])
            
            .Object@cutoff <- cutoff
            
            .Object@head <- clean_table[clean_table[,1] <= cutoff, ]
            .Object@tail <- clean_table[clean_table[,1] > cutoff, ]
            .Object
          })

validSplitFrequencyTable <- function(object) {
  errors <- character()
  
  width_table <- dim(object@table)[2]
  if (!(width_table %in% c(0, 2))) {
    msg <- paste("Table has ", width_table, "columns.  Should be 0 or 2", sep = "")
    errors <- c(errors, msg)
  }
  
  if (object@cutoff < 0) {
    msg <- "Cutoff is negative."
    errors <- c(errors, msg)
  }
  
  ## check that is a frequency count table
  
  ## check that head and tail combined to make table
  
  ## check that cut off is the maximum for head
  
  if (length(errors) == 0) TRUE else errors
}

setValidity("SplitFrequencyTable", validSplitFrequencyTable)

# sft1 <- new("SplitFrequencyTable", table = test_data_set_1, cutoff = 6)
# sft1
# class(sft1)
# wlrm(sft1)
# 
# sft1 <- new("SplitFrequencyTable", table = test_data_set_1, cutoff = 12)
# sft1
# wlrm(sft1)
# wlrm(ft1)
