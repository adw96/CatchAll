models <- c("wlrm_transformed", "wlrm_untransformed")

CatchAll <- function(frequency_table) {
  
  estimate_richness(frequency_table)
  
}

estimate_richness <- function(frequency_table) {
  results <- list()
  cutoff_max <- 12
  for (cutoff in 4:cutoff_max) {
    sft1 <- new("SplitFrequencyTable", 
                table = frequency_table@table, 
                cutoff = cutoff)
    
    for (model in models) {
      this_model <- eval(call(model, sft1))
      results <- append(results, this_model)
    }
    
  }
  format_catchall(results)
}

format_catchall <- function(catchall_output) {
  n <- length(catchall_output)
  models <- unlist(lapply(catchall_output, function(object) object@model))
  cutoffs <- unlist(lapply(catchall_output, function(object) object@cutoff))
  estimates <- unlist(lapply(catchall_output, function(object) object@estimate))
  errors <- unlist(lapply(catchall_output, function(object) object@se))
  lcbs <- unlist(lapply(catchall_output, function(object) object@ci[1]))
  ucbs <- unlist(lapply(catchall_output, function(object) object@ci[2]))
  df <- data.frame("Model"=models,
                   "Cutoff"=cutoffs,
                   "Estimate"=round(estimates),
                   "SE"=round(errors, 2),
                   "LowerCB"=floor(lcbs),
                   "UpperCB" = ceiling(ucbs))
  df <- df[with(df, order(Model, Cutoff)), ]
  print(df)
}

#CatchAll(ft1)
