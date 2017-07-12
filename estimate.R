setClass("Estimate", 
         slots = c(estimate = "numeric", 
                   se = "numeric",
                   model = "character",
                   cutoff = "numeric",
                   ci = "numeric",
                   success = "logical",
                   aic = "numeric"))

setMethod("initialize", "Estimate", 
          function(.Object, 
                   estimate = numeric(0), 
                   se = numeric(0), 
                   model = character(0),
                   cutoff = numeric(0),
                   ci = c(numeric(0), numeric(0)), 
                   success = logical(0), 
                   aic = numeric(0), 
                   ...) {
            .Object <- callNextMethod(.Object, ...)
            .Object@estimate <- estimate
            .Object@se <- se
            .Object@cutoff <- cutoff
            .Object@ci <- ci
            .Object@success <- success
            .Object@model <- model
            #            if(.Object@estimate > 0) .Object@success <- TRUE
            .Object@aic <- aic
            .Object
          })

validEstimate <- function(object) {
  errors <- character()
  
  length_ci <- length(object@ci)
  if (!(length_ci %in% c(0, 2))) {
    msg <- paste("CI is length ", length_ci, ".  Should be 0 or 2", sep = "")
    errors <- c(errors, msg)
  }
  
  if (object@success != 1 & is.numeric(object@estimate)) {
    msg <- "Success is false but an estimate is present."
    errors <- c(errors, msg)
  }
  
  if (length(errors) == 0) TRUE else errors
}

setValidity("Estimate", validEstimate)

printEstimate <- function(object) {
  print(data.frame("Model" = ifelse(length(object@model) != 0, object@model, NA),
                   "Cutoff" = ifelse(length(object@cutoff) != 0, object@cutoff, NA),
                   "Estimate" = ifelse(length(object@estimate) != 0, object@estimate, NA),
                   "SE" = ifelse(length(object@se) != 0, object@se, NA),
                   "LowerCB" = ifelse(length(object@ci) != 0, object@ci[1], NA),
                   "UpperCB" = ifelse(length(object@ci) != 0, object@ci[2], NA)))
}

setMethod("show", "Estimate", printEstimate)

# length(estimate1@model)
# estimate1
# new("Estimate")
# new("Estimate", model = "Hi", cutoff = 5, estimate = 1000)
# 
# 
# estimate1 <- new("Estimate", estimate = 3000, se = 200, ci = c(2000, 4000), success = TRUE)
# estimate2 <- new("Estimate", estimate = 3000, ci = c(2000, 4000))
# estimate3 <- new("Estimate", estimate = 3000, ci = c(2000))
# estimate4 <- new("Estimate", estimate = 3000, se = 2000)
# 
# validObject(estimate1)
# validObject(estimate2)
# validObject(estimate3)
# validObject(estimate4)
