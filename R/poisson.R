#' PoissonModel
#' 
#' A model to estimate the number of missing taxa under a Poisson Model
#' 
#' @param frequency_count A frequency count table
#' @param cutoff The largest frequency to use for predicting f0 
#' 
#' @importFrom magrittr  "%>%"
#' @importFrom magrittr  "%<>%"
#' @importFrom stats uniroot
#' 
#' @examples 
#' data(butterfly)
#' PoissonModel(butterfly, 4)
#' 
#' @export
PoissonModel <- function(frequency_count, 
                         cutoff = NULL) {
  
  if (is.null(cutoff)) {
    cutoff <- Inf
  }
  
  frequency_count <- CheckInput(frequency_count)
  included <- frequency_count[frequency_count$j <= cutoff, ]
  excluded <- frequency_count[frequency_count$j > cutoff, ]
  
  # s = sum f_j
  cc <- included$f %>% sum
  cc_excluded  <- excluded$f %>% sum
  
  # n = sum j f_j
  nn <- crossprod(included$j, included$f)
  
  ## MLE for lambda is solution to 
  ## (1-exp(-lambda))/lambda = c/n
  poisson_fn <- function(lambda) {
    (1-exp(-lambda))/lambda - cc/nn
  }
  
  # eqn nonlinear; so use Newton's method
  lambda_hat <- uniroot(poisson_fn, c(0.0001, 1000000))$root
  
  ccc_subset <- cc / (1-exp(-lambda_hat)) 
  ccc_hat <- ccc_subset + cc_excluded
  
  ccc_se <- sqrt(ccc_subset/(exp(lambda_hat)-1-lambda_hat))
  
  out <- list("model" = "Poisson", 
              "cutoff" = cutoff, 
              "estimate" = ccc_hat, 
              "se" = ccc_se,
              "lambda_hat"= lambda_hat)
  class(out) <- c("richnessEstimate", class(out))
  out
}

#' @export
print.richnessEstimate <- function(est) {
  print(data.frame("Model" = est$model, 
                   "Cutoff" = est$cutoff, 
                   "Estimate" = est$estimate, 
                   "Standard Error" = est$se))
}

#' CheckInput
#' 
#' Check and fix the formatting of a frequency count table
#' 
#' @param frequency_count The frequency count table to check
#' @return  The checked and fixed frequency count table 
#' 
#' @export
CheckInput <- function(frequency_count) {
  
  
  if(!(class(frequency_count) %in% c("matrix", "data.frame"))) stop("Input should be a matrix or a data frame")
  
  if(length(dim(frequency_count)) != 2) stop("Input should have 2 columns")
  
  if(any(frequency_count[,2] %% 1 != 0)) stop("Second input column not integer-valued; should be counts")
  
  if(!all(rank(frequency_count[,1]) == 1:length(frequency_count[,1]))) warning("Frequency count format, right?")
  
  if (frequency_count[,1] %>% class == "factor") {
    frequency_count[,1] %<>% as.character %>% as.integer
  }
  
  colnames(frequency_count)  <- c("j", "f")
  frequency_count
}

PoissonModel0 <- function(s, r, observedCount, n, 
                          s0Init, frequency, 
                          lnSFactorial, sumlnFFactorial, sumFlnFFactorial,
                          maximumObservation) {
  ################################
  ## Poisson Fits
  ################################
  numParams <- 1 
  fitsCheck <- 1
  s0Init <- s[r]/(1-observedCount[1]/n[r]) - s[r]
  poissonConstant <- n[r]/s[r]
  momentsInit <- n[r]/(s0Init + s[r])
  
  ## find maximum likelihood estimator
  mleNonztIter <- 10000
  mleNonztTolerance <- 10^-6
  PoissonedRootResult <- BracetRoot(poissonConstant, momentsInit)
  if (PoissonedRootResult$conclusion == 1) {
    if (PoissonedRootResult$f1 < 0) {
      root <- PoissonedRootResult$x1
      dx <- PoissonedRootResult$x2 - PoissonedRootResult$x1
    } else {
      root <- PoissonedRootResult$x2
      dx <- PoissonedRootResult$x1 - PoissonedRootResult$x2
    }
    
    i <- 0
    while (i < mleNonztIter) {
      dx <- dx/2
      xmid <- root + dx
      f2 <- xmid/(1-exp(-xmid)) - poissonConstant
      root <- ifelse(f2 <= 0, xmid, root)
      i <- i + 1
      if (abs(dx) < mleNonztTolerance || f2 == 0) {
        mlesPoisson <- root
        i <- mleNonztIter
      }
    }
  } else {
    warning("no convergence for Poisson mles");
    fitsCheck <- 0
  }
  
  # dummy initializer, 
  fitsCount <- rep(0.0, times = frequency[r] + 1)
  
  # this is after calling the getPoissonModel
  if (fitsCheck == 1) {
    mlesPoissonExponential <- exp(-mlesPoisson)
    
    lnFactorial <- 0.0
    #sanity check to make lnFactorial again
    
    #bad for loop but lnFactorial and fitsCount now work
    for (t in 1:frequency[r]) {
      lnFactorial <- lnFactorial + log(t)
      fitsCount[t] <- log(s[r]) + log(mlesPoissonExponential) +
        ((t)*log(mlesPoisson)) - log(1-mlesPoissonExponential) -
        lnFactorial
      
      fitsCount[t] <- exp(fitsCount[t])
    }
    
    message("lnFactorial after method")
    message(lnFactorial)
    # correct lnFactorial
    
    # fitsCount <- log(s[r]) + log(mlesPoissonExponential) + 
    #   (1:(frequency[r]))*log(mlesPoisson) - log(1-mlesPoissonExponential) -
    #   lnFactorial
    # 
    # fitsCount <- exp(fitsCount)
    
    if (sum(fitsCount < 0) >= 1) fitsCheck = 0
    
    if (fitsCheck == 1) {
      extendedTau <- frequency[r]*4
      fitsExtended <- rep(NA, extendedTau)
      fitsExtended[1:(frequency[r])] <- fitsCount[1:(frequency[r])] 
      
      fitsExtended[(frequency[r]+1):extendedTau] <-
        exp(log(s[r]) + log(mlesPoissonExponential) +
              ((frequency[r]+1):extendedTau * log(mlesPoisson)) -
              log(1-mlesPoissonExponential) - log(factorial((frequency[r]+1):extendedTau)))
      
      #check frequency
      
      sHatSubset <- s[r]*mlesPoissonExponential/(1-mlesPoissonExponential) + s[r]
      sHatTotal <- sHatSubset + (s[maximumObservation] - s[r])
      
      part1 <- lnSFactorial[r] - sumlnFFactorial[r]
      part2 <- (-s[r]*mlesPoisson) + (n[r]*log(mlesPoisson)) - sumFlnFFactorial[r] - (s[r]*log(1-mlesPoissonExponential))
      
      #paste("part1: ", part1)
      calculate_analysis_variables_result <- CalculateAnalysisVariables(part1, part2, numParams, r, fitsCount, 
                                                                        fitsExtended,
                                                                        s, 1, frequency, observedCount)  
      ## standard error
      se <- sHatSubset/(exp(mlesPoisson)-1-mlesPoisson)
      standard_error_flag <- ifelse(se>0, 1, 0)
      
      ## confidence bounds
      if (standard_error_flag == 1){
        se <- sqrt(se)
        boundsCheck <- GetConfidenceBounds(r, se, sHatSubset, s, maximumObservation)
      }
      
      
      output <- data.frame("Model" = "Poisson", 
                           "Cutoff" = r, 
                           "Estimate" = CheckOutput(sHatTotal), 
                           "SE" = CheckOutput(se), 
                           "LCB"= CheckOutput(boundsCheck$lcb), 
                           "UCB" = CheckOutput(boundsCheck$ucb), 
                           "chiSq" = CheckOutput(calculate_analysis_variables_result$chiSq),
                           "AIC" = CheckOutput(calculate_analysis_variables_result$AIC), 
                           "AICc" = CheckOutput(calculate_analysis_variables_result$AICc), 
                           "GOF0" = CheckOutput(calculate_analysis_variables_result$GOF), 
                           "GOF5" = CheckOutput(calculate_analysis_variables_result$GOF5),
                           "T1"=CheckOutput(mlesPoisson),
                           "T2"=NA,
                           "T3"=NA,
                           "T4"=NA,
                           "T5"=NA,
                           "T6"=NA,
                           "T7"=NA)
      
    }
  } 
}