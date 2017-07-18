models <- c("wlrm_transformed", "wlrm_untransformed")
frequency_table <- test_data_set_1[test_data_set_1[,2]>0, ]
CatchAll <- function(frequency_table) {
  
  output <- data.frame()
  ## global variables that were declared:
  # obsMax, freqMax, freqTau10
  # ACE1Tau10Rule, 
  # freq, observedCount, s
  # n, lnSFactorial, sumlnFFactorial, sumFlnFFactorial
  # w, y, lnW, lnY
  # WLRMSwitch, WLRMGOF0
  # GOF5Test, bestGOF0, bestAICc
  BigChiSq = 1000000000.0
  Criteria = 0.0000000000000001
  maxIter = 100000
  
  
  # Frequency Range for Analysis, model dependent
  # 1=Poisson, 2=1-expl, 3=2-expl, 4=3-exp, 5=4-exp, 6=TWLRM, 7=UWLRM, 8=nonparametric
  fMin <- c(4, 4, 6, 8, 10, 5, 5)
  numParameters <- c(1, 1, 3, 5, 7, 2, 2)
  fMinFlag <- c(0, 0, 0, 0, 0, 0, 0)
  modelDescription <- c("Poisson", 
                        "SingleExponential", 
                        "TwoMixedExponential", 
                        "ThreeMixedExponential", 
                        "FourMixedExponential", 
                        "LogTransformedWeightedLinearRegression", 
                        "UntransformedWeightedLinearRegression", 
                        "Chao1", "ACE", "ACE1")
  maxGoodnessOfFit <- 10
  bestCount <- rep(NA, 4)
  
  ################################
  ## Create input data
  ################################
  positive_frequency_table <- frequency_table[frequency_table[,2]>0, ]
  numberOfRows <- dim(positive_frequency_table)[1]
  if (numberOfRows < 2) {
    stop("Not enough rows?")
    break
  }
  maximumObservation <- numberOfRows # yes, this is correct
  
  bestGOF0  <- array(dim = c(4, 9, maximumObservation))
  bestAICc <- array(dim = c(4, 9, maximumObservation))
  GOFTest <- array(dim = c(9, maximumObservation))
  
  frequency <- positive_frequency_table[, 1]
  observedCount <- positive_frequency_table[, 2]
  
  frequencyTau10 <- max(which(frequency <= 10 & observedCount > 0))+1
  
  ################################
  ## Basic Statistics
  ################################
  
  if (all(positive_frequency_table[,1] == 1:length(positive_frequency_table[,1]))) {
    a <- maximumObservation
  } else {
    a <- min(which(positive_frequency_table[,1] != 1:length(positive_frequency_table[,1])))
  }
  frequencyMaximum <- a - 1
  fMinFlag[maximumObservation >= fMin & c(rep(TRUE, 5), rep(FALSE,2))] <- 1
  fMinFlag[frequencyMaximum >= fMin & c(rep(FALSE, 5), rep(TRUE,2))] <- 1
  
  s <- cumsum(observedCount)
  n <- cumsum(frequency * observedCount)
  
  logFactorial <- function(x) ifelse(x == 0, 0, sum(log(1:x)))
  
  lnSFactorial <- mapply(logFactorial, s)
  lnFFactorial <- mapply(logFactorial, observedCount)
  sumlnFFactorial <- cumsum(lnFFactorial)# NA#mapply(logFactorial, lnFFactorial)
  # 
  lnIFactorial <- observedCount *  mapply(logFactorial, frequency)
  sumFlnFFactorial <- cumsum(lnIFactorial)
  
  # ratios
  y <- (1 + 1:(frequencyMaximum-1)) * 
    observedCount[c(2:frequencyMaximum)] / 
    observedCount[c(1:(frequencyMaximum-1))]
  lnY <- log(y)
  w <- observedCount[c(1:(frequencyMaximum-1))]^3 / 
    ((2:(frequencyMaximum))^2 * 
       observedCount[c(2:(frequencyMaximum))] * 
       (observedCount[c(1:(frequencyMaximum-1))] + 
          observedCount[c(2:(frequencyMaximum))]))
  lnW <- (observedCount[c(1:(frequencyMaximum-1))] * 
            observedCount[c(2:(frequencyMaximum))]) /
    (observedCount[c(1:(frequencyMaximum-1))] + 
       observedCount[c(2:(frequencyMaximum))])
  WLRMSwitch <- rep(0, frequencyMaximum-1)
  
  ## TODO
  #  ACE1Tau10Rule <- ACE1Tau10()
  
  bestCount <- rep(NA, 4)
  
  ################################
  ## Poisson Model
  ################################
  modelNumber <- 1
  if (fMinFlag[modelNumber]==1) {
    
    frequencyMinimum <- 1 + max(which(frequency < fMin[modelNumber]))
    
    for (r in frequencyMinimum:maximumObservation) {
      ################################
      ## Poisson Fits
      ################################
      poison_results <- PoisonModel(s, r, observedCount, n,
                                   s0Init, frequency, numParameters[modelNumber],
                                   lnSFactorial, sumlnFFactorial, sumFlnFFactorial)
      output <- rbind(output, poison_results)
      
    }
  }
  
  output
}
source("R/miscellaneous.R")
source("R/poisson.R")
CatchAll(test_data_set_1)
