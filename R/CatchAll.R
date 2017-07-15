models <- c("wlrm_transformed", "wlrm_untransformed")
frequency_table <- test_data_set_1[test_data_set_1[,2]>0, ]
CatchAll <- function(frequency_table) {
  
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
  # 1=Poisson, 2=1-expl, 3=2-expl, 4=3-exp, 5=4-exp, 6=TWLRM, 7=UWLRM,
  # 8=nonparametric
  fMin <- c(0, 4, 4, 6, 8, 10, 5, 5)
  numParameters <- c(0, 1, 1, 3, 5, 7, 2, 2)
  fMinFlag <- c(0, 0, 0, 0, 0, 0, 0, 0)
  modelDescription <- c("Model", "Poison", 
                        "SingleExponential", "TwoMixedExponential", "ThreeMixedExponential", "FourMixedExponential", 
                        "LogTransformedWeightedLinearRegression", "UntransformedWeightedLinearRegression", 
                        "Chao1", "ACE", "ACE1")
  maxGoodnessOfFit <- 10
  bestCount <- rep(NA, 4)
  
  ################################
  ## Create input data
  ################################
  positive_frequency_table <- frequency_table[frequency_table[,1]>0, ]
  numberOfRows <- dim(positive_frequency_table)[1]
  
  if (numberOfRows > 1) {
    maximumObservation <- numberOfRows
    
    frequency <- rep(0, maximumObservation + 1)
    observedCount <- rep(0, maximumObservation + 1)
    
    bestGOF0  <- array(dim = c(4, 9, maximumObservation + 1))
    bestAICc <- array(dim = c(4, 9, maximumObservation + 1))
    GOFTest <- array(dim = c(9, maximumObservation + 1))
    
    frequency[1+positive_frequency_table[, 1]] <- positive_frequency_table[, 1]
    observedCount[1+positive_frequency_table[, 1]] <- positive_frequency_table[, 2]
    
    frequencyTau10 <- max(which(frequency <= 10 & observedCount > 0))+1
    
  }
  
  ################################
  ## Basic Statistics
  ################################
  
  a <- maximumObservation
  frequencyMaximum <- a - 1
  fMinFlag[maximumObservation >= fMin & c(FALSE, rep(TRUE, 5), rep(FALSE,2))] <- 1
  fMinFlag[frequencyMaximum >= fMin & c(rep(FALSE, 6), rep(TRUE,2))] <- 1
  
  
  s <- cumsum(observedCount)
  n <- cumsum(frequency * observedCount)
  
  lnSFactorial <- cumsum(log(s))
  lnFFactorial <- cumsum(log(observedCount))
  sumlnFFactorial <- cumsum(lnFFactorial)
  
  lnIFactorial <- observedCount * cumsum(log(frequency))
  sumFlnFFactorial <- cumsum(lnIFactorial)
  
  # ratios
  observedCountNoF0 <- observedCount[-1]
  y <- (1 + 1:(frequencyMaximum-1)) * 
    observedCountNoF0[c(2:frequencyMaximum)] / 
    observedCountNoF0[c(1:(frequencyMaximum-1))]
  lnY <- log(y)
  w <- observedCountNoF0[c(1:(frequencyMaximum-1))]^3 / 
    ((2:(frequencyMaximum))^2 * 
       observedCountNoF0[c(2:(frequencyMaximum))] * 
       (observedCountNoF0[c(1:(frequencyMaximum-1))] + 
          observedCountNoF0[c(2:(frequencyMaximum))]))
  lnW <- (observedCountNoF0[c(1:(frequencyMaximum-1))] * 
            observedCountNoF0[c(2:(frequencyMaximum))]) /
    (observedCountNoF0[c(1:(frequencyMaximum-1))] + 
       observedCountNoF0[c(2:(frequencyMaximum))])
  WLRMSwitch <- rep(0, frequencyMaximum-1)
  
  
  
  bestCount <- rep(NA, 4)
  
  ################################
  ## Poisson Model
  ################################
  modelNumber <- 2
  if (fMinFlag[modelNumber]==1) {
    numParams <- numParameters[modelNumber]
    
    LetterT <- rep(NA, 8)
    frequencyMinimum <- max(which(frequency < fMin[modelNumber]))
    
    for (r in frequencyMinimum:maximumObservation) {
      ################################
      ## Poisson Fits
      ################################
      fitsCheck <- 1
      s0Init <- s[r]/(1-observedCount[1]/n[r]) - s[r]
      poisonConstant <- n[r]/s[r]
      momentsInit <- n[r]/(s0Init + s[r])
      
      ## find maximum likelihood estimator
      mleNonztIter <- 10000
      mleNonztTolerance <- 10^-6
      PoisonedRootResult <- BracetRoot(poisonConstant, momentsInit)
      if (PoisonedRootResult$conclusion == 1) {
        if (PoisonedRootResult$f1 < 0) {
          root <- PoisonedRootResult$x1
          dx <- PoisonedRootResult$x2 - PoisonedRootResult$x1
        } else {
          root <- PoisonedRootResult$x2
          dx <- PoisonedRootResult$x1 - PoisonedRootResult$x2
        }
        
        i <- 0
        while (i < mleNonztIter) {
          dx <- dx/2
          xmid <- root + dx
          f2 <- xmid/(1-exp(-xmid)) - poisonConstant
          root <- ifelse(f2 <= 0, xmid, root)
          i <- i + 1
          if (abs(dx) < mleNonztTolerance || f2 == 0) {
            mlesPoison <- root
            i <- mleNonztIter
          }
        }
      } else {
        warning("no convergence for Poisson mles");
        fitsCheck <- 0
      }
      
      if (fitsCheck == 1) {
        mlesPoisonExponential <- exp(-mlesPoison)
        lnFactorial <- log(cumsum(frequency[r-1]))
        fitsCount <- log(s[r]) + log(mlesPoisonExponential) + 
          (1:(frequency[r]-1))*log(mlesPoison) - log(1-mlesPoisonExponential) -
          lnFactorial
        
        fitsCount <- exp(fitsCount)
        
        if (sum(fitsCount < 0) >= 1) fitsCheck = 0
        
        if (fitsCheck == 1) {
          extendedTau <- frequency[r]*4
          fitsExtended <- rep(NA, extendedTau + 1)
          fitsExtended[2:(frequency[r]+1)] <- fitsCount[2:(frequency[r]+1)] 
          lnFactorial
          fitsExtended[(frequency[r]+2):extendedTau] <- 
            exp(log(s[r]) + log(mlesPoisonExponential) +
            ((frequency[r]+2):extendedTau * log(mlesPoison)) -
            log(1-mlesPoisonExponential) - log(factorial((frequency[r]+2):extendedTau)))
          
          
          sHatSubset <- s[r]*mlesPoisonExponential/(1-mlesPoisonExponential) + s[r]
          sHatTotal <- sHatSubset + (s[maximumObservation] - s[r])
          print(sHatTotal)
          part1 <- lnSFactorial[r] - sumlnFFactorial[r]
          part2 <- (-s[r]*mlesPoison) + (n[r]*log(mlesPoison)) - sumFlnFFactorial[r] - (s[r]*log(1-mlesPoisonExponential))

          GOF5Test[modelNumber, r] <- CalculateAnalysisVariables(part1, part2, 
                                                                 numParam, r, fitsCount, fitsExtended, maxGoodnessOfFit)  
          ## TODO: define GOF5Test
                    print("Hi")
        }
      } 
    }
  }
}
CatchAll(test_data_set_1)
warnings()

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
