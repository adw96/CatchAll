PpoissonModel <- function(s, r, observedCount, n, 
                        s0Init, frequency, numParams, 
                        lnSFactorial, sumlnFFactorial, sumFlnFFactorial) {
  ################################
  ## Poisson Fits
  ################################
  fitsCheck <- 1
  s0Init <- s[r]/(1-observedCount[1]/n[r]) - s[r]
  ppoissonConstant <- n[r]/s[r]
  momentsInit <- n[r]/(s0Init + s[r])
  
  ## find maximum likelihood estimator
  mleNonztIter <- 10000
  mleNonztTolerance <- 10^-6
  PpoissonedRootResult <- BracetRoot(ppoissonConstant, momentsInit)
  if (PpoissonedRootResult$conclusion == 1) {
    if (PpoissonedRootResult$f1 < 0) {
      root <- PpoissonedRootResult$x1
      dx <- PpoissonedRootResult$x2 - PpoissonedRootResult$x1
    } else {
      root <- PpoissonedRootResult$x2
      dx <- PpoissonedRootResult$x1 - PpoissonedRootResult$x2
    }
    
    i <- 0
    while (i < mleNonztIter) {
      dx <- dx/2
      xmid <- root + dx
      f2 <- xmid/(1-exp(-xmid)) - ppoissonConstant
      root <- ifelse(f2 <= 0, xmid, root)
      i <- i + 1
      if (abs(dx) < mleNonztTolerance || f2 == 0) {
        mlesPpoisson <- root
        i <- mleNonztIter
      }
    }
  } else {
    warning("no convergence for Poisson mles");
    fitsCheck <- 0
  }
  
  if (fitsCheck == 1) {
    mlesPpoissonExponential <- exp(-mlesPpoisson)
    lnFactorial <- logFactorial(frequency[r])
    fitsCount <- log(s[r]) + log(mlesPpoissonExponential) + 
      (1:(frequency[r]))*log(mlesPpoisson) - log(1-mlesPpoissonExponential) -
      lnFactorial
    
    fitsCount <- exp(fitsCount)
    
    if (sum(fitsCount < 0) >= 1) fitsCheck = 0
    
    if (fitsCheck == 1) {
      extendedTau <- frequency[r]*4
      fitsExtended <- rep(NA, extendedTau)
      fitsExtended[1:(frequency[r])] <- fitsCount[1:(frequency[r])] 
      
      
      fitsExtended[(frequency[r]+1):extendedTau] <- 
        exp(log(s[r]) + log(mlesPpoissonExponential) +
              ((frequency[r]+1):extendedTau * log(mlesPpoisson)) -
              log(1-mlesPpoissonExponential) - log(factorial((frequency[r]+1):extendedTau)))
      
      sHatSubset <- s[r]*mlesPpoissonExponential/(1-mlesPpoissonExponential) + s[r]
      sHatTotal <- sHatSubset + (s[maximumObservation] - s[r])
      
      part1 <- lnSFactorial[r] - sumlnFFactorial[r]
      part2 <- (-s[r]*mlesPpoisson) + (n[r]*log(mlesPpoisson)) - sumFlnFFactorial[r] - (s[r]*log(1-mlesPpoissonExponential))
      
      calculate_analysis_variables_result <- CalculateAnalysisVariables(part1, part2, numParams, r, fitsCount, 
                                                                        fitsExtended, maxGoodnessOfFit, 
                                                                        s, modelNumber, frequency)  
      #print(calculate_analysis_variables_result)
      ## standard error
      se <- sHatSubset/(exp(mlesPpoisson)-1-mlesPpoisson)
      standard_error_flag <- ifelse(se>0, 1, 0)
      
      ## confidence bounds
      if (standard_error_flag == 1){
        se <- sqrt(se)
        boundsCheck <- GetConfidenceBounds(r, se, sHatSubset, s, maximumObservation)
      }
      
      
      output <- data.frame("Model" = modelDescription[2], 
                           "Cutoff" = r, 
                           "Estimate" = CheckOutput(sHatTotal), 
                           "SE" = CheckOutput(se), 
                           "LCB"= CheckOutput(boundsCheck$lcb), 
                           "UCB" = CheckOutput(boundsCheck$ucb), 
                           "chiSq" = CheckOutput(calculate_analysis_variables_result$chiSq),
                           "AIC" = CheckOutput(calculate_analysis_variables_result$AIC), 
                           "AICc" = CheckOutput(calculate_analysis_variables_result$AICc), 
                           "GOF0" = CheckOutput(calculate_analysis_variables_result$GOF), 
                           "GOF5" = CheckOutput(calculate_analysis_variables_result$GOF5))
      
    }
  } 
}