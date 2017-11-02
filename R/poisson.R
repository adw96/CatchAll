PoissonModel <- function(s, r, observedCount, n, 
                        s0Init, frequency, 
                        lnSFactorial, sumlnFFactorial, sumFlnFFactorial,
                        maximumObservation) {
  ################################
  ## Poisson Fits
  ################################
  print("s")
  print(s)
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
  
  # this is after calling the getPoissonModel
  if (fitsCheck == 1) {
    mlesPoissonExponential <- exp(-mlesPoisson)
    # can't find logFactorial
    #			lnFactorial = 0.0;
    # C#: for (int t = 1; t <= freq[r]; t++)
    #{
    #  lnFactorial = lnFactorial + Math.Log(t);
    #
    #
    lnFactorial <- mapply(logFactorial, 1:frequency[r])
    fitsCount <- log(s[r]) + log(mlesPoissonExponential) + 
      (1:(frequency[r]))*log(mlesPoisson) - log(1-mlesPoissonExponential) -
      lnFactorial
    
    fitsCount <- exp(fitsCount)
    
    if (sum(fitsCount < 0) >= 1) fitsCheck = 0
    
    if (fitsCheck == 1) {
      extendedTau <- frequency[r]*4
      fitsExtended <- rep(NA, extendedTau)
      fitsExtended[1:(frequency[r])] <- fitsCount[1:(frequency[r])] 
      
      
      fitsExtended[(frequency[r]+1):extendedTau] <- 
        exp(log(s[r]) + log(mlesPoissonExponential) +
              ((frequency[r]+1):extendedTau * log(mlesPoisson)) -
              log(1-mlesPoissonExponential) - log(factorial((frequency[r]+1):extendedTau)))
      
      sHatSubset <- s[r]*mlesPoissonExponential/(1-mlesPoissonExponential) + s[r]
      sHatTotal <- sHatSubset + (s[maximumObservation] - s[r])
      
      part1 <- lnSFactorial[r] - sumlnFFactorial[r]
      part2 <- (-s[r]*mlesPoisson) + (n[r]*log(mlesPoisson)) - sumFlnFFactorial[r] - (s[r]*log(1-mlesPoissonExponential))
      
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