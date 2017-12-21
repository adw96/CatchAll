SingleExponentialModel <- function(s, r, observedCount, n, 
                        s0Init, frequency, 
                        lnSFactorial, sumlnFFactorial, 
                        maximumObservation) {
  #### Fits
  numParams <- 1
  fits <- SingleExponentialFits(r, n, s, frequency)
  mlesSExp <- fits$mlesSExp
  
  if (fits$check == 1) {
    fitsExtended <- rep(NA, frequency[r]*4)
    fitsExtended[1:frequency[r]] <- fits$fitsCount
    fitsExtended[(frequency[r] + 1):(frequency[r]*4)] <- 
      s[r]*(1/mlesSExp)*(mlesSExp/(1+mlesSExp))^((frequency[r] + 1):(frequency[r]*4))
    
    sHatSubset <- (n[r]*s[r])/(n[r]-s[r])
    
    sHatTotal <- sHatSubset+(s[maximumObservation]-s[r])
    part1 <- lnSFactorial[r]-sumlnFFactorial[r]
    part2 <- (n[r]-s[r])*log(mlesSExp)-n[r]*log(1+mlesSExp)

    calculate_analysis_variables_result <- CalculateAnalysisVariables(part1, part2, numParams, r, fits$fitsCount, 
                                                                      fitsExtended, 
                                                                      s, 2, frequency, observedCount)  
    ## standard error
    se <- sHatSubset/sqrt(n[r]-s[r])
    bounds <- GetConfidenceBounds(r, se, sHatSubset, s, maximumObservation)
    
    output <- data.frame("Model" = "SingleExponential", 
                         "Cutoff" = r, 
                         "Estimate" = CheckOutput(sHatTotal), 
                         "SE" = CheckOutput(se), 
                         "LCB"= CheckOutput(bounds$lcb), 
                         "UCB" = CheckOutput(bounds$ucb), 
                         "chiSq" = CheckOutput(calculate_analysis_variables_result$chiSq),
                         "AIC" = CheckOutput(calculate_analysis_variables_result$AIC), 
                         "AICc" = CheckOutput(calculate_analysis_variables_result$AICc), 
                         "GOF0" = CheckOutput(calculate_analysis_variables_result$GOF0), 
                         "GOF5" = CheckOutput(calculate_analysis_variables_result$GOF5),
                         "T1"=CheckOutput(mlesSExp),
                         "T2"=NA,
                         "T3"=NA,
                         "T4"=NA,
                         "T5"=NA,
                         "T6"=NA,
                         "T7"=NA)
    
  }
}

SingleExponentialFits <- function(r, n, s, frequency) {
  mlesSExp <- (n[r]/s[r])-1
  
  # print(paste("n[r]: ", n[r], sep = " "))
  # print(paste("s[r]: ", s[r], sep = " "))
  
  fitsCount <- s[r]*(1/mlesSExp)*(mlesSExp/(1+mlesSExp))^(1:(frequency[r]))
  fitsCheck <- ifelse(min(fitsCount) < 0, 0, 1)
  
  # print("FITSCOUNT HERE")
  # print(fitsCount)
  list("fitsCount"=fitsCount, "check"=fitsCheck,
       "mlesSExp"=mlesSExp)
}