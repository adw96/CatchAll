LogTWLRModel <- function(lnW, lnY,  WLRMSwitch, s, r, observedCount, n, 
                                   s0Init, frequency, 
                                   lnSFactorial, sumlnFFactorial, 
                                   maximumObservation) {
  
  bigChiSq <- 1000000000
  numParams <- 7
  fits <- LogTWLRFits(lnW, lnY, r, n, s, frequency, observedCount)
  
  if (fits$check == 1) {
    gamma <- fits$gamma
    k <- fits$k
    delta <- fits$delta
    MSE <- fits$MSE
    fitsCount <- fits$fitsCount
    
    sHatSubset <- fitsCount[0] + s[r]
    
    sHatTotal <- sHatSubset + (s[maximumObservation] - s[r])
    chiSqAll <- ChiSqFunction(r, fitsCount, numParams, frequency, observedCount, s)
    
    df <- frequency[r] - numParams
    flag <- 1
    test <- (chiSqAll - df) / sqrt(2 * df)
    GOF0 <- 0
    #where did maxGOF and BigChiSq come from...
    if (test < maxGOF & chiSqAll < BigChiSq) {
      GOF0 <- GoodnessOfFit(chiSqAll, df)
    } else {
      flag <- 0
    }
  }
  
  GOF0Check <- flag
  WLRMSwitch[frequency[r]] <- GOF0 #is it ok for switch and GOF0 to be the same?
  varGamma <- sum((1:r) * (1:r) * lnW[1:r])
  varGamma <- varGamma * MSE / k
  
  se <- (s[r] * fitsCount[0] / sHatSubset) + (exp(-2.0 * gamma) *
                                                observedCount[frequency[1]] * (varGamma * observedCount[freq[1]] + 1.0))
  
  SEFlag <- 0
  if (SE > 0) {
    se <- sqrt(se)
    seFlag <- 1
  }
  
  boundsCheck <- 0
  if(seFlag == 1) {
    boundsCheck <- GetConfidenceBounds(r, se, sHatSubset, maximumObservation)
  }
  
  output <- data.frame("Model" = "LogTWLRModel", 
                       "Cutoff" = r, 
                       "Estimate" = CheckOutput(sHatTotal), 
                       "SE" = CheckOutput(se), 
                       "LCB"= CheckOutput(boundsCheck$lcb), 
                       "UCB" = CheckOutput(boundsCheck$ucb), 
                       "chiSq" = CheckOutput(chiSqAll),
                       "AIC" = CheckOutput(0), 
                       "AICc" = CheckOutput(0), 
                       "GOF0" = CheckOutput(GOF0), 
                       "GOF5" = CheckOutput(0),
                       "T1"=CheckOutput(gamma),
                       "T2"=CheckOutput(delta),
                       "T3"=CheckOutput(MSE),
                       "T4"=NA,
                       "T5"=NA,
                       "T6"=NA,
                       "T7"=NA)
  
  
  
  
}


#want to return gamma, delta, MSE, k, fitsCheck, fitsCount
LogTWLRFits <- function(lnW, lnY, r, n, s, frequency, observedCount) {
  
  k <- 0
  MSE <- 0
  gamma <- 0
  delta <- 0
  print(paste("frequency[r] + 1"), frequency[r] + 1, sep = " ")
  print(frequency[r] + 1)
  fitsCount <- rep(NA, frequency[r] + 1)
  
  ## k should be 2173644.92868474
  ## calculate k
  print("hello world")
  for(j in 1:(r-1)) {
    tmp <- 0.0
    for(i in 1:(r-1)) {
      tmp <- tmp + i * lnW[i] * (i-j)
    }
    k <- k + lnW[j] * tmp
    print(paste("tmp: ", tmp, sep = "   "))
    print(paste("k: ", k, sep = "  "))
  }
  ## calculate gamma
  for(j in 1:(r-1)) {
    tmp <- 0.0
    for(i in 1:(r-1)) {
      tmp <- tmp + (j - i) * lnW[i] * lnY[i]
    }
    gamma <- gamma + j * lnW[j] * tmp
  }
  print(paste("k: ", k, sep = " "))
  gamma <- gamma / k
  
  ## calculate delta
  for(j in 1:(r-1)) {
    tmp <- 0
    for(i in 1:(r-1)) {
      tmp <- tmp + (i - j) * lnW[i] * lnY[i]
    }
    delta <- delta + lnW[j] * tmp
  }
  
  delta <- delta / k
  
  ## calculate MSE
  MSE <- (lnW[1:(r-1)] * (lnY[1:(r-1)] - gamma - delta * j) * (lnY[1:(r-1)] -
                                                        gamma - delta * (1:(r-1))))
  MSE <- MSE * (1/ (r - 3))
  
  print(paste("gamma: ", gamma, sep = " "))
  print(paste("exp(-gamma): ", exp(-gamma), sep = " "))
  print(paste("observedCount[frequency[1]] * exp(-gamma): ", observedCount[frequency[1]] * exp(-gamma), sep = " "))
  fitsCount[1] = ((observedCount[frequency[1]]) * exp(-gamma))
  fitsCount[2] = observedCount[frequency[1]]
  ##fitsCount[0] = fitsCount[1] * exp(-gamma)
  ##fitsCount[0] = 2
  print("ding dong ding")
  # print(fitsCount[0])
  # print(fitsCount[1])
  print(paste("fitsCount[0]: ", fitsCount[1], sep = "  "))
  print(paste("fitsCount[1]: ", fitsCount[2], sep = "  "))
  print(paste("r: ", r, sep = " "))
  for(t in 3:(r + 1)) {
    fitsCount[t] = fitsCount[t - 1] *
      exp(gamma + delta * (t - 1.0)) / t
  }
  
  print("fitsCount")
  print(fitsCount)
  #gamma, delta, MSE, k, fitsCheck, fitsCount
  
  print((sum(fitsCount < 0)))
  #nothing is negative
  if (sum(fitsCount < 0) <= 0) {
    list("fitsCount"=fitsCount, "check"=fitsCheck,
         "gamma"=gamma, "delta"=delta, "MSE"=MSE, "k"=k)
  } else {
    list("check"=0)
  }

}
