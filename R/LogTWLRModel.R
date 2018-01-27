LogTWLRModel <- function(lnW, lnY,  WLRMGOF0, s, r, observedCount, n, 
                                   s0Init, frequency, 
                                   lnSFactorial, sumlnFFactorial, 
                                   maximumObservation) {
  bigChiSq <- 1000000000
  numParams <- 2
  maxGOF <- 10
  modelNumber <- 6
  print(paste("R IN ITERATION: ", r, sep = " "))
  fits <- LogTWLRFits(lnW, lnY, r, n, s, frequency, observedCount)
  fitsCheck <- ifelse(min(fits$fitsCount) < 0, 0, 1)
  if (fitsCheck == 1) {
    gamma <- fits$gamma
    k <- fits$k
    delta <- fits$delta
    MSE <- fits$MSE
    fitsCount <- fits$fitsCount
    
    sHatSubset <- fitsCount[1] + s[r]
    
    sHatTotal <- sHatSubset + (s[maximumObservation] - s[r])
    print(paste("r: ", r, sep = " "))
    print("fitsCount")
    print(fitsCount)
    #print(paste("numParams: ", numParams, sep = " "))
    #print(paste("frequency: ", frequency, sep = " "))
    print("frequency")
    print(frequency)
    print("observedCount")
    print(observedCount)
    #print(paste("observedCount: ", observedCount, sep = " "))
    print("s")
    print(s)
    chiSqAll <- ChiSqFunction(r, fitsCount, modelNumber, frequency, observedCount, s)
    
    df <- frequency[r] - numParams
    flag <- 1
    test <- (chiSqAll - df) / sqrt(2 * df)
    GOF0 <- 0

    if (test < maxGOF & chiSqAll < bigChiSq) {
      GOF0 <- GoodnessOfFit(chiSqAll, df) #nothing?
    } else {
      flag <- 0
    }
  }
  
  GOF0Check <- flag
  print("printing GOF0")
  print(GOF0) #doesn't work when r is 10
  print("printing WLRMGOF0")
  print(WLRMGOF0)
  print(paste("frequency[r]: ", frequency[r], sep = " "))
  ##print(GOF0)
  #print(paste("GOF0$gof: ", GOF0$gof, sep = " "))

  gofOutput <- 0
  # check the flags to set the correct gof0
  if (flag != 0) {
    gofOutput <- GOF0$flag
  } else {
    gofOutput <- GOF0
  }
  #is it ok for switch and GOF0 to be the same?

  # C# is 0 based while R is 1 based
  varGamma <- sum((2:r-1) * (2:r-1) * lnW[2:r-1])
  varGamma <- varGamma * MSE / k

  # print(paste("s[r]: ", s[r], sep = " "))
  # print(paste("fitsCount[0]: ", fitsCount[1], sep = " "))
  # print(paste("sHatSubset: ", sHatSubset, sep = " "))
  # print(paste("(s[r] * fitsCount[0] / sHatSubset): ", s[r] * fitsCount[1] / sHatSubset, sep = " "))
  # print(paste("(exp(-2.0 * gamma) * observedCount[frequency[1]]: ", (exp(-2.0 * gamma) * observedCount[frequency[1]]), sep = " "))
  # print(paste("(varGamma * observedCount[frequency[1]] + 1.0): ", (varGamma * observedCount[frequency[1]] + 1.0), sep = " "))
  se <- (s[r] * fitsCount[1] / sHatSubset) + (exp(-2.0 * gamma) *
                                                observedCount[frequency[1]] * (varGamma * observedCount[frequency[1]] + 1.0))
  # print(paste("varGamma: ", varGamma, sep = " ")) #wrong
  # print(paste("observedCount[frequency[1]]: ", observedCount[frequency[1]], sep = " "))
  # print(paste("freq[1]: ", frequency[1], sep = " "))
  # print(paste("se: ", se, sep = "  "))
  SEFlag <- 0
  if (se > 0) {
    se <- sqrt(se)
    seFlag <- 1
  }
  print(paste("se now", se, sep = " "))
  boundsCheck <- 0
  if(seFlag == 1) {
    boundsCheck <- GetConfidenceBounds(r, se, sHatSubset, s, maximumObservation)
  }
  
  output <- data.frame("Model" = "LogTransfWLR", 
                       "Cutoff" = r, 
                       "Estimate" = CheckOutput(sHatTotal), 
                       "SE" = CheckOutput(se), 
                       "LCB"= CheckOutput(boundsCheck$lcb), 
                       "UCB" = CheckOutput(boundsCheck$ucb), 
                       "chiSq" = CheckOutput(chiSqAll),
                       "AIC" = CheckOutput(0), 
                       "AICc" = CheckOutput(0), 
                       "GOF0" = CheckOutput(gofOutput), #should be checkoutput, don't know why it's angry
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
  for(j in 1:(r-1)) {
    tmp <- 0.0
    for(i in 1:(r-1)) {
      tmp <- tmp + i * lnW[i] * (i-j)
    }
    k <- k + lnW[j] * tmp
   
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
  
  MSE <- sum(MSE)
  
  print(paste("k: ", k, sep = " "))
  print(paste("gamma: ", gamma, sep = " "))
  print(paste("delta: ", delta, sep = " "))
  print(paste("MSE: ", MSE, sep = " "))
  print(paste("exp(-gamma): ", exp(-gamma), sep = " "))
  print(paste("observedCount[frequency[1]] * exp(-gamma): ", observedCount[frequency[1]] * exp(-gamma), sep = " "))
  fitsCount[1] = ((observedCount[frequency[1]]) * exp(-gamma))
  fitsCount[2] = observedCount[frequency[1]]
  ##fitsCount[0] = fitsCount[1] * exp(-gamma)
  ##fitsCount[0] = 2
  # print(fitsCount[0])
  # print(fitsCount[1])
  ## ohh i know off by 1 error
  for(t in 3:(r + 1)) {
    fitsCount[t] = fitsCount[t - 1] *
      exp(gamma + delta * ((t - 1) - 1.0)) / (t - 1)
  }
  
  #print("fitsCount")
  #print(fitsCount)
  #gamma, delta, MSE, k, fitsCheck, fitsCount
  
  #print((sum(fitsCount < 0)))
  #nothing is negative
  #if (sum(fitsCount < 0) <= 0) {
    list("fitsCount"=fitsCount,
         "gamma"=gamma, "delta"=delta, "MSE"=MSE, "k"=k)
 # } else {
   # list("check"=0)
 # }

}
