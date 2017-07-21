DoubleExponentialModel <- function(s, r, observedCount, n, 
                                   s0Init, frequency, 
                                   lnSFactorial, sumlnFFactorial, 
                                   maximumObservation) {
  
  #### Fits
  numParams <- 3
  
  fits <- DoubleExponentialFits(r, n, s, frequency, observedCount)
  
  if (fits$check == 1 & fits$flag == 1) {
    mle1 <- fits$mlesSExp1
    mle2 <- fits$mlesSExp2
    mle3 <- fits$mlesSExp3
    u <- fits$u
    fitsExtended <- rep(NA, frequency[r]*4)
    fitsExtended[1:frequency[r]] <- fits$fitsCount
    fitsExtended[(frequency[r] + 1):(frequency[r]*4)] <- 
      s[r] * (mle3 * ((1 / mle1) *
                        ((mle1 / (1.0 + mle1))^((frequency[r] + 1):(frequency[r]*4)))) +
                ((1 - mle3) * ((1 / mle2) *
                                 ((mle2 / (1 + mle2))^((frequency[r] + 1):(frequency[r]*4))))))
    
    sHatSubset <- s[r] * ((1.0 + mle1) * (1.0 + mle2) /
                            (mle2 + (mle1 * mle2) - (mle3 * mle2) +
                               (mle3 * mle1)))
    
    sHatTotal <- sHatSubset+(s[maximumObservation]-s[r])
    part1 <- lnSFactorial[r]-sumlnFFactorial[r]
    part2 <- sum(observedCount[1:r] * log((u * ((mle1/(1+mle1))^(frequency[1:r]))/mle1) +
                                            ((1-u)*(mle2/(1+mle2))^(frequency[1:r])/mle2)))
    
    calculate_analysis_variables_result <- CalculateAnalysisVariables(part1, part2, numParams, r, fits$fitsCount, 
                                                                      fitsExtended, 
                                                                      s, 2, frequency, observedCount)  
    
    ## standard error
    se <- DoubleExponentialStandardError(mle1, mle2, mle3, sHatSubset)
    bounds <- GetConfidenceBounds(r, se$se, sHatSubset, s, maximumObservation)
    
    output <- data.frame("Model" = "DoubleExponential", 
                         "Cutoff" = r, 
                         "Estimate" = CheckOutput(sHatTotal), 
                         "SE" = CheckOutput(se$se), 
                         "LCB"= CheckOutput(bounds$lcb), 
                         "UCB" = CheckOutput(bounds$ucb), 
                         "chiSq" = CheckOutput(calculate_analysis_variables_result$chiSq),
                         "AIC" = CheckOutput(calculate_analysis_variables_result$AIC), 
                         "AICc" = CheckOutput(calculate_analysis_variables_result$AICc), 
                         "GOF0" = CheckOutput(calculate_analysis_variables_result$GOF), 
                         "GOF5" = CheckOutput(calculate_analysis_variables_result$GOF5),
                         "T1"=CheckOutput(mle1),
                         "T2"=CheckOutput(mle2),
                         "T3"=CheckOutput(mle3),
                         "T4"=NA,
                         "T5"=NA,
                         "T6"=NA,
                         "T7"=NA)
    
  }
}

DoubleExponentialFits <- function(r, n, s, frequency, observedCount) {
  
  mle <- MLEDoubleExponential(r, n, s, frequency, observedCount)
  
  if (mle$flag == 1) {
    mle1 <- mle$mlesSExp1
    mle2 <- mle$mlesSExp2
    u <- mle$u
    mle3 <- (u*mle2*(1+mle1)) / (mle1 + (mle1 * mle2) + (u*mle2) - (u*mle1))
    fitsCount <- s[r] * ((u * ((1.0 / mle1) *
                                 ((mle1 / (1.0 + mle1))^(1:frequency[r])))) +
                           ((1.0 - u) * ((1.0 / mle2) *
                                           ((mle2 / (1.0 + mle2))^(1:frequency[r])))))
    fitsCheck <- ifelse(min(fitsCount) < 0, 0, 1) 
    list("flag" = mle$flag, 
         "fitsCount"=fitsCount, "check"=fitsCheck,
         "mlesSExp1"=mle1, "mlesSExp2"=mle2, "mlesSExp3"=mle3, "u"=u)
    
  } else {
    list("flag" = mle$flag, 
         "check"=0)
    
  }
}

MLEDoubleExponential <- function(r, n, s, frequency, observedCount) {
  results <- list()
  u <- 0.5
  k <- round(frequency[r]*0.67)
  r1 <- max(which(frequency <= k)) - 1
  k1 <- max(which(frequency < frequency[r1]))
  
  k <- round(frequency[r]*0.33)
  r2 <- max(which(frequency <= k)) - 1
  k2 <- ifelse(sum((frequency < frequency[r2])) > 0, max(which(frequency < frequency[r2])), 1)
  
  if (n[k1] != s[k1]) {
    t1 <- n[k1]/s[k1]-1
    t2 <- (n[r]-n[k2])/(s[r]-s[k2])-1
    part2 <- sum(observedCount[1:r] * 
                   log(u * ((t1/(1+t1))^(frequency[1:r]))/t1 +
                         ((1-u)*(t2/(1+t2))^(frequency[1:r]))/t2))
    deltaPart2 <- 1.0001e-10
    part2old <- part2
    iteration <- 1
    while(deltaPart2 > 1e-10 & iteration < 1e6) {
      z <- (u * (1.0 / t1) * ((t1 / (1.0 + t1))^frequency[1:r])) /
        ((u * (1.0 / t1) * (t1 / (1.0 + t1))^frequency[1:r]) +
           ((1.0 - u) * (1.0 / t2) * ((t2 / (1.0 + t2))^frequency[1:r])))
      u <- sum(observedCount[1:r]*z[1:r])
      t1part1 <- sum(observedCount[1:r]*frequency[1:r]*z)
      t2part1 <- sum(observedCount[1:r]*frequency[1:r]*(1-z))
      t1part2 <- sum(observedCount[1:r]*z)
      t2part2 <- sum(observedCount[1:r]*(1-z))
      
      u <- u/(s[r])
      t1 <- t1part1/t1part2-1
      t2 <- t2part1/t2part2-1
      part2 <-  sum(observedCount[1:r] * log((u * ((1.0 / t1) *
                                                     ((t1 / (1.0 + t1))^frequency[1:r]))) +
                                               ((1.0 - u) * ((1.0 / t2) * ((t2 / (1.0 + t2))^frequency[1:r])))))
      deltaPart2 <- part2-part2old
      part2old <- part2
      iteration <- iteration + 1
    }
    if (iteration == 1e6) warning("Double Exp didn't converge?")
    results$u <- u
    results$mlesSExp1 <- t1
    results$mlesSExp2 <- t2
    results$flag <- ifelse(is.nan(part2), 0, 1)
    
  } else {
    results$flag <- 0
  }
  results
}

DoubleExponentialStandardError <- function(t1, t2, t3, sHatSubset) {
  maximumIteration <- 100000
  criteria <- 0.0000000000000001
  a00   <- -(-t3*t2+t3*t1+t2+t1*t2)/(-t3*t2-1-t1+t3*t1)
  a0 <- c(-t3*(1+t2)/(1+t1)/(-t3*t2-1-t1+t3*t1), 
          (t3-1-t1+t3*t1)/(1+t2)/(-t3*t2-1-t1+t3*t1), 
          -1/(-t3*t2-1-t1+t3*t1)*(-t2+t1))
  a <- matrix(0, nrow = 3, ncol = 3) 
  
  ## a11
  test <- 100
  k <- 0
  
  while (test > criteria & k < maximumIteration) {
    a11 <- -t3*Math.Pow((t1/(1+t1)),k)*(t3*Math.Pow((t1/(1+t1)),k)*k+2*k*t1*t3*
                                          Math.Pow((t1/(1+t1)),k)*t2-(t1*t1)*t3*Math.Pow((t1/(1+t1)),k)*t2+2*k*t1*t3*
                                          Math.Pow((t1/(1+t1)),k)-5*k*t1*Math.Pow(t2/(1+t2),k)*t3-4*k*(t1*t1)*
                                          Math.Pow(t2/(1+t2),k)*t3+k*k*Math.Pow(t2/(1+t2),k)*t3*t1+k*t3*
                                          Math.Pow((t1/(1+t1)),k)*t2-(t1*t1)*t3*Math.Pow((t1/(1+t1)),k)+2*(t1*t1)*
                                          Math.Pow(t2/(1+t2),k)*t3+2*Math.Pow(t1,3)*Math.Pow(t2/(1+t2),k)*t3+5*k*t1*
                                          Math.Pow(t2/(1+t2),k)+4*k*(t1*t1)*Math.Pow(t2/(1+t2),k)+k*k*Math.Pow(t2/(1+t2),k)*t3-k*k*
                                          Math.Pow(t2/(1+t2),k)*t1-k*Math.Pow(t2/(1+t2),k)*t3-2*(t1*t1)*Math.Pow(t2/(1+t2),k)-2*
                                          Math.Pow(t1,3)*Math.Pow(t2/(1+t2),k)-k*k*Math.Pow(t2/(1+t2),k)+k*Math.Pow(t2/(1+t2),k))/
      (-t3*Math.Pow((t1/(1+t1)),k)-t3*Math.Pow((t1/(1+t1)),k)*t2+Math.Pow(t2/(1+t2),k)*t3-Math.Pow(t2/(1+t2),k)-
         Math.Pow(t2/(1+t2),k)*t1+Math.Pow(t2/(1+t2),k)*t3*t1)*Math.Pow(t1,(-2))*Math.Pow((1 + t1), (-3))
    
    if (k > 0) test <- abs(a11/a[1,1])
    a[1,1] <- a[1,1] + a11
    k <- k+1
  }
  
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ## a12
  test <- 1
  k <- 0
  while (test > criteria  &  k < maximumIteration) {
    a12 <- Math.Pow((t1/(1+t1)),k)*t3*(-t1+k)*(-1+t3)*
      Math.Pow((t2/(1+t2)),k)*(-t2+k)/t2/(1+t2)/t1/(1+t1)/(-t3*Math.Pow((t1/(1+t1)),k)-t3*
                                                             Math.Pow((t1/(1+t1)),k)*t2+Math.Pow((t2/(1+t2)),k)*t3-Math.Pow((t2/(1+t2)),k)-
                                                             Math.Pow((t2/(1+t2)),k)*t1+Math.Pow((t2/(1+t2)),k)*t3*t1)
    if (k > 0) test <- abs(a12/a[1,2])
    a[1,2] <- a[1,2] + a12
    k <- k+1
  }  
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ## a13
  test <- 1
  k <- 0
  while (test > criteria & k < maximumIteration) {
    a13 <- Math.Pow((t1/(1+t1)),k)*(-t1+k)*Math.Pow((t2/(1+t2)),k)/t1/(1+t1)/(-t3*
                                                                                Math.Pow((t1/(1+t1)),k)-t3*Math.Pow((t1/(1+t1)),k)*t2+Math.Pow((t2/(1+t2)),k)*t3-
                                                                                Math.Pow((t2/(1+t2)),k)-Math.Pow((t2/(1+t2)),k)*t1+Math.Pow((t2/(1+t2)),k)*t3*t1)
    if (k > 0) test <- abs(a13/a[1,3])
    a[1,3] <- a[1,3] + a13
    k <- k+1
    
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ## a22
  test <- 1
  k <- 0
  while (test > criteria & k < maximumIteration) {
    a22 <- (-1+t3)*Math.Pow((t2/(1+t2)),k)*(-k*t1*Math.Pow((t2/(1+t2)),k)+k*
                                              Math.Pow((t2/(1+t2)),k)*t3-t3*Math.Pow(t1/(1+t1),k)*k+k*k*t3*Math.Pow(t1/(1+t1),k)-2*k*t2*
                                              Math.Pow((t2/(1+t2)),k)+(t2*t2)*Math.Pow((t2/(1+t2)),k)*t1-(t2*t2)*Math.Pow((t2/(1+t2)),k)*t3+2*
                                              Math.Pow(t2,3)*t3*Math.Pow(t1/(1+t1),k)+2*(t2*t2)*t3*Math.Pow(t1/(1+t1),k)+2*k*t2*
                                              Math.Pow((t2/(1+t2)),k)*t3-4*k*(t2*t2)*t3*Math.Pow(t1/(1+t1),k)-5*k*t3*
                                              Math.Pow(t1/(1+t1),k)*t2-(t2*t2)*Math.Pow((t2/(1+t2)),k)*t3*t1-2*k*t2*
                                              Math.Pow((t2/(1+t2)),k)*t1-k*Math.Pow((t2/(1+t2)),k)+(t2*t2)*Math.Pow((t2/(1+t2)),k)+k*k*t3*
                                              Math.Pow(t1/(1+t1),k)*t2+2*k*t2*Math.Pow((t2/(1+t2)),k)*t3*t1+k*t1*
                                              Math.Pow((t2/(1+t2)),k)*t3)/(t3*Math.Pow(t1/(1+t1),k)+t3*Math.Pow(t1/(1+t1),k)*t2-
                                                                             Math.Pow((t2/(1+t2)),k)*t3+Math.Pow((t2/(1+t2)),k)+Math.Pow((t2/(1+t2)),k)*t1-
                                                                             Math.Pow((t2/(1+t2)),k)*t3*t1)*Math.Pow(t2,(-2))*Math.Pow((1+t2),(-3))
    
    if (k > 0) test <- abs(a22/a[2,2])
    a[2,2] <- a[2,2] + a22
    k <- k+1
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ## a23
  test <- 1
  k <- 0
  while (test > criteria & k < maximumIteration) {
    a23 <- Math.Pow((t2/(1+t2)),k)*(-t2+k)*Math.Pow((t1/(1+t1)),k)/(t3*Math.Pow((t1/(1+t1)),k)+t3*
                                                                      Math.Pow((t1/(1+t1)),k)*t2-Math.Pow((t2/(1+t2)),k)*t3+Math.Pow((t2/(1+t2)),k)+
                                                                      Math.Pow((t2/(1+t2)),k)*t1-Math.Pow((t2/(1+t2)),k)*t3*t1)/t2/(1+t2)
    if (k > 0) test <- abs(a23/a[2,3])
    a[2,3] <- a[2,3] + a23
    k <- k+1
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ## a33
  test <- 1
  k <- 0
  while (test > criteria & k < maximumIteration) {
    a33 <- Math.Pow(Math.Pow((t1 / (1 + t1)), k) + Math.Pow((t1 / (1 + t1)), k) * t2 - Math.Pow(t2 / (1 + t2), k) -
                      Math.Pow(t2 / (1 + t2), k) * t1, 2) / (1 + t2) / (1 + t1) / (t3 * Math.Pow((t1 / (1 + t1)), k) + t3 *
                                                                                     Math.Pow((t1 / (1 + t1)), k) * t2 - Math.Pow(t2 / (1 + t2), k) * t3 + Math.Pow(t2 / (1 + t2), k) + Math.Pow(t2 / (1 + t2), k) * t1 -
                                                                                     Math.Pow(t2 / (1 + t2), k) * t3 * t1)
    
    if (k > 0) test <- abs(a33/a[3,3])
    a[3,3] <- a[3,3] + a33
    k <- k+1
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  print(a)
  ## invert
  MatrixInversion(sHatSubset, a00, a0, a)
  
}
