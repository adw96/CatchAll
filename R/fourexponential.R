#fix formatting later
FourExponentialModel <-  function(s, r, observedCount, n, 
                                    s0Init, frequency, 
                                    lnSFactorial, sumlnFFactorial, 
                                    maximumObservation) {
  
  
  ### Fits
  numParams <- 7
 
  fits <- FourExponentialFits(r, n, s, frequency, observedCount)
  
  mle1 <- fits$mlesSExp1
  mle2 <- fits$mlesSExp2
  mle3 <- fits$mlesSExp3
  mle4 <- fits$mlesSExp4
  mle5 <- fits$mlesSExp5
  mle6 <- fits$mlesSExp6
  mle7 <- fits$mlesSExp7
  u1 <- fits$u1
  u2 <- fits$u2
  u3 <- fits$u3
  
  fitsExtended <- rep(NA, frequency[r]*4)
  fitsExtended[1:frequency[r]] <- fits$fitsCount

  #change to nonforloop later
  for(t in (frequency[r]+1):(frequency[r]*4)){
    fitsExtended[t] = s[r] *
      ((u1 * ((1.0 / mle1) * pow((mle1 / (1.0 + mle1)), t))) +
         (u2 * ((1.0 / mle2) * pow((mle2 / (1.0 + mle2)), t))) +
         (u3 * ((1.0 / mle3) * pow((mle3 / (1.0 + mle3)), t))) +
         ((1.0 - u1 - u2 - u3) * ((1.0 / mle4) *
                                    pow((mle4 / (1.0 + mle4)), t))));
  }
  
  sHatSubset <- s[r] * (((1.0 + mle1) * (1.0 + mle2) *
                           (1.0 + mle3) * (1.0 + mle4)) / ((mle5 * mle1 * mle3) +
                                                                       (mle4 * mle2 * mle3) - (mle7 * mle2 * mle4) +
                                                                       (mle6 * mle2 * mle3) - (mle7 * mle4 * mle1) +
                                                                       (mle6 * mle1 * mle2) + (mle4 * mle1 * mle2) -
                                                                       (mle5 * mle2 * mle4) - (mle5 * mle4 * mle3) +
                                                                       (mle1 * mle3 * mle4) + (mle7 * mle2 * mle3) -
                                                                       (mle6 * mle4 * mle1) + (mle7 * mle1 * mle3) -
                                                                       (mle7 * mle4) + (mle5 * mle1) +
                                                                       (mle6 * mle2) + mle4 + (mle4 * mle3) +
                                                                       (mle7 * mle3) + (mle2 * mle4) -
                                                                       (mle5 * mle4) - (mle6 * mle4) +
                                                                       (mle7 * mle1 * mle2 * mle3) +
                                                                       (mle1 * mle2 * mle3 * mle4) -
                                                                       (mle6 * mle4 * mle3) + (mle5 * mle1 * mle2) +
                                                                       (mle4 * mle1) - (mle6 * mle1 * mle3 * mle4) -
                                                                       (mle5 * mle4 * mle2 * mle3) +
                                                                       (mle6 * mle1 * mle2 * mle3) +
                                                                       (mle5 * mle1 * mle2 * mle3) -
                                                                       (mle7 * mle4 * mle1 * mle2)));
  
  sHatTotal <- sHatSubset+(s[maximumObservation]-s[r])
  part1 <- lnSFactorial[r]-sumlnFFactorial[r]
  
  part2 <- sum((observedCount[1:r] * log(
    (u1 * ((1.0 / mle1) * pow((mle1 / (1.0 + mle1)), freq[1:r]))) +
      (u2 * ((1.0 / mle2) * pow((mle2 / (1.0 + mle2)), freq[1:r]))) +
      (u3 * ((1.0 / mle3) * pow((mle3 / (1.0 + mle3)), freq[1:r]))) +
      ((1.0 - u1 - u2 - u3) * ((1.0 / mle4) *
                                 pow((mle4 / (1.0 + mle4)), freq[1:r]))))))
  
  
  # model number 5
  calculate_analysis_variables_result <- CalculateAnalysisVariables(part1, part2, numParams, r, fits$fitsCount, 
                                                                    fitsExtended, 
                                                                    s, 5, frequency, observedCount) 
  se <- FourExponentialStandardError(mle1, mle2, mle3, mle4, mle5, mle6, mle7, sHatSubset)
  # s is lower class bound
  # maximumObservation is upper class bound
  bounds <- GetConfidenceBounds(r, se$se, sHatSubset, s, maximumObservation)
  
  output <- data.frame("Model" = "FourExponential", 
                       "Cutoff" = r, 
                       "Estimate" = CheckOutput(sHatTotal), 
                       "SE" = CheckOutput(se$se), 
                       "LCB"= CheckOutput(bounds$lcb), 
                       "UCB" = CheckOutput(bounds$ucb), 
                       "chiSq" = CheckOutput(calculate_analysis_variables_result$chiSq),
                       "AIC" = CheckOutput(calculate_analysis_variables_result$AIC), 
                       "AICc" = CheckOutput(calculate_analysis_variables_result$AICc), 
                       "GOF0" = CheckOutput(calculate_analysis_variables_result$GOF0), 
                       "GOF5" = CheckOutput(calculate_analysis_variables_result$GOF5),
                       "T1"=CheckOutput(mle1),
                       "T2"=CheckOutput(mle2),
                       "T3"=CheckOutput(mle3),
                       "T4"=CheckOutput(mle4),
                       "T5"=CheckOutput(mle5),
                       "T6"=CheckOutput(mle6),
                       "T7"=CheckOutput(mle7))
}


FourExponentialFits <- function(r, n, s, frequency, observedCount) {
  mle <- MLEFourExponential(r, n, s, frequency, observedCount)
  
  if (mle$flag == 1) {
    #access the elements from mle
    mle1 <- mle$mlesSExp1
    mle2 <- mle$mlesSExp2
    mle3 <- mle$mlesSExp3
    mle4 <- mle$mlesSExp4
    u1 <- mle$u1 
    u2 <- mle$u2
    u3 <- mle$u3
    
    denom <- (mle1  *  mle2 * mle3) +
      (u2 * mle1  * mle3) + (u1 *  mle2 * mle3) -
      (u2 * mle1  *  mle2) - (u1 * mle1  *  mle2) +
      (mle1  *  mle2)
    
    mle4 <- ((1.0 + mle1 ) * u1 *  mle2 *  mle3) / denom 
    mle5 <- ((1.0 + mle2) * u2 * mle1  *  mle3) / denom
    
    denom <- (u3 * mle4 * mle1 * mle2)
    + (u2 * mle4 * mle1 * mle3)
    + (mlesSExp4 * mle1 * mle2 * mle3)
    + (u1 * mle4 * mle2 * mle3)
    + (mle1 * mle2 * mles3)
    - (u3 * mle1 * mle2 * mle3)
    - (u1 * mle1 * mle2 * mle3)
    - (u2 * mle1 * mle2 * mle3)
    
    mle5 <- ((1.0 + mle1) * u1 * mle4 * mle2 * mle3) / denom
    
    mle6 <- ((1.0 + mle2) * u2 * mle4 * mle1 * mle3) / denom
    
    mle7 <- ((1.0 + mle3) * u3 * mle4 * mle1 * mle2) / denom
    
    fitsCount <- s[r] *
      ((u1 * ((1.0 / mle1) * pow((mle1 / (1.0 + mle1)), (1:frequency[r]))) +
         (u2 * ((1.0 / mle2) * pow((mle2 / (1.0 + mle2)), (1:frequency[r])))) +
         (u3 * ((1.0 / mle3) * pow((mle3 / (1.0 + mle3)), (1:frequency[r])))) +
         ((1.0 - u1 - u2 - u3) * ((1.0 / mle4) *
                                    pow((mle4 / (1.0 + mle4)), (1:frequency[r])))))
    
    fitsCheck <- ifelse(min(fitsCount) < 0, 0, 1) 
    
    list("flag" = mle$flag, 
         "fitsCount"=fitsCount, "check"=fitsCheck,
         "mlesSExp1"=mle1, "mlesSExp2"=mle2, "mlesSExp3"=mle3, "mlesSExp4"=mle4, "mlesSExp5"=mle5,
         "mlesSExp6"=mle6, "mlesSExp7"=mle7,"u1"=u1, "u2"=u2, "u3"=u3)
    
  } else {
    list("flag" = mle$Flag, "check" = 0)
  }
}

MLEFourExponential <- function(r, n, s, frequency, observedCount) {
  results <- list()
  
  u1 <- 0.25
  u2 <- 0.25
  u3 <- 0.25
  
  #ks, rs, k1s not correct
  k <- round(frequency[r]*0.4)
  #subtracted -1 from max for r1..r3
  #made the k1...k3 <= instead of <
  r1 <- max(which(frequency <= k))
  k1 <- max(which(frequency <= frequency[r1]))

  k <- round(frequency[r]*0.2)
  r2 <- max(which(frequency <= k))
  #not sure I understand this fully or why it's different than above
  k2 <- ifelse(sum((frequency < frequency[r2])) > 0, max(which(frequency <= frequency[r2])), 1)

  k <- round(frequency[r]*0.6)
  r3 <- max(which(frequency <= k))
  k3 <- max(which(frequency <= frequency[r3]))
  
  k <- round(frequency[r]*0.8)
  r4 <- max(which(frequency <= k))
  k4 <- max(which(frequency <= frequency[r4]))

  
  if (n[k1] != s[k1] & (s[k3] != s[k2]) & (s[k4] != s[k2])) {
    t1 <- (n[k1] / s[k1]) - 1;
    t2 <- ((n[k3] - n[k2]) / (s[k3] - s[k2])) - 1;
    t3 <- ((n[k4] - n[k2]) / (s[k4] - s[k2])) - 1;
    t4 <- ((n[r] - n[k3]) / (s[r] - s[k3])) - 1;
    
    part2 <- sum(observedCount[1:r] * log(
      (u1 * ((1.0 / t1) * pow((t1 / (1.0 + t1)), freq[1:r]))) +
        (u2 * ((1.0 / t2) * pow((t2 / (1.0 + t2)), freq[1:r]))) +
        (u3 * ((1.0 / t3) * pow((t3 / (1.0 + t3)), freq[1:r]))) +
        ((1.0 - u1 - u2 - u3) * ((1.0 / t4) * pow((t4 / (1.0 + t4)), freq[1:r])))))
    
    
    deltaPart2 <- 1.0001e-10
    part2old <- part2
    k <- 0
    
    iteration <- 1

    while(deltaPart2 > 1e-10 & iteration < 1e6) { #double check
      
      denom <- ((u1 * (1.0 / t1) * pow((t1 / (1.0 + t1)), freq[1:r])) +
                   (u2 * (1.0 / t2) * pow((t2 / (1.0 + t2)), freq[1:r])) +
                   (u3 * (1.0 / t3) * pow((t3 / (1.0 + t3)), freq[1:r])) +
                   ((1.0 - u1 - u2 - u3) * (1.0 / t4) * pow((t4 / (1.0 + t4)), freq[1:r])));
      
      
      
      z1 <- (u1 * (1.0 / t1) * ((t1 / (1.0 + t1)) ^ frequency[1:r])) / denom
      
      z2 <- (u2 * (1.0 / t2) * ((t2 / (1.0 + t2)) ^ frequency[1:r])) / denom
      
      z3 <- (u3 * (1.0 / t2) * ((t3 / (1.0 + t3)) ^ frequency[1:r])) / denom
      
      u1 <- sum(observedCount[1:r]*z1[1:r])
      u2 <- sum(observedCount[1:r]*z2[1:r])
      u3 <- sum(observedCount[1:r]*z3[1:r])
      
      t1part1 <- sum(observedCount[1:r]*frequency[1:r]*z1)
      t2part1 <- sum(observedCount[1:r]*frequency[1:r]*z2)
      t3part1 <- sum(observedCount[1:r]*frequency[1:r]*z3)
      t4part1 <- sum(observedCount[1:r]*frequency[1:r]*(1-z1-z2-z3))
      
      t1part2 <- sum(observedCount[1:r]*z1)
      t2part2 <- sum(observedCount[1:r]*z2)
      t3Part2 <- sum(observedCount[1:r]*z3)
      t4part2 <- sum(observedCount[1:r]*(1-z1-z2-z3))
      
      u1 <- u1/(s[r])
      u2 <- u2/(s[r])
      u3 <- u3/(s[r])
      
      t1 <- t1part1/t1part2-1
      t2 <- t2part1/t2part2-1
      t3 <- t3part1/t3part2-1
      t4 <- t4part1/t4part2-1
      
      part2 <- sum(observedCount[1:r] * Math.Log(
        (u1 * ((1.0 / t1) * pow((t1 / (1.0 + t1)), freq[1:r]))) +
          (u2 * ((1.0 / t2) * pow((t2 / (1.0 + t2)), freq[1:r]))) +
          (u3 * ((1.0 / t3) * pow((t3 / (1.0 + t3)), freq[1:r]))) +
          ((1.0 - u1 - u2 - u3) * ((1.0 / t4) * pow((t4 / (1.0 + t4)), freq[1:r])))))
      
      deltaPart2 <- part2-part2old
      part2old <- part2
      iteration <- iteration + 1
    }
    
    #where is 1e6 from??
    if (iteration == 1e6) warning("Triple Exp didn't converge?")
    results$u1 <- u1
    results$u2 <- u2
    results$u3 <- u3
    results$mlesSExp1 <- t1
    results$mlesSExp2 <- t2
    results$mlesSExp3 <- t3
    results$mlesSExp4 <- t4
    results$flag <- ifelse(is.nan(part2), 0, 1)
    
  } else {
    results$flag <- 0
  }
  results
}

FourExponentialStandardError <- function(t1, t2, t3, t4, t5, t6, t7, sHatSubset) {
  
  maximumIteration <- 100000
  criteria <- 0.0000000000000001
  
  t23P <- t2^3
  t33P <- t3^3
  
  t13P <- t1^3
  t1bP  <- t1^(-2)
  t1aP <- (1+t1)^(-3)
  t2aP <- (1+t2)^(-3)
  t2bP <- t2^(-2)
  
  t3aP <- (1 + t3)^(-3)
  t3bP <- (t3)^(-2)
  
  a00 <- -(-t7 * t2 * t4 + t6 * t2 * t3 + t6 * t2 + t7 * t3 + t7 * t1 * t2 * t3 -
             t6 * t1 * t4 + t6 * t1 * t2 - t5 * t2 * t4 - t6 * t3 * t4 + t2 * t3 * t4 +
             t7 * t2 * t3 - t5 * t3 * t4 + t7 * t1 * t3 + t1 * t2 * t4 + t5 * t1 * t2 - t7 * t1 * t4 +
             t1 * t3 * t4 + t5 * t1 * t3 - t5 * t2 * t3 * t4 + t5 * t1 - t6 * t1 * t3 * t4 + t4 +
             t5 * t1 * t2 * t3 - t7 * t1 * t2 * t4 + t6 * t1 * t2 * t3 + t3 * t4 + t2 * t4 - t5 * t4 +
             t1 * t4 - t6 * t4 + t1 * t2 * t3 * t4 - t7 * t4) / (-1 - t7 * t2 * t4 + t6 * t2 * t3 - t2 -
                                                                   t1 + t6 * t2 + t7 * t3 + t7 * t1 * t2 * t3 - t6 * t1 * t4 + t6 * t1 * t2 - t5 * t2 * t4 -
                                                                   t6 * t3 * t4 + t7 * t2 * t3 - t5 * t3 * t4 + t7 * t1 * t3 + t5 * t1 * t2 - t7 * t1 * t4 +
                                                                   t5 * t1 * t3 - t1 * t2 * t3 - t5 * t2 * t3 * t4 + t5 * t1 - t3 - t6 * t1 * t3 * t4 +
                                                                   t5 * t1 * t2 * t3 - t7 * t1 * t2 * t4 + t6 * t1 * t2 * t3 - t2 * t3 - t5 * t4 - t1 * t3 -
                                                                   t6 * t4 - t1 * t2 - t7 * t4)
  
  #make a list instead of the array
  a0 <- c(-t5 * (1 + t4) * (1 + t3) * (1 + t2) / (1 + t1) / (-1 - t7 * t2 * t4 + t6 * t2 * t3 -
                                                             t2 - t1 + t6 * t2 + t7 * t3 + t7 * t1 * t2 * t3 - t6 * t1 * t4 + t6 * t1 * t2 -
                                                             t5 * t2 * t4 - t6 * t3 * t4 + t7 * t2 * t3 - t5 * t3 * t4 + t7 * t1 * t3 + t5 * t1 * t2 -
                                                             t7 * t1 * t4 + t5 * t1 * t3 - t1 * t2 * t3 - t5 * t2 * t3 * t4 + t5 * t1 - t3 -
                                                             t6 * t1 * t3 * t4 + t5 * t1 * t2 * t3 - t7 * t1 * t2 * t4 + t6 * t1 * t2 * t3 - t2 * t3 -
                                                             t5 * t4 - t1 * t3 - t6 * t4 - t1 * t2 - t7 * t4),
  -t6 * (t1 * t4 + t3 * t4 + t1 * t3 + t1 * t3 * t4 + 1 + t4 + t3 + t1) / (1 + t2) /
    (-1 - t7 * t2 * t4 + t6 * t2 * t3 - t2 - t1 + t6 * t2 + t7 * t3 + t7 * t1 * t2 * t3 -
       t6 * t1 * t4 + t6 * t1 * t2 - t5 * t2 * t4 - t6 * t3 * t4 + t7 * t2 * t3 - t5 * t3 * t4 +
       t7 * t1 * t3 + t5 * t1 * t2 - t7 * t1 * t4 + t5 * t1 * t3 - t1 * t2 * t3 - t5 * t2 * t3 * t4 +
       t5 * t1 - t3 - t6 * t1 * t3 * t4 + t5 * t1 * t2 * t3 - t7 * t1 * t2 * t4 + t6 * t1 * t2 * t3 -
       t2 * t3 - t5 * t4 - t1 * t3 - t6 * t4 - t1 * t2 - t7 * t4),
  -(1 + t2) * (1 + t4) * t7 * (1 + t1) / (1 + t3) / (-1 - t7 * t2 * t4 + t6 * t2 * t3 - t2 -
                                                       t1 + t6 * t2 + t7 * t3 + t7 * t1 * t2 * t3 - t6 * t1 * t4 + t6 * t1 * t2 - t5 * t2 * t4 -
                                                       t6 * t3 * t4 + t7 * t2 * t3 - t5 * t3 * t4 + t7 * t1 * t3 + t5 * t1 * t2 - t7 * t1 * t4 +
                                                       t5 * t1 * t3 - t1 * t2 * t3 - t5 * t2 * t3 * t4 + t5 * t1 - t3 - t6 * t1 * t3 * t4 +
                                                       t5 * t1 * t2 * t3 - t7 * t1 * t2 * t4 + t6 * t1 * t2 * t3 - t2 * t3 - t5 * t4 - t1 * t3 -
                                                       t6 * t4 - t1 * t2 - t7 * t4),
  (1 + t2) * (t7 * t1 + t6 * t3 - 1 + t6 + t7 * t1 * t3 + t6 * t1 + t7 * t3 + t5 * t1 +
                t5 * t3 - t1 * t3 + t5 * t1 * t3 + t6 * t1 * t3 - t3 + t5 - t1 + t7) / (1 + t4) /
    (-1 - t7 * t2 * t4 + t6 * t2 * t3 - t2 - t1 + t6 * t2 + t7 * t3 + t7 * t1 * t2 * t3 -
       t6 * t1 * t4 + t6 * t1 * t2 - t5 * t2 * t4 - t6 * t3 * t4 + t7 * t2 * t3 - t5 * t3 * t4 +
       t7 * t1 * t3 + t5 * t1 * t2 - t7 * t1 * t4 + t5 * t1 * t3 - t1 * t2 * t3 - t5 * t2 * t3 * t4 +
       t5 * t1 - t3 - t6 * t1 * t3 * t4 + t5 * t1 * t2 * t3 - t7 * t1 * t2 * t4 + t6 * t1 * t2 * t3 -
       t2 * t3 - t5 * t4 - t1 * t3 - t6 * t4 - t1 * t2 - t7 * t4),
  -1 / (-1 - t7 * t2 * t4 + t6 * t2 * t3 - t2 - t1 + t6 * t2 + t7 * t3 + t7 * t1 * t2 * t3 -
          t6 * t1 * t4 + t6 * t1 * t2 - t5 * t2 * t4 - t6 * t3 * t4 + t7 * t2 * t3 - t5 * t3 * t4 +
          t7 * t1 * t3 + t5 * t1 * t2 - t7 * t1 * t4 + t5 * t1 * t3 - t1 * t2 * t3 - t5 * t2 * t3 * t4 +
          t5 * t1 - t3 - t6 * t1 * t3 * t4 + t5 * t1 * t2 * t3 - t7 * t1 * t2 * t4 + t6 * t1 * t2 * t3 -
          t2 * t3 - t5 * t4 - t1 * t3 - t6 * t4 - t1 * t2 - t7 * t4) * (-t2 * t4 - t3 * t4 + t1 * t2 +
                                                                          t1 * t3 - t2 * t3 * t4 + t1 + t1 * t2 * t3 - t4),
  -1 / (-1 - t7 * t2 * t4 + t6 * t2 * t3 - t2 - t1 + t6 * t2 + t7 * t3 + t7 * t1 * t2 * t3 -
          t6 * t1 * t4 + t6 * t1 * t2 - t5 * t2 * t4 - t6 * t3 * t4 + t7 * t2 * t3 - t5 * t3 * t4 +
          t7 * t1 * t3 + t5 * t1 * t2 - t7 * t1 * t4 + t5 * t1 * t3 - t1 * t2 * t3 - t5 * t2 * t3 * t4 +
          t5 * t1 - t3 - t6 * t1 * t3 * t4 + t5 * t1 * t2 * t3 - t7 * t1 * t2 * t4 + t6 * t1 * t2 * t3 -
          t2 * t3 - t5 * t4 - t1 * t3 - t6 * t4 - t1 * t2 - t7 * t4) * (t2 * t3 + t2 - t1 * t4 + t1 * t2 -
                                                                          t3 * t4 - t1 * t3 * t4 + t1 * t2 * t3 - t4),
  -1 / (-1 - t7 * t2 * t4 + t6 * t2 * t3 - t2 - t1 + t6 * t2 + t7 * t3 + t7 * t1 * t2 * t3 -
          t6 * t1 * t4 + t6 * t1 * t2 - t5 * t2 * t4 - t6 * t3 * t4 + t7 * t2 * t3 - t5 * t3 * t4 +
          t7 * t1 * t3 + t5 * t1 * t2 - t7 * t1 * t4 + t5 * t1 * t3 - t1 * t2 * t3 - t5 * t2 * t3 * t4 +
          t5 * t1 - t3 - t6 * t1 * t3 * t4 + t5 * t1 * t2 * t3 - t7 * t1 * t2 * t4 + t6 * t1 * t2 * t3 -
          t2 * t3 - t5 * t4 - t1 * t3 - t6 * t4 - t1 * t2 - t7 * t4) * (-t2 * t4 + t3 + t1 * t2 * t3 +
                                                                          t2 * t3 + t1 * t3 - t1 * t4 - t1 * t2 * t4 - t4))
  
  ##
  
  #hmm, not quite sure why this is 5 and not 6? cuz we're creating a matrix of size 5x5 but in C# it's 6x6 if it's 0 based indexing?
  a <- matrix(0, nrow = 5, ncol = 5) 
  
  
  ## a11
  test <- 100
  k <- 0
  
  while (test > criteria & k < maximumIteration) {
    t1P <- ((t1 / (1 + t1))^ k) 
    t2P <- ((t2 / (1 + t2))^ k) 
    t3P <- ((t3 / (1 + t3))^ k) 
    
    a11 <- -t4 * t1P * (2 * t4 * t1P * k * t1 * t2 - (t1 * t1) * t4 *
                          t1P * t2 * t3 - 2 * (t1 * t1) * t5 * t2P * t3 - 2 * t13P * t5 *
                          t2P * t3 + 2 * (t1 * t1) * t3P * t4 * t2 + 2 * t13P *
                          t3P * t4 * t2 + 2 * (t1 * t1) * t3P * t5 * t2 + 2 * t13P *
                          t3P * t5 * t2 + 5 * k * t1 * t5 * t2P + 4 * k * (t1 * t1) * t5 *
                          t2P + 5 * k * t1 * t3P * t2 - 4 * k * (t1 * t1) *
                          t3P * t4 - 5 * k * t1 * t3P * t5 - 4 * k * (t1 * t1) *
                          t3P * t5 - k * k * t5 * t2P * t3 - k * k * t5 * t2P * t1 - k * k *
                          t3P * t1 * t2 + t3P * k * t2 - 2 * t13P *
                          t3P + 2 * k * t1 * t4 * t1P * t3 + 2 * k * t1 * t4 *
                          t1P * t2 * t3 + 5 * k * t1 * t5 * t2P * t3 + 4 * k * (t1 * t1) * t5 *
                          t2P * t3 - 5 * k * t1 * t3P * t4 * t2 + k * k *
                          t3P * t4 * t2 + k * k * t3P * t4 * t1 + k * k *
                          t3P * t5 * t2 + k * k * t3P * t5 * t1 + k * t4 *
                          t1P * t3 + k * t5 * t2P * t3 - k * t3P * t4 * t2 - k *
                          t3P * t5 * t2 + 2 * t4 * t1P * k * t1 + t3P * k + 5 *
                          t3P * k * t1 - 4 * k * (t1 * t1) * t3P * t4 * t2 - 5 * k * t1 *
                          t3P * t5 * t2 - 4 * k * (t1 * t1) * t3P * t5 * t2 - k * k * t5 *
                          t2P * t1 * t3 + k * k * t3P * t4 * t1 * t2 + k * k *
                          t3P * t5 * t1 * t2 + k * t4 * t1P * t2 * t3 - 2 * t13P *
                          t3P * t2 + 2 * (t1 * t1) * t3P * t4 + 2 * t13P *
                          t3P * t4 + 2 * (t1 * t1) * t3P * t5 + 2 * t13P *
                          t3P * t5 + 4 * k * (t1 * t1) * t3P - k * k * t5 * t2P + t4 *
                          t1P * k - 2 * (t1 * t1) * t3P * t2 - 2 * t13P * t5 *
                          t2P - 2 * (t1 * t1) * t5 * t2P - k * k * t3P * t2 - k * k *
                          t3P * t1 + k * k * t3P * t4 + k * k * t3P * t5 + k * t5 *
                          t2P - k * t3P * t4 - k * t3P * t5 - t4 *
                          t1P * (t1 * t1) - t2 * t4 * t1P * (t1 * t1) + t4 *
                          t1P * k * t2 - 5 * k * t1 * t3P * t4 + 4 * k * (t1 * t1) *
                          t3P * t2 - (t1 * t1) * t4 * t1P * t3 - k * k *
                          t3P - 2 * (t1 * t1) * t3P) * t1aP / 
      (-t4 * t1P - t4 * t1P * t3 - t4 * t1P * t2 - t4 * t1P * t2 * t3 - t5 * t2P * t3 - t5 * t2P * t1 * t3 - t5 *
         t2P - t5 * t2P * t1 - t3P - t3P * t2 - t3P * t1 - t3P * t1 * t2 + t3P * t4 + t3P * t4 * t2 + t3P * t4 * t1 + t3P * t4 * t1 * t2 +
         t3P * t5 + t3P * t5 * t2 + t3P * t5 * t1 + t3P * t5 * t1 * t2) * t1bP 
    
    if (k > 0) test <- abs(a11/a[1,1])
    a[1,1] <- a[1,1] + a11
    k <- k+1
  }
  
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ## a12
  test <- 100
  k <- 0
  t1P <- ((t1 / (1 + t1))^ k) 
  t2P <- ((t2 / (1 + t2))^ k) 
  t3P <- ((t3 / (1 + t3))^ k) 
  while (test > criteria  &  k < maximumIteration) {
    a12 <- -(1 + t3) * t1P * t4 * (-t1 + k) * t5 *
      t2P * (-t2 + k) / t2 / (1 + t2) / t1 / (1 + t1) / (-t4 * t1P - t4 *
                                                           t1P * t3 - t4 * t1P * t2 - t4 * t1P * t2 * t3 - t5 *
                                                           t2P - t5 * t2P * t3 - t5 * t2P * t1 - t5 *
                                                           t2P * t1 * t3 - t3P - t3P * t2 -
                                                           t3P * t1 - t3P * t1 * t2 + t3P * t4 +
                                                           t3P * t4 * t2 + t3P * t4 * t1 + t3P * t4 * t1 * t2 +
                                                           t3P * t5 + t3P * t5 * t2 + t3P * t5 * t1 +
                                                           t3P * t5 * t1 * t2) 
    if (k > 0) test <- abs(a12/a[1,2])
    a[1,2] <- a[1,2] + a12
    k <- k+1
  }  
  
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  
  ## a13
  test <- 100
  k <- 0
  t1P <- ((t1 / (1 + t1))^ k) 
  t2P <- ((t2 / (1 + t2))^ k) 
  t3P <- ((t3 / (1 + t3))^ k) 
  
  while (test > criteria & k < maximumIteration) {
    a13 <- (1 + t2) * t1P * t4 * (-t1 + k) * (-1 + t4 + t5) *
      t3P * (-t3 + k) / t3 / (1 + t3) / t1 / (1 + t1) / (-t4 * t1P - t4 *
                                                           t1P * t3 - t4 * t1P * t2 - t4 * t1P * t2 * t3 - t5 *
                                                           t2P - t5 * t2P * t3 - t5 * t2P * t1 - t5 *
                                                           t2P * t1 * t3 - t3P - t3P * t2 -
                                                           t3P * t1 - t3P * t1 * t2 + t3P * t4 +
                                                           t3P * t4 * t2 + t3P * t4 * t1 + t3P * t4 * t1 * t2 +
                                                           t3P * t5 + t3P * t5 * t2 + t3P * t5 * t1 +
                                                           
                                                           t3P * t5 * t1 * t2) 
    
    if (k > 0) test <- abs(a13/a[1,3])
    a[1,3] <- a[1,3] + a13
    k <- k+1
    
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  
  ## a14
  test <- 100
  k <- 0
  t1P <- ((t1 / (1 + t1))^ k) 
  t2P <- ((t2 / (1 + t2))^ k) 
  t3P <- ((t3 / (1 + t3))^ k) 
  
  while (test > criteria & k < maximumIteration) {
    a14 <- -t1P * (-t1 + k) * (t5 * t2P * t3 + t5 * t2P +
                                 t3P * t2 - t3P * t5 * t2 + t3P -
                                 t3P * t5) / (t4 * t1P + t4 * t1P * t3 + t4 *
                                                t1P * t2 + t4 * t1P * t2 * t3 + t5 * t2P + t5 *
                                                t2P * t3 + t5 * t2P * t1 + t5 * t2P * t1 * t3 +
                                                t3P + t3P * t2 + t3P * t1 + t3P * t1 * t2 -
                                                t3P * t4 - t3P * t4 * t2 - t3P * t4 * t1 -
                                                t3P * t4 * t1 * t2 - t3P * t5 - t3P * t5 * t2 -
                                                t3P * t5 * t1 - t3P * t5 * t1 * t2) / t1 / (1 + t1) 
    if (k > 0) test <- abs(a14/a[1,4])
    a[1,4] <- a[1,4] + a14
    k <- k+1
    
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  
  ## a15
  test <- 100
  k <- 0
  t1P <- ((t1 / (1 + t1))^ k) 
  t2P <- ((t2 / (1 + t2))^ k) 
  t3P <- ((t3 / (1 + t3))^ k) 
  
  while (test > criteria & k < maximumIteration) {
    a15 <-  t1P * t4 * (-t1 + k) * (t2P + t2P * t3 -
                                      t3P - t3P * t2) / t1 / (1 + t1) / (t4 * t1P + t4 *
                                                                           t1P * t3 + t4 * t1P * t2 + t4 * t1P * t2 * t3 + t5 *
                                                                           t2P + t5 * t2P * t3 + t5 * t2P * t1 + t5 *
                                                                           t2P * t1 * t3 + t3P + t3P * t2 +
                                                                           t3P * t1 + t3P * t1 * t2 - t3P * t4 -
                                                                           t3P * t4 * t2 - t3P * t4 * t1 - t3P * t4 * t1 * t2 -
                                                                           t3P * t5 - t3P * t5 * t2 - t3P * t5 * t1 -
                                                                           t3P * t5 * t1 * t2)
    if (k > 0) test <- abs(a15/a[1,5])
    a[1,5] <- a[1,5] + a15
    k <- k+1
    
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ## a22
  test <- 100
  k <- 0
  ## missing t1p etc assignment here?
  while (test > criteria & k < maximumIteration) {
    a22 <- -t5 * t2P * (-k * k * t3P * t5 + k *
                          t3P * t4 + k * k * t4 * t1P + 2 * (t2 * t2) * t4 * t1P + 2 *
                          t23P * t4 * t1P + 2 * (t2 * t2) * t3P * t1 + 2 * t23P *
                          t3P * t1 - 4 * k * (t2 * t2) * t3P - 2 *
                          t3P * t5 * (t2 * t2) - 2 * t5 * t23P * t3P - 2 * t4 * t23P *
                          t3P - 2 * t3P * (t2 * t2) * t4 + 5 * k * t1 *
                          t3P * t4 * t2 + 2 * (t2 * t2) * t3P + 2 * t23P *
                          t3P - k * t5 * t2P - k * k * t3P * t4 + k * k *
                          t3P * t1 - k * t1 * t5 * t2P - 5 * k * t1 *
                          t3P * t2 + k * t1 * t3P * t4 + k * t1 * t3P * t5 + k * k *
                          t3P * t1 * t2 - k * k * t3P * t4 * t2 - k * k *
                          t3P * t4 * t1 - k * k * t3P * t5 * t2 - k * k *
                          t3P * t5 * t1 - k * t4 * t1P * t3 - 5 * k * t4 *
                          t1P * t2 - k * t5 * t2P * t3 + 5 * k *
                          t3P * t4 * t2 + 5 * k * t3P * t5 * t2 - k * t1 * t5 *
                          t2P * t3 + 5 * k * t1 * t3P * t5 * t2 - k * k *
                          t3P * t4 * t1 * t2 - k * k * t3P * t5 * t1 * t2 - 5 * k * t4 *
                          t1P * t2 * t3 + k * k * t3P * t2 - 2 * t5 *
                          t2P * k * t1 * t2 - 2 * k * t2 * t5 * t2P * t1 * t3 + k * k * t4 *
                          t1P * t3 + k * k * t4 * t1P * t2 + t1 * t5 *
                          t2P * (t2 * t2) + 4 * t3P * k * t4 * (t2 * t2) - 2 * t5 *
                          t2P * k * t2 - 2 * t3P * t5 * t1 * t23P - 2 *
                          t3P * t5 * t1 * (t2 * t2) + 4 * t5 * k * (t2 * t2) * t3P + 2 * (t2 * t2) * t4 *
                          t1P * t3 + 2 * t23P * t4 * t1P * t3 + (t2 * t2) * t5 *
                          t2P * t3 - 4 * k * (t2 * t2) * t4 * t1P - 4 * k * (t2 * t2) *
                          t3P * t1 - 2 * t3P * t1 * t23P * t4 - 2 *
                          t3P * t1 * (t2 * t2) * t4 + (t2 * t2) * t5 * t2P * t1 * t3 - 4 * k * (t2 * t2) * t4 *
                          t1P * t3 + 4 * t3P * k * t4 * (t2 * t2) * t1 + 4 * t5 * t1 * k * (t2 * t2) *
                          t3P + k * k * t4 * t1P * t2 * t3 + k * t3P * t5 - k * t1 *
                          t3P - t4 * t1P * k + k * k * t3P - k *
                          t3P - 5 * k * t3P * t2 - 2 * k * t2 * t5 * t2P * t3 + t5 *
                          t2P * (t2 * t2)) * t2aP / (t4 * t1P + t4 *
                                                       t1P * t3 + t4 * t1P * t2 + t4 * t1P * t2 * t3 + t5 *
                                                       t2P + t5 * t2P * t3 + t5 * t2P * t1 + t5 *
                                                       t2P * t1 * t3 + t3P + t3P * t2 +
                                                       t3P * t1 + t3P * t1 * t2 - t3P * t4 -
                                                       t3P * t4 * t2 - t3P * t4 * t1 - t3P * t4 * t1 * t2 -
                                                       t3P * t5 - t3P * t5 * t2 - t3P * t5 * t1 -
                                                       t3P * t5 * t1 * t2) * t2bP 
    
    
    if (k > 0) test <- abs(a22/a[2,2])
    a[2,2] <- a[2,2] + a22
    k <- k+1
  }
  
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ##
  ## a23
  test <- 100
  k <- 0
  while (test > criteria & k < maximumIteration) {
    
    t1P <- ((t1 / (1 + t1))^ k) 
    t2P <- ((t2 / (1 + t2))^ k) 
    t3P <- ((t3 / (1 + t3))^ k) 
    
    #fix math later? formatting problems
    a23 <- -(1 + t1) * t2P * t5 * (-t2 + k) * (-1 + t4 + t5) *
      t3P * (-t3 + k) / t3 / (1 + t3) / t2 / (1 + t2) / (t4 * t1P + t4 *
                                                           t1P * t3 + t4 * t1P * t2 + t4 * t1P * t2 * t3 + t5 *
                                                           t2P * t3 + t5 * t2P * t1 * t3 + t5 * t2P + t5 *
                                                           t2P * t1 - t3P * t4 - t3P * t4 * t2 +
                                                           t3P + t3P * t2 + t3P * t1 +
                                                           t3P * t1 * t2 - t3P * t4 * t1 - t3P * t4 * t1 * t2 -
                                                           t3P * t5 - t3P * t5 * t2 - t3P * t5 * t1 -
                                                           t3P * t5 * t1 * t2) 
    
    if (k > 0) test <- abs(a23/a[2,3])
    a[2,3] <- a[2,3] + a23
    k <- k+1
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ## a24
  test <- 100
  k <- 0
  while (test > criteria & k < maximumIteration) {
    
    t1P <- ((t1 / (1 + t1))^ k) 
    t2P <- ((t2 / (1 + t2))^ k) 
    t3P <- ((t3 / (1 + t3))^ k) 
    
    #fix math later? formatting problems
    a24 <- t5 * t2P * (-t2 + k) * (t1P + t1P * t3 -
                                     t3P - t3P * t1) / (1 + t2) / (t4 * t1P + t4 *
                                                                     t1P * t3 + t4 * t1P * t2 + t4 * t1P * t2 * t3 + t5 *
                                                                     t2P * t3 + t5 * t2P * t1 * t3 + t5 * t2P + t5 *
                                                                     t2P * t1 - t3P * t4 - t3P * t4 * t2 +
                                                                     t3P + t3P * t2 + t3P * t1 +
                                                                     t3P * t1 * t2 - t3P * t4 * t1 - t3P * t4 * t1 * t2 -
                                                                     t3P * t5 - t3P * t5 * t2 - t3P * t5 * t1 -
                                                                     t3P * t5 * t1 * t2) / t2 
    
    
    if (k > 0) test <- abs(a24/a[2,4])
    a[2,4] <- a[2,4] + a24
    k <- k+1
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ## a25
  test <- 100
  k <- 0
  while (test > criteria & k < maximumIteration) {
    
    t1P <- ((t1 / (1 + t1))^ k) 
    t2P <- ((t2 / (1 + t2))^ k) 
    t3P <- ((t3 / (1 + t3))^ k) 
    
    #fix math later? formatting problems
    a25 <-  -t2P * (-t2 + k) * (t3P - t3P * t4 + t4 *
                                  t1P * t3 + t4 * t1P + t3P * t1 -
                                  t3P * t4 * t1) / (t4 * t1P + t4 * t1P * t3 + t4 *
                                                      t1P * t2 + t4 * t1P * t2 * t3 + t5 * t2P * t3 + t5 *
                                                      t2P * t1 * t3 + t5 * t2P + t5 * t2P * t1 -
                                                      t3P * t4 - t3P * t4 * t2 + t3P +
                                                      t3P * t2 + t3P * t1 + t3P * t1 * t2 -
                                                      t3P * t4 * t1 - t3P * t4 * t1 * t2 - t3P * t5 -
                                                      t3P * t5 * t2 - t3P * t5 * t1 -
                                                      t3P * t5 * t1 * t2) / t2 / (1 + t2) 
    
    if (k > 0) test <- abs(a25/a[2,5])
    a[2,5] <- a[2,5] + a25
    k <- k+1
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  
  ## a33
  test <- 100
  k <- 0
  while (test > criteria & k < maximumIteration) {
    
    t1P <- ((t1 / (1 + t1))^ k) 
    t2P <- ((t2 / (1 + t2))^ k) 
    t3P <- ((t3 / (1 + t3))^ k) 
    
    #fix math later? formatting problems
    a33 <-  (-1 + t4 + t5) * t3P * (k * k * t5 * t2P * t1 + k * k * t4 *
                                      t1P * t2 * t3 - (t3 * t3) * t3P * t5 * t1 - 2 * k * t3 *
                                      t3P * t1 - 5 * k * t4 * t1P * t3 - k * t4 *
                                      t1P * t2 - 5 * k * t5 * t2P * t3 + k *
                                      t3P * t4 * t2 + 2 * (t3 * t3) * t4 * t1P + 2 * t33P * t4 *
                                      t1P + 2 * t33P * t5 * t2P + 2 * (t3 * t3) * t5 *
                                      t2P - (t3 * t3) * t3P * t4 + (t3 * t3) *
                                      t3P * t2 + (t3 * t3) * t3P * t1 - (t3 * t3) *
                                      t3P * t5 + k * t1 * t3P * t5 + k * t1 *
                                      t3P * t4 - k * t1 * t5 * t2P + k *
                                      t3P * t5 * t2 + k * k * t4 * t1P + k * k * t4 *
                                      t1P * t3 + k * k * t4 * t1P * t2 - (t3 * t3) *
                                      t3P * t4 * t2 + (t3 * t3) * t3P * t1 * t2 + 2 * t33P * t5 *
                                      t2P * t1 + 2 * (t3 * t3) * t5 * t2P * t1 - 2 * k * t3 *
                                      t3P * t2 + 2 * k * t3 * t3P * t4 + 2 * (t3 * t3) * t4 *
                                      t1P * t2 - t4 * t1P * k + k * t3P * t5 + k *
                                      t3P * t4 - k * t5 * t2P - k * t1 * t3P + k * k * t5 *
                                      t2P - k * t3P * t2 - (t3 * t3) * t3P * t5 * t2 - (t3 * t3) *
                                      t3P * t4 * t1 + 2 * k * t3 * t3P * t5 - 4 * k * (t3 * t3) * t5 *
                                      t2P - 4 * k * (t3 * t3) * t4 * t1P + 2 * t33P * t4 *
                                      t1P * t2 - k * t3P + k * k * t5 * t2P * t3 - (t3 * t3) *
                                      t3P * t4 * t1 * t2 - (t3 * t3) * t3P * t5 * t1 * t2 - 4 * k * (t3 * t3) * t4 *
                                      t1P * t2 - 4 * k * (t3 * t3) * t5 * t2P * t1 + 2 * k * t3 *
                                      t3P * t4 * t2 - 2 * k * t3 * t3P * t1 * t2 + 2 * k * t3 *
                                      t3P * t4 * t1 + 2 * k * t3 * t3P * t4 * t1 * t2 + 2 * k * t3 *
                                      t3P * t5 * t2 + 2 * k * t3 * t3P * t5 * t1 + 2 * k * t3 *
                                      t3P * t5 * t1 * t2 - 5 * k * t1 * t5 * t2P * t3 + k * t1 *
                                      t3P * t4 * t2 + k * t1 * t3P * t5 * t2 + k * k * t5 *
                                      t2P * t1 * t3 - k * t1 * t3P * t2 + (t3 * t3) *
                                      t3P - 5 * k * t4 * t1P * t2 * t3 - 2 * k * t3 * t3P) / (t4 *
                                                                                                t1P + t4 * t1P * t3 + t4 * t1P * t2 + t4 *
                                                                                                t1P * t2 * t3 + t5 * t2P * t3 + t5 * t2P * t1 * t3 + t5 *
                                                                                                t2P + t5 * t2P * t1 - t3P * t4 -
                                                                                                t3P * t4 * t2 + t3P + t3P * t2 +
                                                                                                t3P * t1 + t3P * t1 * t2 - t3P * t4 * t1 -
                                                                                                t3P * t4 * t1 * t2 - t3P * t5 - t3P * t5 * t2 -
                                                                                                t3P * t5 * t1 - t3P * t5 * t1 * t2) * t3bP * t3aP 
    
    
    
    if (k > 0) test <- abs(a33/a[3,3])
    a[3,3] <- a[3,3] + a33
    k <- k+1
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  
  ## a34
  test <- 100
  k <- 0
  while (test > criteria & k < maximumIteration) {
    
    t1P <- ((t1 / (1 + t1))^ k) 
    t2P <- ((t2 / (1 + t2))^ k) 
    t3P <- ((t3 / (1 + t3))^ k) 
    
    #fix math later? formatting problems
    a34 <-  -t3P * (-t3 + k) * (t5 * t1P - t1P - t2 *
                                  t1P + t5 * t2 * t1P - t5 * t2P - t5 *
                                  t2P * t1) / (t4 * t1P + t4 * t1P * t3 + t4 *
                                                 t1P * t2 + t4 * t1P * t2 * t3 + t5 * t2P * t3 + t5 *
                                                 t2P * t1 * t3 + t5 * t2P + t5 * t2P * t1 -
                                                 t3P * t4 - t3P * t4 * t2 + t3P +
                                                 t3P * t2 + t3P * t1 + t3P * t1 * t2 -
                                                 t3P * t4 * t1 - t3P * t4 * t1 * t2 - t3P * t5 -
                                                 t3P * t5 * t2 - t3P * t5 * t1 -
                                                 t3P * t5 * t1 * t2) / t3 / (1 + t3) 
    
    
    
    if (k > 0) test <- abs(a34/a[3,4])
    a[3,4] <- a[3,4] + a34
    k <- k+1
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ## a35
  test <- 100
  k <- 0
  while (test > criteria & k < maximumIteration) {
    
    t1P <- ((t1 / (1 + t1))^ k) 
    t2P <- ((t2 / (1 + t2))^ k) 
    t3P <- ((t3 / (1 + t3))^ k) 
    
    #fix math later? formatting problems
    a35 <-  t3P * (-t3 + k) * (-t4 * t2P + t1 * t2P + t4 *
                                 t1P + t2P - t4 * t1 * t2P + t4 *
                                 t1P * t2) / (t4 * t1P + t4 * t1P * t3 + t4 *
                                                t1P * t2 + t4 * t1P * t2 * t3 + t5 * t2P * t3 + t5 *
                                                t2P * t1 * t3 + t5 * t2P + t5 * t2P * t1 -
                                                t3P * t4 - t3P * t4 * t2 + t3P +
                                                t3P * t2 + t3P * t1 + t3P * t1 * t2 -
                                                t3P * t4 * t1 - t3P * t4 * t1 * t2 - t3P * t5 -
                                                t3P * t5 * t2 - t3P * t5 * t1 -
                                                t3P * t5 * t1 * t2) / t3 / (1 + t3)
    
    
    
    
    if (k > 0) test <- abs(a35/a[3,5])
    a[3,5] <- a[3,5] + a35
    k <- k+1
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  
  
  ## a44
  test <- 100
  k <- 0
  while (test > criteria & k < maximumIteration) {
    
    t1P <- ((t1 / (1 + t1))^ k)
    t2P <- ((t2 / (1 + t2))^ k)
    t3P <- ((t3 / (1 + t3))^ k)
    
    #fix math later? formatting problems
    a44 <-   (1 + t2) * (t1P + t1P * t3 - t3P -
                           t3P * t1 ^ 2) / (1 + t3) / (1 + t1) / (t4 * t1P + t4 *
                                                                    t1P * t3 + t4 * t1P * t2 + t4 * t1P * t2 * t3 + t5 *
                                                                    t2P * t3 + t5 * t2P * t1 * t3 + t5 * t2P + t5 *
                                                                    t2P * t1 - t3P * t4 - t3P * t4 * t2 +
                                                                    t3P + t3P * t2 + t3P * t1 + t3P * t1 * t2 -
                                                                    t3P * t4 * t1 - t3P * t4 * t1 * t2 - t3P * t5 -
                                                                    t3P * t5 * t2 - t3P * t5 * t1 - t3P * t5 * t1 * t2)
    
    
    
    
    if (k > 0) test <- abs(a44/a[4,4])
    a[4,4] <- a[4,4] + a44
    k <- k+1
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ## a45
  test <- 100
  k <- 0
  while (test > criteria & k < maximumIteration) {
    
    t1P <- ((t1 / (1 + t1))^ k) 
    t2P <- ((t2 / (1 + t2))^ k) 
    t3P <- ((t3 / (1 + t3))^ k) 
    
    #fix math later? formatting problems
    a45 <-  (t1P + t1P * t3 - t3P -
               t3P * t1) * (t2P + t2P * t3 - t3P -
                              t3P * t2) / (1 + t3) / (t4 * t1P + t4 * t1P * t3 + t4 *
                                                        t1P * t2 + t4 * t1P * t2 * t3 + t5 * t2P * t3 + t5 *
                                                        t2P * t1 * t3 + t5 * t2P + t5 * t2P * t1 -
                                                        t3P * t4 - t3P * t4 * t2 + t3P + t3P * t2 +
                                                        t3P * t1 + t3P * t1 * t2 - t3P * t4 * t1 -
                                                        t3P * t4 * t1 * t2 - t3P * t5 - t3P * t5 * t2 -
                                                        t3P * t5 * t1 - t3P * t5 * t1 * t2) 
    
    
    
    if (k > 0) test <- abs(a45/a[4,5])
    a[4,5] <- a[4,5] + a45
    k <- k+1
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  
  ## a55
  test <- 100
  k <- 0
  while (test > criteria & k < maximumIteration) {
    
    t1P <- ((t1 / (1 + t1))^ k) 
    t2P <- ((t2 / (1 + t2))^ k) 
    t3P <- ((t3 / (1 + t3))^ k) 
    
    #fix math later? formatting problems
    a55 <-  (1 + t1) * (t2P + t2P * t3 - t3P -
                          t3P * t2 ^ 2) / (1 + t3) / (1 + t2) / (t4 * t1P + t4 *
                                                                   t1P * t3 + t4 * t1P * t2 + t4 * t1P * t2 * t3 + t5 *
                                                                   t2P * t3 + t5 * t2P * t1 * t3 + t5 * t2P + t5 *
                                                                   t2P * t1 - t3P * t4 - t3P * t4 * t2 +
                                                                   t3P + t3P * t2 + t3P * t1 + t3P * t1 * t2 -
                                                                   t3P * t4 * t1 - t3P * t4 * t1 * t2 - t3P * t5 -
                                                                   t3P * t5 * t2 - t3P * t5 * t1 - t3P * t5 * t1 * t2) 
    
    
    
    
    if (k > 0) test <- abs(a55/a[5,5])
    a[5,5] <- a[5,5] + a55
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