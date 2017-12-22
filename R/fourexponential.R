#fix formatting later
FourExponentialModel <-  function(s, r, observedCount, n, 
                                    s0Init, frequency, 
                                    lnSFactorial, sumlnFFactorial, 
                                    maximumObservation) {
  
  
  ### Fits
  numParams <- 7
  # 
  # //find fmin frequency
  # int r = 1;
  # while (freq[r] < fMin)
  #   r++;
  # int freqMin = r;
  # 
  # for (r = freqMin; r <= obsMax; r++)
  # {
  #   double u1 = 0.0;
  #   double u2 = 0.0;
  #   double u3 = 0.0;
  #   
  #   double mle1 = 0.0;
  #   double mle2 = 0.0;
  #   double mle3 = 0.0;
  #   double mle4 = 0.0;
  #   double mle5 = 0.0;
  #   double mle6 = 0.0;
  #   double mle7 = 0.0;
  #   
  #   int fitsCheck = 1;
  #   
  #   double[] fitsCount = new double[freq[r] + 1];
  #   
  #   int MLEFlag = FourMixedExponentialFits(r, ref u1, ref u2, ref u3,
  #                                          ref mle1, ref mle2, ref mle3, ref mle4,
  #                                          ref mle5, ref mle6, ref mle7, ref fitsCheck,
  #                                          ref fitsCount);
  
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
 
#fix mle1, mle2...
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
  
  part2 <- sum((observedCount[1:r] * Math.Log(
    (u1 * ((1.0 / mle1) * Math.Pow((mle1 / (1.0 + mle1)), freq[1:r]))) +
      (u2 * ((1.0 / mle2) * Math.Pow((mle2 / (1.0 + mle2)), freq[1:r]))) +
      (u3 * ((1.0 / mle3) * Math.Pow((mle3 / (1.0 + mle3)), freq[1:r]))) +
      ((1.0 - u1 - u2 - u3) * ((1.0 / mle4) *
                                 Math.Pow((mle4 / (1.0 + mle4)), freq[1:r]))))))
  
  
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
    # confused where 1e6 came from
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
    results$mlesSExp1 <- t1
    results$mlesSExp2 <- t2
    results$mlesSExp3 <- t3
    results$flag <- ifelse(is.nan(part2), 0, 1)
    
  } else {
    results$flag <- 0
  }
  results
}