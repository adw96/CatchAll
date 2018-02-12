ACEModel <- function (s, r, observedCount, n, frequency, singletons, maximumObservation,
                      GTEstimate, G, gammaSqRare, cvRare) {
  
  GTEstimate[r] <- s[r] / (1.0 - (singletons / n[r]))
  
  print(paste("GTEstimate[r]", GTEstimate[r], sep = " "))
  G[r] <- 0
  rr <- 1
  for(i in 1:frequency[r]) {
    if (i == frequency[rr]) {
      G[r] <- G[r] + (i * (i - 1) * observedCount[rr])
      r <- r + 1
    }
  }
  
  print(paste("G[r]", G[r], sep = " ")) #NA, why?
  gammaSqRare[r] <- (GTEstimate[r] * G[r] / (n[r] * (n[r] - 1.0))) - 1.0
  if (gammaSqRare[r] < 0) {
    gammaSqRare[r] <- 0
    cvRare[r] <- 0
  } else {
    cvRare[r] <- sqrt(gammaSqRare[r])
  }
  
  f1 <- singletons
  sHatSubset <- GTEstimate[r] + ((f1 / (1.0 - (f1 / n[r])) * gammaSqRare[r]))
  sHatTotal <- sHatSubset + (s[maximumObservation] - s[r])
  
  ## Standard Error
  derCHat1 <- ((n[r] + s[r]) / (n[r] - f1)) + (((f1 + n[r]) / (n[r] - f1)) *
       ((s[r] / (n[r] - f1)) * (G[r] / (n[r] - 1.0)) - 1.0)) + ((n[r] * f1 * G[r] * (n[r] - 1.0 - s[r])) /
       (((n[r] - f1) * (n[r] - f1) * (n[r] - 1.0) * (n[r] - 1.0))))
  
  a <- pow(derCHat1, 2) * f1 * (1.0 - (f1 / sHatSubset))
  
  derCHati <- rep(NA, frequency[r] + 1)
  
  #should either be 1:freq[r] or 2:freq[r]
  derCHati <- (((n[r] * n[r]) - (f1 * n[r]) -
                  (f1 * s[r] * (2:freq[r]))) / ((n[r] - f1) * (n[r] - f1))) - (((f1 * f1 * (2:freq[r])) / ((n[r] - f1) * (n[r] - f1))) *
       ((s[r] / (n[r] - f1)) * (G[r] / (n[r] - 1.0)) - 1.0)) + (((n[r] * f1) / (n[r] - f1)) *
       (((n[r] - f1 - s[r] * (2:freq[r])) / ((n[r] - f1) * (n[r] - f1))) * (G[r] / (n[r] - 1.0)) +
          (((s[r] / (n[r] - f1)) * (((n[r] - 1.0) * (2:freq[r]) * ((2:freq[r]) - 1.0)) - G[r] * (2:freq[r])) / ((n[r] - 1.0) * (n[r] - 1.0))))))
  
  B <- 0.0
  C <- 0.0
  D <- 0.0
  
  rr <- 1
  rr <- max(which(frequency[rr] < 2))
  
  for(i in 2:freq[r]) {
    if (i == freq[rr]) {
      B <- B + (derCHati[i] * derCHati[i] * observedCount[rr] *
              (1.0 - (observedCount[rr] / sHatSubset)))
      
      C <- C +(derCHat1 * derCHati[i] * (-f1 * observedCount[rr] /
                                        sHatSubset))
      rj <- 1
      rj <- max(which(frequency[rj] < i & rj < r))
      
      for (j in (i+1):frequency[r]) {
        if (j == freq[rj]) {
          D <- D + (derCHati[i] * derCHati[j] * (-observedCount[rr] * observedCount[rj] / sHatSubset))
          rj <- rj + 1
        }
      }
      rr <- rr + 1
    }
  }
  
  se <- A + B + 2.0 * C + 2.0 * D
  if (se >= 0.0) {
    se <- sqrt(se)
    seFlag <- 1
  }
  
  if (seFlag == 1) {
    ##Confidence Bounds
    boundsCheck <- GetConfidenceBounds(r, se, sHatSubset, s, maximumObservation)
  }
  
  
  output <- data.frame("Model" = "ACEModel", 
                       "Cutoff" = r, 
                       "Estimate" = CheckOutput(sHatTotal), 
                       "SE" = CheckOutput(se), 
                       "LCB"= CheckOutput(boundsCheck$lcb), 
                       "UCB" = CheckOutput(boundsCheck$ucb), 
                       "chiSq" = NA,
                       "AIC" = NA, 
                       "AICc" = NA, 
                       "GOF0" = NA, 
                       "GOF5" = NA,
                       "T1"= NA,
                       "T2"= NA,
                       "T3"= NA,
                       "T4"=NA,
                       "T5"=NA,
                       "T6"=NA,
                       "T7"=NA,
                       "CVRare"=cvRare[r])
          
}