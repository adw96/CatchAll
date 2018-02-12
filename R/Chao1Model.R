Chao1Model <- function(s,r,observedCount,n,frequency,singletons,maximumObservation) {
    f1 <- singletons
  
    ##estimated total species
    sHatTotal <- 0.0
    ##Standard Error
    se <- 0.0
    seFlag <- 0
    
    ##if there are doubletons
    if (frequency[1] == 2 || frequency[2] == 2) {
      sHatTotal <- s[maximumObservation] + (pow(f1, 2) / (2.0 * observedCount[2]))
      
      temp <- f1 / observedCount[2]
      se <- observedCount[2] * ((0.5 * (temp * temp)) +
                                  (pow(temp, 3)) + (0.25 * (pow(temp, 4))))
      if (se >= 0.0)
      {
        se <- sqrt(se)
        seFlag <- 1
      }
    } else {
      sHatTotal <- s[maximumObservation] + (f1 * (f1 - 1.0) / 2.0)
      
      se = (0.5 * f1 * (f1 - 1.0)) + (0.25 * f1 * 2.0 *
                                        (f1 - 1.0) * 2.0 * (f1 - 1.0)) - (0.25 * f1 * f1 * f1 * f1 /
                                                                            sHatTotal)
      
      if (se >= 0.0) {
        se <- sqrt(se)
        seFlag <- 1
      }
    }
    
    
    sHatSubset <- sHatTotal - (s[maximumObservation] - s[r])
    
    if (seFlag == 1) {
      ##Confidence Bounds
      boundsCheck <- GetConfidenceBounds(r, se, sHatSubset, s, maximumObservation)
    }
    
    output <- data.frame("Model" = "Chao1", 
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
                         "T7"=NA)
  }