Best_Models <- function(bestCount, maximumObservation, cvrare) {
  ## how to get best count, bestAiCc, GOF5Test, bestModelTau
  
  bestModel <- rep(NA, maximumObservation + 1)
  bestModelTau <- matrix(NA, nrow=10, ncol=2)
  
  flag <- -1
  
  if (bestCount[1] > 0) {
    flag <- 1
  } else if (bestCount[2] > 0) {
    flag <- 2
  } else if (bestCount[0] > 0) {
    flag <- 0
  }
  
  ##Apply entirely different Criteria if all filters fail
  if (flag == -1 & bestCount[3] > 0) {
    flag <- 0
    testValue <- rep(NA, obsMax + 1)
    order <- rep(NA, obsMax + 1)
    for(r in 1:obsMax) { #index?
      minAICc <- 10000000
      
      for(m in 1:6) {
        if (bestAICc[flag, m, r] > 0.0 &
            bestAICc[flag, m, r] < minAICc) {
        
            bestModel[r] <- m 
            minAICc <- bestAICc[flag, m, r] 
        }
      }
      testValue[r] <- GOf5Test[bestModel[r], r]
      order[r] <- r
    }
    #sort the arrays based on K-V pairings
    testValueOrder <- order(testValue)
    
    testValue[testValueOrder]
    order[testValueOrder]
    
    ##Arrays.sort(testValue, order)
    
    bm <- 0
    b <- 0
    while (bm < 4 & b <= maximumObservation) {
      if(testValue[b] > 0) {
        bestModelTau[bm, 0] <- bestModel[order[b]]
        bestModelTau[bm, 1] <- order[b]
        bm <- bm + 1
      }
      b <- b + 1
    }
    
    ##If there are candidate models proceed to find best ones
  } else if (flag >= 0) {
    for(r in 1:maximumObservation) {
      minAICc <- 10000000
      for(m in 1:6) {
        if (bestAICc[flag, m, r] > 0.0 &
            bestAICc[flag, m, r] < minAICc) {
        
          bestModel[r] <- m 
          minAICc <- bestAICc[flag, m, r] 
        }
      }
    }
    maxGOF0 <- bestGOF0[flag, bestModel[freqTau10], freqTau10]
    
    for(r in 1:maximumObservation) {
      if (bestModel[r] > 0) {
        ##Find Best Model--largest tau with GOF0 >= 0.01
        if (bestGOF0[flag, bestModel[r], r] >= 0.01) {
          bestModelTau[0, 0] <- bestModel[r] 
          bestModelTau[0, 1] <- r 
        }
      
        ##Find Good Model 1--tau with largest GOF0
        if (bestGOF0[flag, bestModel[r], r] >= maxGOF0) {
        
          bestModelTau[1, 0] <- bestModel[r] 
          bestModelTau[1, 1] <- r 
          maxGOF0 <- bestGOF0[flag, bestModel[r], r] 
        }
        
        ##Find Good Model 2--largest tau with data
        if (bestGOF0[flag, bestModel[r], r] > 0) {
        
          bestModelTau[2, 0] <- bestModel[r] 
          bestModelTau[2, 1] <- r 
        }
      }
    }
    
    ##Find Good Model 3--tau closest to 10 with data
    if (bestGOF0[flag, bestModel[freqTau10], freqTau10] > 0) {
      bestModelTau[3, 0] <- bestModel[freqTau10]
      bestModelTau[3, 1] <- freqTau10
    } else {
      ##look > tau10
      t <- freqTau10 + 1; ##how to get freqTau10
      while (bestModelTau[3, 1] == 0 & t <= obsMax)
      {
        if (bestModel[t] > 0)
        {
          
          if (bestGOF0[flag, bestModel[t], t] > 0)
          {
            bestModelTau[3, 0] <- bestModel[t]
            bestModelTau[3, 1] <- t
          }
        }
        t <- t + 1
      }
      
      ##look < tau10
      t = freqTau10 - 1;
      while (bestModelTau[3, 1] == 0 & t > 0)
      {
        if (bestModel[t] > 0)
        {
          
          if (bestGOF0[flag, bestModel[t], t] > 0)
          {
            bestModelTau[3, 0] <- bestModel[t]
            bestModelTau[3, 1] <- t
          }
        }
        t <- t - 1
      }
      
    }
    
    
  }
  
 
  
}