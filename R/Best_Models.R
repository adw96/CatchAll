Best_Models <- function(bestCount, maximumObservation, frequencyTau10, bestGOF0, 
                        bestAICc, GOFTest, cvrare) {
  ## how to get best count...
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
    maxGOF0 <- bestGOF0[flag, bestModel[frequencyTau10], frequencyTau10]
    
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
    if (bestGOF0[flag, bestModel[frequencyTau10], frequencyTau10] > 0) {
      bestModelTau[3, 0] <- bestModel[frequencyTau10]
      bestModelTau[3, 1] <- frequencyTau10
    } else {
      ##look > tau10
      t <- frequencyTau10 + 1  ##how to get frequencyTau10
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
      t = frequencyTau10 - 1 
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
  
  ##Find Good Model 4--best WLRM
  ##if there are taus with G0FO >= 0.01, find the max tau
  if (freqMax > 0) {
  
    bestModelTau[4, 0] <- 6 
    maxGOF0WLRM <- 0.01
    bestModelTau[4, 1] <- 0
    for (r in 1:frequencyMaximum) {
      ##determine whether to use logged (6) or unlogged(7) version
      if (WLRMSwitch[r] == 0) {
      
        if (bestGOF0[0, 6, r] >= maxGOF0WLRM) {
          bestModelTau[4, 0] <- 6
          bestModelTau[4, 1] <- r
        }
      }
      else if (WLRMSwitch[r] == 1) {
        if (bestGOF0[0, 7, r] >= maxGOF0WLRM) {
          bestModelTau[4, 0] <- 7 
          bestModelTau[4, 1] <- r 
        }
      }
    }
    
    ##if there are no taus with GOF0 >= 0.01
    ##use tau with highest GOF0
    if (bestModelTau[4, 1] == 0) {
      if (WLRMSwitch[1] == 0) {
        maxGOF0WLRM <- bestGOF0[0, 6, 1]
      } else if (WLRMSwitch[1] == 1)
        maxGOF0WLRM <- bestGOF0[0, 7, 1] 
      
      for (r in 2:maximumFrequency) {
        if (WLRMSwitch[r] == 0)
        {
          if (bestGOF0[0, 6, r] >= maxGOF0WLRM)
          {
            bestModelTau[4, 0] <- 6 
            bestModelTau[4, 1] <- r 
            maxGOF0WLRM <- bestGOF0[0, 6, r] 
          }
        }
        else if (WLRMSwitch[r] == 1) {
          if (bestGOF0[0, 7, r] >= maxGOF0WLRM) {
            bestModelTau[4, 0] <- 7 
            bestModelTau[4, 1] <- r 
            maxGOF0WLRM <- bestGOF0[0, 7, r] 
          }
        }
      }
    }
  }
  
  ##if there are none, leave blank
  if (bestModelTau[4, 1] == 0) {
    bestModelTau[4, 0] <- 0 
  }
  
  ##Find Chao1 Model--all taus give the  same answer
  bestModelTau[5, 0] <- 8 
  bestModelTau[5, 1] <- freqTau10 
  
  ##Find ACE/ACE1 Model closest to tau = 10
  if (cvrare <= 0.8) {
    bestModelTau[6, 0] <- 9 
  } else {
    bestModelTau[6, 0] <- 10 
  }
  bestModelTau[6, 1] = freqTau10 
  
  ##Max Tau for Best Model
  if (flag >= 0) {
    bestModelTau[7, 0] <- bestModelTau[0, 0] 
    bestModelTau[7, 1] <- maximumObservation
  }
  
  ##Max Tau for WLRM Model
  if (WLRMSwitch[freqMax] == 0) {
    bestModelTau[8, 0] <- 6 
  } else if (WLRMSwitch[freqMax] == 1) {
    bestModelTau[8, 0] <- 7 
  }
  
  ##if there are none
  if (bestModelTau[4, 0] == 0) {
    bestModelTau[8, 0] <- 0 
  }
  bestModelTau[8, 1] <- freqMax 
  
  ##ACE Model at tau = 10 or < 10 if necessary
  bestModelTau[9, 0] <- 9 
  bestModelTau[9, 1] <- frequencyTau10 
  
  
  ## some output thing
  ## OutputBestModelsAnalysis , need to pass in all the results?
}

# pass in what's already been outputted
OutputBestModelsAnalysis <- function(output) {
 
  foundAnalysis <- rep(NA, 10)
  bestAnalysis <- rep(NA, 10) #string, not sure how used
  
  
  #output
  bestModels <- list()
  #reads in data from all of the outputs of the models output
  numRows <- nrows(output)
  
  currRow <- 0

  ## read the output
  for(bm in 0:10) {
    ## look at the entire row 
    data <- output[currRow,]
    while(foundAnalysis[bm] == 0 && currRow < numRows) {
      ##strcmp? is it a string
      if(output[currRow,0] == aConditionHere && output[currRow, 1] == frequency[bestModelTau[bm,1]]) {
        foundAnalysis[bm] <- 1
        bestAnalysis[bm] <- data
      }
      currRow <- currRow + 1
    }
  }

  
  #remember index starts at 1
  bestDescription <- list("Best Parm Model", "Parm Model 2a  ", "Parm Model 2b  ", "Parm Model 2c  ",
                          "WLRM           ", "Non-P 1        ",
                          "Non-P 2        ", "Parm Max Tau   ",
                          "WLRM Max Tau   ", "Non-P 3        ",
                          "Best Discounted")
  
  ## for non-parametric?
  T <- rep(NA, 7)
  sHatTotal <- 0
  SE <- 0
  
  ##Write out best analysis data for parametric models
  
  for(bm in 0:10) {
    if (bm < 5 || bm == 7 || bm == 8) {
      #for each column in the row
      analysis <- output[bm, ]
      print(bestDescription[bm + 1])
      if(foundAnalysis[bm] == 1) {
        if(bm == 5) {
          # do something
          #analysis[1] = "2";
        }
        #wrong but print to output
        print(analysis[0:2])
        if(!is.isNull(analysis[3:6])){
          bestModels <- c(bestModels, analysis[3:6])
        }
        
        ##certain stats not available for non parametric models
        if(!is.isNull(analysis[8:9])){
          bestModels <- c(bestModels, analysis[8:9])
        }
        
        ## save shatsubset and t's for best model
        if(bm == 0) {
          sHatTotal <- analysis[3]
          SE <- analysis[4]
        }
      }
    }
  }
  
  ##Discounted Model for Best Model
  if(foundAnalysis[0] == 1 && bm == 9) {
    ##Step down from Four Mixed to Three Mixed
    if (bestModelTau[0, 0] == 5) {
      #how to get these
      cStar <- DiscountedTFourToThreeExponentialModel(r, s, t, obsMax, sHatTotal, se, lcb, ucb)$cStar
      
    }
    
    ##Step down from Three Mixed to Two Mixed
    if (bestModelTau[0, 0] == 4) {
      cStar <- DiscountedThreeToTwoExponentialModel(r, s, t, obsMax, sHatTotal, se, lcb, ucb)$cStar
    }
  }
  
  bestModels
  # output <- data.frame("Total Number of Observed Species" = "dummy", 
  #                      "Model" = "dummy", 
  #                      "Tau" = "dummy", 
  #                      "Observed Sp" = "dummy", 
  #                      "Estimated Total Sp"= "dummy", 
  #                      "SE" = "dummy", 
  #                      "Lower CB" = "dummy",
  #                      "Upper CB" = "dummy", 
  #                      "GOF0" = "dummy", 
  #                      "GOF5" = "dummy")
  
}
