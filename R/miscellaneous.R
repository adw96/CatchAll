CalculateAnalysisVariables <- function(part1, part2, 
                                       numberParameters, r, fitsCount, 
                                       fitsExtended, 
                                       s, modelNumber, frequency, observedCount) {
  # is it better to pass this in?
  BigChiSq <- 1000000000.0
  maxGoodnessOfFit <- 10
  return_variable <- list()
  return_variable$AIC <- 2 * numberParameters - 2*(part1 + part2)
  if (s[r] - numberParameters - 1 > 0) {
    return_variable$AICc <- return_variable$AIC + (2*numberParameters*(numberParameters+1)/(s[r]-numberParameters-1))
    return_variable$AICcFlag <- 1
  }
  
  ## calculate ChiSq, no binning
  
  # removed observedCountNoF0 ? not sure what that is
  chiSqAll <- ChiSqFunction(r, fitsCount, modelNumber, frequency, observedCount, s)
  return_variable$chiSq <- chiSqAll
  
  ## calculate Goodness of Fit
  df <- frequency[r] - numberParameters
  test <- (chiSqAll - df)/sqrt(2*df)
  GOF0 <- list()
  #bigChisq is a global variable
  if (test < maxGoodnessOfFit  & chiSqAll < BigChiSq){
    GOF0 <- GoodnessOfFit(chiSqAll, df)
    return_variable$GOF0Check <- GOF0$flag
    return_variable$GOF0 <- GOF0$gof
  } else{
    return_variable$GOF0Check <- 0
  }
  
  ## calculate ChiSq, bin 5
  chiSq5 <- ChiSqBin(r, fitsExtended, 5, df, numberParameters, frequency, s, observedCount)
  
  GOF5Check <- chiSq5$flag
  chiSq5 <- chiSq5$chiSq
  
  ## calculate goodness of fit
  test <- (chiSq5 - df)/sqrt(2*df)
  if (test < maxGoodnessOfFit & GOF5Check == 1 & chiSq5 < BigChiSq) {
    GOF5 <- GoodnessOfFit(chiSq5, df)
    return_variable$GOF5Check <- GOF5$flag
    return_variable$GOF5 <- GOF5$gof
  }
  return_variable
}

ChiSqFunction <- function(r, fitsCount, modelNumber,
                  frequency, observedCount, s) {
  chiSqTemporary <- 0
  sumFit <- 0
  rr <- 1
  if (frequency[1] == 0) {
    stop("first frequency is 0?")
  }
  
  # this bizarre looking flow adjusts for non-contiguous frequencies
  for (t in 1:frequency[r]) {
    if (t == frequency[rr]) {
      chiSqTemporary <- chiSqTemporary + ((observedCount)[rr] - fitsCount[t])^2/fitsCount[t]
      rr <- rr+1
    } else {
      chiSqTemporary <- chiSqTemporary + fitsCount[t]
    }
  }
  sumFit <- sum(fitsCount)
  
  if(modelNumber<6) {
    chiSqTemporary <- chiSqTemporary + s[r] - sumFit
  }
  #list("chiSq"=chiSqTemporary, "sumFit"=sumFit)
  chiSqTemporary
}

ChiSqBin <- function(r, fitsExtended, bin, 
                     df, numberParameters, 
                     frequency, s, observedCount) {
  extendedTau <- frequency[r] * 4
  
  ## find terminal indices of binned cells
  check <- rep(NA, extendedTau)
  accumulatedFit <- 0
  df <- 0
  stop <- 0
  t <- 1
  repeat {
    check[t] <- 0
    accumulatedFit  <- accumulatedFit + fitsExtended[t]
    if (accumulatedFit >= bin  & (s[r] - accumulatedFit) >= bin) {
      check[t] <- 1
      df <- df + 1
      stop <- t
      accumulatedFit <- 0
    }
    t <- t + 1
    if (!(t <= extendedTau  &  accumulatedFit < bin & (s[r]-accumulatedFit) >= bin)) {
      break
    }
  }
  ## todo: fix
  check[t-1]<-0
  
  ## check for enough data for bininng and positive df
  chiSqTemporary <- 0
  df <- df - numberParameters
  if (stop > 0 & df > 0) {
    cellObservation <- 0
    cellFit <- 0
    rr <- 1
    for (t in 1:extendedTau) {
      if (t <= frequency[r] & t == frequency[rr]) {
        cellObservation <- cellObservation + observedCount[rr]
        rr <- rr + 1
      } 
      cellFit <- cellFit + fitsExtended[t]
      
      print(check[t]) # null value here?
      if (check[t] == 1) {
        chiSqTemporary <- chiSqTemporary + (cellObservation-cellFit)^2/cellFit
        cellObservation <- 0
        cellFit <- 0
      }
    }
    observedTail <- cellObservation
    fitTail <- cellFit
    chiSqTemporary <- chiSqTemporary + (observedTail - fitTail)^2/fitTail
    flag <- 1
  } else {
    flag <- 0
  }
  list("chiSq"=chiSqTemporary, "flag"=flag)
}

GoodnessOfFit <- function(chiSqAll, df) {
  v <- df/2 + 1
  x <- chiSqAll/2
  g <- 1
  p <- 1

  while (v <= 2) {
    g <- g*x
    p <- p*v + g
    v <- v + 1
  }
  
  j <- floor(2.5*(3 + abs(x))) - 1
  f <- 1/(j + v - x)
  
  while (j >= 0) {
    f <- (f*x+1)/(j+v)
    j <- j - 1
  }
  
  p <- p + (f*g*x)
  g <- (1-(2/(7*v^2))*(1-(2/(3*v^2))))/(30*v^2)
  g <- ((g-1)/(12*v)) - (v*(log(v)-1))
  f <- p*exp(g-x)*sqrt(v/(2*pi))
  
  flag <- 1
  if (is.nan(f)) flag <- 0
  gof  <- 1-f*(chiSqAll/2)^(df/2)
  
  if (is.nan(gof)) flag <- 0
  
  list("gof"=gof, "flag"=flag)
}

GetConfidenceBounds <- function(r, se, sHatSubset, s, observationMaximum) {
  answer <- list()
  if (sHatSubset != s[r]) {
    dtemp <- exp(1.96*sqrt(log(1+se^2/(sHatSubset-s[r])^2)))
    answer$lcb <- s[observationMaximum] + (sHatSubset-s[r])/dtemp
    answer$ucb <- s[observationMaximum] + (sHatSubset-s[r])*dtemp
    answer$test <- 1
  } else{
    answer$test  <- 0
  }
  answer
  # should return a lower and upper bound
}

CheckOutput  <- function(x) {
  ifelse(is.null(x), NA, x)
}

BracetRoot <- function(poissonConstant, momentsInit) {
  
  ## initial guess range
  factor <- 2.0
  x1 <- momentsInit / factor
  x2 <- momentsInit * factor
  
  f1 <- (x1 / (1.0 - exp(-x1))) - poissonConstant
  f2 <- (x2 / (1.0 - exp(-x2))) - poissonConstant
  
  conclusion <- 0
  i <- 0
  while (conclusion == 0 & i < 20) {
    ## one estimate is negative and the other positive
    if ((f1 * f2) < 0.0) {
      conclusion <- 1
    }
    ## move the appropriate bound
    if (abs(f1) < abs(f2)) {
      x1 <- x1 + factor * (x1- x2)
      f1 <- (x1 / (1.0 - exp(-x1))) - poissonConstant
    }
    else
    {
      x2 <- x2 + factor * (x2 - x1)
      f2 <- (x2 / (1.0 - exp(-x2))) - poissonConstant
    }
    i <- i + 1
  }
  result <- list()
  result$conclusion <- conclusion
  result$x1 <- x1
  result$x2 <- x2
  result$f1 <- f1
  result$f2 <- f2
  result
}

Math.Pow <- function(a, b) a^b

# x should be an array
# is this correct?? 
logFactorial <- function(x) {
  cumsum(log(x))
}

MatrixInversion <- function(sHat, a00, a0, A) {
  result <- list()
  # complete the symmetric matrix
  A <- A + t(A)
  diag(A) <- diag(A)/2
  
  aInverse <- try(solve(A), silent = TRUE)
  
  if (class(aInverse) != "try-error") {
    answer <- a0 %*% aInverse %*% a0
    
    if (a00>answer) {
      result$answer <- sqrt(a00-answer)
      result$se <- sqrt(sHat)/answer
      result$flag <- 1
    } else {
      result$flag <- 0
    }
    
  } else {
    result$se <- 0
    result$flag <- 0
  }
  result
}
