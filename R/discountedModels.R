DiscountedTFourToThreeExponentialModel <- function(r, s, t, obsMax, sHatTotal, se, lcb, ucb) {
  
  ## estimated total species
  cStar <- s[r] * (1.0 - (T[4] * T[0]) / ((1.0 + T[0]) *
                                            (1.0 - (T[4] / (1.0 + T[0])) - (T[5] / (1.0 + T[1])) - (T[6] / (1.0 + T[2])) -
                                               ((1.0 - T[4] - T[5] - T[6]) / (1.0 + T[3])))))
  
  excess <- s[obsMax] - s[r]
  sHatSubset <- sHatTotal - excess
  sHatSubset <- (1.0 - T[4]) * sHatSubset
  sHatTotal <- sHatSubset + excess
  
  ## standard error
  se <- (1.0 - T[3]) * se;
  
  bounds <- GetConfidenceBoundsDiscounted (r, SE,sHatSubset, cStar,excess, LCB,UCB)
  ucb <- bounds$ucb
  lcb <- bounds$lcb
  
  ## confidence bounds
  list("cStar"=cStar,
       "sHatTotal"=sHatTotal, "SE"=se, "sHatTotal" = sHatTotal, "LCB"=lcb, "UCB"=ucb)
  
}







DiscountedThreeToTwoExponentialModel <- function(r, s, t, obsMax, sHatTotal, se, LCB, UCB) {
  
  ## estimated total species
  cStar <- s[r] * (1.0 - (T[3] * T[0]) / ((1.0 + T[0]) *
                                            (1.0 - (T[3] / (1.0 + T[0])) - (T[4] / (1.0 + T[1])) -
                                               ((1.0 - T[3] - T[4]) / (1.0 + T[2])))))
  
  excess <- s[obsMax] - s[r]
  sHatSubset <- sHatTotal - excess
  sHatSubset <- (1.0 - T[3]) * sHatSubset
  sHatTotal <- sHatSubset + excess
  
  ## standard error
  se <- (1.0 - T[3]) * se;
  
  bounds <- GetConfidenceBoundsDiscounted (r, SE,sHatSubset, cStar,excess, LCB,UCB)
  ucb <- bounds$ucb
  lcb <- bounds$lcb
  
  ## confidence bounds
  list("cStar"=cStar,
       "sHatTotal"=sHatTotal, "SE"=se, "sHatTotal" = sHatTotal, "LCB"=LCB, "UCB"=ucb)
  
}



DiscountedTwoToSingleExponentialModel <- function(r, s, t, obsMax, sHatTotal, se, LCB, UCB) {
  ## estimated total species
  cStar <- s[r] * (1.0 - (T[2] * T[0]) / ((1.0 + T[0]) *
                                                  (1.0 - (T[2] / (1.0 + T[0])) - ((1.0 - T[2]) / (1.0 + T[1])))))
  excess <- s[obsMax] - s[r]
  sHatSubset <- (1.0 - T[2]) * sHatSubset
  sHatTotal <- sHatSubset + excess
  
  ## standard error
  se <- (1.0 - T[2]) * se;
  
  bounds <- GetConfidenceBoundsDiscounted (r, SE,sHatSubset, cStar,excess, LCB,UCB)
  ucb <- bounds$ucb
  lcb <- bounds$lcb
  
  ## confidence bounds
  list("cStar"=cStar,
       "sHatTotal"=sHatTotal, "SE"=se, "sHatTotal" = sHatTotal, "LCB"=LCB, "UCB"=ucb)

}