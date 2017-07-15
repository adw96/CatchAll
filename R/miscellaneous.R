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
BracetRoot(40, 5)
