SEFourMixedExpA5_A7 <- function(t1, t2, t3, t4, t5, t6, t7, a, sHatSubset) {
  
  test <- 100.0
   k <- 0
  
  ##A55
  while (test > criteria & k < maximumIteration)
  {
     t1P <- pow((t1/(1+t1)),k)
     t2P <- pow((t2/(1+t2)),k)
     t3P <- pow((t3/(1+t3)),k)
     t4P <- pow((t4/(1+t4)),k)
    
     A55 <- 1/(t7*t3P+t6*t2P+t5*t1P+t4P-t4P*t5*t1*t2*t3+t5*t1P*t2+t6*t2P*t4+t6*t2P*t3+t6*t2P*t1
                    +t7*t3P*t4+t7*t3P*t2+t7*t3P*t1-t4P*t6*t2-t4P*t7*t3-t4P*t5*t1+t4P*t2*t3-t4P*t5*t3-t4P
                    *t5*t2+t4P*t1*t3-t4P*t6*t3-t4P*t6*t1+t4P*t1*t2-t4P*t7*t2-t4P*t7*t1+t5*t1P*t4+t5*t1P*t3
                    +t4P*t3-t4P*t5-t4P*t6-t4P*t7+t4P*t2+t4P*t1+t5*t1P*t2*t4+t5*t1P*t3*t4+t5*t1P*t2*t3+t6
                    *t2P*t1*t4+t6*t2P*t3*t4+t6*t2P*t1*t3+t7*t3P*t2*t4+t7*t3P*t1*t2+t7*t3P*t1*t4-t4P*t7*t1*t2-t4P
                    *t6*t2*t3-t4P*t6*t1*t2-t4P*t7*t2*t3-t4P*t7*t1*t3-t4P*t6*t1*t3-t4P*t5*t1*t2-t4P*t5*t1*t3-t4P
                    *t5*t2*t3+t4P*t1*t2*t3-t4P*t6*t1*t2*t3+t5*t1P*t2*t3*t4+t6*t2P*t1*t3*t4+t7*t3P*t1*t2*t4-t4P
                    *t7*t1*t2*t3)/(1+t1)*(1+t2)*(1+t3)/(1+t4)*pow(t1P+t1P*t4-t4P-t4P*t1,2)
    
     if (k > 0) test <- abs(A55/a[5,5])
     a[5,5] <- a[5,5] + A55
     k <- k+1
    
  } 
  
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ##A56
  test <- 100.0
  k <- 0
  while (test > criteria & k < maximumIteration)
  {
     t1P <- pow((t1/(1+t1)),k)
     t2P <- pow((t2/(1+t2)),k)
     t3P <- pow((t3/(1+t3)),k)
     t4P <- pow((t4/(1+t4)),k) 
    
     t42kP <- pow((t4/(1+t4)),(2*k))
    
     A56 <- -(1+t3)*(-t2P*t1P-2*t2P*t1P*t4-t2P*t1P*t4*t4+t2P*t4P+t2P*t4P*t1+t2P*t4P*t4+t2P
                          *t4P*t1*t4+t4P*t1P+t4P*t1P*t2+t4P*t1P*t4+t4P*t1P*t2*t4-t42kP-t42kP*t1-t42kP*t2-t42kP
                          *t1*t2)/(t7*t3P+t6*t2P+t5*t1P+t4P-t4P*t5*t1*t2*t3+t5*t1P*t2+t6*t2P*t4+t6*t2P*t3+t6*t2P
                                   *t1+t7*t3P*t4+t7*t3P*t2+t7*t3P*t1-t4P*t6*t2-t4P*t7*t3-t4P*t5*t1+t4P*t2*t3-t4P*t5*t3-t4P
                                   *t5*t2+t4P*t1*t3-t4P*t6*t3-t4P*t6*t1+t4P*t1*t2-t4P*t7*t2-t4P*t7*t1+t5*t1P*t4+t5*t1P*t3
                                   +t4P*t3-t4P*t5-t4P*t6-t4P*t7+t4P*t2+t4P*t1+t5*t1P*t2*t4+t5*t1P*t3*t4+t5*t1P*t2*t3+t6
                                   *t2P*t1*t4+t6*t2P*t3*t4+t6*t2P*t1*t3+t7*t3P*t2*t4+t7*t3P*t1*t2+t7*t3P*t1*t4-t4P*t7*t1*t2-t4P
                                   *t6*t2*t3-t4P*t6*t1*t2-t4P*t7*t2*t3-t4P*t7*t1*t3-t4P*t6*t1*t3-t4P*t5*t1*t2-t4P*t5*t1*t3-t4P
                                   *t5*t2*t3+t4P*t1*t2*t3-t4P*t6*t1*t2*t3+t5*t1P*t2*t3*t4+t6*t2P*t1*t3*t4+t7*t3P*t1*t2*t4-t4P
                                   *t7*t1*t2*t3)/(1+t4)
    
     if (k > 0) test <- abs(A56/a[5,6])
     a[5,6] <- a[5,6] + A56
     k <- k+1
    
  } 
  
  
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ##A57
  test <- 100.0
  k <- 0
  while (test > criteria & k < maximumIteration)
  {
     t1P <- pow((t1/(1+t1)),k)
     t2P <- pow((t2/(1+t2)),k)
     t3P <- pow((t3/(1+t3)),k)
     t4P <- pow((t4/(1+t4)),k)
    
     t42kP <- pow((t4/(1+t4)),(2*k))
    
     A57 <- -(1+t2)*(-t3P*t1P-2*t3P*t1P*t4-t3P*t1P*t4*t4+t3P*t4P+t3P*t4P*t1+t3P*t4P*t4+t3P
                          *t4P*t1*t4+t4P*t1P+t4P*t1P*t3+t4P*t1P*t4+t4P*t1P*t3*t4-t42kP-t42kP*t3-t42kP*t1-t42kP
                          *t1*t3)/(t7*t3P+t6*t2P+t5*t1P+t4P-t4P*t5*t1*t2*t3+t5*t1P*t2+t6*t2P*t4+t6*t2P*t3+t6*t2P
                                   *t1+t7*t3P*t4+t7*t3P*t2+t7*t3P*t1-t4P*t6*t2-t4P*t7*t3-t4P*t5*t1+t4P*t2*t3-t4P*t5*t3-t4P
                                   *t5*t2+t4P*t1*t3-t4P*t6*t3-t4P*t6*t1+t4P*t1*t2-t4P*t7*t2-t4P*t7*t1+t5*t1P*t4+t5*t1P*t3
                                   +t4P*t3-t4P*t5-t4P*t6-t4P*t7+t4P*t2+t4P*t1+t5*t1P*t2*t4+t5*t1P*t3*t4+t5*t1P*t2*t3+t6
                                   *t2P*t1*t4+t6*t2P*t3*t4+t6*t2P*t1*t3+t7*t3P*t2*t4+t7*t3P*t1*t2+t7*t3P*t1*t4-t4P*t7*t1*t2-t4P
                                   *t6*t2*t3-t4P*t6*t1*t2-t4P*t7*t2*t3-t4P*t7*t1*t3-t4P*t6*t1*t3-t4P*t5*t1*t2-t4P*t5*t1*t3-t4P
                                   *t5*t2*t3+t4P*t1*t2*t3-t4P*t6*t1*t2*t3+t5*t1P*t2*t3*t4+t6*t2P*t1*t3*t4+t7*t3P*t1*t2*t4-t4P
                                   *t7*t1*t2*t3)/(1+t4)
    
     
     if (k > 0) test <- abs(A57/a[5,7])
     a[5,7] <- a[5,7] + A57
     k <- k+1
    
  } 
  
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ##A66
  test <- 100.0
  k <- 0
  while (test > criteria & k < maximumIteration)
  {
     t1P <- pow((t1/(1+t1)),k)
     t2P <- pow((t2/(1+t2)),k)
     t3P <- pow((t3/(1+t3)),k)
     t4P <- pow((t4/(1+t4)),k) 
    
     A66 <- 1/(t7*t3P+t6*t2P+t5*t1P+t4P-t4P*t5*t1*t2*t3+t5*t1P*t2+t6*t2P*t4+t6*t2P*t3+t6*t2P*t1
                    +t7*t3P*t4+t7*t3P*t2+t7*t3P*t1-t4P*t6*t2-t4P*t7*t3-t4P*t5*t1+t4P*t2*t3-t4P*t5*t3-t4P
                    *t5*t2+t4P*t1*t3-t4P*t6*t3-t4P*t6*t1+t4P*t1*t2-t4P*t7*t2-t4P*t7*t1+t5*t1P*t4+t5*t1P*t3
                    +t4P*t3-t4P*t5-t4P*t6-t4P*t7+t4P*t2+t4P*t1+t5*t1P*t2*t4+t5*t1P*t3*t4+t5*t1P*t2*t3+t6
                    *t2P*t1*t4+t6*t2P*t3*t4+t6*t2P*t1*t3+t7*t3P*t2*t4+t7*t3P*t1*t2+t7*t3P*t1*t4-t4P*t7*t1*t2-t4P
                    *t6*t2*t3-t4P*t6*t1*t2-t4P*t7*t2*t3-t4P*t7*t1*t3-t4P*t6*t1*t3-t4P*t5*t1*t2-t4P*t5*t1*t3-t4P
                    *t5*t2*t3+t4P*t1*t2*t3-t4P*t6*t1*t2*t3+t5*t1P*t2*t3*t4+t6*t2P*t1*t3*t4+t7*t3P*t1*t2*t4-t4P
                    *t7*t1*t2*t3)*(1+t1)/(1+t2)*(1+t3)/(1+t4)*pow(-t2P-t2P*t4+t4P+t4P*t2,2)
    
     if (k > 0) test <- abs(A66/a[6,6])
     a[6,6] <- a[6,6] + A66
     k <- k+1
    
  } 
  
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ##A67
  test <- 100.0
  k <- 0
  while (test > criteria & k < maximumIteration)
  {
     t1P <- pow((t1/(1+t1)),k)
     t2P <- pow((t2/(1+t2)),k)
     t3P <- pow((t3/(1+t3)),k)
     t4P <- pow((t4/(1+t4)),k) 
    
     t42kP <- pow((t4/(1+t4)),(2*k))
    
     A67 <- -(1+t1)*(-t3P*t2P-2*t3P*t2P*t4-t3P*t2P*t4*t4+t3P*t4P+t3P*t4P*t2+t3P*t4P*t4+t3P
                          *t4P*t2*t4+t2P*t4P+t2P*t4P*t3+t2P*t4P*t4+t2P*t4P*t3*t4-t42kP-t42kP*t3-t42kP*t2-t42kP
                          *t2*t3)/(t7*t3P+t6*t2P+t5*t1P+t4P-t4P*t5*t1*t2*t3+t5*t1P*t2+t6*t2P*t4+t6*t2P*t3+t6*t2P
                                   *t1+t7*t3P*t4+t7*t3P*t2+t7*t3P*t1-t4P*t6*t2-t4P*t7*t3-t4P*t5*t1+t4P*t2*t3-t4P*t5*t3-t4P
                                   *t5*t2+t4P*t1*t3-t4P*t6*t3-t4P*t6*t1+t4P*t1*t2-t4P*t7*t2-t4P*t7*t1+t5*t1P*t4+t5*t1P*t3
                                   +t4P*t3-t4P*t5-t4P*t6-t4P*t7+t4P*t2+t4P*t1+t5*t1P*t2*t4+t5*t1P*t3*t4+t5*t1P*t2*t3+t6
                                   *t2P*t1*t4+t6*t2P*t3*t4+t6*t2P*t1*t3+t7*t3P*t2*t4+t7*t3P*t1*t2+t7*t3P*t1*t4-t4P*t7*t1*t2-t4P
                                   *t6*t2*t3-t4P*t6*t1*t2-t4P*t7*t2*t3-t4P*t7*t1*t3-t4P*t6*t1*t3-t4P*t5*t1*t2-t4P*t5*t1*t3-t4P
                                   *t5*t2*t3+t4P*t1*t2*t3-t4P*t6*t1*t2*t3+t5*t1P*t2*t3*t4+t6*t2P*t1*t3*t4+t7*t3P*t1*t2*t4-t4P
                                   *t7*t1*t2*t3)/(1+t4)
    
     if (k > 0) test <- abs(A67/a[6,7])
     a[6,7] <- a[6,7] + A67
     k <- k+1
    
  } 
  
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ##A77
  test <- 100.0
  k <- 0
  while (test > criteria & k < maximumIteration)
  {
     t1P <- pow((t1/(1+t1)),k)
     t2P <- pow((t2/(1+t2)),k)
     t3P <- pow((t3/(1+t3)),k)
     t4P <- pow((t4/(1+t4)),k) 
    
     A77 <- 1/(t7*t3P+t6*t2P+t5*t1P+t4P-t4P*t5*t1*t2*t3+t5*t1P*t2+t6*t2P*t4+t6*t2P*t3+t6*t2P*t1
                    +t7*t3P*t4+t7*t3P*t2+t7*t3P*t1-t4P*t6*t2-t4P*t7*t3-t4P*t5*t1+t4P*t2*t3-t4P*t5*t3-t4P
                    *t5*t2+t4P*t1*t3-t4P*t6*t3-t4P*t6*t1+t4P*t1*t2-t4P*t7*t2-t4P*t7*t1+t5*t1P*t4+t5*t1P*t3
                    +t4P*t3-t4P*t5-t4P*t6-t4P*t7+t4P*t2+t4P*t1+t5*t1P*t2*t4+t5*t1P*t3*t4+t5*t1P*t2*t3+t6
                    *t2P*t1*t4+t6*t2P*t3*t4+t6*t2P*t1*t3+t7*t3P*t2*t4+t7*t3P*t1*t2+t7*t3P*t1*t4-t4P*t7*t1*t2-t4P
                    *t6*t2*t3-t4P*t6*t1*t2-t4P*t7*t2*t3-t4P*t7*t1*t3-t4P*t6*t1*t3-t4P*t5*t1*t2-t4P*t5*t1*t3-t4P
                    *t5*t2*t3+t4P*t1*t2*t3-t4P*t6*t1*t2*t3+t5*t1P*t2*t3*t4+t6*t2P*t1*t3*t4+t7*t3P*t1*t2*t4-t4P
                    *t7*t1*t2*t3)*(1+t1)*(1+t2)/(1+t3)/(1+t4)*pow(-t3P-t3P*t4+t4P+t4P*t3,2)
    
     if (k > 0) test <- abs(A77/a[7,7])
     a[7,7] <- a[7,7] + A77
     k <- k+1
    
  } 
  
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  a
}