FourExponentialStandardError <- function(t1, t2, t3, t4, t5, t6, t7, sHatSubset) {
  a00 <- -(-t7*t2*t4+t6*t2*t3+t6*t2+t7*t3+t7*t1*t2*t3-
                   t6*t1*t4+t6*t1*t2-t5*t2*t4-t6*t3*t4+t2*t3*t4+
                   t7*t2*t3-t5*t3*t4+t7*t1*t3+t1*t2*t4+t5*t1*t2-t7*t1*t4+
                   t1*t3*t4+t5*t1*t3-t5*t2*t3*t4+t5*t1-t6*t1*t3*t4+t4+
                   t5*t1*t2*t3-t7*t1*t2*t4+t6*t1*t2*t3+t3*t4+t2*t4-t5*t4+
                   t1*t4-t6*t4+t1*t2*t3*t4-t7*t4)/(-1-t7*t2*t4+t6*t2*t3-t2-
                                                     t1+t6*t2+t7*t3+t7*t1*t2*t3-t6*t1*t4+t6*t1*t2-t5*t2*t4-
                                                     t6*t3*t4+t7*t2*t3-t5*t3*t4+t7*t1*t3+t5*t1*t2-t7*t1*t4+
                                                     t5*t1*t3-t1*t2*t3-t5*t2*t3*t4+t5*t1-t3-t6*t1*t3*t4+
                                                     t5*t1*t2*t3-t7*t1*t2*t4+t6*t1*t2*t3-t2*t3-t5*t4-t1*t3-
                                                     t6*t4-t1*t2-t7*t4)
  
  # create a vector with 8 elements
  a0 <- c(-t5*(1+t4)*(1+t3)*(1+t2)/(1+t1)/(-1-t7*t2*t4+t6*t2*t3-
                                             t2-t1+t6*t2+t7*t3+t7*t1*t2*t3-t6*t1*t4+t6*t1*t2-
                                             t5*t2*t4-t6*t3*t4+t7*t2*t3-t5*t3*t4+t7*t1*t3+t5*t1*t2-
                                             t7*t1*t4+t5*t1*t3-t1*t2*t3-t5*t2*t3*t4+t5*t1-t3-
                                             t6*t1*t3*t4+t5*t1*t2*t3-t7*t1*t2*t4+t6*t1*t2*t3-t2*t3-
                                             t5*t4-t1*t3-t6*t4-t1*t2-t7*t4), 
          -t6*(t1*t4+t3*t4+t1*t3+t1*t3*t4+1+t4+t3+t1)/(1+t2)/
            (-1-t7*t2*t4+t6*t2*t3-t2-t1+t6*t2+t7*t3+t7*t1*t2*t3-
               t6*t1*t4+t6*t1*t2-t5*t2*t4-t6*t3*t4+t7*t2*t3-t5*t3*t4+
               t7*t1*t3+t5*t1*t2-t7*t1*t4+t5*t1*t3-t1*t2*t3-t5*t2*t3*t4+
               t5*t1-t3-t6*t1*t3*t4+t5*t1*t2*t3-t7*t1*t2*t4+t6*t1*t2*t3-
               t2*t3-t5*t4-t1*t3-t6*t4-t1*t2-t7*t4),
          -(1+t2)*(1+t4)*t7*(1+t1)/(1+t3)/(-1-t7*t2*t4+t6*t2*t3-t2-
                                             t1+t6*t2+t7*t3+t7*t1*t2*t3-t6*t1*t4+t6*t1*t2-t5*t2*t4-
                                             t6*t3*t4+t7*t2*t3-t5*t3*t4+t7*t1*t3+t5*t1*t2-t7*t1*t4+
                                             t5*t1*t3-t1*t2*t3-t5*t2*t3*t4+t5*t1-t3-t6*t1*t3*t4+
                                             t5*t1*t2*t3-t7*t1*t2*t4+t6*t1*t2*t3-t2*t3-t5*t4-t1*t3-
                                             t6*t4-t1*t2-t7*t4),
          (1+t2)*(t7*t1+t6*t3-1+t6+t7*t1*t3+t6*t1+t7*t3+t5*t1+
                    t5*t3-t1*t3+t5*t1*t3+t6*t1*t3-t3+t5-t1+t7)/(1+t4)/
            (-1-t7*t2*t4+t6*t2*t3-t2-t1+t6*t2+t7*t3+t7*t1*t2*t3-
               t6*t1*t4+t6*t1*t2-t5*t2*t4-t6*t3*t4+t7*t2*t3-t5*t3*t4+
               t7*t1*t3+t5*t1*t2-t7*t1*t4+t5*t1*t3-t1*t2*t3-t5*t2*t3*t4+
               t5*t1-t3-t6*t1*t3*t4+t5*t1*t2*t3-t7*t1*t2*t4+t6*t1*t2*t3-
               t2*t3-t5*t4-t1*t3-t6*t4-t1*t2-t7*t4),
          
          -1/(-1-t7*t2*t4+t6*t2*t3-t2-t1+t6*t2+t7*t3+t7*t1*t2*t3-
                t6*t1*t4+t6*t1*t2-t5*t2*t4-t6*t3*t4+t7*t2*t3-t5*t3*t4+
                t7*t1*t3+t5*t1*t2-t7*t1*t4+t5*t1*t3-t1*t2*t3-t5*t2*t3*t4+
                t5*t1-t3-t6*t1*t3*t4+t5*t1*t2*t3-t7*t1*t2*t4+t6*t1*t2*t3-
                t2*t3-t5*t4-t1*t3-t6*t4-t1*t2-t7*t4)*(-t2*t4-t3*t4+t1*t2+
                                                        t1*t3-t2*t3*t4+t1+t1*t2*t3-t4),
          -1/(-1-t7*t2*t4+t6*t2*t3-t2-t1+t6*t2+t7*t3+t7*t1*t2*t3-
                t6*t1*t4+t6*t1*t2-t5*t2*t4-t6*t3*t4+t7*t2*t3-t5*t3*t4+
                t7*t1*t3+t5*t1*t2-t7*t1*t4+t5*t1*t3-t1*t2*t3-t5*t2*t3*t4+
                t5*t1-t3-t6*t1*t3*t4+t5*t1*t2*t3-t7*t1*t2*t4+t6*t1*t2*t3-
                t2*t3-t5*t4-t1*t3-t6*t4-t1*t2-t7*t4)*(t2*t3+t2-t1*t4+t1*t2-
                                                        t3*t4-t1*t3*t4+t1*t2*t3-t4),
          -1/(-1-t7*t2*t4+t6*t2*t3-t2-t1+t6*t2+t7*t3+t7*t1*t2*t3-
                t6*t1*t4+t6*t1*t2-t5*t2*t4-t6*t3*t4+t7*t2*t3-t5*t3*t4+
                t7*t1*t3+t5*t1*t2-t7*t1*t4+t5*t1*t3-t1*t2*t3-t5*t2*t3*t4+
                t5*t1-t3-t6*t1*t3*t4+t5*t1*t2*t3-t7*t1*t2*t4+t6*t1*t2*t3-
                t2*t3-t5*t4-t1*t3-t6*t4-t1*t2-t7*t4)*(-t2*t4+t3+t1*t2*t3+
                                                        t2*t3+t1*t3-t1*t4-t1*t2*t4-t4))
  
        #TODO: confused where the objects(??) are created or which methods are called in C#
}








# i don't think it needs an array param cuz we make a matrix
FourExponentialStandardErrorA5_A7 <- function(t1, t2, t3, t4, t5, t6, t7, arrayPassedIn?) {
  a <- matrix(0, nrow = 7, ncol = 7) # not sure these numbers are correct
  
  # global variables in C#
  maximumIteration <- 100000
  criteria <-  0.0000000000000001
  
  #a55
  test <- 100
  k <- 0
  
  while (test > criteria & k < maximumIteration) {
    
    t1P <- (t1/(1+t1))^k
    t2P <- (t2/(1+t2))^k
    t3P <- (t3/(1+t3))^k
    t4P <- (t4/(1+t4))^k
    
    a55 <- -t3*((t1/(1+t1))^k)*(t3*((t1/(1+t1))^k)*k+2*k*t1*t3*
                                  ((t1/(1+t1))^k)*t2-(t1*t1)*t3*((t1/(1+t1))^k)*t2+2*k*t1*t3*
                                  ((t1/(1+t1))^k)-5*k*t1*(t2/(1+t2)^k)*t3-4*k*(t1*t1)*
                                  (t2/(1+t2)^k)*t3+k*k*(t2/(1+t2)^k)*t3*t1+k*t3*
                                  ((t1/(1+t1))^k)*t2-(t1*t1)*t3*((t1/(1+t1))^k)+2*(t1*t1)*
                                  (t2/(1+t2)^k)*t3+2*(t1^3)*(t2/(1+t2)^k)*t3+5*k*t1*
                                  (t2/(1+t2)^k)+4*k*(t1*t1)*(t2/(1+t2)^k)+k*k*(t2/(1+t2)^k)*t3-k*k*
                                  (t2/(1+t2)^k)*t1-k*(t2/(1+t2)^k)*t3-2*(t1*t1)*(t2/(1+t2)^k)-2*
                                  (t1^3)*(t2/(1+t2)^k)-k*k*(t2/(1+t2)^k)+k*(t2/(1+t2)^k))/
      (-t3*((t1/(1+t1))^k)-t3*((t1/(1+t1))^k)*t2+(t2/(1+t2)^k)*t3-(t2/(1+t2)^k)-
         (t2/(1+t2)^k)*t1+(t2/(1+t2)^k)*t3*t1)*(t1^(-2))*((1 + t1)^ (-3))
    
    if (k > 0) 
      test <- abs(a55/a[5,5])
    a[5,5] <- a[5,5] + a55
    k <- k+1
  }
  
  #I think it's 0 here since it's broken and then u check for the flag later
  if (k == maximumIteration) {
    return(list("flag"=0))
    break
  }
  
  #a56
  test <- 100
  k <- 0
  
  while (test > criteria & k < maximumIteration) {
    
    t1P <- (t1/(1+t1))^k
    t2P <- (t2/(1+t2))^k
    t3P <- (t3/(1+t3))^k
    t4P <- (t4/(1+t4))^k
    
    t42kP <- ((t4/(1+t4))^(2*k))
    
    a56 <- -(1+t3)*(-t2P*t1P-2*t2P*t1P*t4-t2P*t1P*t4*t4+t2P*t4P+t2P*t4P*t1+t2P*t4P*t4+t2P
                    *t4P*t1*t4+t4P*t1P+t4P*t1P*t2+t4P*t1P*t4+t4P*t1P*t2*t4-t42kP-t42kP*t1-t42kP*t2-t42kP
                    *t1*t2)/(t7*t3P+t6*t2P+t5*t1P+t4P-t4P*t5*t1*t2*t3+t5*t1P*t2+t6*t2P*t4+t6*t2P*t3+t6*t2P
                             *t1+t7*t3P*t4+t7*t3P*t2+t7*t3P*t1-t4P*t6*t2-t4P*t7*t3-t4P*t5*t1+t4P*t2*t3-t4P*t5*t3-t4P
                             *t5*t2+t4P*t1*t3-t4P*t6*t3-t4P*t6*t1+t4P*t1*t2-t4P*t7*t2-t4P*t7*t1+t5*t1P*t4+t5*t1P*t3
                             +t4P*t3-t4P*t5-t4P*t6-t4P*t7+t4P*t2+t4P*t1+t5*t1P*t2*t4+t5*t1P*t3*t4+t5*t1P*t2*t3+t6
                             *t2P*t1*t4+t6*t2P*t3*t4+t6*t2P*t1*t3+t7*t3P*t2*t4+t7*t3P*t1*t2+t7*t3P*t1*t4-t4P*t7*t1*t2-t4P
                             *t6*t2*t3-t4P*t6*t1*t2-t4P*t7*t2*t3-t4P*t7*t1*t3-t4P*t6*t1*t3-t4P*t5*t1*t2-t4P*t5*t1*t3-t4P
                             *t5*t2*t3+t4P*t1*t2*t3-t4P*t6*t1*t2*t3+t5*t1P*t2*t3*t4+t6*t2P*t1*t3*t4+t7*t3P*t1*t2*t4-t4P
                             *t7*t1*t2*t3)/(1+t4)
    
    if (k > 0) 
      test <- abs(a56/a[5,6])
    a[5,6] <- a[5,6] + a56
    k <- k+1
  }
  
  #I think it's 0 here since it's broken and then u check for the flag later
  if (k == maximumIteration) {
    return(list("flag"=0))
    break
  }
  
  #a57
  test <- 100
  k <- 0
  
  while (test > criteria & k < maximumIteration) {
    
    t1P <- (t1/(1+t1))^k
    t2P <- (t2/(1+t2))^k
    t3P <- (t3/(1+t3))^k
    t4P <- (t4/(1+t4))^k
    
    t42kP <- ((t4/(1+t4))^(2*k))
    
    a57 <- -(1+t2)*(-t3P*t1P-2*t3P*t1P*t4-t3P*t1P*t4*t4+t3P*t4P+t3P*t4P*t1+t3P*t4P*t4+t3P
                    *t4P*t1*t4+t4P*t1P+t4P*t1P*t3+t4P*t1P*t4+t4P*t1P*t3*t4-t42kP-t42kP*t3-t42kP*t1-t42kP
                    *t1*t3)/(t7*t3P+t6*t2P+t5*t1P+t4P-t4P*t5*t1*t2*t3+t5*t1P*t2+t6*t2P*t4+t6*t2P*t3+t6*t2P
                             *t1+t7*t3P*t4+t7*t3P*t2+t7*t3P*t1-t4P*t6*t2-t4P*t7*t3-t4P*t5*t1+t4P*t2*t3-t4P*t5*t3-t4P
                             *t5*t2+t4P*t1*t3-t4P*t6*t3-t4P*t6*t1+t4P*t1*t2-t4P*t7*t2-t4P*t7*t1+t5*t1P*t4+t5*t1P*t3
                             +t4P*t3-t4P*t5-t4P*t6-t4P*t7+t4P*t2+t4P*t1+t5*t1P*t2*t4+t5*t1P*t3*t4+t5*t1P*t2*t3+t6
                             *t2P*t1*t4+t6*t2P*t3*t4+t6*t2P*t1*t3+t7*t3P*t2*t4+t7*t3P*t1*t2+t7*t3P*t1*t4-t4P*t7*t1*t2-t4P
                             *t6*t2*t3-t4P*t6*t1*t2-t4P*t7*t2*t3-t4P*t7*t1*t3-t4P*t6*t1*t3-t4P*t5*t1*t2-t4P*t5*t1*t3-t4P
                             *t5*t2*t3+t4P*t1*t2*t3-t4P*t6*t1*t2*t3+t5*t1P*t2*t3*t4+t6*t2P*t1*t3*t4+t7*t3P*t1*t2*t4-t4P
                             *t7*t1*t2*t3)/(1+t4)
    
    if (k > 0) 
      test <- abs(a57/a[5,7])
    a[5,7] <- a[5,7] + a57
    k <- k+1
  }
  
  #I think it's 0 here since it's broken and then u check for the flag later
  if (k == maximumIteration) {
    return(list("flag"=0))
    break
  }
  
  #a66
  test <- 100
  k <- 0
  
  while (test > criteria & k < maximumIteration) {
    
    t1P <- (t1/(1+t1))^k
    t2P <- (t2/(1+t2))^k
    t3P <- (t3/(1+t3))^k
    t4P <- (t4/(1+t4))^k
    
    a66 <- 1/(t7*t3P+t6*t2P+t5*t1P+t4P-t4P*t5*t1*t2*t3+t5*t1P*t2+t6*t2P*t4+t6*t2P*t3+t6*t2P*t1
              +t7*t3P*t4+t7*t3P*t2+t7*t3P*t1-t4P*t6*t2-t4P*t7*t3-t4P*t5*t1+t4P*t2*t3-t4P*t5*t3-t4P
              *t5*t2+t4P*t1*t3-t4P*t6*t3-t4P*t6*t1+t4P*t1*t2-t4P*t7*t2-t4P*t7*t1+t5*t1P*t4+t5*t1P*t3
              +t4P*t3-t4P*t5-t4P*t6-t4P*t7+t4P*t2+t4P*t1+t5*t1P*t2*t4+t5*t1P*t3*t4+t5*t1P*t2*t3+t6
              *t2P*t1*t4+t6*t2P*t3*t4+t6*t2P*t1*t3+t7*t3P*t2*t4+t7*t3P*t1*t2+t7*t3P*t1*t4-t4P*t7*t1*t2-t4P
              *t6*t2*t3-t4P*t6*t1*t2-t4P*t7*t2*t3-t4P*t7*t1*t3-t4P*t6*t1*t3-t4P*t5*t1*t2-t4P*t5*t1*t3-t4P
              *t5*t2*t3+t4P*t1*t2*t3-t4P*t6*t1*t2*t3+t5*t1P*t2*t3*t4+t6*t2P*t1*t3*t4+t7*t3P*t1*t2*t4-t4P
              *t7*t1*t2*t3)*(1+t1)/(1+t2)*(1+t3)/(1+t4)*((-t2P-t2P*t4+t4P+t4P*t2) ^ 2)
    
    if (k > 0) 
      test <- abs(a66/a[6,6])
    a[6,6] <- a[6,6] + a66
    k <- k+1
  }
  
  #I think it's 0 here since it's broken and then u check for the flag later
  if (k == maximumIteration) {
    return(list("flag"=0))
    break
  }
  
  #a67
  test <- 100
  k <- 0
  
  while (test > criteria & k < maximumIteration) {
    
    t1P <- (t1/(1+t1))^k
    t2P <- (t2/(1+t2))^k
    t3P <- (t3/(1+t3))^k
    t4P <- (t4/(1+t4))^k
    
    t42kP <- ((t4/(1+t4))^(2*k))
    
    a67 <- -(1+t1)*(-t3P*t2P-2*t3P*t2P*t4-t3P*t2P*t4*t4+t3P*t4P+t3P*t4P*t2+t3P*t4P*t4+t3P
                    *t4P*t2*t4+t2P*t4P+t2P*t4P*t3+t2P*t4P*t4+t2P*t4P*t3*t4-t42kP-t42kP*t3-t42kP*t2-t42kP
                    *t2*t3)/(t7*t3P+t6*t2P+t5*t1P+t4P-t4P*t5*t1*t2*t3+t5*t1P*t2+t6*t2P*t4+t6*t2P*t3+t6*t2P
                             *t1+t7*t3P*t4+t7*t3P*t2+t7*t3P*t1-t4P*t6*t2-t4P*t7*t3-t4P*t5*t1+t4P*t2*t3-t4P*t5*t3-t4P
                             *t5*t2+t4P*t1*t3-t4P*t6*t3-t4P*t6*t1+t4P*t1*t2-t4P*t7*t2-t4P*t7*t1+t5*t1P*t4+t5*t1P*t3
                             +t4P*t3-t4P*t5-t4P*t6-t4P*t7+t4P*t2+t4P*t1+t5*t1P*t2*t4+t5*t1P*t3*t4+t5*t1P*t2*t3+t6
                             *t2P*t1*t4+t6*t2P*t3*t4+t6*t2P*t1*t3+t7*t3P*t2*t4+t7*t3P*t1*t2+t7*t3P*t1*t4-t4P*t7*t1*t2-t4P
                             *t6*t2*t3-t4P*t6*t1*t2-t4P*t7*t2*t3-t4P*t7*t1*t3-t4P*t6*t1*t3-t4P*t5*t1*t2-t4P*t5*t1*t3-t4P
                             *t5*t2*t3+t4P*t1*t2*t3-t4P*t6*t1*t2*t3+t5*t1P*t2*t3*t4+t6*t2P*t1*t3*t4+t7*t3P*t1*t2*t4-t4P
                             *t7*t1*t2*t3)/(1+t4)
    
    if (k > 0) 
      test <- abs(a67/a[6,7])
    a[6,7] <- a[6,7] + a67
    k <- k+1
  }
  
  #I think it's 0 here since it's broken and then u check for the flag later
  if (k == maximumIteration) {
    return(list("flag"=0))
    break
  }
  
  #a77
  test <- 100
  k <- 0
  
  while (test > criteria & k < maximumIteration) {
    
    t1P <- (t1/(1+t1))^k
    t2P <- (t2/(1+t2))^k
    t3P <- (t3/(1+t3))^k
    t4P <- (t4/(1+t4))^k
    
    
    a77 <- 1/(t7*t3P+t6*t2P+t5*t1P+t4P-t4P*t5*t1*t2*t3+t5*t1P*t2+t6*t2P*t4+t6*t2P*t3+t6*t2P*t1
              +t7*t3P*t4+t7*t3P*t2+t7*t3P*t1-t4P*t6*t2-t4P*t7*t3-t4P*t5*t1+t4P*t2*t3-t4P*t5*t3-t4P
              *t5*t2+t4P*t1*t3-t4P*t6*t3-t4P*t6*t1+t4P*t1*t2-t4P*t7*t2-t4P*t7*t1+t5*t1P*t4+t5*t1P*t3
              +t4P*t3-t4P*t5-t4P*t6-t4P*t7+t4P*t2+t4P*t1+t5*t1P*t2*t4+t5*t1P*t3*t4+t5*t1P*t2*t3+t6
              *t2P*t1*t4+t6*t2P*t3*t4+t6*t2P*t1*t3+t7*t3P*t2*t4+t7*t3P*t1*t2+t7*t3P*t1*t4-t4P*t7*t1*t2-t4P
              *t6*t2*t3-t4P*t6*t1*t2-t4P*t7*t2*t3-t4P*t7*t1*t3-t4P*t6*t1*t3-t4P*t5*t1*t2-t4P*t5*t1*t3-t4P
              *t5*t2*t3+t4P*t1*t2*t3-t4P*t6*t1*t2*t3+t5*t1P*t2*t3*t4+t6*t2P*t1*t3*t4+t7*t3P*t1*t2*t4-t4P
              *t7*t1*t2*t3)*(1+t1)*(1+t2)/(1+t3)/(1+t4)*(-t3P-t3P*t4+t4P+t4P*t3^2)
    
    if (k > 0) 
      test <- abs(a77/a[7,7])
    a[7,7] <- a[7,7] + a77
    k <- k+1
  }
  
  #I think it's 0 here since it's broken and then u check for the flag later
  if (k == maximumIteration) {
    return(list("flag"=0))
    break
  }
  
  
}
