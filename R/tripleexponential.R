#where did the SE flag go?
TripleExponentialStandardError <- function(t1, t2, t3, t4, t5, sHatSubset) {
  
  maximumIteration <- 100000
  criteria <- 0.0000000000000001
  
  # not sure why can't do Math.pow in the console so stuck with ^ for now
  t23P <- t2^3
  t33P <- t3^3;
  
  t13P <- t1^3
  t1bP  <- t1^(-2);
  t1aP <- (1+t1)^(-3)
  t2aP <- (1+t2)^(-3);
  t2bP <- t2^(-2);
  
  t3aP <- (1 + t3)^(-3);
  t3bP <- (t3)^(-2);
  
  a00 <- -((-t4 * t3) - (t4 * t2 * t3) - (t5 * t3) - (t5 * t1 * t3) + (t4 * t1) +
             (t4 * t1 * t2) + (t5 * t2) + (t5 * t1 * t2) + t3 + (t2 * t3) + (t1 * t3) + (t1 * t2 * t3)) /
    ((-t4 * t3) - (t4 * t2 * t3) - (t5 * t3) - (t5 * t1 * t3) - 1 - t2 - t1 - (t1 * t2) +
       (t4 * t1) + (t4 * t1 * t2) + (t5 * t2) + (t5 * t1 * t2))
  
  #make a list instead of the array
  a0 <- c(-t4 * (1 + t2) * (1 + t3) / (1 + t1) / (-t4 * t3 - t4 * t2 * t3 - t5 * t3 - t5 * t1 *
                                                    t3 - 1 - t2 - t1 - t1 * t2 + t4 * t1 + t4 * t1 * t2 + t5 * t2 + t5 * t1 * t2), 
          -t5 * (1 + t3 + t1 + t1 * t3) / (1 + t2) / (-t4 * t3 - t4 * t2 * t3 - t5 * t3 - t5 *
                                                        t1 * t3 - 1 - t2 - t1 - t1 * t2 + t4 * t1 + t4 * t1 * t2 + t5 * t2 + t5 * t1 * t2), 
          (-1 - t1 + t4 + t4 * t1 + t5 + t5 * t1) * (1 + t2) / (1 + t3) /
            (-t4 * t3 - t4 * t2 * t3 - t5 * t3 - t5 * t1 * t3 - 1 - t2 - t1 - t1 * t2 + t4 * t1 + t4 *
               t1 * t2 + t5 * t2 + t5 * t1 * t2),
          -1 / (-t4 * t3 - t4 * t2 * t3 - t5 * t3 - t5 * t1 * t3 - 1 - t2 - t1 - t1 * t2 + t4 *
                  t1 + t4 * t1 * t2 + t5 * t2 + t5 * t1 * t2) * (-t3 - t2 * t3 + t1 + t1 * t2),
          -1 / (-t4 * t3 - t4 * t2 * t3 - t5 * t3 - t5 * t1 * t3 - 1 - t2 - t1 - t1 * t2 + t4 *
                  t1 + t4 * t1 * t2 + t5 * t2 + t5 * t1 * t2) * (-t3 - t1 * t3 + t2 + t1 * t2))
  
  #hmm, not quite sure why this is 5 and not 6? cuz we're creating a matrix of size 5x5 but in C# it's 6x6 if it's 0 based indexing?
  a <- matrix(0, nrow = 5, ncol = 5) 
  
  
  ## a11
  test <- 100
  k <- 0
  
  while (test > criteria & k < maximumIteration) {
    t1P <- Math.Pow((t1 / (1 + t1)), k);
    t2P <- Math.Pow((t2 / (1 + t2)), k);
    t3P <- Math.Pow((t3 / (1 + t3)), k);
    
    a11 <- -t4 * t1P * (2 * t4 * t1P * k * t1 * t2 - (t1 * t1) * t4 *
                          t1P * t2 * t3 - 2 * (t1 * t1) * t5 * t2P * t3 - 2 * t13P * t5 *
                          t2P * t3 + 2 * (t1 * t1) * t3P * t4 * t2 + 2 * t13P *
                          t3P * t4 * t2 + 2 * (t1 * t1) * t3P * t5 * t2 + 2 * t13P *
                          t3P * t5 * t2 + 5 * k * t1 * t5 * t2P + 4 * k * (t1 * t1) * t5 *
                          t2P + 5 * k * t1 * t3P * t2 - 4 * k * (t1 * t1) *
                          t3P * t4 - 5 * k * t1 * t3P * t5 - 4 * k * (t1 * t1) *
                          t3P * t5 - k * k * t5 * t2P * t3 - k * k * t5 * t2P * t1 - k * k *
                          t3P * t1 * t2 + t3P * k * t2 - 2 * t13P *
                          t3P + 2 * k * t1 * t4 * t1P * t3 + 2 * k * t1 * t4 *
                          t1P * t2 * t3 + 5 * k * t1 * t5 * t2P * t3 + 4 * k * (t1 * t1) * t5 *
                          t2P * t3 - 5 * k * t1 * t3P * t4 * t2 + k * k *
                          t3P * t4 * t2 + k * k * t3P * t4 * t1 + k * k *
                          t3P * t5 * t2 + k * k * t3P * t5 * t1 + k * t4 *
                          t1P * t3 + k * t5 * t2P * t3 - k * t3P * t4 * t2 - k *
                          t3P * t5 * t2 + 2 * t4 * t1P * k * t1 + t3P * k + 5 *
                          t3P * k * t1 - 4 * k * (t1 * t1) * t3P * t4 * t2 - 5 * k * t1 *
                          t3P * t5 * t2 - 4 * k * (t1 * t1) * t3P * t5 * t2 - k * k * t5 *
                          t2P * t1 * t3 + k * k * t3P * t4 * t1 * t2 + k * k *
                          t3P * t5 * t1 * t2 + k * t4 * t1P * t2 * t3 - 2 * t13P *
                          t3P * t2 + 2 * (t1 * t1) * t3P * t4 + 2 * t13P *
                          t3P * t4 + 2 * (t1 * t1) * t3P * t5 + 2 * t13P *
                          t3P * t5 + 4 * k * (t1 * t1) * t3P - k * k * t5 * t2P + t4 *
                          t1P * k - 2 * (t1 * t1) * t3P * t2 - 2 * t13P * t5 *
                          t2P - 2 * (t1 * t1) * t5 * t2P - k * k * t3P * t2 - k * k *
                          t3P * t1 + k * k * t3P * t4 + k * k * t3P * t5 + k * t5 *
                          t2P - k * t3P * t4 - k * t3P * t5 - t4 *
                          t1P * (t1 * t1) - t2 * t4 * t1P * (t1 * t1) + t4 *
                          t1P * k * t2 - 5 * k * t1 * t3P * t4 + 4 * k * (t1 * t1) *
                          t3P * t2 - (t1 * t1) * t4 * t1P * t3 - k * k *
                          t3P - 2 * (t1 * t1) * t3P) * t1aP / 
    (-t4 * t1P - t4 * t1P * t3 - t4 * t1P * t2 - t4 * t1P * t2 * t3 - t5 * t2P * t3 - t5 * t2P * t1 * t3 - t5 *
    t2P - t5 * t2P * t1 - t3P - t3P * t2 - t3P * t1 - t3P * t1 * t2 + t3P * t4 + t3P * t4 * t2 + t3P * t4 * t1 + t3P * t4 * t1 * t2 +
    t3P * t5 + t3P * t5 * t2 + t3P * t5 * t1 + t3P * t5 * t1 * t2) * t1bP;
    
    if (k > 0) test <- abs(a11/a[1,1])
    a[1,1] <- a[1,1] + a11
    k <- k+1
  }
  
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ## a12
  test <- 100
  k <- 0
  t1P <- Math.Pow((t1 / (1 + t1)), k);
  t2P <- Math.Pow((t2 / (1 + t2)), k);
  t3P <- Math.Pow((t3 / (1 + t3)), k);
  while (test > criteria  &  k < maximumIteration) {
    a12 <- -(1 + t3) * t1P * t4 * (-t1 + k) * t5 *
      t2P * (-t2 + k) / t2 / (1 + t2) / t1 / (1 + t1) / (-t4 * t1P - t4 *
                                                           t1P * t3 - t4 * t1P * t2 - t4 * t1P * t2 * t3 - t5 *
                                                           t2P - t5 * t2P * t3 - t5 * t2P * t1 - t5 *
                                                           t2P * t1 * t3 - t3P - t3P * t2 -
                                                           t3P * t1 - t3P * t1 * t2 + t3P * t4 +
                                                           t3P * t4 * t2 + t3P * t4 * t1 + t3P * t4 * t1 * t2 +
                                                           t3P * t5 + t3P * t5 * t2 + t3P * t5 * t1 +
                                                           t3P * t5 * t1 * t2);
    if (k > 0) test <- abs(a12/a[1,2])
    a[1,2] <- a[1,2] + a12
    k <- k+1
  }  
  
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  
  ## a13
  test <- 100
  k <- 0
  t1P <- Math.Pow((t1 / (1 + t1)), k);
  t2P <- Math.Pow((t2 / (1 + t2)), k);
  t3P <- Math.Pow((t3 / (1 + t3)), k);
  
  while (test > criteria & k < maximumIteration) {
    a13 <- (1 + t2) * t1P * t4 * (-t1 + k) * (-1 + t4 + t5) *
      t3P * (-t3 + k) / t3 / (1 + t3) / t1 / (1 + t1) / (-t4 * t1P - t4 *
                                                           t1P * t3 - t4 * t1P * t2 - t4 * t1P * t2 * t3 - t5 *
                                                           t2P - t5 * t2P * t3 - t5 * t2P * t1 - t5 *
                                                           t2P * t1 * t3 - t3P - t3P * t2 -
                                                           t3P * t1 - t3P * t1 * t2 + t3P * t4 +
                                                           t3P * t4 * t2 + t3P * t4 * t1 + t3P * t4 * t1 * t2 +
                                                           t3P * t5 + t3P * t5 * t2 + t3P * t5 * t1 +
                                                           
                                                           t3P * t5 * t1 * t2);
    
    if (k > 0) test <- abs(a13/a[1,3])
    a[1,3] <- a[1,3] + a13
    k <- k+1
    
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  
  ## a14
  test <- 100
  k <- 0
  t1P <- Math.Pow((t1 / (1 + t1)), k);
  t2P <- Math.Pow((t2 / (1 + t2)), k);
  t3P <- Math.Pow((t3 / (1 + t3)), k);
  
  while (test > criteria & k < maximumIteration) {
    a14 <- -t1P * (-t1 + k) * (t5 * t2P * t3 + t5 * t2P +
                                 t3P * t2 - t3P * t5 * t2 + t3P -
                                 t3P * t5) / (t4 * t1P + t4 * t1P * t3 + t4 *
                                                t1P * t2 + t4 * t1P * t2 * t3 + t5 * t2P + t5 *
                                                t2P * t3 + t5 * t2P * t1 + t5 * t2P * t1 * t3 +
                                                t3P + t3P * t2 + t3P * t1 + t3P * t1 * t2 -
                                                t3P * t4 - t3P * t4 * t2 - t3P * t4 * t1 -
                                                t3P * t4 * t1 * t2 - t3P * t5 - t3P * t5 * t2 -
                                                t3P * t5 * t1 - t3P * t5 * t1 * t2) / t1 / (1 + t1);
    if (k > 0) test <- abs(a14/a[1,4])
    a[1,4] <- a[1,4] + a14
    k <- k+1
    
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  
  ## a15
  test <- 100
  k <- 0
  t1P <- Math.Pow((t1 / (1 + t1)), k);
  t2P <- Math.Pow((t2 / (1 + t2)), k);
  t3P <- Math.Pow((t3 / (1 + t3)), k);
  
  while (test > criteria & k < maximumIteration) {
    a15 <-  t1P * t4 * (-t1 + k) * (t2P + t2P * t3 -
                                      t3P - t3P * t2) / t1 / (1 + t1) / (t4 * t1P + t4 *
                                                                           t1P * t3 + t4 * t1P * t2 + t4 * t1P * t2 * t3 + t5 *
                                                                           t2P + t5 * t2P * t3 + t5 * t2P * t1 + t5 *
                                                                           t2P * t1 * t3 + t3P + t3P * t2 +
                                                                           t3P * t1 + t3P * t1 * t2 - t3P * t4 -
                                                                           t3P * t4 * t2 - t3P * t4 * t1 - t3P * t4 * t1 * t2 -
                                                                           t3P * t5 - t3P * t5 * t2 - t3P * t5 * t1 -
                                                                           t3P * t5 * t1 * t2)
    if (k > 0) test <- abs(a15/a[1,5])
    a[1,5] <- a[1,5] + a15
    k <- k+1
    
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ## a22
  test <- 100
  k <- 0
  ## missing t1p etc assignment here?
  while (test > criteria & k < maximumIteration) {
    a22 <- -t5 * t2P * (-k * k * t3P * t5 + k *
                          t3P * t4 + k * k * t4 * t1P + 2 * (t2 * t2) * t4 * t1P + 2 *
                          t23P * t4 * t1P + 2 * (t2 * t2) * t3P * t1 + 2 * t23P *
                          t3P * t1 - 4 * k * (t2 * t2) * t3P - 2 *
                          t3P * t5 * (t2 * t2) - 2 * t5 * t23P * t3P - 2 * t4 * t23P *
                          t3P - 2 * t3P * (t2 * t2) * t4 + 5 * k * t1 *
                          t3P * t4 * t2 + 2 * (t2 * t2) * t3P + 2 * t23P *
                          t3P - k * t5 * t2P - k * k * t3P * t4 + k * k *
                          t3P * t1 - k * t1 * t5 * t2P - 5 * k * t1 *
                          t3P * t2 + k * t1 * t3P * t4 + k * t1 * t3P * t5 + k * k *
                          t3P * t1 * t2 - k * k * t3P * t4 * t2 - k * k *
                          t3P * t4 * t1 - k * k * t3P * t5 * t2 - k * k *
                          t3P * t5 * t1 - k * t4 * t1P * t3 - 5 * k * t4 *
                          t1P * t2 - k * t5 * t2P * t3 + 5 * k *
                          t3P * t4 * t2 + 5 * k * t3P * t5 * t2 - k * t1 * t5 *
                          t2P * t3 + 5 * k * t1 * t3P * t5 * t2 - k * k *
                          t3P * t4 * t1 * t2 - k * k * t3P * t5 * t1 * t2 - 5 * k * t4 *
                          t1P * t2 * t3 + k * k * t3P * t2 - 2 * t5 *
                          t2P * k * t1 * t2 - 2 * k * t2 * t5 * t2P * t1 * t3 + k * k * t4 *
                          t1P * t3 + k * k * t4 * t1P * t2 + t1 * t5 *
                          t2P * (t2 * t2) + 4 * t3P * k * t4 * (t2 * t2) - 2 * t5 *
                          t2P * k * t2 - 2 * t3P * t5 * t1 * t23P - 2 *
                          t3P * t5 * t1 * (t2 * t2) + 4 * t5 * k * (t2 * t2) * t3P + 2 * (t2 * t2) * t4 *
                          t1P * t3 + 2 * t23P * t4 * t1P * t3 + (t2 * t2) * t5 *
                          t2P * t3 - 4 * k * (t2 * t2) * t4 * t1P - 4 * k * (t2 * t2) *
                          t3P * t1 - 2 * t3P * t1 * t23P * t4 - 2 *
                          t3P * t1 * (t2 * t2) * t4 + (t2 * t2) * t5 * t2P * t1 * t3 - 4 * k * (t2 * t2) * t4 *
                          t1P * t3 + 4 * t3P * k * t4 * (t2 * t2) * t1 + 4 * t5 * t1 * k * (t2 * t2) *
                          t3P + k * k * t4 * t1P * t2 * t3 + k * t3P * t5 - k * t1 *
                          t3P - t4 * t1P * k + k * k * t3P - k *
                          t3P - 5 * k * t3P * t2 - 2 * k * t2 * t5 * t2P * t3 + t5 *
                          t2P * (t2 * t2)) * t2aP / (t4 * t1P + t4 *
                                                       t1P * t3 + t4 * t1P * t2 + t4 * t1P * t2 * t3 + t5 *
                                                       t2P + t5 * t2P * t3 + t5 * t2P * t1 + t5 *
                                                       t2P * t1 * t3 + t3P + t3P * t2 +
                                                       t3P * t1 + t3P * t1 * t2 - t3P * t4 -
                                                       t3P * t4 * t2 - t3P * t4 * t1 - t3P * t4 * t1 * t2 -
                                                       t3P * t5 - t3P * t5 * t2 - t3P * t5 * t1 -
                                                       t3P * t5 * t1 * t2) * t2bP)


if (k > 0) test <- abs(a22/a[2,2])
a[2,2] <- a[2,2] + a22
k <- k+1
  }
  
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ##
  ## a23
  test <- 100
  k <- 0
  while (test > criteria & k < maximumIteration) {
    
    t1P <- Math.Pow((t1 / (1 + t1)), k);
    t2P <- Math.Pow((t2 / (1 + t2)), k);
    t3P <- Math.Pow((t3 / (1 + t3)), k);
    
    #fix math later? formatting problems
    a23 <- -(1 + t1) * t2P * t5 * (-t2 + k) * (-1 + t4 + t5) *
      t3P * (-t3 + k) / t3 / (1 + t3) / t2 / (1 + t2) / (t4 * t1P + t4 *
                                                           t1P * t3 + t4 * t1P * t2 + t4 * t1P * t2 * t3 + t5 *
                                                           t2P * t3 + t5 * t2P * t1 * t3 + t5 * t2P + t5 *
                                                           t2P * t1 - t3P * t4 - t3P * t4 * t2 +
                                                           t3P + t3P * t2 + t3P * t1 +
                                                           t3P * t1 * t2 - t3P * t4 * t1 - t3P * t4 * t1 * t2 -
                                                           t3P * t5 - t3P * t5 * t2 - t3P * t5 * t1 -
                                                           t3P * t5 * t1 * t2);
    
    if (k > 0) test <- abs(a23/a[2,3])
    a[2,3] <- a[2,3] + a23
    k <- k+1
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ## a24
  test <- 100
  k <- 0
  while (test > criteria & k < maximumIteration) {
    
    t1P <- Math.Pow((t1 / (1 + t1)), k);
    t2P <- Math.Pow((t2 / (1 + t2)), k);
    t3P <- Math.Pow((t3 / (1 + t3)), k);
    
    #fix math later? formatting problems
    a24 <- t5 * t2P * (-t2 + k) * (t1P + t1P * t3 -
                                     t3P - t3P * t1) / (1 + t2) / (t4 * t1P + t4 *
                                                                     t1P * t3 + t4 * t1P * t2 + t4 * t1P * t2 * t3 + t5 *
                                                                     t2P * t3 + t5 * t2P * t1 * t3 + t5 * t2P + t5 *
                                                                     t2P * t1 - t3P * t4 - t3P * t4 * t2 +
                                                                     t3P + t3P * t2 + t3P * t1 +
                                                                     t3P * t1 * t2 - t3P * t4 * t1 - t3P * t4 * t1 * t2 -
                                                                     t3P * t5 - t3P * t5 * t2 - t3P * t5 * t1 -
                                                                     t3P * t5 * t1 * t2) / t2;
    
    
    if (k > 0) test <- abs(a24/a[2,4])
    a[2,4] <- a[2,4] + a24
    k <- k+1
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ## a25
  test <- 100
  k <- 0
  while (test > criteria & k < maximumIteration) {
    
    t1P <- Math.Pow((t1 / (1 + t1)), k);
    t2P <- Math.Pow((t2 / (1 + t2)), k);
    t3P <- Math.Pow((t3 / (1 + t3)), k);
    
    #fix math later? formatting problems
    a25 <-  -t2P * (-t2 + k) * (t3P - t3P * t4 + t4 *
                                  t1P * t3 + t4 * t1P + t3P * t1 -
                                  t3P * t4 * t1) / (t4 * t1P + t4 * t1P * t3 + t4 *
                                                      t1P * t2 + t4 * t1P * t2 * t3 + t5 * t2P * t3 + t5 *
                                                      t2P * t1 * t3 + t5 * t2P + t5 * t2P * t1 -
                                                      t3P * t4 - t3P * t4 * t2 + t3P +
                                                      t3P * t2 + t3P * t1 + t3P * t1 * t2 -
                                                      t3P * t4 * t1 - t3P * t4 * t1 * t2 - t3P * t5 -
                                                      t3P * t5 * t2 - t3P * t5 * t1 -
                                                      t3P * t5 * t1 * t2) / t2 / (1 + t2);
    
                                                                     t3P * t1 * t2 - t3P * t4 * t1 - t3P * t4 * t1 * t2 -
                                                                     t3P * t5 - t3P * t5 * t2 - t3P * t5 * t1 -
                                                                     t3P * t5 * t1 * t2) / t2;
    
    
    if (k > 0) test <- abs(a25/a[2,5])
    a[2,5] <- a[2,5] + a25
    k <- k+1
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  
  ## a33
  test <- 100
  k <- 0
  while (test > criteria & k < maximumIteration) {
    
    t1P <- Math.Pow((t1 / (1 + t1)), k);
    t2P <- Math.Pow((t2 / (1 + t2)), k);
    t3P <- Math.Pow((t3 / (1 + t3)), k);
    
    #fix math later? formatting problems
    a33 <-  (-1 + t4 + t5) * t3P * (k * k * t5 * t2P * t1 + k * k * t4 *
                                      t1P * t2 * t3 - (t3 * t3) * t3P * t5 * t1 - 2 * k * t3 *
                                      t3P * t1 - 5 * k * t4 * t1P * t3 - k * t4 *
                                      t1P * t2 - 5 * k * t5 * t2P * t3 + k *
                                      t3P * t4 * t2 + 2 * (t3 * t3) * t4 * t1P + 2 * t33P * t4 *
                                      t1P + 2 * t33P * t5 * t2P + 2 * (t3 * t3) * t5 *
                                      t2P - (t3 * t3) * t3P * t4 + (t3 * t3) *
                                      t3P * t2 + (t3 * t3) * t3P * t1 - (t3 * t3) *
                                      t3P * t5 + k * t1 * t3P * t5 + k * t1 *
                                      t3P * t4 - k * t1 * t5 * t2P + k *
                                      t3P * t5 * t2 + k * k * t4 * t1P + k * k * t4 *
                                      t1P * t3 + k * k * t4 * t1P * t2 - (t3 * t3) *
                                      t3P * t4 * t2 + (t3 * t3) * t3P * t1 * t2 + 2 * t33P * t5 *
                                      t2P * t1 + 2 * (t3 * t3) * t5 * t2P * t1 - 2 * k * t3 *
                                      t3P * t2 + 2 * k * t3 * t3P * t4 + 2 * (t3 * t3) * t4 *
                                      t1P * t2 - t4 * t1P * k + k * t3P * t5 + k *
                                      t3P * t4 - k * t5 * t2P - k * t1 * t3P + k * k * t5 *
                                      t2P - k * t3P * t2 - (t3 * t3) * t3P * t5 * t2 - (t3 * t3) *
                                      t3P * t4 * t1 + 2 * k * t3 * t3P * t5 - 4 * k * (t3 * t3) * t5 *
                                      t2P - 4 * k * (t3 * t3) * t4 * t1P + 2 * t33P * t4 *
                                      t1P * t2 - k * t3P + k * k * t5 * t2P * t3 - (t3 * t3) *
                                      t3P * t4 * t1 * t2 - (t3 * t3) * t3P * t5 * t1 * t2 - 4 * k * (t3 * t3) * t4 *
                                      t1P * t2 - 4 * k * (t3 * t3) * t5 * t2P * t1 + 2 * k * t3 *
                                      t3P * t4 * t2 - 2 * k * t3 * t3P * t1 * t2 + 2 * k * t3 *
                                      t3P * t4 * t1 + 2 * k * t3 * t3P * t4 * t1 * t2 + 2 * k * t3 *
                                      t3P * t5 * t2 + 2 * k * t3 * t3P * t5 * t1 + 2 * k * t3 *
                                      t3P * t5 * t1 * t2 - 5 * k * t1 * t5 * t2P * t3 + k * t1 *
                                      t3P * t4 * t2 + k * t1 * t3P * t5 * t2 + k * k * t5 *
                                      t2P * t1 * t3 - k * t1 * t3P * t2 + (t3 * t3) *
                                      t3P - 5 * k * t4 * t1P * t2 * t3 - 2 * k * t3 * t3P) / (t4 *
                                                                                                t1P + t4 * t1P * t3 + t4 * t1P * t2 + t4 *
                                                                                                t1P * t2 * t3 + t5 * t2P * t3 + t5 * t2P * t1 * t3 + t5 *
                                                                                                t2P + t5 * t2P * t1 - t3P * t4 -
                                                                                                t3P * t4 * t2 + t3P + t3P * t2 +
                                                                                                t3P * t1 + t3P * t1 * t2 - t3P * t4 * t1 -
                                                                                                t3P * t4 * t1 * t2 - t3P * t5 - t3P * t5 * t2 -
                                                                                                t3P * t5 * t1 - t3P * t5 * t1 * t2) * t3bP * t3aP;
    


if (k > 0) test <- abs(a33/a[3,3])
a[3,3] <- a[3,3] + a33
k <- k+1
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  
  ## a34
  test <- 100
  k <- 0
  while (test > criteria & k < maximumIteration) {
    
    t1P <- Math.Pow((t1 / (1 + t1)), k);
    t2P <- Math.Pow((t2 / (1 + t2)), k);
    t3P <- Math.Pow((t3 / (1 + t3)), k);
    
    #fix math later? formatting problems
    a34 <-  -t3P * (-t3 + k) * (t5 * t1P - t1P - t2 *
                                  t1P + t5 * t2 * t1P - t5 * t2P - t5 *
                                  t2P * t1) / (t4 * t1P + t4 * t1P * t3 + t4 *
                                                 t1P * t2 + t4 * t1P * t2 * t3 + t5 * t2P * t3 + t5 *
                                                 t2P * t1 * t3 + t5 * t2P + t5 * t2P * t1 -
                                                 t3P * t4 - t3P * t4 * t2 + t3P +
                                                 t3P * t2 + t3P * t1 + t3P * t1 * t2 -
                                                 t3P * t4 * t1 - t3P * t4 * t1 * t2 - t3P * t5 -
                                                 t3P * t5 * t2 - t3P * t5 * t1 -
                                                 t3P * t5 * t1 * t2) / t3 / (1 + t3);
    
    
    
    if (k > 0) test <- abs(a34/a[3,4])
    a[3,4] <- a[3,4] + a34
    k <- k+1
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ## a35
  test <- 100
  k <- 0
  while (test > criteria & k < maximumIteration) {
    
    t1P <- Math.Pow((t1 / (1 + t1)), k);
    t2P <- Math.Pow((t2 / (1 + t2)), k);
    t3P <- Math.Pow((t3 / (1 + t3)), k);
    
    #fix math later? formatting problems
    a35 <-  t3P * (-t3 + k) * (-t4 * t2P + t1 * t2P + t4 *
                                 t1P + t2P - t4 * t1 * t2P + t4 *
                                 t1P * t2) / (t4 * t1P + t4 * t1P * t3 + t4 *
                                                t1P * t2 + t4 * t1P * t2 * t3 + t5 * t2P * t3 + t5 *
                                                t2P * t1 * t3 + t5 * t2P + t5 * t2P * t1 -
                                                t3P * t4 - t3P * t4 * t2 + t3P +
                                                t3P * t2 + t3P * t1 + t3P * t1 * t2 -
                                                t3P * t4 * t1 - t3P * t4 * t1 * t2 - t3P * t5 -
                                                t3P * t5 * t2 - t3P * t5 * t1 -
                                                t3P * t5 * t1 * t2) / t3 / (1 + t3);
    
    
    
    
    if (k > 0) test <- abs(a35/a[3,5])
    a[3,5] <- a[3,5] + a35
    k <- k+1
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  
  
  ## a44
  test <- 100
  k <- 0
  while (test > criteria & k < maximumIteration) {
    
    t1P <- Math.Pow((t1 / (1 + t1)), k);
    t2P <- Math.Pow((t2 / (1 + t2)), k);
    t3P <- Math.Pow((t3 / (1 + t3)), k);
    
    #fix math later? formatting problems
    a44 <-   (1 + t2) * Math.Pow(t1P + t1P * t3 - t3P -
                                   t3P * t1, 2) / (1 + t3) / (1 + t1) / (t4 * t1P + t4 *
                                                                           t1P * t3 + t4 * t1P * t2 + t4 * t1P * t2 * t3 + t5 *
                                                                           t2P * t3 + t5 * t2P * t1 * t3 + t5 * t2P + t5 *
                                                                           t2P * t1 - t3P * t4 - t3P * t4 * t2 +
                                                                           t3P + t3P * t2 + t3P * t1 + t3P * t1 * t2 -
                                                                           t3P * t4 * t1 - t3P * t4 * t1 * t2 - t3P * t5 -
                                                                           t3P * t5 * t2 - t3P * t5 * t1 - t3P * t5 * t1 * t2);
    
    
    
    
    if (k > 0) test <- abs(a44/a[4,4])
    a[4,4] <- a[4,4] + a44
    k <- k+1
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ## a45
  test <- 100
  k <- 0
  while (test > criteria & k < maximumIteration) {
    
    t1P <- Math.Pow((t1 / (1 + t1)), k);
    t2P <- Math.Pow((t2 / (1 + t2)), k);
    t3P <- Math.Pow((t3 / (1 + t3)), k);
    
    #fix math later? formatting problems
    a45 <-  (t1P + t1P * t3 - t3P -
               t3P * t1) * (t2P + t2P * t3 - t3P -
                              t3P * t2) / (1 + t3) / (t4 * t1P + t4 * t1P * t3 + t4 *
                                                        t1P * t2 + t4 * t1P * t2 * t3 + t5 * t2P * t3 + t5 *
                                                        t2P * t1 * t3 + t5 * t2P + t5 * t2P * t1 -
                                                        t3P * t4 - t3P * t4 * t2 + t3P + t3P * t2 +
                                                        t3P * t1 + t3P * t1 * t2 - t3P * t4 * t1 -
                                                        t3P * t4 * t1 * t2 - t3P * t5 - t3P * t5 * t2 -
                                                        t3P * t5 * t1 - t3P * t5 * t1 * t2);
    
    
    
    if (k > 0) test <- abs(a45/a[4,5])
    a[4,5] <- a[4,5] + a45
    k <- k+1
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  

  ## a55
  test <- 100
  k <- 0
  while (test > criteria & k < maximumIteration) {
    
    t1P <- Math.Pow((t1 / (1 + t1)), k);
    t2P <- Math.Pow((t2 / (1 + t2)), k);
    t3P <- Math.Pow((t3 / (1 + t3)), k);
    
    #fix math later? formatting problems
    a55 <-  (1 + t1) * Math.Pow(t2P + t2P * t3 - t3P -
                                  t3P * t2, 2) / (1 + t3) / (1 + t2) / (t4 * t1P + t4 *
                                                                          t1P * t3 + t4 * t1P * t2 + t4 * t1P * t2 * t3 + t5 *
                                                                          t2P * t3 + t5 * t2P * t1 * t3 + t5 * t2P + t5 *
                                                                          t2P * t1 - t3P * t4 - t3P * t4 * t2 +
                                                                          t3P + t3P * t2 + t3P * t1 + t3P * t1 * t2 -
                                                                          t3P * t4 * t1 - t3P * t4 * t1 * t2 - t3P * t5 -
                                                                          t3P * t5 * t2 - t3P * t5 * t1 - t3P * t5 * t1 * t2);
    
    
    
    
    if (k > 0) test <- abs(a55/a[5,5])
    a[5,5] <- a[5,5] + a55
    k <- k+1
  }
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  print(a)
  ## invert
  MatrixInversion(sHatSubset, a00, a0, a)
  
}