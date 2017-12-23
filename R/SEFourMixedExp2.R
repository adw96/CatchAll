SEFourMixedExpA2 <- function(t1, t2, t3, t4, t5, t6, t7, a, sHatSubset) {
  
  criteria <- 0.0000000000000001
  maximumIteration <- 100000
  test <- 100.0
  k <- 0
  
  t23P <- pow(t2, 3)
  t24P <- pow(t2, 4)
  t2aP <- pow((1 + t2), (-3))
  t2bP <- pow(t2, (-2))
  
  ##A22
  while (test > criteria & k < maximumIteration) {
    t1P <- pow((t1 / (1 + t1)), k)
    t2P <- pow((t2 / (1 + t2)), k)
    t3P <- pow((t3 / (1 + t3)), k)
    t4P <- pow((t4 / (1 + t4)), k)
    
    t12kP <- pow((t1 / (1 + t1)), (2 * k))
    t22kP <- pow((t2 / (1 + t2)), (2 * k))
    t32kP <- pow((t3 / (1 + t3)), (2 * k))
    t42kP <- pow((t4 / (1 + t4)), (2 * k))
    t23kP <- pow((t2 / (1 + t2)), (3 * k))
    
    a22 <- t2aP * t2bP * pow(t7 * t3P + t6 * t2P + t5 * t1P + t4P - t4P * t5 * t1 * t2 * t3 + t5 * t1P * t2 + t6 * t2P * t4 + t6 * t2P
                             * t3 + t6 * t2P * t1 + t7 * t3P * t4 + t7 * t3P * t2 + t7 * t3P * t1 - t4P * t6 * t2 - t4P * t7 * t3 - t4P * t5 * t1 + t4P * t2 * t3 - t4P
                             * t5 * t3 - t4P * t5 * t2 + t4P * t1 * t3 - t4P * t6 * t3 - t4P * t6 * t1 + t4P * t1 * t2 - t4P * t7 * t2 - t4P * t7 * t1 + t5 * t1P * t4
                             + t5 * t1P * t3 + t4P * t3 - t4P * t5 - t4P * t6 - t4P * t7 + t4P * t2 + t4P * t1 + t5 * t1P * t2 * t4 + t5 * t1P * t3 * t4 + t5 * t1P
                             * t2 * t3 + t6 * t2P * t1 * t4 + t6 * t2P * t3 * t4 + t6 * t2P * t1 * t3 + t7 * t3P * t2 * t4 + t7 * t3P * t1 * t2 + t7 * t3P * t1 * t4 - t4P
                             * t7 * t1 * t2 - t4P * t6 * t2 * t3 - t4P * t6 * t1 * t2 - t4P * t7 * t2 * t3 - t4P * t7 * t1 * t3 - t4P * t6 * t1 * t3 - t4P * t5 * t1 * t2 - t4P
                             * t5 * t1 * t3 - t4P * t5 * t2 * t3 + t4P * t1 * t2 * t3 - t4P * t6 * t1 * t2 * t3 + t5 * t1P * t2 * t3 * t4 + t6 * t2P * t1 * t3 * t4 + t7 * t3P
                             * t1 * t2 * t4 - t4P * t7 * t1 * t2 * t3, -2) * t6 * (2 * t42kP * (t1 * t1) * t6 * t2P * k * k * (t3 * t3) - 2 * t42kP * t2P * k * k * t2 + 8 * t23P
                                                                                   * (t3 * t3) * t42kP * t2P * t6 - 2 * t42kP * t2P * t24P * t5 * t5 + 4 * t4P * t1 * t6 * t6 * t22kP * k * k * t3 * t2 - 8 * t42kP * (t1
                                                                                                                                                                                                                       * t1) * t6 * t2P * k * t23P - 2 * t42kP * t5 * t2P * k * k * (t2 * t2) * t7 * (t1 * t1) - 16 * t42kP * t1 * t2P * k * t23P * t7 + 4 * t42kP
                                                                                   * t5 * t5 * (t1 * t1) * t2P * k * t23P - t42kP * (t1 * t1) * t2P * k * k * (t2 * t2) + 2 * t6 * t6 * t23kP * t1 * (t3 * t3) * k + 4 * t42kP
                                                                                   * t5 * (t3 * t3) * t2P * k * k * t2 - 24 * t42kP * t1 * (t3 * t3) * t2P * k * t5 * t2 - 2 * t42kP * t3 * t2P * k * k * (t2 * t2) + 8 * (t2 * t2)
                                                                                   * t3 * t42kP * t2P * t6 - 4 * t5 * t5 * t12kP * t4 * t2P * (t2 * t2) + 2 * t6 * t6 * t23kP * (t1 * t1) * t4 * k - 4 * t42kP * t7 * t1 * t2P
                                                                                   * k + 4 * t42kP * t7 * (t1 * t1) * t2P * k * t5 * t3 - 8 * t42kP * t5 * t2P * k * k * t2 * t7 * t1 - 2 * t42kP * t2P * t24P * t5 * t5 * (t3
                                                                                                                                                                                                                            * t3) * (t1 * t1) + t4P * t5 * t22kP * k * k * t2 * t6 * t4 - 18 * t42kP * (t3 * t3) * t2P * k * t5 * (t2 * t2) + 4 * t42kP * t6 * t2P * k
                                                                                   * k * t2 - t4P * t22kP * k * k * t6 * t2 - 2 * t42kP * (t1 * t1) * (t3 * t3) * t2P * k * t7 + 8 * t42kP * t2P * t23P * t5 * (t3 * t3)
                                                                                   * (t1 * t1) + 4 * t42kP * t5 * t2P * k * k * t2 - 8 * t23P * t42kP * t6 * t2P * t7 - 18 * t42kP * t6 * t2P * k * (t2 * t2) + 24 * t42kP
                                                                                   * t1 * t6 * t2P * k * t2 * t5 * (t3 * t3) - 2 * t42kP * t7 * t7 * (t3 * t3) * t2P * k * k * t2 + 4 * t42kP * t7 * (t1 * t1) * t3 * t2P * k * k * (t2
                                                                                                                                                                                                                                     * t2) - 2 * t5 * t1P * (t3 * t3) * t4 * t6 * t22kP * k * k * t1 - 2 * (t2 * t2) * t42kP * t6 * t6 * t2P + 2 * t4P * t3 * t4 * t6 * t6 * t22kP
                                                                                   * k * k * t2 - 2 * t42kP * t7 * t7 * (t1 * t1) * t2P * k * k * t2 - 12 * t42kP * t2P * k * t7 * t2 + 4 * t42kP * t7 * t3 * t2P * k * k - 2 * (t2
                                                                                                                                                                                                                                 * t2) * t7 * t7 * t32kP * t2P * (t4 * t4) + 8 * t7 * t7 * t32kP * t2P * k * t23P * t1 - 16 * t23P * t1 * t6 * t6 * t2P * t42kP * t3
                                                                                   + 2 * t42kP * t7 * (t1 * t1) * t2P * k * t5 + 8 * t42kP * t6 * t2P * k * t23P * t7 - 12 * t42kP * (t1 * t1) * t2P * k * t2 * t7 - 4 * t42kP
                                                                                   * t7 * t1 * (t3 * t3) * t2P * k * k * t6 + 8 * t42kP * (t1 * t1) * t6 * t2P * k * t23P * t7 * (t3 * t3) + 8 * t42kP * t3 * t2P * k * t23P
                                                                                   + 18 * t42kP * t7 * t2P * k * t5 * (t1 * t1) * (t2 * t2) * (t3 * t3) - 4 * t42kP * t2P * t24P * t5 * (t3 * t3) * t7 * (t1 * t1) + 2 * t5 * t5
                                                                                   * t12kP * t4 * t2P * k * (t3 * t3) - t7 * t3P * t3 * t22kP * k * k * t6 * (t1 * t1) - 2 * t4P * t1 * t22kP * k * k * t6 * (t3 * t3) * t2 - 4
                                                                                   * (t2 * t2) * t1 * t2P * t42kP * t7 * t7 + 2 * t42kP * t5 * (t3 * t3) * t2P * k * k * (t2 * t2) * (t1 * t1) - 16 * t23P * t7 * t7 * t32kP
                                                                                   * t2P * t1 * t4 + 6 * t42kP * t7 * t7 * t2P * k * t2 * (t3 * t3) + t4P * t7 * t22kP * k * k * t6 * t4 + 18 * t7 * t7 * t32kP * t2P * k * (t2
                                                                                                                                                                                                                             * t2) * t1 - 4 * t23P * t42kP * t6 * t6 * t2P * (t3 * t3) + 4 * (t2 * t2) * (t3 * t3) * t42kP * t2P * t6 * (t1 * t1) + 12 * t42kP * t5
                                                                                   * t5 * t2P * k * t2 * t3 + 2 * t42kP * t6 * t2P * k * k - 4 * t24P * (t3 * t3) * t42kP * t7 * t2P * t6 + 24 * t42kP * t6 * t2P * k * t2
                                                                                   * t7 * t3 + 6 * t5 * t5 * t12kP * (t3 * t3) * t2P * k * t2 * (t4 * t4) + 2 * t6 * t6 * t23kP * (t1 * t1) * t3 * k - 8 * t42kP * t7 * (t1 * t1) * t3
                                                                                   * t2P * k * k * t6 * t2 + 18 * t42kP * t3 * t2P * k * (t2 * t2) - 4 * t42kP * (t3 * t3) * t6 * t2P * k * k * t7 * t2 - t7 * t7 * t32kP * t2P
                                                                                   * k * k * (t2 * t2) * (t4 * t4) + 4 * (t2 * t2) * t42kP * t2P * t5 * (t1 * t1) + 8 * t42kP * t6 * t6 * t2P * k * t23P * t3 - t7 * t3P * t3
                                                                                   * (t4 * t4) * t22kP * k * k * t6 - 4 * t4P * t1 * t22kP * k * k * t6 * t3 - 4 * t42kP * t2P * t24P * t5 * t6 - t5 * t1P * (t3 * t3) * (t4
                                                                                                                                                                                                                          * t4) * t6 * t22kP * k * k * t1 * t2 + 8 * t42kP * t7 * t3 * t2P * k * k * t2 - 2 * t5 * t1P * t3 * (t4 * t4) * t6 * t22kP * k * k * t1 - 4 * t5 * t5
                                                                                   * t12kP * t24P * t2P * t4 + 8 * t23P * t42kP * t2P * t6 * (t1 * t1) - 2 * (t2 * t2) * (t1 * t1) * t6 * t6 * t2P * t42kP * (t3
                                                                                                                                                                                                              * t3) - 2 * t42kP * t5 * t2P * k * k * (t2 * t2) * t6 - 36 * t42kP * t6 * t2P * k * (t2 * t2) * t3 - 2 * t5 * t5 * t12kP * (t4 * t4) * t2P
                                                                                   * (t2 * t2) - 4 * t24P * (t1 * t1) * t6 * t6 * t2P * t42kP * t3 + 4 * t42kP * t5 * t2P * k * k * t2 * (t1 * t1) + 18 * t42kP * t7 * t7 * t2P
                                                                                   * k * t1 * (t3 * t3) * (t2 * t2) + 12 * t7 * t7 * t32kP * t1 * (t4 * t4) * t2P * k * t2 + 2 * t4P * t1 * t6 * t6 * t22kP * k * k + 18 * t42kP
                                                                                   * t1 * t2P * k * (t2 * t2) - 2 * t42kP * t5 * (t3 * t3) * t2P * k * k * (t2 * t2) * t7 * (t1 * t1) + 2 * t6 * t6 * t23kP * t4 * k + 8 * t42kP
                                                                                   * t2P * t24P * t5 * (t3 * t3) * t1 - 8 * t5 * t5 * t12kP * t23P * t2P * (t3 * t3) * t4 + 12 * t42kP * t7 * t7 * t2P * k * t1 * t2 + 4
                                                                                   * (t2 * t2) * t42kP * t2P * t6 * (t1 * t1) + 18 * t42kP * t6 * t2P * k * (t2 * t2) * t7 + 8 * t23P * (t3 * t3) * t42kP * t2P * t7 - 4
                                                                                   * t42kP * t1 * t2P * k * k * t3 * (t2 * t2) - 0.48e2 * t42kP * t1 * t3 * t2P * k * t5 * t2 - 8 * t42kP * t5 * t2P * k * t23P + 2 * t42kP
                                                                                   * t7 * t7 * t1 * t2P * k * (t3 * t3) + t6 * t6 * t23kP * (t1 * t1) * (t3 * t3) * k - t5 * t5 * t12kP * (t3 * t3) * (t4 * t4) * t2P * k * k - 2 * t7
                                                                                   * t3P * t1 * t22kP * k * k * t6 + 8 * (t2 * t2) * t42kP * t2P * t6 * t1 - 2 * t42kP * t1 * t2P * k * k * (t3 * t3) * (t2 * t2) + 18 * t42kP
                                                                                   * (t1 * t1) * t6 * t2P * k * (t2 * t2) * t5 * (t3 * t3) + 4 * t42kP * t7 * (t1 * t1) * (t3 * t3) * t2P * k * k * t2 + 18 * t42kP * t1 * t6 * t6
                                                                                   * t2P * k * (t2 * t2) * (t3 * t3) - t4P * t22kP * k * k * t6 + t4P * t6 * t6 * t22kP * k * k - 4 * t23P * t7 * t7 * t32kP * t2P * (t1
                                                                                                                                                                                                                      * t1) * (t4 * t4) + 8 * t5 * t5 * t12kP * (t3 * t3) * t2P * k * t23P * t4 - 2 * t42kP * t5 * t5 * (t3 * t3) * t2P * k * k * t2 * (t1 * t1) - 0.72e2
                                                                                   * t42kP * t1 * t6 * t2P * k * (t2 * t2) * t3 - 2 * t42kP * t5 * t2P * k * k * t7 * (t3 * t3) - 8 * t42kP * t2P * t24P * t5 * (t3 * t3)
                                                                                   * t7 * t1 + 0.72e2 * t42kP * t1 * t6 * t2P * k * (t2 * t2) * t7 * t3 - 2 * t42kP * (t3 * t3) * t2P * k * t6 - 4 * t24P * t1 * t6 * t6 * t2P
                                                                                   * t42kP - 4 * t24P * t42kP * t6 * t6 * t2P * t3 - 4 * t5 * t5 * t12kP * t23P * t2P * (t4 * t4) + 2 * t4P * t2P * k * t7 * t3P
                                                                                   * (t1 * t1) + 2 * t4P * t2P * k * t5 * t1P * t1 + 2 * t6 * t22kP * t1 * (t3 * t3) * k * t5 * t1P * (t4 * t4) - 4 * t6 * t22kP * t1 * (t3 * t3)
                                                                                   * k * t4P * t7 * t4 + 8 * t6 * t22kP * t1 * t3 * k * t4P + 2 * t4P * t7 * t7 * (t1 * t1) * t2P * k * k * t3P - 2 * t6 * t22kP * k * t4P
                                                                                   * t7 - 2 * t6 * t22kP * k * t4P * t5 + 4 * t7 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P * t1 * t3 * t4 + 2 * t7 * t3P * t2P * k * k * (t2 * t2)
                                                                                   * t4P * t5 * t3 * t4 - 8 * t7 * t3P * t2P * k * k * t2 * t4P * t1 - 2 * t5 * t1P * t2P * k * k * t7 * t3P + 4 * t7 * t3P * t2P * k * k * t2
                                                                                   * t4P * t5 * (t1 * t1) * t4 + 3 * t4P * t6 * t6 * t22kP * (t1 * t1) * (t2 * t2) + 4 * t5 * t5 * t1P * t1 * t2P * t24P * t4P * (t3 * t3) - 36
                                                                                   * t4P * t5 * t4 * t2P * k * (t2 * t2) * t7 * t3P * t1 + 4 * t5 * t5 * t1P * t3 * t4 * t2P * k * k * t4P * (t2 * t2) + 4 * t5 * t5 * t1P * (t3
                                                                                                                                                                                                                              * t3) * t4 * t2P * k * k * t4P * t2 + 2 * t5 * t5 * t1P * (t3 * t3) * t4 * t2P * k * k * t4P * t1 + 4 * t7 * t3P * t3 * t4 * t2P * (t2 * t2)
                                                                                   * t4P * t6 + 4 * t7 * t3P * t3 * t4 * t2P * (t2 * t2) * t4P * t5 - 2 * t6 * t6 * t23kP * (t4 * t4) * (t2 * t2) * t1 - 6 * t6 * t22kP * t4
                                                                                   * (t2 * t2) * t4P * t1 - 8 * t5 * t1P * t24P * t2P * t4P * t3 * t4 + 4 * t5 * t1P * t24P * t2P * t4P * t7 * t4 + 4 * t5 * t5 * t1P
                                                                                   * t24P * t2P * t4P * t4 - 8 * t5 * t1P * t23P * t2P * t4P * (t3 * t3) * t4 - 8 * t4P * t5 * t6 * t22kP * k * t2 - 24 * t6 * t22kP
                                                                                   * k * (t2 * t2) * t4P * t7 * t1 * t3 - 6 * t6 * t22kP * k * (t2 * t2) * t4P * t7 * (t1 * t1) * (t3 * t3) + 3 * t6 * t6 * t22kP * (t1 * t1) * t23P
                                                                                   * t4P * t4 - 8 * t6 * t6 * t22kP * (t1 * t1) * t4 * k * t2 * t4P + 3 * t6 * t22kP * (t1 * t1) * t23P * t4P * t7 * t4 + 12 * t6 * t22kP
                                                                                   * k * (t2 * t2) * t7 * t3P * t1 * t3 - 6 * t6 * t22kP * k * (t2 * t2) * t4P * t5 * (t3 * t3) + 2 * t5 * t1P * t2P * k * k * t4P * t7 - 12 * t4P
                                                                                   * t3 * t4 * t6 * t2P * k * t2 * t7 * t3P - 8 * t7 * t3P * t6 * t2P * k * t23P * t4P * (t1 * t1) * t3 - 2 * t6 * t6 * t22kP * (t1 * t1) * t4
                                                                                   * k * t4P + 12 * t5 * t1P * (t3 * t3) * t2P * k * t2 * t4P - 8 * t6 * t6 * t22kP * t4 * k * t2 * t4P - 8 * t6 * t22kP * t4 * k * t2 * t4P
                                                                                   * t5 - 18 * t4P * t7 * t7 * t4 * t2P * k * (t2 * t2) * t3P - 2 * t6 * t6 * t23kP * (t2 * t2) * t1 + 3 * t4P * t6 * t6 * t22kP * (t2 * t2) - 3
                                                                                   * t4P * t6 * t22kP * (t2 * t2) + 6 * t4P * t7 * t6 * t22kP * (t2 * t2) * t1 + 8 * t5 * t1P * t1 * t6 * t2P * (t2 * t2) * t4P * t3 * t4 - 6
                                                                                   * t6 * t6 * t22kP * k * (t2 * t2) * (t1 * t1) * (t3 * t3) * t4P + 6 * t6 * t22kP * k * (t2 * t2) * t5 * t1P * t1 - 4 * t24P * t3 * t4P * t2P
                                                                                   * t7 * t3P + 4 * t7 * t3P * t6 * t2P * k * k * (t2 * t2) * t4P * t1 * t4 - 8 * t4P * t1 * t4 * t2P * (t2 * t2) * t7 * t3P - 4 * t4P * (t1
                                                                                                                                                                                                                          * t1) * t4 * t2P * (t2 * t2) * t7 * t3P - 3 * t5 * t1P * t6 * t22kP * t1 * t23P + 8 * t5 * t1P * t1 * t2P * k * t23P * t7 * t3P - 8
                                                                                   * t4P * t5 * t2P * k * t7 * t3P * t23P * t3 - 2 * t4P * t5 * t2P * k * t7 * t3P * t3 - 2 * t4P * t5 * t5 * t2P * k * t1P * (t3 * t3) - 16
                                                                                   * t6 * t22kP * t3 * k * t2 * t4P * t7 * (t1 * t1) + 8 * t7 * t3P * t3 * t4 * t2P * t24P * t4P * t5 * t1 + 6 * t6 * t22kP * k * (t2 * t2)
                                                                                   * t7 * t3P * (t1 * t1) - 6 * t6 * t22kP * k * (t2 * t2) * t4P * t7 * (t1 * t1) - 6 * t6 * t6 * t22kP * k * (t2 * t2) * t4P * (t1 * t1) - 6 * t5
                                                                                   * t1P * t6 * t22kP * t1 * t23P * t4 - 8 * t4P * t7 * t7 * t4 * t2P * k * t23P * t3P + 36 * t5 * t1P * t3 * t2P * k * (t2 * t2) * t4P - 36
                                                                                   * t5 * t1P * t3 * t2P * k * (t2 * t2) * t4P * t7 + 8 * t5 * t1P * t1 * t6 * t2P * t23P * t4P * t4 + 4 * t5 * t1P * t1 * t6 * t2P * t24P
                                                                                   * t4P * t4 + 18 * t5 * t1P * (t3 * t3) * t2P * k * (t2 * t2) * t4P + 18 * t5 * t1P * t3 * t2P * k * (t2 * t2) * t7 * t3P - 4 * t7 * t3P
                                                                                   * t3 * t4 * t2P * t24P * t4P + 2 * t7 * t3P * t3 * t2P * k * t5 * t1P + 4 * t6 * t22kP * t1 * t3 * k * t5 * t1P + 4 * t6 * t22kP * t1
                                                                                   * t3 * k * t7 * t3P + 4 * t6 * t22kP * t1 * (t3 * t3) * k * t4P + 2 * t6 * t22kP * t1 * (t3 * t3) * k * t5 * t1P + 8 * t6 * t22kP * t1 * t3
                                                                                   * k * t5 * t1P * t4 - 6 * t5 * t1P * t6 * t22kP * t1 * t23P * t3 - 4 * t7 * t3P * t1 * t2P * k * t4P * t6 + 2 * t5 * t1P * t1 * t2P
                                                                                   * k * t7 * t3P + 4 * t5 * t1P * t1 * t6 * t2P * t24P * t4P + 4 * t5 * t1P * t1 * t6 * t2P * (t2 * t2) * t4P - 6 * t6 * t6 * t22kP
                                                                                   * (t1 * t1) * (t3 * t3) * k * (t2 * t2) * t4P * t4 - 3 * t4P * t6 * t22kP * (t1 * t1) * t4 * (t2 * t2) - 16 * t7 * t3P * t6 * t2P * k * t23P
                                                                                   * t4P * t1 * t4 + 2 * t4P * t6 * t2P * k * k * t7 * t3P + 2 * t5 * t5 * t1P * (t3 * t3) * t2P * k * k * t4P + 12 * t4P * (t1 * t1) * t3
                                                                                   * t2P * k * t7 * t3P * t2 - 2 * t6 * t22kP * (t1 * t1) * (t3 * t3) * k * t4P * t5 - 3 * t6 * t22kP * t23P * t7 * t3P * (t4 * t4) + 16
                                                                                   * t5 * t1P * t1 * t3 * t2P * k * t23P * t4P - 8 * t5 * t1P * t4 * t2P * k * t23P * t4P * t7 - 8 * t5 * t1P * t4 * t2P * k * t23P
                                                                                   * t4P * t6 + 6 * t6 * t22kP * t4 * k * (t2 * t2) * t4P + 8 * (t2 * t2) * t3 * t1 * t2P * t4P * t7 * t7 * t3P - 3 * t4P * t6 * t22kP
                                                                                   * (t1 * t1) * (t2 * t2) * (t3 * t3) + 3 * t6 * t22kP * (t1 * t1) * (t2 * t2) * t4P * t5 + 4 * t6 * t22kP * t4 * k * t5 * t1P + 4 * t6 * t22kP
                                                                                   * t4 * k * t7 * t3P + 2 * t6 * t22kP * (t4 * t4) * k * t5 * t1P + 2 * t6 * t22kP * (t4 * t4) * k * t7 * t3P + 16 * t6 * t22kP * t1 * (t3 * t3)
                                                                                   * k * t4P * t2 + 12 * t6 * t22kP * t1 * (t3 * t3) * k * t4P * (t2 * t2) + 24 * t5 * t1P * t1 * t3 * t2P * k * t2 * t4P - 12 * t7 * t3P * t6
                                                                                   * t22kP * t1 * t4 * (t2 * t2) + 16 * t5 * t1P * t23P * t6 * t2P * t4P * t3 + 3 * t4P * t7 * t6 * t22kP * (t2 * t2) - 8 * t4P * t1
                                                                                   * t6 * t2P * k * t23P * t5 * t1P * (t3 * t3) + 18 * t4P * t3 * t2P * k * t7 * t3P * (t2 * t2) + 8 * t5 * t1P * t1 * t6 * t2P * (t2 * t2)
                                                                                   * t4P * t3 + 16 * t5 * t1P * t1 * t6 * t2P * t23P * t4P * t3 - 2 * t5 * t1P * t4 * t2P * k * t4P * t7 + 6 * t4P * t7 * t6 * t22kP
                                                                                   * (t1 * t1) * (t2 * t2) * t3 + 4 * t4P * t4 * t6 * t2P * (t2 * t2) * t5 * t1P * (t3 * t3) - 3 * t6 * t22kP * t3 * (t2 * t2) * t7 * t3P * (t1
                                                                                                                                                                                                                             * t1) + 6 * t6 * t6 * t22kP * t3 * (t2 * t2) * t4P * (t1 * t1) - 12 * t4P * t7 * t2P * k * t5 * t1P * t2 * (t3 * t3) * t1 + 16 * t4P * t3
                                                                                   * t4 * t2P * k * t23P * t5 * t1P + 36 * t4P * t3 * t4 * t2P * k * (t2 * t2) * t5 * t1P + 18 * t4P * (t3 * t3) * t4 * t2P * k * (t2 * t2)
                                                                                   * t5 * t1P + 4 * t6 * t22kP * t3 * k * t4P * t4 + 8 * t4P * (t1 * t1) * t3 * t2P * k * t7 * t3P * t23P + 4 * t7 * t3P * t6 * t2P * k
                                                                                   * k * (t2 * t2) * t4P * t1 * t3 + 16 * t5 * t1P * t1 * t6 * t2P * t23P * t4P * t3 * t4 - 6 * t6 * t22kP * t4 * k * (t2 * t2) * t4P * t7
                                                                                   + 4 * t4P * t5 * t3 * t4 * t2P * k * k * t7 * t3P * t1 + 4 * t6 * t22kP * t3 * k * t7 * t3P * t4 + 4 * t4P * t2P * (t2 * t2) * t5 * t3 * t7
                                                                                   * t3P + 3 * t6 * t6 * t22kP * (t3 * t3) * (t2 * t2) * t4P + 3 * t6 * t22kP * (t3 * t3) * (t2 * t2) * t4P * t5 + 3 * t6 * t22kP * (t3 * t3)
                                                                                   * (t2 * t2) * t4P * t7 + 6 * t6 * t22kP * t3 * (t2 * t2) * t4P * t7 * t4 + 6 * t6 * t22kP * t3 * (t2 * t2) * t4P * t5 * t4 - 6 * t6 * t22kP
                                                                                   * t3 * (t2 * t2) * t5 * t1P * (t4 * t4) - 2 * t4P * t2P * k * k * t7 * t3P - 3 * t6 * t22kP * t23P * t4P * t4 + 2 * t5 * t1P * (t3 * t3)
                                                                                   * t6 * t2P * k * k * t4P * t1 * (t2 * t2) - 6 * t6 * t22kP * t4 * k * (t2 * t2) * t4P * t5 + 4 * t5 * t1P * t1 * t6 * t2P * (t2 * t2) * t4P
                                                                                   * (t3 * t3) * t4 - 8 * t4P * t5 * t4 * t2P * k * t23P * t7 * t3P - 6 * t6 * t22kP * k * (t2 * t2) * t4P * t7 - 6 * t6 * t6 * t22kP * k * (t2
                                                                                                                                                                                                                             * t2) * t4P + 12 * t5 * t1P * t6 * t22kP * k * t1 * t3 * (t2 * t2) + 4 * t4P * t5 * t1 * t2P * k * k * t7 * t3P * t3 - 36 * t7 * t3P * t6
                                                                                   * t2P * k * (t2 * t2) * t4P * t1 - 24 * t7 * t3P * t6 * t2P * k * t2 * t4P * t1 * t3 - 8 * t4P * t5 * t5 * t2P * k * t1P * t23P - 6
                                                                                   * t4P * t6 * t22kP * (t2 * t2) * t3 + 2 * t5 * t1P * t6 * t2P * k * k * (t2 * t2) * t4P * t4 - 8 * t4P * (t3 * t3) * t4 * t6 * t2P * k * t23P
                                                                                   * t5 * t1P - 8 * t4P * t7 * t6 * t22kP * k * (t1 * t1) * (t3 * t3) * t4 * t2 + 8 * t5 * t1P * t1 * t3 * (t4 * t4) * t2P * k * t7 * t3P * t23P - 8
                                                                                   * t5 * t1P * t1 * (t3 * t3) * t4 * t2P * k * t4P * t7 * t23P - 8 * t7 * t7 * t3P * t2P * k * t23P * t4P * (t1 * t1) * t3 - t6 * t6 * t23kP
                                                                                   * (t2 * t2) - 12 * t5 * t1P * t6 * t22kP * t1 * (t2 * t2) * t3 * t4 - 16 * t4P * t1 * t6 * t2P * k * t23P * t5 * t1P * t3 - 8 * t4P * t1
                                                                                   * t6 * t2P * k * t23P * t5 * t1P * t4 - 4 * t4P * t5 * t5 * t2P * k * t1P * t3 * t4 - 12 * t5 * t1P * t6 * t22kP * t1 * t23P * t3
                                                                                   * t4 + 8 * t4P * t23P * t3 * t4 * t6 * t2P * t7 * t3P - 12 * t5 * t5 * t1P * t1 * t2P * k * t2 * t4P + 8 * t7 * t3P * t2P * k * t23P
                                                                                   * t4P * (t1 * t1) + 4 * t5 * t1P * t3 * t2P * k * k * t4P * t7 * t1 * (t2 * t2) - 12 * t5 * t1P * t1 * (t3 * t3) * t4 * t2P * k * t4P * t7
                                                                                   * t2 - 8 * t5 * t5 * t1P * t1 * (t3 * t3) * t2P * k * t23P * t4P - 16 * t6 * t22kP * t1 * t4 * k * t2 * t4P * t7 - 12 * t7 * t7 * t3P * t2P
                                                                                   * k * t2 * t4P * (t1 * t1) - 4 * t5 * t1P * t1 * t2P * (t2 * t2) * t4P * t4 + 16 * t4P * t7 * t3 * t2P * t23P * t5 * t1P + 8 * t4P
                                                                                   * t7 * t3 * t2P * (t2 * t2) * t5 * t1P + 2 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P * t5 * (t1 * t1) + 4 * t7 * t3P * t2P * k * k * t2 * t4P
                                                                                   * t5 * t3 * t4 + 4 * (t2 * t2) * t3 * (t1 * t1) * t2P * t4P * t7 * t7 * t3P + 16 * t23P * t3 * t1 * t2P * t4P * t7 * t7 * t3P + 36 * t4P
                                                                                   * t1 * t3 * t4 * t2P * k * (t2 * t2) * t7 * t3P + 8 * t7 * t3P * t6 * t22kP * k * (t4 * t4) * t2 * (t1 * t1) - 6 * t6 * t22kP * (t1 * t1) * k
                                                                                   * (t2 * t2) * t4P * t7 * t4 - 4 * t7 * t3P * t2P * k * k * t2 * t4P * (t1 * t1) * t3 - 4 * t7 * t3P * t2P * k * k * t2 * t4P * (t1 * t1) + 6
                                                                                   * t6 * t22kP * t1 * (t3 * t3) * k * (t2 * t2) * t5 * t1P * (t4 * t4) + 24 * t6 * t22kP * t1 * t3 * k * (t2 * t2) * t4P + 16 * t5 * t5 * t1P
                                                                                   * t23P * t2P * t4P * t3 - 4 * t4P * t7 * t3 * t4 * t2P * k * t5 * t1P + 8 * t5 * t1P * t1 * t3 * t2P * k * t23P * t7 * t3P - 18
                                                                                   * t5 * t5 * t1P * t1 * (t3 * t3) * t2P * k * (t2 * t2) * t4P + 16 * t6 * t22kP * t3 * k * t2 * t4P - 8 * t6 * t6 * t22kP * (t3 * t3) * k * t2
                                                                                   * t4P - 16 * t5 * t5 * t1P * t1 * t3 * t2P * k * t23P * t4P + 8 * t6 * t22kP * (t3 * t3) * k * t2 * t4P - 16 * t6 * t6 * t22kP * t3
                                                                                   * k * t2 * t4P - 24 * t7 * t3P * t6 * t2P * k * t2 * t4P * t1 + 4 * t4P * t2P * t24P * t5 * t3 * t7 * t3P * (t1 * t1) + 3 * t4P * t7
                                                                                   * t6 * t22kP * (t3 * t3) * t4 * (t2 * t2) * (t1 * t1) + 8 * t5 * t1P * t1 * (t3 * t3) * t2P * k * t23P * t4P - 12 * t7 * t3P * t6 * t2P
                                                                                   * k * t2 * t4P * (t1 * t1) * t4 - 8 * t7 * t3P * t6 * t2P * k * t23P * t4P * (t1 * t1) + 4 * t4P * t24P * t3 * t4 * t6 * t2P * t7 * t3P
                                                                                   + 12 * t7 * t3P * t2P * k * t2 * t4P * t4 + 6 * t6 * t22kP * t23P * t3 * t4P * t7 + 6 * t6 * t6 * t22kP * t3 * (t2 * t2) * t4P * t4 - 3
                                                                                   * t6 * t22kP * (t3 * t3) * (t2 * t2) * t4P * t4 - 6 * t6 * t22kP * (t3 * t3) * (t2 * t2) * t4P * t1 * t4 - t42kP * t5 * t5 * t2P * k * k
                                                                                   * (t2 * t2) + 4 * t4P * t5 * t24P * t4 * t2P * t7 * t3P * (t1 * t1) + 8 * t4P * t5 * t23P * t4 * t2P * t7 * t3P * (t1 * t1) - 8 * t4P
                                                                                   * t5 * t6 * t22kP * k * (t1 * t1) * (t3 * t3) * t2 - 12 * t4P * t5 * t6 * t22kP * k * t1 * (t3 * t3) * (t2 * t2) - 6 * t4P * t5 * t6 * t22kP
                                                                                   * k * (t1 * t1) * (t3 * t3) * (t2 * t2) + 4 * t4P * t3 * t2P * k * t5 * t1P + 2 * t4P * t3 * t2P * k * t7 * t3P - 2 * t4P * t7 * t7 * t3 * t4
                                                                                   * t2P * k * t3P + 4 * t42kP * t7 * t1 * t2P * k * t5 * (t3 * t3) + t4P * t7 * (t3 * t3) * t22kP * k * k * t6 * t4 - 4 * (t2 * t2) * t3 * (t1
                                                                                                                                                                                                                             * t1) * t2P * t42kP * t7 * t7 - t7 * t3P * t6 * t22kP * k * k * t2 * (t1 * t1) * t3 * (t4 * t4) - 12 * t42kP * (t3 * t3) * t2P * k * t5
                                                                                   * t2 + 12 * t42kP * t1 * (t3 * t3) * t2P * k * t2 - 12 * t42kP * (t1 * t1) * (t3 * t3) * t2P * k * t5 * t2 + 16 * t42kP * t7 * t1 * t3 * t2P
                                                                                   * k * k * t2 - 2 * t42kP * t2P * (t2 * t2) * t5 * t5 * (t3 * t3) * (t1 * t1) - 2 * t6 * t6 * t23kP * t4 * (t2 * t2) - 3 * t4P * t6 * t22kP * t23P - 2
                                                                                   * t6 * t6 * t23kP * t3 * (t2 * t2) + 2 * t6 * t6 * t23kP * k * t2 - t6 * t6 * t23kP * (t3 * t3) * (t2 * t2) + 3 * t6 * t6 * t22kP * t23P * t4P - t6
                                                                                   * t6 * t23kP * (t1 * t1) * (t2 * t2) - 2 * t6 * t6 * t22kP * k * t4P + 2 * t6 * t22kP * k * t4P - t6 * t6 * t23kP * (t4 * t4) * (t2 * t2)
                                                                                   + 0.32e2 * t42kP * t1 * t6 * t2P * k * t23P * t5 * t3 - 8 * t42kP * (t3 * t3) * t2P * k * t7 * t23P - 8 * t42kP * t7 * t1 * t3 * t2P
                                                                                   * k * k * t5 - t5 * t5 * t12kP * t2P * k * k - 2 * t24P * t42kP * t2P * (t1 * t1) + 4 * t24P * t42kP * t2P * t6 + 16 * t42kP * t5
                                                                                   * t3 * t2P * k * k * t2 * t1 + 2 * t42kP * (t1 * t1) * t6 * t2P * k * k * (t2 * t2) + 0.48e2 * t42kP * t1 * t6 * t2P * k * t2 * t7 * t3 + 2 * t4P
                                                                                   * t3 * t6 * t6 * t22kP * k * k - 2 * t7 * t3P * t6 * t22kP * k * k * t2 * t4 + 12 * t42kP * t5 * t5 * (t1 * t1) * t2P * k * t2 * t3 + 4 * t42kP
                                                                                   * t7 * t1 * t2P * k * k * (t2 * t2) + 18 * t42kP * t6 * t2P * k * (t2 * t2) * t5 * (t3 * t3) + 2 * t4P * t7 * (t1 * t1) * t3 * t22kP * k * k * t6 - 4
                                                                                   * t42kP * t5 * t2P * k * k * t2 * t7 + 2 * t4P * t3 * t4 * t6 * t6 * t22kP * k * k * (t1 * t1) * t2 - 2 * t42kP * t5 * t2P * k * k * (t2 * t2)
                                                                                   * t7 - 8 * t42kP * t2P * t23P * t5 * t6 * (t1 * t1) - 36 * t42kP * t1 * t2P * k * (t2 * t2) * t7 * (t3 * t3) - 18 * t42kP * t5 * (t1 * t1)
                                                                                   * t2P * k * (t2 * t2) + 0.72e2 * t42kP * t1 * t6 * t2P * k * (t2 * t2) * t5 * t3 - 2 * t4P * t3 * t4 * t6 * t22kP * k * k * (t1 * t1) + 2 * t4P
                                                                                   * t5 * t22kP * k * k * t2 * t6 * t1 - 4 * t42kP * t2P * t24P * t5 * (t3 * t3) * t6 * (t1 * t1) - 8 * t42kP * t2P * t23P * t5 * t7 * (t1
                                                                                                                                                                                                                        * t1) + 12 * t42kP * t5 * t2P * k * t7 * t2 - 2 * t42kP * (t3 * t3) * t6 * t2P * k * k * t7 - 4 * (t2 * t2) * t7 * t7 * t32kP * t2P * t1 * (t4
                                                                                                                                                                                                                                                                                                                                                                    * t4) + 6 * t42kP * t7 * t7 * t2P * k * t2 - 24 * t42kP * t3 * t2P * k * t7 * t2 + 2 * t42kP * t5 * t2P * k * t7 * (t3 * t3) - 4 * t23P
                                                                                   * t42kP * t2P * (t1 * t1) - t42kP * t6 * t6 * t2P * k * k + 4 * t6 * t6 * t23kP * (t1 * t1) * t3 * k * t4 - 4 * t7 * t7 * t32kP * t2P
                                                                                   * k * k * t2 * (t1 * t1) * t4 + t42kP * t5 * t5 * t2P * k - t42kP * t7 * t7 * (t1 * t1) * t2P * k * k - t5 * t1P * t22kP * k * k * t6 - 8 * t23P
                                                                                   * t3 * t42kP * t7 * t7 * t2P - t5 * t5 * t12kP * (t3 * t3) * t2P * k * k + 2 * t4P * t1 * t6 * t6 * t22kP * k * k * t4 * t2 - t7 * t3P * t6
                                                                                   * t22kP * k * k * t2 * t3 - 8 * (t2 * t2) * t1 * t6 * t6 * t2P * t42kP * t3 - 8 * t42kP * (t1 * t1) * (t3 * t3) * t2P * k * t5 * t23P + 4
                                                                                   * t42kP * t3 * t6 * t2P * k * k + t42kP * t7 * t7 * (t1 * t1) * t2P * k * (t3 * t3) - 16 * t23P * t3 * t42kP * t7 * t2P * t6 - 2 * t42kP
                                                                                   * t7 * t7 * (t1 * t1) * t3 * t2P * k * k * (t2 * t2) + 18 * t42kP * (t1 * t1) * t6 * t2P * k * (t2 * t2) * t7 - 2 * t42kP * t7 * t7 * t1 * t2P
                                                                                   * k * k * (t2 * t2) + 16 * (t2 * t2) * t3 * t42kP * t2P * t7 * t1 - 4 * t42kP * t5 * (t3 * t3) * t2P * k * k * t2 * t6 + 8 * t42kP * t6 * t2P
                                                                                   * k * t23P * t5 * (t3 * t3) + 12 * t42kP * t1 * t6 * t6 * t2P * k * t2 * (t3 * t3) - 4 * t5 * t1P * t3 * t4 * t6 * t22kP * k * k * t2 - 2 * t42kP
                                                                                   * t2P * t24P * t5 * t5 * (t1 * t1) - 4 * t5 * t5 * t12kP * t24P * t2P * (t3 * t3) * t4 + 2 * t4P * t7 * t3 * t22kP * k * k * t6 * t4 + 4
                                                                                   * (t2 * t2) * (t3 * t3) * t42kP * t2P * t7 * (t1 * t1) + 8 * t42kP * (t1 * t1) * t3 * t2P * k * t23P + 24 * t42kP * t1 * t6 * t2P
                                                                                   * k * t2 * t7 - 4 * t42kP * t2P * t24P * t5 * t5 * t1 - 16 * t42kP * t2P * t23P * t5 * (t3 * t3) * t6 * t1 - t42kP * (t3 * t3) * t6
                                                                                   * t6 * t2P * k * k + 8 * t24P * (t3 * t3) * t42kP * t2P * t7 * t1 - 2 * t24P * (t1 * t1) * t6 * t6 * t2P * t42kP * (t3 * t3) + 6 * t42kP
                                                                                   * (t1 * t1) * (t3 * t3) * t2P * k * t2 - t42kP * (t1 * t1) * t6 * t6 * t2P * k * k * (t3 * t3) * (t2 * t2) + 0.9e1 * t5 * t5 * t12kP * (t3 * t3)
                                                                                   * t2P * k * (t2 * t2) * (t4 * t4) - 8 * t42kP * t6 * t2P * k * t23P * (t3 * t3) - 24 * t42kP * t1 * t6 * t2P * k * t2 * (t3 * t3) - 4 * t42kP
                                                                                   * t1 * t6 * t6 * t2P * k * k * (t3 * t3) * t2 + 4 * t42kP * t5 * t3 * t2P * k * k * (t2 * t2) - t7 * t3P * t6 * t22kP * k * k * t2 * t3 * (t4 * t4) - 2
                                                                                   * t4P * t3 * t22kP * k * k * t6 * t2 + 0.9e1 * t42kP * t6 * t6 * t2P * k * (t2 * t2) - 4 * t42kP * t2P * (t2 * t2) * t5 * t6 * (t1 * t1)
                                                                                   + 4 * t42kP * t7 * t1 * t2P * k * t6 * (t3 * t3) + 4 * t4P * t7 * t1 * t3 * t4 * t22kP * k * k * t6 - 4 * t42kP * (t1 * t1) * t6 * t6 * t2P
                                                                                   * k * k * t2 * t3 - 2 * t7 * t7 * t32kP * t2P * k * k * (t2 * t2) * (t1 * t1) * t4 + 4 * t24P * (t3 * t3) * t42kP * t2P * t6 + 4 * t42kP * t7
                                                                                   * t1 * t2P * k * t5 - 0.32e2 * t42kP * t1 * t2P * k * t23P * t7 * t3 - 8 * t42kP * t2P * t23P * t5 * t5 * t1 - 8 * t42kP * (t3 * t3)
                                                                                   * t2P * k * t5 * t23P - t42kP * t7 * t7 * (t3 * t3) * t2P * k * k + t4P * t5 * (t1 * t1) * t22kP * k * k * t6 * t4 + 0.72e2 * t42kP
                                                                                   * t7 * t2P * k * t5 * t1 * (t2 * t2) * t3 - 8 * t42kP * (t1 * t1) * t2P * k * t23P * t7 * (t3 * t3) + 36 * t42kP * t7 * t2P * k * t5 * t1
                                                                                   * (t2 * t2) + t4P * t5 * (t1 * t1) * t22kP * k * k * t6 * (t3 * t3) - 16 * t23P * t1 * t2P * t42kP * t7 * t6 - 4 * t42kP * t2P * (t2
                                                                                                                                                                                                                     * t2) * t5 * t7 * (t1 * t1) - 2 * t42kP * t7 * (t1 * t1) * t2P * k + 8 * t6 * t6 * t23kP * t1 * t3 * k * t4 + t4P * t5 * (t3 * t3) * t4 * t22kP
                                                                                   * k * k * t6 * (t1 * t1) - 4 * t42kP * t3 * t6 * t2P * k * k * t7 * (t2 * t2) - 16 * t42kP * t2P * t23P * t5 * t3 * t6 * (t1 * t1) - 4 * (t2 * t2)
                                                                                   * (t3 * t3) * t42kP * t2P * t1 - 4 * t42kP * t3 * t2P * k * k * t2 - 2 * t42kP * t5 * t5 * t2P * k * k * (t2 * t2) * t1 - 2 * t7 * t3P * t6
                                                                                   * t22kP * k * k * t2 * t1 * t3 * (t4 * t4) + 8 * t42kP * (t1 * t1) * t6 * t2P * k * t23P * t5 * (t3 * t3) - t7 * t7 * t32kP * (t1 * t1) * t2P
                                                                                   * k * k - 8 * t42kP * t7 * t7 * t1 * t3 * t2P * k * k * t2 - t5 * t1P * t22kP * k * k * t6 * t2 - 2 * t42kP * t2P * k * t6 * (t1 * t1) - 4 * (t2
                                                                                                                                                                                                                                 * t2) * t7 * t7 * t32kP * t2P * t1 - 4 * t5 * t1P * t3 * t4 * t6 * t22kP * k * k * t1 + 8 * t42kP * t1 * t6 * t6 * t2P * k * t23P * (t3
                                                                                                                                                                                                                                                                                                                                                                      * t3) - 2 * t42kP * t7 * t7 * (t1 * t1) * t3 * t2P * k * k - 8 * t42kP * t2P * (t2 * t2) * t5 * t6 * t1 - 2 * t7 * t7 * t32kP * t2P * k * k
                                                                                   * (t2 * t2) * t4 - 4 * t42kP * (t1 * t1) * t3 * t2P * k * t5 + 2 * t4P * t3 * t4 * t6 * t6 * t22kP * k * k - 8 * t42kP * t2P * t23P * t5
                                                                                   * t5 * t3 * (t1 * t1) + 2 * t42kP * t7 * (t1 * t1) * t2P * k * t6 + 2 * t4P * t7 * t4 * t22kP * k * k * t2 * t6 * t3 - 4 * t42kP * t5 * t5 * t3
                                                                                   * t2P * k * k * t2 * (t1 * t1) + t6 * t6 * t23kP * (t1 * t1) * (t4 * t4) * k + 8 * t23P * (t3 * t3) * t42kP * t2P * t7 * (t1 * t1) + 4 * t42kP
                                                                                   * t5 * t2P * k * t6 * t1 + 4 * t42kP * t5 * (t1 * t1) * t2P * k * k * t3 - 2 * t42kP * t5 * t5 * t3 * t2P * k * k * (t2 * t2) - 4 * t5 * t5 * t12kP
                                                                                   * t3 * t4 * t2P * k * k * (t2 * t2) + 24 * t42kP * (t1 * t1) * t6 * t2P * k * t2 * t7 * t3 - 4 * t42kP * t3 * t2P * k * t6 - t5 * t5 * t12kP
                                                                                   * (t3 * t3) * (t4 * t4) * t2P * k * k * (t2 * t2) - 8 * t42kP * t2P * k * t23P * t7 + 12 * t42kP * t6 * t2P * k * t2 * t5 + 2 * t7 * t7 * t32kP
                                                                                   * t4 * t2P * k + 12 * t42kP * t7 * t7 * t2P * k * t2 * t3 - t42kP * t6 * t6 * t2P * k * k * (t2 * t2) + 12 * t42kP * t5 * t2P * k * t7 * t2
                                                                                   * (t3 * t3) + 0.9e1 * t42kP * (t1 * t1) * t2P * k * (t2 * t2) - 18 * t42kP * (t1 * t1) * t2P * k * (t2 * t2) * t7 * (t3 * t3) + 2 * t4P
                                                                                   * t7 * t3 * t22kP * k * k * t6 - 2 * t7 * t3P * t6 * t22kP * k * k * t2 * (t1 * t1) * t4 - 8 * (t2 * t2) * t3 * t42kP * t7 * t2P * t6 - 8 * t42kP
                                                                                   * t2P * t24P * t5 * t3 * t7 - 4 * (t2 * t2) * (t3 * t3) * t1 * t2P * t42kP * t7 * t7 - t5 * t1P * (t3 * t3) * (t4 * t4) * t6 * t22kP * k
                                                                                   * k * t2 - 2 * t42kP * t7 * (t1 * t1) * (t3 * t3) * t2P * k * k * t6 - 16 * t42kP * (t1 * t1) * t3 * t2P * k * t5 * t23P - 4 * t24P * (t3
                                                                                                                                                                                                                          * t3) * t1 * t2P * t42kP * t7 * t7 - t5 * t1P * t1 * t22kP * k * k * t6 + 4 * t42kP * t5 * t5 * (t1 * t1) * t2P * k * t23P * (t3 * t3)
                                                                                   + 16 * t42kP * t5 * t5 * t1 * t2P * k * t23P * t3 - 18 * t42kP * t6 * t2P * k * (t2 * t2) * (t3 * t3) - t4P * t4 * t22kP * k * k * t2
                                                                                   * t6 + 8 * t7 * t7 * t32kP * t2P * k * t23P * t4 + 8 * t42kP * t1 * (t3 * t3) * t2P * k * t23P + 12 * t7 * t7 * t32kP * t2P * k * t2
                                                                                   * t4 + 2 * t4P * t5 * t3 * t22kP * k * k * t2 * t6 * (t1 * t1) + t42kP * t2P * k * (t1 * t1) - 2 * t7 * t7 * t32kP * t2P * k * k * t2 * (t4
                                                                                                                                                                                                                            * t4) - t42kP * t5 * t5 * (t3 * t3) * t2P * k * k * (t2 * t2) + 2 * t42kP * (t3 * t3) * t6 * t2P * k * k * (t2 * t2) - 8 * (t2 * t2) * t1 * t6 * t2P
                                                                                   * t42kP * t7 + 2 * t5 * t5 * t12kP * t4 * t2P * k - 4 * t42kP * t5 * t5 * t3 * t2P * k * k * (t2 * t2) * t1 - 8 * t5 * t1P * t23P * t2P
                                                                                   * t7 * t3P - 16 * t5 * t1P * t23P * t2P * t4P * t3 - 8 * t5 * t1P * t1 * t2P * (t2 * t2) * t4P * t3 - 16 * t5 * t1P * t1 * t2P
                                                                                   * t23P * t4P * t3 - 4 * t5 * t1P * t3 * t4 * t2P * k * k * t7 * t3P - 2 * t5 * t1P * (t3 * t3) * t4 * t2P * k * k * t4P - 4 * t4P * t4
                                                                                   * t2P * (t2 * t2) * t5 * t1P * (t3 * t3) + 18 * t7 * t3P * t3 * (t4 * t4) * t2P * k * t5 * t1P * (t2 * t2) - 4 * t7 * t3P * t2P * k * k
                                                                                   * (t2 * t2) * t4P * t1 * t3 - 4 * t7 * t3P * t2P * k * k * (t2 * t2) * t5 * t1P * t3 * t4 - 16 * t5 * t1P * t1 * t2P * t23P * t4P * t3
                                                                                   * t4 + 8 * t5 * t1P * t1 * t2P * t23P * t4P * t7 * t4 - 8 * t5 * t1P * t3 * t4 * t2P * k * k * t4P * t1 * t2 - 2 * t5 * t1P * (t3 * t3)
                                                                                   * t4 * t2P * k * k * t4P * t1 - 4 * t5 * t1P * (t3 * t3) * t4 * t2P * k * k * t4P * t2 - 4 * t5 * t1P * t3 * t4 * t2P * k * k * t7 * t3P * t1 - 2
                                                                                   * t5 * t1P * t3 * (t4 * t4) * t2P * k * k * t7 * t3P + 8 * t5 * t1P * (t4 * t4) * t2P * k * t23P * t7 * t3P + 6 * t6 * t22kP * t3 * (t2
                                                                                                                                                                                                                        * t2) * t4P * t7 * (t1 * t1) * t4 + 3 * t6 * t22kP * (t3 * t3) * (t2 * t2) * t4P * t7 * (t1 * t1) + 12 * t6 * t22kP * t3 * (t2 * t2) * t4P
                                                                                   * t7 * t1 + 12 * t6 * t6 * t22kP * t3 * (t2 * t2) * t4P * t1 - 6 * t7 * t3P * t6 * t22kP * (t4 * t4) * (t2 * t2) * t1 - 6 * t7 * t3P * t6 * t22kP
                                                                                   * (t4 * t4) * t23P * t1 + 8 * t5 * t5 * t1P * t24P * t2P * t4P * t3 * t4 - 4 * t5 * t1P * t24P * t2P * t4P + 4 * t5 * t1P * t24P
                                                                                   * t2P * t4P * t7 + 4 * t5 * t5 * t1P * t24P * t2P * t4P - 4 * t5 * t1P * t1 * t2P * t24P * t7 * t3P * (t4 * t4) + 2 * t4P
                                                                                   * t5 * (t1 * t1) * t2P * k * k * t7 * t3P * t3 - 8 * t5 * t1P * t1 * t2P * t24P * t4P * t3 * t4 + 4 * t5 * t5 * t1P * (t3 * t3) * t2P
                                                                                   * k * k * t4P * t1 * t2 + 8 * t5 * t5 * t1P * t3 * t2P * k * k * t4P * t1 * t2 + 4 * t5 * t1P * (t3 * t3) * t2P * k * k * t4P * t7 * t1 * t2 + 12
                                                                                   * t5 * t1P * t1 * t3 * (t4 * t4) * t2P * k * t7 * t3P * t2 + 2 * t5 * t5 * t1P * (t3 * t3) * t2P * k * k * t4P * t1 * (t2 * t2) + 6 * t6 * t22kP
                                                                                   * t1 * (t3 * t3) * (t2 * t2) * t4P * t5 + 6 * t6 * t22kP * (t1 * t1) * t3 * (t2 * t2) * t4P * t5 + 8 * t7 * t3P * t6 * t2P * k * k * t2 * t4P
                                                                                   * t1 + 2 * t7 * t7 * t3P * t3 * t2P * k * k * t4P + 2 * t7 * t3P * t3 * t2P * k * k * t4P * t6 - 6 * t6 * t22kP * t23P * (t3 * t3) * t5
                                                                                   * t1P * t4 - 18 * t4P * t6 * t2P * k * (t2 * t2) * t7 * t3P + 2 * t7 * t7 * t3P * t3 * t4 * t2P * k * k * t4P + 8 * t4P * t6 * t22kP
                                                                                   * k * t2 + 2 * t5 * t1P * (t3 * t3) * t2P * k * k * t4P * t7 + 2 * t4P * t1 * (t3 * t3) * t2P * k * t5 * t1P + 4 * t5 * t5 * t1P * t3 * t2P
                                                                                   * k * k * t4P * t1 - 36 * t7 * t3P * t6 * t2P * k * (t2 * t2) * t4P * t1 * t3 + 2 * t6 * t22kP * (t3 * t3) * k * t5 * t1P * (t4 * t4) - 24 * t4P
                                                                                   * t5 * t1 * t2P * k * t2 * t7 * t3P - 12 * t4P * t5 * (t1 * t1) * t2P * k * t2 * t7 * t3P - 2 * t5 * t1P * t2P * k * k * t4P * (t2 * t2) - 8
                                                                                   * t4P * t6 * t2P * k * t23P * t7 * t3P * t4 - 18 * t4P * t6 * t2P * k * (t2 * t2) * t5 * t1P - 12 * t4P * t6 * t2P * k * t2 * t7 * t3P
                                                                                   + 8 * t5 * t5 * t1P * t3 * t2P * k * k * t4P * t2 + 8 * t4P * t3 * t4 * t2P * k * t23P * t7 * t3P + 6 * t6 * t22kP * t23P * (t3 * t3)
                                                                                   * t4P * t7 * t1 + 6 * t6 * t6 * t22kP * t23P * (t3 * t3) * t4P * t1 - 6 * t7 * t3P * t6 * t22kP * (t1 * t1) * t4 * t23P + 6 * t6 * t22kP
                                                                                   * k * (t2 * t2) * t7 * t3P * t3 + 4 * t4P * t1 * t6 * t2P * k * k * t7 * t3P - 24 * t6 * t6 * t22kP * t1 * t3 * k * (t2 * t2) * t4P * t4 + 8 * t24P
                                                                                   * t3 * t1 * t2P * t4P * t7 * t7 * t3P - 8 * (t2 * t2) * t7 * t3P * t2P * t4P * t1 - 2 * t6 * t22kP * (t1 * t1) * (t3 * t3) * k * t4P
                                                                                   * t5 * t4 + 24 * t7 * t3P * t2P * k * t2 * t4P * t1 + 36 * t5 * t1P * t1 * t3 * t2P * k * (t2 * t2) * t4P - 2 * t7 * t3P * t1 * t2P * k
                                                                                   * k * t5 * t1P - 36 * t7 * t3P * t3 * t4 * t6 * t2P * k * t4P * t1 * (t2 * t2) - 16 * t7 * t3P * t3 * t4 * t6 * t2P * k * t4P * t1 * t23P
                                                                                   + 6 * t6 * t22kP * t1 * t23P * t4P * t7 + 6 * t6 * t6 * t22kP * t1 * t23P * t4P - 18 * t7 * t7 * t3P * t2P * k * (t2 * t2) * t4P
                                                                                   + 4 * (t2 * t2) * t7 * t7 * t3P * t2P * t4P * (t1 * t1) - 6 * t6 * t6 * t22kP * (t1 * t1) * k * (t2 * t2) * t4P * t4 + 4 * t7 * t3P * t6 * t2P
                                                                                   * k * k * t2 * t4P * (t1 * t1) * t3 + 4 * t7 * t3P * t6 * t2P * k * k * t2 * t4P * (t1 * t1) + 3 * t4P * t5 * t6 * t22kP * (t3 * t3) * t4 * t23P - 12
                                                                                   * t6 * t22kP * t3 * k * (t2 * t2) * t4P * t5 * t4 + 6 * t6 * t22kP * (t3 * t3) * k * (t2 * t2) * t4P * t4 + 8 * t5 * t5 * t1P * t3 * t4 * t2P
                                                                                   * k * k * t4P * t2 + 2 * t5 * t5 * t1P * (t3 * t3) * t4 * t2P * k * k * t4P + 4 * t5 * t5 * t1P * t3 * t4 * t2P * k * k * t4P * t1 + 2 * t5 * t1P
                                                                                   * (t3 * t3) * t4 * t2P * k * k * t4P * t7 + 4 * t4P * t7 * (t3 * t3) * t2P * t24P * t5 * t1P + 8 * t4P * t7 * t3 * t2P * t24P * t5
                                                                                   * t1P * t4 - 24 * t5 * t5 * t1P * t1 * t3 * t4 * t2P * k * t4P * t2 - 2 * t5 * t5 * t1P * t1 * (t3 * t3) * t4 * t2P * k * t4P + 2 * t4P
                                                                                   * (t3 * t3) * t2P * k * t5 * t1P + 4 * t4P * t3 * t2P * k * t5 * t1P * t4 + 12 * t4P * t3 * t2P * k * t7 * t3P * t2 + 2 * t4P * t3 * t2P
                                                                                   * k * t7 * t3P * t4 - 18 * t7 * t7 * t3P * t3 * t4 * t2P * k * t4P * (t1 * t1) * (t2 * t2) - 8 * t7 * t7 * t3P * t2P * k * t23P * t4P
                                                                                   * t3 - 3 * t6 * t22kP * (t3 * t3) * (t2 * t2) * t5 * t1P * (t4 * t4) - 6 * t6 * t22kP * t3 * (t2 * t2) * t4P * t4 - 4 * (t2 * t2) * t7 * t3P
                                                                                   * t2P * t4P - 6 * t6 * t6 * t22kP * (t3 * t3) * k * (t2 * t2) * t4P * t4 - 8 * t5 * t5 * t1P * (t3 * t3) * t2P * k * t23P * t4P - 16
                                                                                   * t6 * t22kP * (t3 * t3) * k * t2 * t4P * t7 * t1 - 6 * t6 * t22kP * t23P * t5 * t1P * t4 - 6 * t6 * t22kP * t23P * t7 * t3P * t4 - 8
                                                                                   * t4P * t5 * t2P * k * t7 * t3P * t23P - 4 * t4P * t5 * t5 * t2P * k * t1P * t3 * t1 - 8 * t5 * t1P * t3 * t2P * k * k * t4P * t1
                                                                                   * t2 + 2 * t4P * t5 * t3 * t4 * t2P * k * k * t7 * t3P * (t1 * t1) + 8 * t7 * t3P * t6 * t22kP * k * (t1 * t1) * t3 * (t4 * t4) * t2 - 16 * t4P
                                                                                   * t7 * t6 * t22kP * k * t3 * t4 * t2 - 8 * t4P * t7 * t6 * t22kP * k * (t3 * t3) * t4 * t2 + 8 * t6 * t22kP * (t1 * t1) * t4 * k * t2 * t4P + 18
                                                                                   * t5 * t1P * t1 * t4 * t2P * k * (t2 * t2) * t4P - 4 * t7 * t3P * t1 * t4 * t2P * k * t4P * t6 - 4 * t7 * t3P * t1 * t4 * t2P * k * t4P
                                                                                   * t5 - 4 * t7 * t7 * t3P * t1 * t4 * t2P * k * t4P + 8 * t4P * t2P * t23P * t5 * t3 * t7 * t3P - 8 * t4P * (t1 * t1) * t4 * t2P * t23P
                                                                                   * t7 * t3P + 4 * t5 * t1P * t1 * t2P * t24P * t4P * t7 + 4 * t5 * t5 * t1P * t1 * t2P * t24P * t4P + 16 * t5 * t5 * t1P * t1 * t2P
                                                                                   * t23P * t4P * t3 - 4 * t5 * t1P * t1 * t2P * (t2 * t2) * t7 * t3P - 3 * t6 * t22kP * t23P * (t3 * t3) * t4P - 24 * t7 * t3P * t3
                                                                                   * t4 * t6 * t2P * k * t4P * t1 * t2 - 8 * t5 * t1P * (t3 * t3) * t2P * k * t23P * t4P * t6 - 2 * t5 * t1P * t2P * k * k * t4P + 8 * t5
                                                                                   * t1P * t23P * t6 * t2P * t4P + 4 * t5 * t1P * t24P * t6 * t2P * t4P * (t3 * t3) + 18 * t7 * t3P * t1 * (t4 * t4) * t2P * k
                                                                                   * t5 * t1P * (t2 * t2) - 4 * t7 * t3P * t1 * t2P * k * k * t5 * t1P * t4 - 8 * t4P * t7 * t2P * k * t5 * t1P * t23P * (t3 * t3) * t4 - 18
                                                                                   * t4P * t7 * t2P * k * t5 * t1P * (t3 * t3) * t4 * (t2 * t2) + 4 * (t2 * t2) * t7 * t3P * t2P * t4P * t6 * (t1 * t1) - 8 * t6 * t6 * t22kP
                                                                                   * k * t2 * t4P * (t1 * t1) + 8 * t6 * t22kP * k * t2 * t7 * t3P * (t1 * t1) + 4 * t5 * t1P * t1 * t3 * t4 * t2P * k * t7 * t3P + 2 * t5 * t1P
                                                                                   * t1 * (t3 * t3) * t4 * t2P * k * t4P + 4 * (t2 * t2) * t7 * t3P * t2P * t4P * t5 * (t1 * t1) + 16 * t6 * t22kP * (t1 * t1) * t3 * k * t4P
                                                                                   * t2 + 8 * t6 * t22kP * t1 * (t3 * t3) * (t4 * t4) * k * t2 * t5 * t1P - 4 * t7 * t3P * t4 * t2P * k * k * t5 * t1P - 2 * t7 * t3P * (t4 * t4)
                                                                                   * t2P * k * k * t5 * t1P + 6 * t6 * t22kP * (t1 * t1) * k * (t2 * t2) * t7 * t3P * (t4 * t4) + 4 * (t2 * t2) * t7 * t3P * t2P * t4P * t6
                                                                                   * t3 + 4 * t6 * t6 * t23kP * k * (t1 * t1) * t2 * t3 + 8 * t6 * t6 * t23kP * k * (t1 * t1) * t2 * t3 * t4 - 4 * t5 * t1P * t3 * t4 * t2P * k * k * t4P
                                                                                   * (t2 * t2) - 4 * t5 * t1P * t3 * t4 * t2P * k * k * t4P + 4 * t5 * t1P * t3 * t4 * t2P * k * k * t4P * t7 * t1 * (t2 * t2) + 2 * t5 * t5 * t1P
                                                                                   * (t3 * t3) * t4 * t2P * k * k * t4P * (t2 * t2) + 8 * (t2 * t2) * t7 * t3P * t2P * t4P * t6 * t1 + 2 * t4P * t7 * t1 * t4 * t2P * k * k * (t2
                                                                                                                                                                                                                               * t2) * t5 * t1P + 4 * t4P * t7 * t1 * t4 * t2P * k * k * t2 * t5 * t1P + 6 * t6 * t22kP * (t3 * t3) * t4 * (t2 * t2) * t4P * t5 * t1 + 8 * t6
                                                                                   * t22kP * (t3 * t3) * k * t4P * t2 * t4 - 18 * t5 * t1P * (t3 * t3) * t2P * k * (t2 * t2) * t4P * t7 - 6 * t6 * t22kP * t3 * (t4 * t4) * (t2
                                                                                                                                                                                                                             * t2) * t5 * t1P * t1 + 8 * t5 * t1P * t3 * t4 * t2P * k * k * t4P * t7 * t1 * t2 - 8 * t7 * t3P * t23P * t3 * t2P * t4P * (t1 * t1) * t4 - 12
                                                                                   * t6 * t22kP * t23P * t3 * t5 * t1P * t4 + 4 * t7 * t3P * t2P * k * k * t2 * t4P * t5 + 4 * t4P * t7 * (t3 * t3) * t2P * t24P * t5
                                                                                   * t1P * t4 - 36 * t4P * t7 * t2P * k * t5 * t1P * t3 * t4 * (t2 * t2) - 16 * t4P * t7 * t2P * k * t5 * t1P * t23P * t3 * t4 - 4 * t7 * t3P
                                                                                   * t3 * t2P * (t2 * t2) * t5 * t1P * (t4 * t4) - 3 * t6 * t22kP * t23P * (t3 * t3) * t5 * t1P + 8 * (t2 * t2) * t7 * t3P * t2P * t4P
                                                                                   * t5 * t1 + 16 * t7 * t3P * t2P * k * t23P * t4P * t1 - 18 * t7 * t3P * t6 * t2P * k * (t2 * t2) * t4P * (t1 * t1) * t4 + 2 * t6 * t6 * t23kP
                                                                                   * k * (t1 * t1) * (t4 * t4) * t2 - 4 * t6 * t22kP * t1 * k * t4P * t7 - 4 * t6 * t6 * t22kP * t1 * k * t4P - 4 * t6 * t22kP * t1 * k * t4P * t5 - 8
                                                                                   * t5 * t1P * t23P * t2P * t4P - 8 * t5 * t1P * t23P * t2P * t7 * t3P * t3 - 4 * t5 * t1P * t24P * t2P * t4P * (t3 * t3) - 2
                                                                                   * t4P * (t1 * t1) * t2P * k * k * t7 * t3P * t4 - 8 * t7 * t7 * t3P * t2P * k * t23P * t4P * (t1 * t1) - 36 * t7 * t3P * t6 * t2P
                                                                                   * k * (t2 * t2) * t4P * t1 * t4 - 3 * t7 * t3P * t6 * t22kP * (t2 * t2) + 4 * t7 * t3P * t6 * t2P * k * k * t2 * t4P + 4 * t7 * t3P * t6 * t2P
                                                                                   * k * k * t2 * t4P * (t1 * t1) * t4 - 12 * t4P * t5 * (t1 * t1) * t2P * k * t2 * t7 * t3P * t3 - 16 * t5 * t1P * t3 * t2P * k * t23P * t4P
                                                                                   * t7 + 24 * t7 * t3P * t1 * t4 * t2P * k * t5 * t1P * t2 + 4 * t7 * t3P * t1 * t4 * t2P * k * t5 * t1P + 8 * t6 * t22kP * t4 * k * t2 * t4P - 6
                                                                                   * t6 * t22kP * t23P * (t3 * t3) * t4P * t1 + 24 * t5 * t1P * t4 * t2P * k * t2 * t7 * t3P - 18 * t5 * t1P * t4 * t2P * k * (t2 * t2)
                                                                                   * t4P * t6 - 16 * t5 * t1P * t3 * t2P * k * t23P * t4P * t6 - 16 * t5 * t5 * t1P * t3 * t2P * k * t23P * t4P - 16 * t4P * t5 * t4
                                                                                   * t2P * k * t23P * t7 * t3P * t1 + 2 * t6 * t22kP * (t1 * t1) * (t3 * t3) * k * t4P * t4 - 2 * t6 * t6 * t22kP * (t1 * t1) * (t3 * t3) * k
                                                                                   * t4P * t4 + 8 * t6 * t22kP * t1 * t3 * k * t4P * t4 - 12 * t5 * t1P * t4 * t2P * k * t2 * t4P * t7 - 24 * t4P * t5 * t4 * t2P * k * t2
                                                                                   * t7 * t3P * t1 - 6 * t6 * t22kP * t23P * t3 * t7 * t3P * t1 + 12 * t7 * t3P * (t1 * t1) * t4 * t2P * k * t4P * t2 + 2 * t7 * t3P * (t1
                                                                                                                                                                                                                        * t1) * t4 * t2P * k * t4P * t3 - 2 * t7 * t3P * t2P * k * k * (t2 * t2) * t5 * t1P * t3 * (t4 * t4) + 2 * t7 * t7 * t3P * t2P * k * k * (t2
                                                                                                                                                                                                                                                                                                                                                                  * t2) * t4P * t3 * t4 + 6 * t6 * t6 * t22kP * t23P * t3 * t4P * (t1 * t1) * t4 + 8 * t5 * t5 * t1P * t23P * t2P * t4P * t4 - 16 * t5
                                                                                   * t1P * t23P * t2P * t4P * t3 * t4 - 8 * t5 * t1P * t23P * t2P * t7 * t3P * (t4 * t4) - 4 * t5 * t1P * t24P * t2P * t4P
                                                                                   * t4 + 3 * t6 * t22kP * (t3 * t3) * t4 * (t2 * t2) * t4P * t5 * (t1 * t1) + 2 * t7 * t3P * t1 * (t4 * t4) * t2P * k * t5 * t1P - 24 * t4P
                                                                                   * t5 * t5 * t4 * t2P * k * t1P * t2 * t3 + 4 * t4P * t7 * (t3 * t3) * t2P * (t2 * t2) * t5 * t1P * t4 * t1 + 8 * t4P * t7 * (t3 * t3) * t2P
                                                                                   * t23P * t5 * t1P * t4 * t1 - 12 * t7 * t7 * t3P * t3 * t4 * t2P * k * t4P * (t1 * t1) * t2 + 4 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P
                                                                                   * t5 * t1 * t3 * t4 + 4 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P * t5 * t1 * t4 + 16 * t5 * t1P * t1 * t3 * t4 * t2P * k * t7 * t3P * t23P - 16
                                                                                   * t5 * t1P * t1 * t3 * t4 * t2P * k * t4P * t7 * t23P - 16 * t5 * t5 * t1P * t1 * t3 * t4 * t2P * k * t4P * t23P - 8 * t5 * t1P * t24P
                                                                                   * t2P * t7 * t3P * t4 - 6 * t6 * t22kP * t23P * t3 * t7 * t3P * t4 - 0.32e2 * t4P * t5 * t6 * t22kP * k * t1 * t3 * t4 * t2 - 16 * t4P
                                                                                   * t5 * t6 * t22kP * k * t1 * (t3 * t3) * t4 * t2 - 8 * t4P * t5 * t6 * t22kP * k * (t1 * t1) * (t3 * t3) * t4 * t2 + 2 * t7 * t3P * t2P * k * k
                                                                                   * (t2 * t2) * t4P * t5 * t3 + 8 * t7 * t3P * t2P * k * k * t2 * t4P * t5 * t1 * t3 + 4 * t7 * t7 * t3P * t2P * k * k * t2 * t4P * t4 - 8 * t7
                                                                                   * t3P * t24P * t3 * t2P * t4P * t1 * t4 + 4 * t7 * t3P * t1 * t4 * t2P * k * t4P * t3 + 16 * t7 * t3P * t3 * t4 * t2P * t23P * t4P
                                                                                   * t5 * t1 - 3 * t6 * t22kP * (t1 * t1) * t23P * t7 * t3P - 2 * t5 * t1P * (t3 * t3) * t4 * t2P * k * k * t4P * t1 * (t2 * t2) - 18 * t4P
                                                                                   * t5 * t5 * t4 * t2P * k * t1P * (t3 * t3) * (t2 * t2) - 2 * t7 * t3P * t1 * t2P * k * k * t5 * t1P * (t4 * t4) - 16 * t7 * t7 * t3P * t2P
                                                                                   * k * t23P * t4P * t1 * t3 - 18 * t5 * t5 * t1P * (t3 * t3) * t2P * k * (t2 * t2) * t4P + 8 * t4P * t7 * t3 * t2P * (t2 * t2) * t5 * t1P
                                                                                   * t4 * t1 + 0.32e2 * t6 * t22kP * t3 * k * t5 * t1P * t2 * t4 - 4 * t5 * t1P * (t3 * t3) * t4 * t2P * k * k * t4P * t1 * t2 - 2 * t5 * t1P * t3
                                                                                   * (t4 * t4) * t2P * k * k * t7 * t3P * t1 - 4 * t5 * t1P * t24P * t2P * t7 * t3P * t3 - 2 * t7 * t3P * t2P * k * k * (t2 * t2) * t5 * t1P
                                                                                   + 8 * t5 * t5 * t1P * t23P * t2P * t4P * (t3 * t3) * t4 - 8 * t5 * t1P * t23P * t2P * t7 * t3P * (t4 * t4) * t3 - 4 * t7 * t3P * t2P
                                                                                   * k * k * t2 * t5 * t1P - 4 * t7 * t3P * t2P * k * k * t2 * t4P * t3 - 4 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P * t1 - 4 * t7 * t3P * (t4
                                                                                                                                                                                                                              * t4) * t2P * (t2 * t2) * t5 * t1P + 12 * t6 * t22kP * (t1 * t1) * t3 * k * (t2 * t2) * t4P * t4 - 4 * t5 * t1P * t1 * t3 * t4 * t2P * k * t4P
                                                                                   * t7 - 4 * t5 * t5 * t1P * t1 * t3 * t4 * t2P * k * t4P + 16 * t7 * t3P * t3 * t4 * t2P * k * t5 * t1P * t23P - 8 * t4P * t5 * t6 * t22kP
                                                                                   * k * (t1 * t1) * t4 * t2 - 16 * t4P * t5 * t6 * t22kP * k * (t1 * t1) * t4 * t2 * t3 + 4 * t5 * t1P * (t3 * t3) * t4 * t2P * k * k * t4P * t7 * t2 - 2
                                                                                   * t6 * t6 * t23kP * (t3 * t3) * t4 * (t2 * t2) - 4 * t6 * t6 * t23kP * (t3 * t3) * t4 * (t2 * t2) * t1 + 2 * t5 * t1P * t1 * t2P * k * k * t4P
                                                                                   * t7 * t4 + 2 * t4P * t5 * t5 * t2P * k * k * (t2 * t2) * t1P + 6 * t6 * t22kP * t23P * t3 * t4P * t7 * (t1 * t1) * t4 - t6 * t6 * t23kP
                                                                                   * (t3 * t3) * (t4 * t4) * (t2 * t2) - 2 * t6 * t6 * t23kP * (t3 * t3) * (t4 * t4) * (t2 * t2) * t1 + 8 * t5 * t1P * (t3 * t3) * t2P * k * t23P
                                                                                   * t4P - 12 * t4P * t6 * t2P * k * t2 * t7 * t3P * t3 + 3 * t4P * t7 * t6 * t22kP * t4 * (t2 * t2) - 2 * t6 * t6 * t22kP * (t3 * t3) * k
                                                                                   * t4P + 4 * t6 * t22kP * (t3 * t3) * k * t5 * t1P * t4 + 3 * t6 * t22kP * t23P * (t3 * t3) * t4P * t7 * (t1 * t1) + 8 * t5 * t1P * t1
                                                                                   * t6 * t2P * t23P * t4P - 2 * t4P * t5 * t5 * t2P * k * t1P * t4 * t1 - 4 * t4P * t5 * t2P * k * t7 * t3P * t1 * t3 + 4 * t7 * t3P
                                                                                   * t3 * t4 * t2P * t24P * t4P * t5 - 4 * t7 * t3P * t6 * t22kP * k * k * t2 * t1 * t3 * t4 + 12 * t5 * t5 * t12kP * t3 * t2P * k * t2 - 4 * t6
                                                                                   * t6 * t22kP * t1 * t4 * k * t4P + 36 * t7 * t3P * t1 * t4 * t2P * k * t5 * t1P * (t2 * t2) - 6 * t7 * t3P * t6 * t22kP * (t2 * t2) * t4 - 2
                                                                                   * t5 * t1P * t3 * t2P * k * k * t7 * t3P * t1 + 12 * t5 * t1P * t1 * t3 * t2P * k * t2 * t7 * t3P - 12 * t4P * t5 * t5 * t4 * t2P * k * t2
                                                                                   * t1P + 2 * t4P * (t1 * t1) * t6 * t2P * k * k * t7 * t3P * t4 + 3 * t4P * t5 * t6 * t22kP * t23P + 4 * t5 * t1P * t3 * t2P * k * k
                                                                                   * t4P * t7 * t1 + 8 * t7 * t3P * t6 * t2P * k * k * t2 * t4P * t1 * t3 + 2 * t7 * t3P * t6 * t2P * k * k * (t2 * t2) * t4P * t3 + 2 * t7 * t3P
                                                                                   * t6 * t2P * k * k * (t2 * t2) * t4P * (t1 * t1) * t4 - 3 * t5 * t1P * t6 * t22kP * t1 * (t2 * t2) * (t4 * t4) + 6 * t4P * t5 * t6 * t22kP
                                                                                   * t23P * t1 - 6 * t4P * t6 * t22kP * (t1 * t1) * t4 * (t2 * t2) * t3 - 8 * t5 * t1P * t3 * t2P * k * k * t4P * t2 - 4 * t5 * t1P * t3 * t2P
                                                                                   * k * k * t4P * t1 + 3 * t4P * t5 * t6 * t22kP * (t2 * t2) - 2 * t7 * t3P * (t1 * t1) * t4 * t2P * k * t4P * t5 * t3 - 4 * t6 * t22kP * (t1
                                                                                                                                                                                                                            * t1) * t3 * k * t4P * t7 * t4 - 4 * t6 * t6 * t22kP * (t1 * t1) * t3 * k * t4P * t4 - 4 * t6 * t22kP * t1 * (t3 * t3) * k * t4P * t5 * t4 - 4 * t6
                                                                                   * t6 * t22kP * t1 * (t3 * t3) * k * t4P * t4 - 4 * t6 * t22kP * (t1 * t1) * t3 * k * t4P * t5 * t4 - 8 * t4P * t5 * t5 * t4 * t2P * k * t23P
                                                                                   * t1P * t1 - 8 * t4P * t5 * (t1 * t1) * t2P * k * t23P * t7 * t3P + 8 * t5 * t1P * t3 * t2P * k * k * t4P * t7 * t2 - 4 * t42kP * t6
                                                                                   * t2P * k * k * t7 * t2 + 8 * t4P * (t1 * t1) * t4 * t6 * t2P * t23P * t7 * t3P + 4 * t4P * t1 * t3 * t2P * k * t5 * t1P + 4 * t4P * t1
                                                                                   * t3 * t2P * k * t7 * t3P + 12 * t5 * t1P * t1 * (t3 * t3) * t2P * k * t2 * t4P + 8 * (t2 * t2) * t7 * t3P * t2P * t4P * t6 * t1 * t3
                                                                                   + 4 * t4P * t2P * (t2 * t2) * t5 * t3 * t7 * t3P * (t1 * t1) - 6 * t5 * t1P * t6 * t22kP * t1 * (t2 * t2) * t4 + 16 * t4P * t1 * t3 * t2P
                                                                                   * k * t7 * t3P * t23P + 2 * t6 * t22kP * (t3 * t3) * k * t4P + 2 * t6 * t22kP * (t3 * t3) * k * t5 * t1P + 8 * t6 * t22kP * t3 * k * t5
                                                                                   * t1P * t4 - 2 * t7 * t3P * t3 * t2P * k * k * t4P + 8 * t5 * t1P * t24P * t6 * t2P * t4P * t3 * t4 + 8 * t5 * t1P * t23P * t6 * t2P
                                                                                   * t4P * (t3 * t3) * t4 + 2 * t5 * t1P * (t3 * t3) * t4 * t6 * t2P * k * k * t4P * t1 * (t2 * t2) - 8 * t6 * t22kP * t4 * k * t2 * t4P * t7 - 12
                                                                                   * t6 * t22kP * t3 * (t2 * t2) * t4P * t1 * t4 + 4 * (t2 * t2) * t7 * t3P * t2P * t4P * t6 * (t1 * t1) * t3 + 8 * t7 * t3P * t2P * k * t23P
                                                                                   * t4P + 4 * t4P * t6 * t2P * (t2 * t2) * t5 * t1P + 8 * t4P * t6 * t2P * (t2 * t2) * t5 * t1P * t3 + 2 * t4P * t5 * (t1 * t1) * t2P
                                                                                   * k * k * t7 * t3P + 4 * t4P * t5 * t1 * t2P * k * k * t7 * t3P * t4 - 2 * t6 * t6 * t23kP * (t1 * t1) * t4 * (t2 * t2) + 2 * t7 * t3P * t3 * t2P
                                                                                   * k * t5 * t1P * (t4 * t4) + 2 * t42kP * t5 * (t1 * t1) * t2P * k * k * (t3 * t3) - 8 * t42kP * t2P * (t2 * t2) * t5 * t5 * t3 * t1 - 2 * t42kP
                                                                                   * t5 * t5 * (t1 * t1) * t2P * k * k * t3 + 2 * t42kP * t7 * (t1 * t1) * t2P * k * k * (t2 * t2) + 2 * t4P * t3 * t4 * t6 * t6 * t22kP * k * k * (t1
                                                                                                                                                                                                                                    * t1) - 2 * t42kP * (t1 * t1) * (t3 * t3) * t2P * k * t6 + 18 * t42kP * (t1 * t1) * t6 * t2P * k * (t2 * t2) * t7 * (t3 * t3) - 2 * t5 * t5 * t12kP
                                                                                   * t3 * (t4 * t4) * t2P * k * k * (t2 * t2) - t42kP * t5 * t5 * t2P * k * k * (t3 * t3) + t4P * t7 * (t3 * t3) * t22kP * k * k * t6 * t2 - 4 * t24P
                                                                                   * t7 * t7 * t32kP * t2P * t4 - 4 * t42kP * t5 * (t3 * t3) * t2P * k * k * t2 * t7 * (t1 * t1) - 24 * t42kP * (t1 * t1) * t2P * k * t2 * t7
                                                                                   * t3 + 24 * t42kP * t7 * t2P * k * t5 * (t1 * t1) * t2 * t3 - 2 * t5 * t5 * t12kP * t2P * (t2 * t2) + 6 * t42kP * t2P * k * t2 - 8 * t23P
                                                                                   * t3 * t42kP * t2P - t42kP * t7 * t7 * t2P * k * k - 36 * t42kP * t3 * t2P * k * t7 * (t2 * t2) - 16 * (t2 * t2) * t3 * t1 * t2P * t42kP
                                                                                   * t7 * t6 - 2 * t4P * (t3 * t3) * t4 * t6 * t22kP * k * k * t1 * t2 + 12 * t42kP * t1 * t6 * t6 * t2P * k * t2 + 2 * t4P * t5 * t1 * t22kP * k
                                                                                   * k * t6 * (t3 * t3) - 2 * t7 * t3P * t3 * t22kP * k * k * t6 * t1 + 2 * t4P * t3 * t6 * t6 * t22kP * k * k * t2 + 4 * t6 * t6 * t23kP * t1 * t3 * k - 4
                                                                                   * t42kP * t5 * t2P * k * k * t6 * t3 - 2 * t42kP * t7 * (t1 * t1) * (t3 * t3) * t2P * k * k * t5 - 2 * (t2 * t2) * (t3 * t3) * t42kP * t2P
                                                                                   * (t1 * t1) + 8 * t23P * t42kP * t2P * t7 - 4 * t42kP * t3 * t2P * k * t7 - 4 * t42kP * t3 * t6 * t6 * t2P * k * k * t2 - t42kP * (t3
                                                                                                                                                                                                                      * t3) * t6 * t6 * t2P * k * k * (t2 * t2) - 36 * t42kP * t1 * t2P * k * (t2 * t2) * t7 - 2 * t6 * t2P * t1P * k * t1 * t4P * t5 - 4 * t23P
                                                                                   * (t1 * t1) * t2P * t42kP * t7 * t7 + t4P * t7 * t22kP * k * k * t2 * t6 + 6 * t5 * t5 * t12kP * (t3 * t3) * t2P * k * t2 + 16 * t5 * t5 * t12kP
                                                                                   * t3 * t2P * k * t23P * t4 - 24 * t6 * t22kP * t1 * t3 * k * (t2 * t2) * t4P * t5 * t4 + 12 * t6 * t22kP * t1 * (t3 * t3) * k * (t2 * t2) * t4P
                                                                                   * t4 + 12 * t6 * t22kP * t1 * t3 * k * (t2 * t2) * t5 * t1P * (t4 * t4) - 12 * t6 * t22kP * t1 * (t3 * t3) * k * (t2 * t2) * t4P * t5 * t4 - 12
                                                                                   * t6 * t22kP * t1 * (t3 * t3) * k * (t2 * t2) * t4P * t7 * t4 - 12 * t5 * t5 * t1P * t1 * (t3 * t3) * t4 * t2P * k * t4P * t2 - 6 * t7 * t3P
                                                                                   * t6 * t22kP * t1 * (t2 * t2) + 16 * t6 * t22kP * (t1 * t1) * t3 * t4 * k * t2 * t4P + 8 * t5 * t5 * t1P * t1 * t2P * t23P * t4P * (t3
                                                                                                                                                                                                                       * t3) - 4 * t5 * t1P * t1 * t2P * t24P * t4P * (t3 * t3) - 8 * t5 * t1P * t1 * t2P * t23P * t7 * t3P * (t4 * t4) * t3 + 8 * t5 * t1P
                                                                                   * t23P * t2P * t4P * t7 + 8 * t5 * t5 * t1P * t23P * t2P * t4P - 4 * t5 * t1P * t24P * t2P * t4P * (t3 * t3) * t4 - 4 * t5
                                                                                   * t1P * t3 * t2P * k * k * t4P * t1 * (t2 * t2) - 16 * t7 * t3P * t23P * t3 * t2P * t4P * t1 * t4 - 8 * t7 * t3P * t4 * t2P * (t2
                                                                                                                                                                                                                  * t2) * t5 * t1P + 8 * t5 * t1P * t1 * t4 * t2P * k * t23P * t4P + 12 * t5 * t1P * t1 * t4 * t2P * k * t2 * t4P - 8 * t5 * t5 * t1P
                                                                                   * t1 * (t3 * t3) * t4 * t2P * k * t4P * t23P - 6 * t6 * t22kP * t23P * t3 * t4P * (t1 * t1) * t4 + 2 * t5 * t5 * t1P * (t3 * t3) * t4
                                                                                   * t2P * k * k * t4P * t1 * (t2 * t2) + 4 * t7 * t3P * t2P * k * k * t2 * t4P * t5 * (t1 * t1) * t3 * t4 - 2 * t7 * t3P * t2P * k * k * (t2 * t2)
                                                                                   * t4P * t4 + 4 * t5 * t1P * t3 * t4 * t2P * k * k * t4P * t7 - 8 * t4P * t5 * t4 * t2P * k * t23P * t7 * t3P * t3 + 4 * t5 * t5 * t1P
                                                                                   * t3 * t4 * t2P * k * k * t4P + 2 * t4P * t5 * (t1 * t1) * t2P * k * k * t7 * t3P * t4 - 18 * t7 * t7 * t3P * t2P * k * (t2 * t2) * t4P
                                                                                   * (t1 * t1) * t3 - 4 * t7 * t3P * t2P * k * k * (t2 * t2) * t5 * t1P * t4 - 2 * t5 * t1P * (t3 * t3) * t4 * t2P * k * k * t4P * (t2 * t2) - 4
                                                                                   * t5 * t1P * t3 * t4 * t2P * k * k * t4P * t1 * (t2 * t2) + 2 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P * t5 + 4 * t7 * t3P * t2P * k * k
                                                                                   * t2 * t4P * t5 * t3 + 8 * t7 * t3P * t2P * k * k * t2 * t4P * t5 * t1 + 4 * t7 * t7 * t3P * t2P * k * k * t2 * t4P * t3 - 2 * t6 * t22kP
                                                                                   * (t1 * t1) * t4 * k * t4P * t5 - 6 * t4P * t6 * t22kP * t1 * (t2 * t2) + 8 * t7 * t3P * t24P * t3 * t6 * t2P * t4P * t1 * t4 + 4 * t7 * t3P
                                                                                   * t1 * t4 * t2P * k * t4P - 8 * t7 * t3P * t3 * t4 * t6 * t2P * k * t4P * (t1 * t1) * t23P + 6 * t6 * t6 * t22kP * t1 * t23P * t4P
                                                                                   * t4 - 3 * t6 * t22kP * t1 * t23P * t5 * t1P * (t4 * t4) - 8 * t4P * t6 * t2P * k * t23P * t7 * t3P * t3 + 4 * t24P * t7 * t7 * t3P
                                                                                   * t2P * t4P * (t1 * t1) - 4 * t24P * t7 * t3P * t2P * t4P * t4 - 4 * t24P * t7 * t3P * t2P * t4P * (t1 * t1) + 8 * t23P
                                                                                   * t7 * t7 * t3P * t2P * t4P * (t1 * t1) + 8 * t23P * t7 * t3P * t2P * t4P * t5 * (t1 * t1) + 8 * t23P * t7 * t3P * t2P * t6
                                                                                   * t4P * (t1 * t1) + 12 * t5 * t1P * t1 * t2P * k * t2 * t7 * t3P - 18 * t4P * t1 * t6 * t2P * k * (t2 * t2) * t5 * t1P + 2 * t4P * t3
                                                                                   * t4 * t6 * t2P * k * k * t7 * t3P * (t1 * t1) - 18 * t4P * t1 * t6 * t2P * k * (t2 * t2) * t5 * t1P * t4 + 6 * t4P * t6 * t6 * t22kP * t1
                                                                                   * (t3 * t3) * t4 * (t2 * t2) + 2 * t7 * t3P * t4 * t2P * k * t4P + 2 * t7 * t3P * t6 * t2P * k * k * (t2 * t2) * t4P * (t1 * t1) * t3 + 4 * t4P
                                                                                   * t3 * t4 * t6 * t2P * k * k * t7 * t3P * t1 + 18 * t4P * t3 * t4 * t2P * k * (t2 * t2) * t7 * t3P + 2 * t5 * t1P * t1 * t3 * t2P * k * t7 * t3P
                                                                                   + 4 * t6 * t6 * t23kP * k * t3 * (t4 * t4) * t2 - 8 * t23P * t7 * t3P * t2P * t4P * t4 - 18 * t4P * t5 * t4 * t2P * k * (t2 * t2) * t7 * t3P
                                                                                   * (t1 * t1) - 6 * t6 * t22kP * t23P * (t3 * t3) * t4P * t1 * t4 + 8 * t5 * t1P * t6 * t22kP * k * t1 * t2 + 3 * t4P * t7 * t6 * t22kP
                                                                                   * (t3 * t3) * t4 * (t2 * t2) - 2 * t7 * t7 * t3P * t4 * t2P * k * t4P - 12 * t6 * t22kP * k * (t2 * t2) * t4P * t7 * (t1 * t1) * t3 - 12 * t6
                                                                                   * t6 * t22kP * k * (t2 * t2) * t4P * (t1 * t1) * t3 - 12 * t6 * t22kP * k * (t2 * t2) * t4P * t7 * t1 - 12 * t6 * t6 * t22kP * k * (t2 * t2)
                                                                                   * t4P * t1 - 12 * t6 * t6 * t22kP * k * (t2 * t2) * t4P * t3 - 12 * t6 * t22kP * k * (t2 * t2) * t4P * t7 * t3 + 6 * t6 * t22kP * k * (t2
                                                                                                                                                                                                                          * t2) * t4P + 4 * t4P * t1 * t6 * t2P * k * k * t7 * t3P * t3 - 36 * t4P * t1 * t6 * t2P * k * (t2 * t2) * t5 * t1P * t3 + 18 * t5 * t1P
                                                                                   * t1 * t2P * k * (t2 * t2) * t7 * t3P + 4 * t5 * t1P * t1 * t6 * t2P * (t2 * t2) * t4P * t4 - 4 * t6 * t22kP * t3 * k * t4P * t5 * t4 - 4
                                                                                   * t6 * t6 * t22kP * t3 * k * t4P * t4 - 4 * t6 * t22kP * t3 * k * t4P * t7 * t4 - 18 * t4P * t5 * (t1 * t1) * t2P * k * (t2 * t2) * t7 * t3P
                                                                                   + 2 * t6 * t22kP * t4 * k * t4P - 18 * t4P * t5 * t4 * t2P * k * (t2 * t2) * t7 * t3P * t3 + 3 * t6 * t6 * t22kP * t23P * (t3 * t3) * t4P
                                                                                   * t4 + 12 * t6 * t6 * t22kP * t23P * t3 * t4P * t1 * t4 + 2 * t4P * (t3 * t3) * t2P * k * t5 * t1P * t4 + 8 * t23P * t7 * t7 * t3P * t2P
                                                                                   * t4P + 8 * t23P * t7 * t3P * t2P * t4P * t5 + 16 * t7 * t3P * t1 * t4 * t2P * k * t4P * t23P - 18 * t4P * t5 * t5 * t2P * k
                                                                                   * t1P * (t2 * t2) - 18 * t4P * t5 * t2P * k * t7 * t3P * (t2 * t2) - 4 * t7 * t3P * t2P * (t2 * t2) * t5 * t1P - 2 * t6 * t6 * t22kP
                                                                                   * (t1 * t1) * (t3 * t3) * k * t4P - 2 * t6 * t22kP * (t1 * t1) * (t3 * t3) * k * t4P * t7 - 12 * t4P * t7 * t2P * k * t5 * t1P * t2 * (t3
                                                                                                                                                                                                                          * t3) * t4 - 36 * t7 * t7 * t3P * t3 * t4 * t2P * k * t4P * t1 * (t2 * t2) - 3 * t6 * t22kP * t23P * (t3 * t3) * t5 * t1P * t1 - 6 * t6 * t22kP
                                                                                   * t3 * (t4 * t4) * t23P * t5 * t1P * t1 - 3 * t6 * t22kP * (t3 * t3) * (t4 * t4) * (t2 * t2) * t5 * t1P * t1 - 3 * t6 * t22kP * (t3 * t3)
                                                                                   * (t4 * t4) * t23P * t5 * t1P * t1 + 4 * t5 * t5 * t1P * t1 * t2P * (t2 * t2) * t4P * t4 - 8 * t5 * t1P * t1 * t2P * (t2 * t2) * t4P
                                                                                   * t3 * t4 - 4 * t5 * t1P * t1 * t2P * (t2 * t2) * t7 * t3P * (t4 * t4) - 18 * t4P * t6 * t2P * k * (t2 * t2) * t7 * t3P * t3 - 36 * t7 * t7
                                                                                   * t3P * t2P * k * (t2 * t2) * t4P * t1 * t4 - 4 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P * t1 * t4 - 24 * t7 * t7 * t3P * t2P * k * t2
                                                                                   * t4P * t1 * t4 - 6 * t6 * t6 * t22kP * k * (t2 * t2) * (t3 * t3) * t4P - 24 * t6 * t6 * t22kP * k * (t2 * t2) * t4P * t1 * t3 - 8 * t6 * t6 * t22kP
                                                                                   * (t3 * t3) * k * t2 * t4P * (t1 * t1) - 12 * t4P * t5 * t4 * t2P * k * t2 * t7 * t3P * (t1 * t1) + 6 * t4P * t7 * t6 * t22kP * (t3 * t3)
                                                                                   * t4 * (t2 * t2) * t1 - 2 * t7 * t3P * t4 * t2P * k * t4P * t6 - 16 * t6 * t6 * t22kP * (t3 * t3) * k * t2 * t4P * t1 - 12 * t6 * t6 * t22kP
                                                                                   * t3 * k * (t2 * t2) * t4P * t4 - 2 * t4P * t7 * t1 * t2P * k * t5 * t1P - 4 * t4P * t7 * t7 * t1 * t2P * k * t3P - 4 * t4P * t7 * t1 * t2P
                                                                                   * k * t5 * t1P * t3 - 8 * t4P * t7 * t2P * k * t23P * t5 * t1P - 18 * t4P * t7 * t2P * k * (t2 * t2) * t5 * t1P - 8 * t4P * t7 * t2P
                                                                                   * k * t5 * t1P * t23P * t1 - 8 * t4P * t7 * t2P * k * t5 * t1P * t23P * (t3 * t3) * t1 + 8 * t5 * t1P * t23P * t2P * t4P * t7
                                                                                   * t4 + 4 * t7 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P * t1 * t4 - 4 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P * t1 * t3 * t4 + 16 * t6 * t22kP
                                                                                   * t1 * t4 * k * t2 * t4P + 8 * t5 * t1P * t1 * t2P * t23P * t4P * t7 + 8 * t5 * t5 * t1P * t1 * t2P * t23P * t4P + 8 * t5 * t5 * t1P
                                                                                   * t1 * t2P * (t2 * t2) * t4P * t3 + 12 * t6 * t22kP * t23P * t3 * t4P * t7 * t1 * t4 - 8 * t23P * t7 * t3P * t2P * t4P * (t1 * t1)
                                                                                   + 16 * t6 * t22kP * (t1 * t1) * t3 * t4 * k * t2 * t7 * t3P + 8 * t4P * (t1 * t1) * t3 * t4 * t2P * k * t23P * t7 * t3P - 2 * t6 * t22kP
                                                                                   * t4 * k * t4P * t7 - 2 * t6 * t6 * t22kP * t4 * k * t4P - 2 * t6 * t22kP * t4 * k * t4P * t5 - 8 * t5 * t1P * t1 * t2P * t23P * t4P
                                                                                   * (t3 * t3) + 4 * t5 * t5 * t1P * t1 * t2P * (t2 * t2) * t4P * (t3 * t3) + 8 * t5 * t5 * t1P * t1 * t2P * t24P * t4P * t3 - 8 * t5 * t1P
                                                                                   * t1 * t2P * t24P * t7 * t3P * t4 * t3 - 4 * t7 * t3P * t2P * k * k * t2 * t5 * t1P * (t4 * t4) * t1 - 4 * t7 * t3P * t2P * k * k * t2 * t4P
                                                                                   * (t1 * t1) * t3 * t4 - 8 * t6 * t6 * t23kP * t3 * t4 * (t2 * t2) * t1 + 4 * t5 * t1P * t1 * t2P * (t2 * t2) * t4P * t7 * t4 - 8 * t7 * t3P * t2P
                                                                                   * k * k * t2 * t4P * t1 * t3 * t4 - 2 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P * t3 * t4 - 8 * t5 * t1P * t24P * t2P * t4P * t3 - 4 * t5
                                                                                   * t1P * t24P * t2P * t7 * t3P - 16 * t5 * t1P * t23P * t2P * t7 * t3P * t4 - 8 * t4P * t5 * t5 * t4 * t2P * k * t1P * t23P
                                                                                   * (t3 * t3) + 8 * t4P * t7 * t7 * t3 * t2P * t23P * t3P * (t1 * t1) * t4 + 4 * t24P * t7 * t3P * t2P * t4P * t6 * (t1 * t1) + 4 * t24P
                                                                                   * t7 * t3P * t2P * t4P * t6 - 8 * t24P * t7 * t3P * t2P * t4P * t1 + 8 * t23P * t7 * t7 * t3P * t2P * t4P * (t1 * t1) * t4
                                                                                   + 4 * t24P * t7 * t7 * t3P * t2P * t4P * (t1 * t1) * t4 + 0.32e2 * t6 * t22kP * t1 * t3 * k * t4P * t2 + 2 * t4P * t7 * t1 * t2P * k
                                                                                   * k * t5 * t1P + 4 * t4P * t7 * t7 * t1 * t2P * k * k * t3P - 36 * t5 * t1P * t1 * t3 * t4 * t2P * k * t4P * t7 * (t2 * t2) - 36 * t5 * t5 * t1P
                                                                                   * t1 * t3 * t4 * t2P * k * t4P * (t2 * t2) + 16 * t6 * t22kP * t3 * k * t4P * t2 * t4 - 8 * t4P * t5 * t4 * t2P * k * t23P * t7 * t3P
                                                                                   * (t1 * t1) + 4 * t5 * t5 * t1P * t1 * t2P * (t2 * t2) * t4P - 8 * t5 * t1P * t1 * t2P * t23P * t4P * (t3 * t3) * t4 + 8 * t6 * t22kP
                                                                                   * t1 * (t3 * t3) * k * t5 * t1P * t2 + 4 * t4P * t7 * (t3 * t3) * t2P * t24P * t5 * t1P * t4 * t1 + 2 * t5 * t1P * t6 * t2P * k * k * (t2
                                                                                                                                                                                                                          * t2) * t4P * t1 * t4 - 18 * t4P * t6 * t2P * k * (t2 * t2) * t7 * t3P * t4 - 4 * t6 * t22kP * t1 * t4 * k * t4P * t7 + 4 * t5 * t1P * t1
                                                                                   * t6 * t2P * t24P * t4P * (t3 * t3) * t4 - 8 * t5 * t1P * (t3 * t3) * t2P * k * t23P * t4P * t7 - 2 * t7 * t7 * t3P * t2P * k * t4P - 8
                                                                                   * t6 * t22kP * (t3 * t3) * k * t2 * t4P * t7 * (t1 * t1) + 4 * (t2 * t2) * t7 * t3P * t2P * t4P * t5 - 8 * t5 * t1P * t1 * (t3 * t3) * t4
                                                                                   * t6 * t2P * k * t4P * t23P + 4 * t5 * t1P * (t3 * t3) * t2P * k * k * t4P * t7 * t2 + 4 * (t2 * t2) * t7 * t3P * t2P * t4P * t6 - 18
                                                                                   * t5 * t1P * t1 * (t3 * t3) * t4 * t6 * t2P * k * t4P * (t2 * t2) - 24 * t5 * t1P * t3 * t2P * k * t2 * t4P * t7 - 24 * t5 * t5 * t1P * t3
                                                                                   * t2P * k * t2 * t4P + 18 * t7 * t3P * t2P * k * (t2 * t2) * t4P * (t1 * t1) + 12 * t7 * t3P * t2P * k * t2 * t4P - 12 * t4P * t5
                                                                                   * t5 * t4 * t2P * k * t2 * t1P * t1 + 8 * t4P * t1 * t4 * t6 * t2P * (t2 * t2) * t7 * t3P + 4 * t4P * (t1 * t1) * t4 * t6 * t2P * (t2 * t2)
                                                                                   * t7 * t3P + 16 * t4P * t1 * t4 * t6 * t2P * t23P * t7 * t3P + 18 * t5 * t1P * t1 * t3 * t2P * k * (t2 * t2) * t7 * t3P + 2 * t5 * t1P
                                                                                   * t2P * k * k * t4P * t7 * (t2 * t2) + 2 * t5 * t1P * t2P * k * k * t4P * t6 * (t2 * t2) - 12 * t6 * t22kP * t23P * t3 * t7 * t3P * t1
                                                                                   * t4 - 4 * t24P * t3 * t4P * t2P * t7 * t3P * (t1 * t1) - 6 * t6 * t22kP * t1 * t23P * t4P * t4 + 12 * t7 * t3P * t2P * k * t2 * t5
                                                                                   * t1P + 18 * t7 * t3P * t2P * k * (t2 * t2) * t5 * t1P + 6 * t6 * t22kP * (t1 * t1) * (t3 * t3) * k * t4P * (t2 * t2) - 12 * t4P * t6
                                                                                   * t2P * k * t2 * t7 * t3P * t4 - 18 * t7 * t3P * t6 * t2P * k * (t2 * t2) * t4P * (t1 * t1) - 12 * t7 * t3P * t6 * t2P * k * t2 * t4P
                                                                                   * (t1 * t1) * t3 - 12 * t7 * t3P * t6 * t2P * k * t2 * t4P * (t1 * t1) + 18 * t7 * t3P * t2P * k * (t2 * t2) * t4P + 36 * t7 * t3P * t2P
                                                                                   * k * (t2 * t2) * t4P * t1 + 8 * t23P * t3 * (t1 * t1) * t2P * t4P * t7 * t7 * t3P - 12 * t6 * t6 * t22kP * t1 * k * (t2 * t2) * t4P * t4
                                                                                   + 18 * t5 * t1P * t1 * (t3 * t3) * t2P * k * (t2 * t2) * t4P + 2 * t7 * t3P * t6 * t2P * k * k * (t2 * t2) * t4P + 2 * t4P * (t1 * t1) * t3
                                                                                   * t2P * k * t7 * t3P + 24 * t4P * t1 * t3 * t2P * k * t7 * t3P * t2 + 4 * t6 * t6 * t23kP * k * t4 * t2 * (t3 * t3) + 16 * t6 * t22kP * t1
                                                                                   * (t3 * t3) * t4 * k * t2 * t5 * t1P + 16 * t6 * t22kP * t1 * t3 * (t4 * t4) * k * t2 * t5 * t1P + 8 * t5 * t5 * t1P * t24P * t2P * t4P
                                                                                   * t3 + 16 * t7 * t3P * t6 * t22kP * k * t4 * t2 + 0.32e2 * t7 * t3P * t6 * t22kP * k * t4 * t2 * t1 + 16 * t7 * t3P * t6 * t22kP * k * (t4
                                                                                                                                                                                                                           * t4) * t2 * t1 + 12 * t7 * t3P * t6 * t22kP * k * t4 * (t2 * t2) + 6 * t7 * t3P * t6 * t22kP * k * (t1 * t1) * t3 * (t2 * t2) - 2 * t5 * t1P
                                                                                   * (t3 * t3) * t2P * k * k * t4P * t1 * (t2 * t2) + 16 * t5 * t5 * t1P * t23P * t2P * t4P * t3 * t4 + 8 * t5 * t5 * t1P * t1 * t2P * t23P
                                                                                   * t4P * t4 - 4 * t5 * t1P * t1 * t2P * (t2 * t2) * t4P * (t3 * t3) * t4 + 4 * t5 * t1P * t2P * k * k * t2 * t4P * t7 * t4 - 3 * t6 * t22kP
                                                                                   * t3 * (t4 * t4) * (t2 * t2) * t7 * t3P * (t1 * t1) - 6 * t6 * t22kP * t3 * t4 * t23P * t7 * t3P * (t1 * t1) - 8 * t5 * t1P * t1 * t2P * t23P
                                                                                   * t7 * t3P - 8 * t5 * t1P * t1 * t2P * (t2 * t2) * t7 * t3P * t4 + 4 * t5 * t5 * t1P * t2P * k * k * t2 * t4P * t4 - 4 * t5 * t1P * t2P
                                                                                   * k * k * t2 * t4P * t1 * t4 - 2 * t5 * t1P * t2P * k * k * (t2 * t2) * t4P * t4 + 4 * t4P * t7 * t7 * t3 * t2P * t24P * t3P * (t1 * t1)
                                                                                   * t4 + 16 * t4P * t7 * t3 * t2P * t23P * t5 * t1P * t4 * t1 + 8 * t4P * t7 * (t3 * t3) * t2P * t23P * t5 * t1P * t1 + 8 * t4P * (t3
                                                                                                                                                                                                                    * t3) * t4 * t2P * k * t23P * t5 * t1P + 16 * t7 * t3P * t6 * t22kP * k * t3 * t4 * t2 + 8 * t4P * t5 * t5 * t3 * t4 * t2P * (t2 * t2) * t1P - 2
                                                                                   * t6 * t6 * t23kP * (t1 * t1) * (t2 * t2) * t3 - 2 * t7 * t3P * t2P * k * k * (t2 * t2) * t5 * t1P * t3 * (t4 * t4) * t1 + 16 * t4P * t6 * t22kP
                                                                                   * k * t2 * t1 + 16 * t4P * t6 * t22kP * k * t1 * (t3 * t3) * t4 * t2 + 8 * t4P * t6 * t22kP * k * (t1 * t1) * (t3 * t3) * t4 * t2 + 12 * t6 * t22kP
                                                                                   * t1 * k * (t2 * t2) * t4P + 8 * t7 * t3P * t2P * k * k * t2 * t4P * t5 * t1 * t3 * t4 + 8 * t7 * t7 * t3P * t2P * k * k * t2 * t4P * t1 + 2 * t7
                                                                                   * t7 * t3P * t2P * k * k * (t2 * t2) * t4P - 6 * t6 * t22kP * t3 * t4 * (t2 * t2) * t7 * t3P * (t1 * t1) - 6 * t6 * t22kP * t23P * t3
                                                                                   * t4P * (t1 * t1) - 4 * t4P * t7 * t7 * t1 * t2P * k * t3P * t3 - 2 * t4P * t7 * t1 * t2P * k * t5 * t1P * (t3 * t3) + 4 * t7 * t7 * t3P
                                                                                   * t3 * t4 * t2P * (t2 * t2) * t4P - 4 * t7 * t3P * t3 * t4 * t2P * (t2 * t2) * t4P * (t1 * t1) - 3 * t6 * t22kP * (t1 * t1) * t23P * t7
                                                                                   * t3P * (t4 * t4) + 8 * t7 * t3P * t2P * k * t23P * t5 * t1P - 2 * t4P * t4 * t2P * k * k * t5 * t1P + 4 * t4P * t7 * (t3 * t3) * t2P
                                                                                   * (t2 * t2) * t5 * t1P + 8 * t4P * t7 * t3 * t2P * (t2 * t2) * t5 * t1P * t4 + 2 * t5 * t1P * t2P * k * k * (t2 * t2) * t4P * t7 * t4 + 2
                                                                                   * t5 * t5 * t1P * t2P * k * k * (t2 * t2) * t4P * t4 + 2 * t6 * t22kP * k * t5 * t1P + 2 * t6 * t22kP * k * t7 * t3P + 4 * t6 * t22kP
                                                                                   * t1 * (t4 * t4) * k * t7 * t3P + 2 * t4P * t5 * t5 * t2P * k * k * t1P * t4 + 8 * t6 * t22kP * k * t2 * t4P * (t1 * t1) - 2 * t5 * t1P * t1
                                                                                   * (t3 * t3) * t4 * t2P * k * t4P * t7 + 16 * t6 * t6 * t23kP * k * t4 * t2 * t1 * t3 - 16 * t7 * t7 * t3P * t3 * t4 * t2P * k * t4P * t1 * t23P
                                                                                   + 8 * t6 * t6 * t23kP * k * t3 * (t4 * t4) * t2 * t1 + 2 * t6 * t6 * t23kP * k * (t3 * t3) * (t4 * t4) * t2 + 4 * t6 * t6 * t23kP * k * (t3 * t3) * (t4
                                                                                                                                                                                                                                        * t4) * t2 * t1 + 18 * t5 * t1P * t1 * t2P * k * (t2 * t2) * t4P - 18 * t5 * t5 * t1P * t1 * t2P * k * (t2 * t2) * t4P - 8 * t5 * t5 * t1P
                                                                                   * t1 * t2P * k * t23P * t4P + 4 * t4P * t7 * t7 * t1 * t3 * t2P * k * k * t3P + 16 * t6 * t22kP * t3 * k * t2 * t7 * t3P * t1 - 3 * t4P
                                                                                   * t6 * t22kP * (t1 * t1) * (t2 * t2) + 4 * t6 * t6 * t23kP * k * t1 * (t4 * t4) * t2 - 4 * (t2 * t2) * t7 * t3P * t2P * t4P * (t1 * t1) + 2
                                                                                   * t4P * t5 * t2P * k * k * t7 * t3P * t3 - 16 * t7 * t3P * t6 * t2P * k * t23P * t4P * t1 + 4 * t7 * t3P * t3 * t4 * t2P * t24P
                                                                                   * t4P * t5 * (t1 * t1) + 8 * t5 * t1P * t23P * t6 * t2P * t4P * (t3 * t3) - 36 * t4P * t5 * t1 * t2P * k * (t2 * t2) * t7 * t3P + 4
                                                                                   * t6 * t6 * t23kP * k * (t1 * t1) * (t4 * t4) * t2 * t3 + 2 * t4P * t5 * t5 * t1 * t2P * k * k * t1P + 4 * t4P * t5 * t1 * t2P * k * k * t7 * t3P - 0.32e2
                                                                                   * t6 * t6 * t22kP * t1 * t3 * t4 * k * t2 * t4P + 4 * t6 * t22kP * t3 * k * t5 * t1P + 2 * t6 * t22kP * t3 * k * t7 * t3P + 16 * t4P * t5
                                                                                   * t23P * t4 * t2P * t7 * t3P * t1 + 4 * t4P * t5 * t24P * t4 * t2P * t7 * t3P - 4 * t7 * t3P * t3 * t4 * t2P * (t2 * t2) * t4P
                                                                                   + 16 * t23P * t7 * t7 * t3P * t2P * t4P * t1 * t4 + 4 * t24P * t7 * t7 * t3P * t2P * t4P * t4 - 6 * t6 * t22kP * t1 * t23P * t7
                                                                                   * t3P - 0.32e2 * t6 * t22kP * t3 * k * t2 * t4P * t7 * t1 + 2 * t4P * t7 * t7 * (t1 * t1) * t3 * t2P * k * k * t3P + 2 * t4P * t5 * t5 * t2P
                                                                                   * k * k * t1P + 2 * t4P * t5 * t2P * k * k * t7 * t3P + 2 * t7 * t7 * t3P * t4 * t2P * k * k * t4P + 2 * t7 * t3P * t4 * t2P * k * k * t4P
                                                                                   * t6 + 2 * t7 * t3P * t4 * t2P * k * k * t4P * t5 - 8 * t4P * t7 * t6 * t22kP * k * t2 + 3 * t4P * t7 * t6 * t22kP * (t1 * t1) * (t2 * t2)
                                                                                   + 8 * t4P * t2P * t24P * t5 * t3 * t7 * t3P * t1 + 12 * t6 * t22kP * t4 * k * (t2 * t2) * t5 * t1P + 16 * t6 * t22kP * t4 * k * t2 * t5
                                                                                   * t1P + 2 * t7 * t7 * t3P * t2P * k * k * t4P + 12 * t7 * t3P * t1 * (t4 * t4) * t2P * k * t5 * t1P * t2 + 6 * t6 * t6 * t22kP * t23P
                                                                                   * (t3 * t3) * t4P * t1 * t4 - 12 * t4P * t6 * t22kP * t1 * (t2 * t2) * t3 - 6 * t6 * t22kP * t23P * (t3 * t3) * t5 * t1P * t4 * t1 - 3 * t5
                                                                                   * t1P * t6 * t22kP * t1 * (t2 * t2) - 12 * t4P * t5 * t4 * t2P * k * t2 * t7 * t3P - 2 * t6 * t22kP * (t1 * t1) * (t3 * t3) * k * t4P
                                                                                   * t7 * t4 + 2 * t7 * t3P * t6 * t2P * k * k * (t2 * t2) * t4P * (t1 * t1) * t3 * t4 + 4 * t7 * t3P * t6 * t2P * k * k * t2 * t4P * t3 * t4 + 16
                                                                                   * t5 * t1P * t6 * t22kP * k * t1 * t3 * t2 - 18 * t4P * t5 * t4 * t2P * k * (t2 * t2) * t7 * t3P - 12 * t7 * t3P * t3 * t4 * t6 * t2P * k
                                                                                   * t4P * (t1 * t1) * t2 + 6 * t6 * t22kP * t23P * (t3 * t3) * t4P * t7 * t1 * t4 + 8 * t7 * t3P * t6 * t2P * k * k * t2 * t4P * t1 * t4 - 2
                                                                                   * t6 * t6 * t22kP * (t3 * t3) * k * t4P * t4 - 2 * t6 * t22kP * (t3 * t3) * k * t4P * t5 * t4 - 4 * t5 * t1P * (t3 * t3) * t2P * k * k * t4P
                                                                                   * t2 + 8 * t5 * t1P * t23P * t6 * t2P * t4P * t4 - 18 * t4P * t5 * t5 * t4 * t2P * k * (t2 * t2) * t1P - 8 * t7 * t3P * t6 * t2P * k
                                                                                   * t23P * t4P * (t1 * t1) * t4 - 18 * t7 * t3P * t6 * t2P * k * (t2 * t2) * t4P * (t1 * t1) * t3 + 4 * t5 * t1P * t1 * t6 * t2P * (t2 * t2)
                                                                                   * t4P * (t3 * t3) + 8 * t6 * t22kP * (t1 * t1) * (t3 * t3) * k * t4P * t2 - 2 * t5 * t1P * (t3 * t3) * t2P * k * k * t4P * t1 + 8 * t5 * t1P
                                                                                   * t1 * t6 * t2P * t23P * t4P * (t3 * t3) + 8 * t7 * t3P * t3 * t4 * t2P * t23P * t4P * t5 - 3 * t6 * t22kP * (t1 * t1) * t23P
                                                                                   * t4P * t4 + 4 * t7 * t3P * t6 * t2P * k * k * t2 * t4P * (t1 * t1) * t3 * t4 - 2 * t6 * t22kP * (t3 * t3) * k * t4P * t7 * t4 + 4 * t6 * t22kP
                                                                                   * t1 * t4 * k * t4P + 2 * t6 * t22kP * (t1 * t1) * t4 * k * t4P + 8 * t5 * t1P * t4 * t2P * k * t23P * t4P + 8 * t7 * t3P * t6 * t2P
                                                                                   * k * k * t2 * t4P * t1 * t3 * t4 + 2 * t7 * t3P * t6 * t2P * k * k * (t2 * t2) * t4P * t3 * t4 + 8 * t23P * t7 * t3P * t2P * t4P * t6 * (t1
                                                                                                                                                                                                                             * t1) * t3 - 16 * t23P * t7 * t3P * t2P * t4P * t1 - 4 * t24P * t7 * t3P * t2P * t4P + 4 * t24P * t7 * t3P * t2P * t4P
                                                                                   * t5 * (t1 * t1) + 16 * t23P * t7 * t7 * t3P * t2P * t4P * t1 + 4 * t24P * t7 * t7 * t3P * t2P * t4P + 16 * t23P * t7 * t3P
                                                                                   * t2P * t4P * t6 * t1 + 8 * t23P * t7 * t3P * t2P * t4P * t6 * t3 + 4 * t24P * t7 * t3P * t2P * t4P * t5 + 16 * t23P * t7
                                                                                   * t3P * t2P * t4P * t5 * t1 + 8 * t23P * t7 * t7 * t3P * t2P * t4P * t3 + 8 * t23P * t7 * t7 * t3P * t2P * t4P * t4 + 8 * t23P
                                                                                   * t7 * t3P * t2P * t4P * t6 * t4 + 8 * t5 * t1P * t24P * t6 * t2P * t4P * t3 + 4 * t7 * t3P * t3 * t2P * k * t5 * t1P * t4 + 4 * t6
                                                                                   * t22kP * t3 * k * t4P + 12 * t7 * t3P * (t1 * t1) * t4 * t2P * k * t4P * t2 * t3 - 16 * t6 * t6 * t22kP * t3 * k * t4P * t2 * t4 - 3 * t7
                                                                                   * t3P * t6 * t22kP * t3 * (t4 * t4) * (t2 * t2) + 8 * t5 * t1P * t1 * t6 * t2P * t23P * t4P * (t3 * t3) * t4 - 2 * t7 * t3P * t4 * t2P
                                                                                   * k * k * t4P + 2 * t4P * t7 * t7 * (t1 * t1) * t3 * t4 * t2P * k * k * t3P + 4 * t4P * t7 * t7 * t1 * t3 * t4 * t2P * k * k * t3P + 12 * t5 * t1P
                                                                                   * t4 * t2P * k * t2 * t4P + 2 * t5 * t1P * t6 * t2P * k * k * (t2 * t2) * t4P * t1 + 8 * t5 * t1P * t1 * t2P * k * t23P * t4P + 12 * t5
                                                                                   * t1P * t1 * t2P * k * t2 * t4P + 4 * t4P * t7 * t7 * t1 * t2P * k * k * t3P * t4 + 8 * t5 * t1P * t1 * t6 * t2P * t24P * t4P * t3 - 8
                                                                                   * t4P * t5 * t6 * t22kP * k * (t1 * t1) * t2 - 18 * t4P * t5 * t5 * t4 * t2P * k * (t2 * t2) * t1P * t1 - 12 * t6 * t22kP * t23P * t3
                                                                                   * t4P * t1 * t4 - 3 * t6 * t22kP * t23P * t3 * t7 * t3P * (t4 * t4) - 12 * t6 * t6 * t22kP * t1 * (t3 * t3) * k * (t2 * t2) * t4P * t4 + 2
                                                                                   * t5 * t1P * (t3 * t3) * t2P * k * k * t4P * t7 * (t2 * t2) + 3 * t4P * t5 * t6 * t22kP * (t3 * t3) * t4 * (t2 * t2) - 16 * t6 * t6 * t22kP
                                                                                   * (t1 * t1) * t3 * t4 * k * t2 * t4P + 4 * t5 * t1P * t1 * t6 * t2P * t24P * t4P * (t3 * t3) + 3 * t4P * t7 * t6 * t22kP * (t1 * t1) * t4
                                                                                   * (t2 * t2) - 8 * t23P * t7 * t3P * t2P * t4P + 4 * t24P * t7 * t3P * t2P * t4P * t6 * t4 + 8 * t24P * t7 * t7 * t3P * t2P
                                                                                   * t4P * t1 * t4 + 4 * t5 * t1P * t24P * t6 * t2P * t4P * (t3 * t3) * t4 - 18 * t7 * t3P * t3 * t4 * t6 * t2P * k * t4P * (t2 * t2) - 8
                                                                                   * t7 * t3P * t3 * t4 * t6 * t2P * k * t4P * t23P + 4 * t5 * t1P * t3 * t6 * t2P * k * k * t4P * t1 * (t2 * t2) + 6 * t6 * t22kP * t23P
                                                                                   * t3 * t4P * t7 * (t1 * t1) + 16 * t7 * t3P * t23P * t3 * t6 * t2P * t4P * t1 * t4 + 4 * t5 * t1P * t3 * t2P * k * k * t4P * t7 - 3 * t6
                                                                                   * t22kP * t23P * t3 * t7 * t3P * (t1 * t1) - 3 * t6 * t22kP * t23P * (t3 * t3) * t4P * (t1 * t1) + 2 * t4P * t7 * t7 * (t1 * t1) * t2P
                                                                                   * k * k * t3P * t4 - 24 * t5 * t5 * t1P * t1 * t3 * t2P * k * t2 * t4P + 4 * t24P * t7 * t3P * t2P * t4P * t6 * (t1 * t1) * t3 + 4 * t5
                                                                                   * t1P * t3 * t4 * t6 * t2P * k * k * t4P * (t2 * t2) - 8 * t4P * t1 * t6 * t2P * k * t23P * t5 * t1P + 4 * t5 * t5 * t1P * t3 * t2P
                                                                                   * k * k * t4P + 6 * t6 * t6 * t22kP * t23P * t3 * t4P * (t1 * t1) - 2 * t6 * t6 * t23kP * (t1 * t1) * t4 * (t2 * t2) * (t3 * t3) - 18 * t4P
                                                                                   * t1 * t6 * t2P * k * (t2 * t2) * t5 * t1P * (t3 * t3) - 18 * t4P * t5 * t4 * t2P * k * (t2 * t2) * t7 * t3P * (t1 * t1) * t3 - 24 * t4P * t5
                                                                                   * t4 * t2P * k * t2 * t7 * t3P * t1 * t3 + 4 * t24P * t7 * t3P * t2P * t4P * t6 * t3 + 8 * t24P * t7 * t7 * t3P * t2P * t4P * t1
                                                                                   + 4 * t24P * t7 * t7 * t3P * t2P * t4P * t3 + 8 * t24P * t7 * t3P * t2P * t4P * t6 * t1 + 8 * t24P * t7 * t3P * t2P * t4P
                                                                                   * t6 * t1 * t3 + 8 * t24P * t7 * t3P * t2P * t4P * t5 * t1 + 16 * t23P * t7 * t3P * t2P * t4P * t6 * t1 * t3 + 8 * t24P * t42kP
                                                                                   * t2P * t7 * t1 - 2 * t7 * t7 * t3P * t3 * t2P * k * t4P - 8 * t6 * t6 * t22kP * (t3 * t3) * k * t4P * t2 * t4 - 8 * t23P * t3 * t4P * t2P
                                                                                   * t7 * t3P * (t1 * t1) + 16 * t5 * t1P * t6 * t22kP * k * t3 * t2 + 8 * t5 * t1P * t6 * t22kP * k * (t3 * t3) * t2 + 12 * t5 * t1P * t6 * t22kP
                                                                                   * k * t3 * (t2 * t2) + 6 * t5 * t1P * t6 * t22kP * k * (t3 * t3) * (t2 * t2) - t6 * t6 * t23kP * (t1 * t1) * (t4 * t4) * (t2 * t2) * (t3 * t3) + 8
                                                                                   * t7 * t3P * t23P * t3 * t6 * t2P * t4P * (t1 * t1) * t4 - 2 * t7 * t3P * t2P * k * t4P * t6 + 6 * t4P * t5 * t6 * t22kP * t1 * t4
                                                                                   * (t2 * t2) + 2 * t6 * t22kP * (t1 * t1) * (t3 * t3) * k * t4P + 2 * t6 * t22kP * (t1 * t1) * t3 * k * t7 * t3P + 8 * t6 * t22kP * t1 * t3 * k
                                                                                   * t7 * t3P * t4 + 4 * t6 * t22kP * (t1 * t1) * t3 * k * t4P - 4 * (t2 * t2) * t3 * t4P * t2P * t7 * t3P * (t1 * t1) - 12 * t4P * t5 * t4
                                                                                   * t2P * k * t2 * t7 * t3P * (t1 * t1) * t3 - 8 * t4P * t5 * t4 * t2P * k * t23P * t7 * t3P * (t1 * t1) * t3 - 6 * t6 * t22kP * t23P
                                                                                   * t3 * t7 * t3P * t1 * (t4 * t4) + 12 * t4P * t5 * t6 * t22kP * t1 * t4 * (t2 * t2) * t3 - 16 * t4P * t5 * t4 * t2P * k * t23P * t7 * t3P
                                                                                   * t1 * t3 + 3 * t4P * t5 * t6 * t22kP * (t1 * t1) * t4 * (t2 * t2) - 12 * t6 * t6 * t22kP * k * (t2 * t2) * t1 * (t3 * t3) * t4P - 6 * t6 * t22kP
                                                                                   * t23P * t3 * t4P * t4 - 3 * t6 * t22kP * t23P * (t3 * t3) * t5 * t1P * (t4 * t4) + 2 * t7 * t3P * (t4 * t4) * t2P * k * t5 * t1P - 8
                                                                                   * t7 * t3P * t3 * t4 * t2P * (t2 * t2) * t4P * t1 + 18 * t42kP * (t1 * t1) * t6 * t2P * k * (t2 * t2) * t5 - 12 * t7 * t7 * t3P * t2P
                                                                                   * k * t2 * t4P + 6 * t4P * t5 * t6 * t22kP * t1 * t4 * t23P + 3 * t6 * t6 * t22kP * t23P * (t3 * t3) * t4P * (t1 * t1) * t4 - 8 * t7 * t3P
                                                                                   * t3 * t4 * t2P * t23P * t4P + 8 * t7 * t3P * t3 * t4 * t2P * t23P * t4P * t5 * (t1 * t1) - 2 * t4P * t7 * t1 * t2P * k * t5 * t1P
                                                                                   * t4 - 2 * t4P * t7 * t7 * (t1 * t1) * t2P * k * t3P - 36 * t4P * t5 * t4 * t2P * k * (t2 * t2) * t7 * t3P * t1 * t3 - 2 * t7 * t3P * (t1
                                                                                                                                                                                                                          * t1) * t4 * t2P * k * t4P * t6 * t3 + 6 * t4P * t5 * t6 * t22kP * (t1 * t1) * t4 * (t2 * t2) * t3 + 4 * t7 * t3P * t6 * t2P * k * k * t2 * t4P
                                                                                   * t3 + 4 * t7 * t3P * t6 * t2P * k * k * (t2 * t2) * t4P * t1 + 3 * t6 * t22kP * t23P * (t3 * t3) * t4P * t7 * (t1 * t1) * t4 - 12 * t6 * t6
                                                                                   * t22kP * (t1 * t1) * t3 * k * (t2 * t2) * t4P * t4 + 3 * t4P * t5 * t6 * t22kP * (t1 * t1) * t4 * t23P + 6 * t4P * t5 * t6 * t22kP
                                                                                   * (t2 * t2) * t3 + 18 * t4P * (t1 * t1) * t3 * t2P * k * t7 * t3P * (t2 * t2) + 2 * t7 * t3P * t2P * k * t5 * t1P - 8 * t4P * t7 * t6 * t22kP
                                                                                   * k * t2 * (t1 * t1) + 8 * t7 * t7 * t3P * t3 * t4 * t2P * (t2 * t2) * t4P * t1 - 2 * t7 * t7 * t3P * (t1 * t1) * t4 * t2P * k * t4P * t3 - 2
                                                                                   * t5 * t1P * t3 * t2P * k * k * t7 * t3P - 2 * t7 * t3P * t3 * t2P * k * t4P * t6 - 2 * t5 * t1P * (t3 * t3) * t2P * k * k * t4P + 6 * t4P
                                                                                   * t5 * t6 * t22kP * (t2 * t2) * t1 + 16 * t7 * t3P * t1 * t4 * t2P * k * t5 * t1P * t23P + 8 * t42kP * t1 * t2P * k * t23P + 18 * t42kP
                                                                                   * t6 * t2P * k * (t2 * t2) * t5 + 2 * t42kP * t3 * t6 * t6 * t2P * k + 12 * t7 * t7 * t32kP * (t1 * t1) * t4 * t2P * k * t2 - 2 * t5 * t5 * t12kP
                                                                                   * t24P * t2P * (t4 * t4) + 2 * t42kP * t5 * (t1 * t1) * t2P * k * k + 8 * (t2 * t2) * (t3 * t3) * t42kP * t2P * t7 * t1 + 12 * t42kP
                                                                                   * (t1 * t1) * t6 * t2P * k * t2 * t5 + 4 * t4P * t2 * t5 * t1P * k * k * t6 * t2P * t1 * (t3 * t3) * t4 - 2 * t42kP * t5 * t2P * k * (t1 * t1)
                                                                                   + 2 * t4P * t5 * t1P * k * k * t1 * t6 * t2P * t4 + 2 * t4P * t5 * t1P * k * k * t1 * t6 * t2P - 12 * t4P * t4 * t5 * t1P * k * t1 * t6 * t2P
                                                                                   * t2 - 12 * t4P * t4 * t5 * t1P * k * t1 * t6 * t2P * (t3 * t3) * t2 - 2 * t4P * t4 * t5 * t1P * k * t1 * t6 * t2P * (t3 * t3) + 2 * t6 * t2P
                                                                                   * (t3 * t3) * t4 * t1P * k * k * t4P * t5 + 4 * t6 * t2P * t3 * t1P * k * k * t1 * t4P * t5 - 2 * t4P * t4 * t5 * t1P * k * t6 * t2P - 4 * t4P
                                                                                   * t4 * t5 * t1P * k * t6 * t2P * t3 + 2 * t6 * t6 * t23kP * (t1 * t1) * (t3 * t3) * k * t4 - 12 * t4P * t4 * t5 * t1P * k * t6 * t2P * t2 - 2
                                                                                   * t4P * t4 * t5 * t1P * k * t6 * t2P * (t3 * t3) + 4 * t4P * t2 * t5 * t1P * k * k * t6 * t2P * t4 - 36 * t42kP * t1 * t6 * t2P * k * (t2
                                                                                                                                                                                                                          * t2) - 2 * t5 * t1P * t6 * t22kP * k * k * t2 * t4 - 2 * t42kP * t7 * t7 * t1 * t2P * k * k + 2 * t42kP * t7 * (t3 * t3) * t2P * k * k + 8 * t24P
                                                                                   * t3 * t42kP * t2P * t7 + 4 * t4P * t5 * t1P * k * k * t6 * t2P * t3 + 2 * t4P * t5 * t1P * k * k * t6 * t2P * t4 + 2 * t4P * t5 * t1P
                                                                                   * k * k * t6 * t2P - 4 * t6 * t2P * t3 * t1P * k * t4P * t5 - 12 * t42kP * t5 * (t1 * t1) * t2P * k * t2 + 4 * t4P * t2 * t5 * t1P * k * k
                                                                                   * t6 * t2P * t1 * (t3 * t3) + 4 * t4P * t2 * t5 * t1P * k * k * t6 * t2P * (t3 * t3) * t4 - t4P * t4 * t22kP * k * k * t6 - 2 * t4P * (t3
                                                                                                                                                                                                                          * t3) * t5 * t1P * k * t1 * t6 * t2P - 4 * t4P * t3 * t5 * t1P * k * t1 * t6 * t2P - 4 * t4P * t4 * t5 * t1P * k * t1 * t6 * t2P * t3 - 2
                                                                                   * t4P * t4 * t5 * t1P * k * t1 * t6 * t2P - 0.32e2 * t42kP * t2P * t23P * t5 * t3 * t6 * t1 + 2 * t42kP * (t3 * t3) * t6 * t2P * k
                                                                                   * t7 + 36 * t7 * t7 * t32kP * t1 * t4 * t2P * k * (t2 * t2) - 2 * t42kP * t6 * t2P * k * k * t7 + 8 * t24P * (t3 * t3) * t42kP * t2P * t6
                                                                                   * t1 + 16 * t42kP * t1 * t6 * t2P * k * t23P * t5 * (t3 * t3) - 2 * t42kP * t7 * t7 * t1 * (t3 * t3) * t2P * k * k * (t2 * t2) - 4 * t42kP
                                                                                   * t7 * t7 * (t1 * t1) * t3 * t2P * k * k * t2 - t5 * t5 * t12kP * (t3 * t3) * t2P * k * k * (t2 * t2) + 4 * t42kP * t5 * t5 * t2P * k * t1 * t3 - t4P
                                                                                   * (t1 * t1) * t6 * t22kP * k * k * t4 * t2 - t42kP * t7 * t7 * (t1 * t1) * t2P * k * k * (t2 * t2) - 2 * t7 * t3P * t6 * t22kP * k * k * t2 * t1
                                                                                   + 24 * t42kP * (t1 * t1) * t6 * t2P * k * t2 * t5 * t3 - 2 * t42kP * t6 * t6 * t2P * k * k * t2 + t4P * (t3 * t3) * t4 * t6 * t6 * t22kP * k
                                                                                   * k - 2 * (t2 * t2) * (t1 * t1) * t2P * t42kP * t7 * t7 - 4 * t42kP * t5 * (t3 * t3) * t2P * k * k * t2 * t6 * (t1 * t1) - 4 * t5 * t5 * t12kP
                                                                                   * t3 * t4 * t2P * k * k - 2 * t7 * t3P * t6 * t22kP * k * k * t2 * t3 * t4 - 2 * t24P * t7 * t7 * t32kP * t2P * (t4 * t4) + 8 * t42kP * t7
                                                                                   * t7 * t2P * k * t1 * t23P * (t3 * t3) - 8 * t5 * t5 * t12kP * t24P * t2P * t3 * t4 + t4P * (t1 * t1) * t6 * t6 * t22kP * k * k + t4P
                                                                                   * (t1 * t1) * t6 * t6 * t22kP * k * k * t4 + 4 * t42kP * (t1 * t1) * t6 * t2P * k * k * t2 - 4 * t23P * t42kP * t7 * t7 * t2P - t4P * (t3
                                                                                                                                                                                                                          * t3) * t4 * t6 * t22kP * k * k * (t1 * t1) * t2 - 8 * t42kP * t2P * t24P * t5 * t5 * t3 * t1 + 0.9e1 * t7 * t7 * t32kP * t2P * k * (t2
                                                                                                                                                                                                                                                                                                                                                               * t2) * (t4 * t4) - 4 * t5 * t1P * t3 * t4 * t6 * t22kP * k * k * t1 * t2 + 2 * t4P * t5 * t3 * t4 * t22kP * k * k * t6 * t2 + 2 * t42kP * t5 * t2P
                                                                                   * k * k * (t3 * t3) + 2 * t4P * t5 * t3 * t4 * t22kP * k * k * t6 * (t1 * t1) * t2 - t7 * t3P * t6 * t22kP * k * k * t2 * (t1 * t1) * t3 - 4 * t42kP
                                                                                   * t1 * t2P * k * k * t3 - 2 * t7 * t7 * t32kP * t2P * k * k * (t2 * t2) * t1 * (t4 * t4) - 4 * t23P * t42kP * t2P - 2 * t42kP * t7 * (t1
                                                                                                                                                                                                                         * t1) * t2P * k * k * t6 + 12 * t42kP * (t1 * t1) * t6 * t6 * t2P * k * t2 * t3 - 4 * t42kP * t5 * t2P * k * k * t7 * t3 + 2 * t42kP * (t3 * t3)
                                                                                   * t6 * t6 * t2P * k * t1 - 2 * t4P * t1 * t22kP * k * k * t6 * t2 - 4 * t7 * t7 * t32kP * t4 * t2P * k * k * t1 - 4 * t24P * (t3 * t3) * (t1 * t1)
                                                                                   * t2P * t42kP * t7 * t6 + 2 * t4P * t5 * t3 * t22kP * k * k * t2 * t6 + 2 * t4P * t5 * (t3 * t3) * t4 * t22kP * k * k * t6 * t1 * t2 - 8 * t42kP
                                                                                   * t7 * t1 * t3 * t2P * k * k * t6 + 2 * t42kP * t6 * t2P * k * t7 + 4 * t5 * t5 * t12kP * t2P * k * t23P + 8 * t42kP * t5 * t2P * k * t6
                                                                                   * t1 * t3 - 8 * t42kP * t2P * (t2 * t2) * t5 * (t3 * t3) * t7 * t1 + 2 * t4P * t1 * t6 * t6 * t22kP * k * k * t4 + 4 * t42kP * t5 * t2P * k
                                                                                   * k * (t2 * t2) * t1 + 18 * t42kP * t5 * t5 * t1 * t2P * k * (t2 * t2) + 2 * t42kP * t5 * t5 * t2P * k * t3 + 4 * t42kP * t2P * k * t23P
                                                                                   + 0.32e2 * t42kP * t1 * t6 * t2P * k * t23P * t7 * t3 + 2 * t4P * t7 * t1 * (t3 * t3) * t4 * t22kP * k * k * t6 * t2 - 4 * t42kP * t3 * t6
                                                                                   * t2P * k * k * t7 + t7 * t7 * t32kP * (t1 * t1) * t2P * k + 4 * t4P * t7 * t1 * t3 * t22kP * k * k * t6 - 2 * t7 * t3P * (t4 * t4) * t22kP
                                                                                   * k * k * t6 * t1 - 8 * t42kP * t5 * (t1 * t1) * t2P * k * t23P + 16 * t42kP * t6 * t2P * k * t23P * t5 * t3 - 4 * t42kP * t7 * t1 * t2P
                                                                                   * k * k * t6 - t42kP * (t1 * t1) * t6 * t6 * t2P * k * k * (t2 * t2) + 4 * t5 * t5 * t12kP * t4 * t2P * k * t3 + 18 * t42kP * t7 * t7 * t2P
                                                                                   * k * (t2 * t2) * t3 + 36 * t42kP * t6 * t2P * k * (t2 * t2) * t7 * t3 - 8 * t42kP * t1 * t3 * t2P * k * t5 - t5 * t1P * (t4 * t4) * t6 * t22kP
                                                                                   * k * k - 2 * t42kP * t7 * (t1 * t1) * t2P * k * k * t6 * (t2 * t2) - 18 * t42kP * t5 * t2P * k * (t2 * t2) + 4 * t42kP * t1 * t6 * t2P * k
                                                                                   * k - 24 * t4P * t4 * t5 * t1P * k * t1 * t6 * t2P * t3 * t2 - 2 * t4P * t5 * t1P * k * t6 * t2P - 24 * t4P * t3 * t5 * t1P * k * t1 * t6
                                                                                   * t2P * t2 + 2 * t4P * t5 * t1P * k * k * t6 * t2P * (t3 * t3) - 12 * t4P * (t3 * t3) * t5 * t1P * k * t1 * t6 * t2P * t2 - 12 * t6 * t2P
                                                                                   * t2 * t1P * k * t4P * t5 - 12 * t4P * t2 * (t3 * t3) * t5 * t1P * k * t6 * t2P * t4 + 4 * t6 * t2P * t3 * t4 * t1P * k * k * t4P * t5 + 4
                                                                                   * t4P * t2 * t5 * t1P * k * k * t6 * t2P * (t3 * t3) + t4P * (t3 * t3) * t6 * t6 * t22kP * k * k * t2 + 8 * t42kP * t7 * t1 * (t3 * t3) * t2P
                                                                                   * k * k * t2 + 2 * t42kP * t3 * t6 * t6 * t2P * k * (t1 * t1) + 4 * t5 * t5 * t12kP * (t3 * t3) * t2P * k * t23P + 16 * t23P * t3 * t42kP
                                                                                   * t2P * t6 * (t1 * t1) - 2 * t42kP * (t3 * t3) * t2P * k * t7 + 8 * t42kP * t1 * t6 * t2P * k * k * t3 - 4 * t42kP * t7 * t7 * t1 * t3 * t2P
                                                                                   * k * k + 8 * t42kP * t7 * t1 * t2P * k * t6 * t3 - 4 * t42kP * t7 * t7 * t3 * t2P * k * k * t2 + 4 * t42kP * t5 * t5 * t2P * k * t23P - 4 * t5
                                                                                   * t5 * t12kP * (t4 * t4) * t2P * (t2 * t2) * t3 - 8 * t42kP * t6 * t2P * k * t23P + 16 * t42kP * t2P * t23P * t5 * (t3 * t3) * t1 - 4
                                                                                   * t23P * t7 * t7 * t32kP * t2P + 4 * (t2 * t2) * t42kP * t2P * t6 + 8 * t23P * t42kP * t2P * t5 + 16 * t42kP * t7 * t2P
                                                                                   * k * t5 * t1 * t23P + 8 * t42kP * t5 * t1 * t2P * k * k * t3 - 0.32e2 * t42kP * t1 * t3 * t2P * k * t5 * t23P + 8 * (t2 * t2) * t42kP
                                                                                   * t2P * t5 * t1 - 2 * t4P * t4 * t22kP * k * k * t6 * t3 - 4 * t42kP * t2P * t23P * t5 * t5 * (t1 * t1) + t4P * (t1 * t1) * t6 * t6 * t22kP
                                                                                   * k * k * (t3 * t3) + 36 * t42kP * t1 * t6 * t6 * t2P * k * (t2 * t2) * t3 + 6 * t42kP * t5 * t5 * (t1 * t1) * t2P * k * t2 * (t3 * t3) - 2 * t7 * t7
                                                                                   * t32kP * t2P * k * k * (t2 * t2) * t1 - 2 * t42kP * t1 * t6 * t6 * t2P * k * k * (t3 * t3) - 24 * t42kP * t1 * t2P * k * t2 * t7 - 8 * t23P
                                                                                   * t3 * t42kP * t2P * (t1 * t1) + 2 * t4P * t1 * t6 * t6 * t22kP * k * k * t2 - 8 * t42kP * (t1 * t1) * t2P * k * t23P * t7 - 2 * t5 * t5
                                                                                   * t12kP * t2P * (t2 * t2) * (t3 * t3) + 16 * t42kP * t5 * t2P * k * t7 * t23P * t3 + 4 * t42kP * t7 * (t3 * t3) * t2P * k * k * t2 + 2
                                                                                   * t42kP * t5 * (t3 * t3) * t2P * k * k * (t2 * t2) - 16 * t23P * t3 * t1 * t2P * t42kP * t7 * t7 + 4 * t42kP * t2P * (t2 * t2) * t5
                                                                                   * (t3 * t3) * (t1 * t1) - 8 * t42kP * t2P * t23P * t5 * (t3 * t3) * t7 - 16 * t42kP * t2P * t23P * t5 * t6 * t1 - t7 * t3P * t6 * t22kP
                                                                                   * k * k * t2 * (t4 * t4) - t42kP * (t1 * t1) * t2P * k * k * (t3 * t3) * (t2 * t2) - 4 * t42kP * t5 * (t3 * t3) * t2P * k * k * t2 * t7 + 4 * t42kP
                                                                                   * (t3 * t3) * t2P * k * t23P + 2 * t4P * t7 * (t1 * t1) * t3 * t4 * t22kP * k * k * t6 * t2 - 8 * t42kP * t5 * t3 * t2P * k * k * (t2 * t2)
                                                                                   * t6 * t1 - 8 * t23P * t1 * t6 * t6 * t2P * t42kP + 8 * t42kP * (t1 * t1) * t6 * t2P * k * t23P * t5 - 4 * t23P * (t3 * t3) * t42kP
                                                                                   * t7 * t7 * t2P - 4 * (t2 * t2) * (t1 * t1) * t6 * t2P * t42kP * t7 + 4 * t4P * t3 * t4 * t6 * t6 * t22kP * k * k * t1 * t2 - 2 * t7 * t7 * t32kP
                                                                                   * t2P * k * k * t2 + t4P * (t3 * t3) * t4 * t6 * t6 * t22kP * k * k * t2 + 16 * t42kP * t7 * t2P * k * t5 * (t1 * t1) * t23P * t3 + 6 * t4P
                                                                                   * t6 * t6 * t22kP * (t1 * t1) * t3 * t4 * (t2 * t2) + 16 * t4P * t2P * t23P * t5 * t3 * t7 * t3P * t1 + 2 * t7 * t3P * t2P * k * k * (t2
                                                                                                                                                                                                                         * t2) * t4P * t5 * (t1 * t1) * t3 + 4 * t7 * t7 * t3P * t2P * k * k * t2 * t4P * (t1 * t1) * t3 + 2 * t7 * t7 * t3P * t2P * k * k * (t2 * t2)
                                                                                   * t4P * (t1 * t1) * t4 + 8 * (t2 * t2) * t7 * t7 * t3P * t2P * t4P * t1 * t4 - 12 * t7 * t7 * t3P * t2P * k * t2 * t4P * (t1 * t1) * t4
                                                                                   + 4 * t4P * t5 * t5 * t1 * t4 * t2P * k * k * t1P * t2 + 2 * t4P * t5 * t5 * t1 * t4 * t2P * k * k * t1P + 3 * t4P * t6 * t6 * t22kP * (t1
                                                                                                                                                                                                                           * t1) * (t3 * t3) * t4 * (t2 * t2) + 8 * t5 * t5 * t1P * t23P * t2P * t4P * (t3 * t3) - 8 * t5 * t1P * t23P * t2P * t4P * (t3 * t3) - 16
                                                                                   * t5 * t1P * t23P * t2P * t7 * t3P * t4 * t3 + 0.32e2 * t6 * t22kP * t1 * t3 * t4 * k * t2 * t5 * t1P + 0.32e2 * t6 * t22kP * t1 * t3
                                                                                   * t4 * k * t2 * t4P - 24 * t6 * t22kP * t1 * t3 * k * (t2 * t2) * t4P * t7 * t4 + 12 * t6 * t22kP * t23P * t3 * t4P * t7 * t1 + 24 * t6 * t22kP
                                                                                   * t3 * k * (t2 * t2) * t5 * t1P * t4 - 16 * t6 * t22kP * (t1 * t1) * t3 * t4 * k * t2 * t4P * t7 + 2 * t5 * t1P * (t3 * t3) * t4 * t2P * k * k * t4P
                                                                                   * t7 * t1 * (t2 * t2) - 4 * t7 * t3P * t2P * k * k * t2 * t5 * t1P * t1 - 2 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P * (t1 * t1) - 6 * t6 * t22kP
                                                                                   * (t3 * t3) * (t2 * t2) * t5 * t1P * t4 - 12 * t6 * t22kP * t3 * (t2 * t2) * t7 * t3P * t1 * t4 + 12 * t5 * t1P * t6 * t22kP * k * t3 * (t4
                                                                                                                                                                                                                            * t4) * (t2 * t2) + 16 * t5 * t1P * t6 * t22kP * k * (t3 * t3) * t4 * t2 + 16 * t5 * t1P * t6 * t22kP * k * t3 * (t4 * t4) * t2 - 2 * t6 * t22kP
                                                                                   * (t3 * t3) * k * t4P * t5 - 2 * t6 * t22kP * (t3 * t3) * k * t4P * t7 + 24 * t5 * t1P * t1 * t3 * t4 * t2P * k * t4P * t2 - 24 * t5 * t1P
                                                                                   * t1 * t3 * t4 * t2P * k * t4P * t7 * t2 + 12 * t6 * t22kP * t1 * k * (t2 * t2) * t5 * t1P * t4 + 24 * t6 * t22kP * t1 * k * (t2 * t2) * t7 * t3P
                                                                                   * t4 + 6 * t6 * t22kP * (t1 * t1) * k * (t2 * t2) * t4P + 4 * t4P * t5 * t5 * t2P * k * k * t2 * t1P + 2 * t4P * t5 * t5 * t1 * t4 * t2P * k
                                                                                   * k * t1P * (t2 * t2) + 8 * t6 * t6 * t23kP * k * t4 * t2 * t3 + 2 * t4P * t7 * t2P * k * k * t5 * t1P * t4 + 4 * t4P * t5 * t5 * t2P * (t2
                                                                                                                                                                                                                            * t2) * t1P + 8 * t4P * t5 * t5 * t2P * (t2 * t2) * t1P * t3 + 4 * t4P * t5 * t5 * t2P * (t2 * t2) * t1P * t4 - 12 * t6 * t22kP * t23P
                                                                                   * t3 * t4P * t1 + 3 * t6 * t6 * t22kP * t23P * (t3 * t3) * t4P * (t1 * t1) + 8 * t4P * t1 * t4 * t6 * t2P * t24P * t7 * t3P + 24 * t7
                                                                                   * t3P * t1 * t4 * t2P * k * t4P * t2 * t3 + 6 * t6 * t22kP * (t3 * t3) * (t2 * t2) * t4P * t7 * t1 - 4 * t6 * t6 * t23kP * t3 * (t2 * t2)
                                                                                   * t1 + 16 * t5 * t5 * t1P * t1 * t2P * t23P * t4P * t3 * t4 - 8 * t5 * t1P * t1 * t2P * t23P * t4P - 18 * t4P * t7 * t7 * t3 * t4
                                                                                   * t2P * k * t3P * (t2 * t2) + 12 * t6 * t6 * t22kP * t23P * t3 * t4P * t1 + 4 * t5 * t1P * t1 * t2P * t24P * t4P * t7 * t4 + 4 * t5
                                                                                   * t5 * t1P * t1 * t2P * t24P * t4P * t4 - 8 * t4P * t1 * t4 * t2P * t24P * t7 * t3P + 2 * t6 * t6 * t23kP * k * (t1 * t1) * t2 * (t3
                                                                                                                                                                                                                     * t3) * (t4 * t4) + 4 * t5 * t1P * t24P * t6 * t2P * t4P + 8 * t5 * t1P * t3 * t2P * k * t23P * t7 * t3P + 12 * t7 * t3P * t3 * (t4
                                                                                                                                                                                                                                                                                                                                                      * t4) * t2P * k * t5 * t1P * t2 - 8 * t5 * t1P * t1 * t2P * t23P * t7 * t3P * (t4 * t4) - 24 * t7 * t7 * t3P * t3 * t4 * t2P * k * t4P
                                                                                   * t1 * t2 - 12 * t6 * t22kP * t1 * k * (t2 * t2) * t4P * t7 * t4 - 2 * t4P * t3 * t4 * t2P * k * k * t7 * t3P * (t1 * t1) + 3 * t6 * t6 * t22kP
                                                                                   * t23P * (t3 * t3) * t4P - 6 * t6 * t22kP * (t1 * t1) * (t3 * t3) * k * (t2 * t2) * t4P * t7 * t4 - 16 * t5 * t1P * t1 * t2P * t23P
                                                                                   * t7 * t3P * t4 + 8 * t5 * t5 * t1P * t1 * t2P * t23P * t4P * (t3 * t3) * t4 + 2 * t7 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P * (t1
                                                                                                                                                                                                                       * t1) - 2 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P * (t1 * t1) * t3 - 2 * t7 * t3P * t2P * k * k * (t2 * t2) * t5 * t1P * t3 * t1 - 8 * t7 * t3P
                                                                                   * t2P * k * k * t2 * t5 * t1P * t3 * t4 * t1 - 4 * t7 * t3P * t2P * k * k * (t2 * t2) * t5 * t1P * t3 * t4 * t1 - 4 * t4P * t3 * t4 * t2P * k
                                                                                   * k * t7 * t3P * t1 + 8 * t7 * t7 * t3P * t2P * k * k * t2 * t4P * t1 * t3 * t4 - 4 * t7 * t3P * t2P * k * k * (t2 * t2) * t5 * t1P * t4 * t1
                                                                                   + 8 * t7 * t3P * t2P * k * k * t2 * t4P * t5 * t1 * t4 + 4 * t7 * t7 * t3P * t2P * k * k * t2 * t4P * t3 * t4 + 2 * t7 * t7 * t3P * t2P * k
                                                                                   * k * (t2 * t2) * t4P * (t1 * t1) * t3 * t4 + 12 * t6 * t22kP * t3 * k * (t2 * t2) * t4P * t4 - 12 * t4P * t5 * t5 * t4 * t2P * k * t1P * t2
                                                                                   * (t3 * t3) + 4 * t5 * t5 * t1P * t3 * t4 * t2P * k * k * t4P * t1 * (t2 * t2) + 2 * t5 * t1P * (t3 * t3) * t4 * t2P * k * k * t4P * t7 * (t2
                                                                                                                                                                                                                              * t2) + 4 * t4P * t7 * (t3 * t3) * t2P * (t2 * t2) * t5 * t1P * t1 + 8 * t4P * t7 * (t3 * t3) * t2P * t23P * t5 * t1P + 4 * t4P * t7
                                                                                   * (t3 * t3) * t2P * (t2 * t2) * t5 * t1P * t4 - 2 * t5 * t1P * t2P * k * k * (t2 * t2) * t4P * t1 * t4 - 24 * t4P * t5 * t1 * t2P * k * t2
                                                                                   * t7 * t3P * t3 + 8 * t5 * t5 * t1P * t1 * t2P * (t2 * t2) * t4P * t3 * t4 - 4 * t5 * t1P * t1 * t2P * t24P * t4P * (t3 * t3) * t4 + 3
                                                                                   * t6 * t22kP * t23P * (t3 * t3) * t4P * t5 - 4 * t6 * t6 * t23kP * t4 * (t2 * t2) * t1 + 2 * t7 * t3P * (t1 * t1) * t4 * t2P * k * t4P
                                                                                   + 24 * t7 * t3P * t1 * t4 * t2P * k * t4P * t2 + 4 * (t2 * t2) * t7 * t7 * t3P * t2P * t4P * (t1 * t1) * t4 - 3 * t6 * t22kP * (t1 * t1)
                                                                                   * t23P * t4P + 6 * t4P * t7 * t6 * t22kP * t4 * (t2 * t2) * t1 + 8 * t5 * t1P * t1 * t6 * t2P * t24P * t4P * t3 * t4 + 24 * t4P
                                                                                   * t3 * t4 * t2P * k * t2 * t5 * t1P + 12 * t4P * (t3 * t3) * t4 * t2P * k * t2 * t5 * t1P - 16 * t6 * t22kP * t1 * (t3 * t3) * t4 * k * t2 * t4P
                                                                                   * t7 - 3 * t6 * t22kP * t3 * (t4 * t4) * t23P * t7 * t3P * (t1 * t1) - 24 * t7 * t7 * t3P * t2P * k * t2 * t4P * t1 - 16 * t7 * t7 * t3P
                                                                                   * t2P * k * t23P * t4P * t1 + 4 * t5 * t5 * t1P * t24P * t2P * t4P * (t3 * t3) + 3 * t6 * t22kP * t23P * (t3 * t3) * t4P * t7
                                                                                   + 18 * t7 * t3P * t2P * k * (t2 * t2) * t4P * t4 + 3 * t6 * t22kP * (t3 * t3) * t4 * t23P * t4P * t5 * (t1 * t1) + 2 * t6 * t22kP * t3
                                                                                   * k * t7 * t3P * (t4 * t4) + 4 * t6 * t22kP * t3 * k * t5 * t1P * (t4 * t4) + 2 * t6 * t22kP * (t3 * t3) * k * t4P * t4 - 4 * t5 * t1P * t3
                                                                                   * t4 * t2P * k * k * t4P * t1 - 8 * t5 * t1P * t3 * t4 * t2P * k * k * t4P * t2 + 4 * t5 * t1P * t3 * t4 * t2P * k * k * t4P * t7 * t1 + 8 * t5
                                                                                   * t1P * t3 * t4 * t2P * k * k * t4P * t7 * t2 - 16 * t4P * t5 * t5 * t4 * t2P * k * t1P * t23P * t3 + 0.32e2 * t7 * t3P * t6 * t22kP
                                                                                   * k * t3 * t4 * t2 * t1 + 8 * t7 * t3P * t6 * t22kP * k * t3 * (t4 * t4) * t2 + 16 * t7 * t3P * t6 * t22kP * k * t3 * (t4 * t4) * t2 * t1 + 12 * t7
                                                                                   * t3P * t6 * t22kP * k * t3 * t4 * (t2 * t2) + 24 * t7 * t3P * t6 * t22kP * k * t3 * t4 * (t2 * t2) * t1 + 6 * t7 * t3P * t6 * t22kP * k * t3
                                                                                   * (t4 * t4) * (t2 * t2) + 8 * t23P * t7 * t3P * t2P * t4P * t6 - 6 * t6 * t22kP * (t3 * t3) * k * (t2 * t2) * t4P * t5 * t4 - 6 * t6 * t22kP
                                                                                   * (t3 * t3) * k * (t2 * t2) * t4P * t7 * t4 + 6 * t6 * t22kP * k * (t2 * t2) * t4P * (t3 * t3) + 12 * t6 * t22kP * (t1 * t1) * t3 * k * (t2 * t2)
                                                                                   * t7 * t3P * t4 + 24 * t6 * t22kP * t1 * t3 * k * (t2 * t2) * t4P * t4 + 8 * t5 * t1P * t6 * t22kP * k * (t3 * t3) * (t4 * t4) * t2 - 24 * t7
                                                                                   * t7 * t3P * t2P * k * t2 * t4P * t1 * t3 - 36 * t7 * t7 * t3P * t2P * k * (t2 * t2) * t4P * t1 * t3 + 16 * t4P * t7 * t7 * t3 * t2P * t23P
                                                                                   * t3P * t1 * t4 + 4 * t5 * t1P * t1 * t2P * (t2 * t2) * t4P * t7 + 4 * t7 * t3P * t2P * k * k * t2 * t4P * t5 * (t1 * t1) * t3 - 2 * t7 * t3P
                                                                                   * t2P * k * k * (t2 * t2) * t5 * t1P * t1 - 2 * t7 * t3P * t2P * k * k * (t2 * t2) * t5 * t1P * t3 - 4 * t7 * t3P * t24P * t3 * t2P * t4P
                                                                                   * (t1 * t1) * t4 - 4 * t7 * t3P * t3 * t2P * (t2 * t2) * t5 * t1P - 8 * t7 * t3P * t3 * t2P * (t2 * t2) * t5 * t1P * t4 - 0.32e2 * t6 * t22kP
                                                                                   * t1 * t3 * t4 * k * t2 * t4P * t7 + 4 * t5 * t5 * t1P * t1 * t2P * (t2 * t2) * t4P * (t3 * t3) * t4 - 4 * t5 * t1P * t1 * t2P * (t2 * t2) * t7
                                                                                   * t3P * (t4 * t4) * t3 + 4 * (t2 * t2) * t7 * t3P * t2P * t4P * t6 * t4 - 36 * t4P * t7 * t2P * k * t5 * t1P * t3 * (t2 * t2) * t1 - 18
                                                                                   * t4P * t7 * t2P * k * t5 * t1P * t4 * (t2 * t2) * t1 + 4 * (t2 * t2) * t7 * t3P * t2P * t4P * t5 * t4 - 12 * t7 * t7 * t3P * t2P * k
                                                                                   * t2 * t4P * (t1 * t1) * t3 - 18 * t7 * t7 * t3P * t2P * k * (t2 * t2) * t4P * (t1 * t1) * t4 - 4 * t7 * t3P * t2P * k * k * t2 * t5 * t1P
                                                                                   * t3 * t1 - 8 * t7 * t3P * t2P * k * k * t2 * t5 * t1P * t3 * t4 - 16 * t7 * t7 * t3P * t2P * k * t23P * t4P * t1 * t4 - 8 * t5 * t1P * t1
                                                                                   * t2P * t24P * t7 * t3P * t4 - 16 * t5 * t1P * t1 * t2P * t23P * t7 * t3P * t4 * t3 + 36 * t5 * t1P * t4 * t2P * k * (t2 * t2) * t7
                                                                                   * t3P - 3 * t6 * t22kP * (t3 * t3) * (t2 * t2) * t5 * t1P * t1 + 12 * t6 * t6 * t22kP * t3 * (t2 * t2) * t4P * t1 * t4 + 3 * t6 * t6 * t22kP
                                                                                   * (t3 * t3) * (t2 * t2) * t4P * t4 + 12 * t6 * t22kP * t3 * (t2 * t2) * t4P * t7 * t1 * t4 - 4 * t7 * t3P * t3 * t2P * k * k * t4P * t1 + 4
                                                                                   * t7 * t3P * t6 * t2P * k * k * (t2 * t2) * t4P * t1 * t3 * t4 - 16 * t6 * t6 * t22kP * t1 * t4 * k * t2 * t4P + 4 * t6 * t6 * t23kP * k * t2
                                                                                   * t3 - 12 * t7 * t7 * t3P * t2P * k * t2 * t4P * t4 + 8 * t7 * t3P * t2P * k * t23P * t4P * t4 + 4 * t6 * t6 * t23kP * k * t1 * t2 + 2 * t6
                                                                                   * t6 * t23kP * k * (t1 * t1) * t2 + 8 * t6 * t6 * t23kP * k * t1 * t2 * t4 + 4 * t6 * t6 * t23kP * k * (t1 * t1) * t2 * t4 + 12 * t5 * t1P * (t4 * t4)
                                                                                   * t2P * k * t2 * t7 * t3P + 18 * t5 * t1P * t4 * t2P * k * (t2 * t2) * t4P - 18 * t5 * t1P * t4 * t2P * k * (t2 * t2) * t4P * t7 + 8 * t4P
                                                                                   * t7 * t3 * t2P * t24P * t5 * t1P - 8 * t7 * t7 * t3P * t3 * t4 * t2P * k * t4P * (t1 * t1) * t23P - 8 * t5 * t1P * t1 * t2P * t23P
                                                                                   * t4P * t4 - 4 * t5 * t1P * t1 * t2P * t24P * t4P * t4 + 2 * t5 * t1P * t2P * k * k * (t2 * t2) * t4P * t7 * t1 - 4 * t5 * t1P * t2P
                                                                                   * k * k * t2 * t4P * t4 + 2 * t5 * t5 * t1P * t2P * k * k * (t2 * t2) * t4P * t1 - 2 * t6 * t6 * t23kP * (t3 * t3) * (t2 * t2) * t1 - 4 * t5 * t1P
                                                                                   * t1 * t2P * t24P * t7 * t3P * t3 + 4 * t5 * t5 * t1P * t1 * t2P * t24P * t4P * (t3 * t3) * t4 - 4 * t5 * t1P * t1 * t2P * t24P
                                                                                   * t4P - 4 * t5 * t1P * t1 * t2P * (t2 * t2) * t4P + 6 * t6 * t22kP * (t1 * t1) * (t3 * t3) * k * (t2 * t2) * t4P * t4 - 12 * t6 * t22kP
                                                                                   * (t1 * t1) * t3 * k * (t2 * t2) * t4P * t7 * t4 + 6 * t6 * t22kP * k * (t2 * t2) * t5 * t1P + 12 * t6 * t22kP * k * (t2 * t2) * t4P * t3 - 36
                                                                                   * t7 * t7 * t3P * t2P * k * (t2 * t2) * t4P * t1 - 4 * t5 * t1P * t24P * t2P * t7 * t3P * (t4 * t4) - 4 * t5 * t1P * t24P * t2P
                                                                                   * t7 * t3P * (t4 * t4) * t3 - 6 * t6 * t22kP * (t1 * t1) * (t3 * t3) * k * (t2 * t2) * t4P * t5 * t4 + 12 * t6 * t22kP * (t1 * t1) * t3 * k * (t2
                                                                                                                                                                                                                                  * t2) * t4P - 8 * t4P * t6 * t6 * t22kP * k * t2 - 16 * t4P * t6 * t6 * t22kP * k * t2 * t1 + 6 * t6 * t22kP * (t4 * t4) * k * (t2 * t2) * t5
                                                                                   * t1P + 6 * t6 * t22kP * t23P * t3 * t4P * t7 * t4 - 4 * t7 * t3P * t2P * k * k * t2 * t4P + 4 * t7 * t7 * t3P * t2P * k * k * t2 * t4P
                                                                                   * (t1 * t1) + 4 * t7 * t3P * t2P * k * k * t2 * t4P * t5 * (t1 * t1) - 4 * t7 * t3P * t2P * k * k * t2 * t4P * (t1 * t1) * t4 - 2 * t4P * t5
                                                                                   * t2P * k * t7 * t3P * (t1 * t1) * t3 - 2 * t4P * t5 * t5 * t2P * k * t1P * (t3 * t3) * t1 - 12 * t4P * t5 * t2P * k * t7 * t3P * t2 * t3 - 2
                                                                                   * t4P * t5 * t5 * t2P * k * t1P * t1 - 2 * t4P * t5 * t2P * k * t7 * t3P * (t1 * t1) - 12 * t4P * t7 * t2P * k * t5 * t1P * t2 * t4
                                                                                   * t1 - 24 * t4P * t7 * t2P * k * t5 * t1P * t2 * t3 * t1 - 12 * t4P * t5 * t6 * t22kP * k * (t1 * t1) * (t2 * t2) * t3 - 16 * t4P * t5 * t6
                                                                                   * t22kP * k * (t1 * t1) * t2 * t3 - 2 * t7 * t7 * t3P * (t1 * t1) * t4 * t2P * k * t4P + 3 * t6 * t22kP * (t1 * t1) * t23P * t4P * t7
                                                                                   + 3 * t6 * t6 * t22kP * (t1 * t1) * t23P * t4P - 4 * t4P * t2P * (t2 * t2) * t5 * t1P * (t3 * t3) - 4 * t4P * t2P * (t2 * t2) * t5
                                                                                   * t1P - 8 * t4P * t2P * (t2 * t2) * t5 * t1P * t3 + 6 * t6 * t22kP * t1 * (t3 * t3) * k * (t2 * t2) * t5 * t1P + 24 * t6 * t22kP * t1
                                                                                   * t3 * k * (t2 * t2) * t5 * t1P * t4 - 18 * t5 * t1P * t1 * (t3 * t3) * t4 * t2P * k * t4P * t7 * (t2 * t2) - 2 * t7 * t3P * t2P * k * k * (t2
                                                                                                                                                                                                                               * t2) * t5 * t1P * (t4 * t4) - 4 * t7 * t3P * t2P * k * k * t2 * t5 * t1P * t3 * (t4 * t4) + 12 * t4P * t3 * t4 * t2P * k * t2 * t7 * t3P - 36
                                                                                   * t4P * t5 * t5 * t4 * t2P * k * t1P * t3 * (t2 * t2) + 8 * t5 * t1P * t3 * t2P * k * k * t4P * t7 * t1 * t2 - 4 * t5 * t1P * (t3 * t3) * t2P
                                                                                   * k * k * t4P * t1 * t2 - 2 * t4P * t5 * t2P * k * t7 * t3P * t4 * t3 + 24 * t5 * t1P * t1 * t3 * t4 * t2P * k * t7 * t3P * t2 + 2 * t5 * t1P
                                                                                   * t1 * t3 * (t4 * t4) * t2P * k * t7 * t3P - 2 * t7 * t3P * (t1 * t1) * t4 * t2P * k * t4P * t6 - 4 * t7 * t3P * t1 * t4 * t2P * k * t4P
                                                                                   * t6 * t3 + 16 * t5 * t1P * t23P * t6 * t2P * t4P * t3 * t4 + 4 * t5 * t1P * t24P * t6 * t2P * t4P * t4 + 6 * t6 * t6 * t22kP * t23P
                                                                                   * t3 * t4P * t4 + 8 * t4P * t5 * t24P * t4 * t2P * t7 * t3P * t1 + 3 * t4P * t6 * t6 * t22kP * (t1 * t1) * (t3 * t3) * (t2 * t2) + 6 * t6
                                                                                   * t22kP * t23P * t3 * t4P * t5 * t4 + 2 * t5 * t1P * (t3 * t3) * t4 * t2P * k * k * t4P * t7 * t1 + 4 * t5 * t1P * (t3 * t3) * t4 * t2P
                                                                                   * k * k * t4P * t7 * t1 * t2 + 4 * t5 * t1P * t3 * t4 * t2P * k * k * t4P * t7 * (t2 * t2) + 4 * t5 * t5 * t1P * (t3 * t3) * t4 * t2P * k * k * t4P
                                                                                   * t1 * t2 + 8 * t5 * t5 * t1P * t3 * t4 * t2P * k * k * t4P * t1 * t2 - 3 * t6 * t22kP * t23P * (t3 * t3) * t4P * t4 + 16 * t4P * t7 * t3
                                                                                   * t2P * t23P * t5 * t1P * t1 - 4 * t7 * t3P * t2P * k * k * t2 * t5 * t1P * t3 - 8 * t7 * t3P * t2P * k * k * t2 * t5 * t1P * t4 - 8 * t7
                                                                                   * t3P * t2P * k * k * t2 * t4P * t1 * t3 - 2 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P * t3 - 2 * t7 * t3P * t2P * k * k * (t2 * t2) * t5
                                                                                   * t1P * (t4 * t4) * t1 - 4 * t7 * t3P * t2P * k * k * t2 * t5 * t1P * t3 * (t4 * t4) * t1 - 2 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P * (t1
                                                                                                                                                                                                                               * t1) * t4 - 6 * t6 * t22kP * t23P * t3 * t5 * t1P * (t4 * t4) - 16 * t6 * t22kP * t3 * k * t2 * t4P * t7 - 2 * t7 * t3P * (t1 * t1) * t4
                                                                                   * t2P * k * t4P * t5 - 4 * t7 * t7 * t3P * t1 * t4 * t2P * k * t4P * t3 + 3 * t6 * t22kP * (t1 * t1) * t23P * t4P * t5 - 4 * t7 * t3P
                                                                                   * t1 * t4 * t2P * k * t4P * t5 * t3 - 12 * t4P * t5 * t4 * t2P * k * t2 * t7 * t3P * t3 + 2 * t6 * t6 * t23kP * k * t2 * (t3 * t3) + 8 * t4P
                                                                                   * t7 * t7 * t3 * t2P * t23P * t3P * t4 - 8 * t4P * t7 * t2P * k * t5 * t1P * t23P * t4 * t1 - 16 * t4P * t7 * t2P * k * t5 * t1P
                                                                                   * t23P * t3 * t1 - 8 * t5 * t1P * t24P * t2P * t7 * t3P * t4 * t3 + 6 * t5 * t1P * t6 * t22kP * k * (t3 * t3) * (t4 * t4) * (t2 * t2) - 12
                                                                                   * t6 * t22kP * t1 * k * (t2 * t2) * t4P * t5 * t4 + 6 * t6 * t22kP * t1 * k * (t2 * t2) * t5 * t1P * (t4 * t4) + 12 * t6 * t22kP * t1 * k * (t2
                                                                                                                                                                                                                                * t2) * t7 * t3P * (t4 * t4) + 6 * t6 * t22kP * (t1 * t1) * k * (t2 * t2) * t4P * t4 + 12 * t4P * t2P * k * t5 * t1P * t2 + 2 * t4P * t2P
                                                                                   * k * t5 * t1P + 2 * t4P * t2P * k * t7 * t3P + 2 * t4P * t2P * k * t5 * t1P * t4 + 8 * t6 * t22kP * t3 * k * t2 * t7 * t3P - 2 * t4P
                                                                                   * t7 * t2P * k * t5 * t1P - 3 * t5 * t1P * t6 * t22kP * (t2 * t2) + 4 * t4P * t2P * k * t7 * t3P * t1 - 8 * t6 * t22kP * (t3 * t3) * k
                                                                                   * t2 * t4P * t7 - 8 * t6 * t22kP * (t3 * t3) * k * t2 * t4P * t5 - 0.32e2 * t6 * t6 * t22kP * t3 * k * t2 * t4P * t1 - 16 * t6 * t6 * t22kP
                                                                                   * t3 * k * t2 * t4P * (t1 * t1) + 2 * t6 * t22kP * t1 * k * t5 * t1P + 4 * t6 * t22kP * t1 * k * t7 * t3P + 2 * t6 * t22kP * (t1 * t1) * k * t7
                                                                                   * t3P + 2 * t6 * t22kP * (t1 * t1) * k * t4P - 2 * t6 * t22kP * (t1 * t1) * k * t4P * t7 - 2 * t6 * t6 * t22kP * (t1 * t1) * k * t4P - 2
                                                                                   * t6 * t22kP * (t1 * t1) * k * t4P * t5 + 4 * t6 * t22kP * t1 * k * t4P + 3 * t6 * t22kP * t23P * t4P * t7 * t4 + 3 * t6 * t22kP * t23P
                                                                                   * t4P * t5 * t4 + 3 * t6 * t6 * t22kP * t23P * t4P * t4 - 3 * t6 * t22kP * t23P * t5 * t1P * (t4 * t4) + 8 * t5 * t5 * t1P * t1 * t2P
                                                                                   * t24P * t4P * t3 * t4 - 4 * t5 * t1P * t1 * t2P * t24P * t7 * t3P * (t4 * t4) * t3 + 4 * t7 * t7 * t3P * t2P * k * k * t2 * t4P + 8
                                                                                   * t7 * t7 * t3P * t2P * k * k * t2 * t4P * t1 * t4 + 2 * t7 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P * t4 + 2 * t7 * t3P * t2P * k * k * (t2
                                                                                                                                                                                                                               * t2) * t4P * t5 * t4 + 6 * t6 * t22kP * t23P * t3 * t4P * t5 * (t1 * t1) * t4 - 2 * t5 * t1P * t2P * k * k * (t2 * t2) * t4P * t1 - 4 * t5
                                                                                   * t1P * t2P * k * k * t2 * t4P * t1 + 4 * t5 * t1P * t2P * k * k * t2 * t4P * t7 * t1 + 4 * t5 * t5 * t1P * t2P * k * k * t2 * t4P * t1
                                                                                   + 36 * t7 * t3P * t3 * t4 * t2P * k * t5 * t1P * (t2 * t2) + 8 * t6 * t6 * t23kP * k * t1 * t2 * t3 - 4 * t5 * t1P * t1 * t2P * t24P * t7
                                                                                   * t3P - 8 * t5 * t1P * t1 * t2P * t24P * t4P * t3 + 6 * t6 * t22kP * (t3 * t3) * t4 * t23P * t4P * t5 * t1 + 2 * t4P * t7 * t4 * t22kP
                                                                                   * k * k * t2 * t6 * t1 + 6 * t6 * t6 * t22kP * t23P * t3 * t4P + 8 * t4P * t2P * (t2 * t2) * t5 * t3 * t7 * t3P * t1 + 4 * t7 * t3P * t6 * t2P
                                                                                   * k * k * t2 * t4P * t4 - 6 * t4P * t5 * t6 * t22kP * k * (t1 * t1) * (t2 * t2) + 18 * t4P * (t1 * t1) * t3 * t4 * t2P * k * (t2 * t2) * t7 * t3P
                                                                                   + 3 * t4P * t6 * t6 * t22kP * (t1 * t1) * t4 * (t2 * t2) + 6 * t6 * t22kP * t23P * t3 * t4P * t5 - 6 * t7 * t3P * t6 * t22kP * t1 * t3
                                                                                   * (t4 * t4) * (t2 * t2) + 16 * t4P * t1 * t3 * t4 * t2P * k * t23P * t7 * t3P + 16 * t5 * t1P * t4 * t2P * k * t23P * t7 * t3P - 2 * t4P
                                                                                   * t7 * (t3 * t3) * t4 * t2P * k * t5 * t1P - 18 * t4P * t5 * (t1 * t1) * t2P * k * (t2 * t2) * t7 * t3P * t3 - t6 * t6 * t23kP * (t1 * t1)
                                                                                   * (t4 * t4) * (t2 * t2) - 4 * t6 * t6 * t23kP * (t1 * t1) * t4 * (t2 * t2) * t3 - 2 * t6 * t22kP * (t1 * t1) * t4 * k * t4P * t7 + 4 * (t2 * t2) * t7
                                                                                   * t7 * t3P * t2P * t4P - 2 * t7 * t3P * t3 * t4 * t2P * k * k * t4P - 2 * t7 * t3P * t3 * t2P * k * t4P * t6 * (t1 * t1) + 8 * t7 * t7
                                                                                   * t3P * t2P * k * k * t2 * t4P * t1 * t3 + 4 * t7 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P * t1 * t3 - 4 * t4P * t1 * t2P * k * k * t7 * t3P
                                                                                   * t4 - 18 * t4P * t5 * t2P * k * t7 * t3P * (t2 * t2) * t3 + 36 * t5 * t1P * t1 * t3 * t4 * t2P * k * t4P * (t2 * t2) + 16 * t5 * t1P * t1
                                                                                   * t3 * t4 * t2P * k * t4P * t23P + 4 * t5 * t1P * t1 * t3 * t4 * t2P * k * t4P - 16 * t4P * t5 * t6 * t22kP * k * t4 * t2 * t1 - 16 * t4P
                                                                                   * t5 * t6 * t22kP * k * t4 * t2 * t3 + 2 * t4P * (t1 * t1) * t6 * t2P * k * k * t7 * t3P * t3 - 18 * t5 * t1P * (t3 * t3) * t2P * k * (t2 * t2)
                                                                                   * t4P * t6 - 12 * t4P * t7 * t2P * k * t5 * t1P * t2 * t1 - 24 * t4P * t7 * t2P * k * t5 * t1P * t2 * t3 * t4 - 4 * t4P * (t1 * t1) * t4
                                                                                   * t2P * t24P * t7 * t3P - 18 * t5 * t5 * t1P * t1 * (t3 * t3) * t4 * t2P * k * t4P * (t2 * t2) - 2 * t6 * t6 * t23kP * (t1 * t1) * (t4
                                                                                                                                                                                                                       * t4) * (t2 * t2) * t3 + 8 * t4P * t7 * t3 * t2P * (t2 * t2) * t5 * t1P * t1 + 2 * t6 * t22kP * t1 * (t4 * t4) * k * t5 * t1P + 8 * t4P * t7
                                                                                   * (t3 * t3) * t2P * t23P * t5 * t1P * t4 + 4 * t4P * t7 * (t3 * t3) * t2P * t24P * t5 * t1P * t1 + 8 * t4P * t7 * t3 * t2P * t24P
                                                                                   * t5 * t1P * t4 * t1 + 2 * t5 * t1P * (t3 * t3) * t2P * k * k * t4P * t7 * t1 * (t2 * t2) - 12 * t4P * t7 * t7 * t3 * t4 * t2P * k * t3P * t2 - 3
                                                                                   * t6 * t22kP * t23P * (t3 * t3) * t4P * (t1 * t1) * t4 - 12 * t4P * t5 * t5 * t2P * k * t1P * t2 - 2 * t4P * t5 * t5 * t2P * k * t1P - 2
                                                                                   * t4P * t5 * t2P * k * t7 * t3P - 4 * t4P * t5 * t5 * t2P * k * t1P * t3 - 2 * t4P * t5 * t5 * t2P * k * t1P * t4 - 4 * t4P * t5 * t2P
                                                                                   * k * t7 * t3P * t1 - 12 * t4P * t5 * t2P * k * t7 * t3P * t2 - 2 * t4P * t5 * t2P * k * t7 * t3P * t4 + 2 * t7 * t3P * t6 * t2P * k * k
                                                                                   * (t2 * t2) * t4P * (t1 * t1) - 8 * t24P * t3 * t4P * t2P * t7 * t3P * t1 - 16 * t4P * t7 * t6 * t22kP * k * t2 * t1 + 2 * t4P * t5
                                                                                   * t3 * t4 * t2P * k * k * t7 * t3P - 6 * t6 * t22kP * t23P * t3 * t5 * t1P - t42kP * (t1 * t1) * t6 * t6 * t2P * k * k * (t3 * t3) - 4 * (t2
                                                                                                                                                                                                                             * t2) * t3 * t4P * t2P * t7 * t3P - 8 * t23P * t3 * t4P * t2P * t7 * t3P + 12 * t7 * t3P * t6 * t22kP * k * t3 * (t4 * t4) * (t2
                                                                                                                                                                                                                                                                                                                                                           * t2) * t1 + 4 * t4P * t5 * t5 * t2P * (t2 * t2) * t1P * (t3 * t3) + 6 * t4P * t6 * t6 * t22kP * t1 * t4 * (t2 * t2) + 12 * t5 * t1P * t1
                                                                                   * (t3 * t3) * t4 * t2P * k * t4P * t2 + 4 * t7 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P * t1 + 12 * t5 * t1P * t6 * t22kP * k * (t3 * t3)
                                                                                   * t4 * (t2 * t2) - 8 * (t2 * t2) * t3 * t4P * t2P * t7 * t3P * t1 - 2 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P * (t1 * t1) * t3 * t4 + 4 * t7
                                                                                   * t3P * t2P * k * k * t2 * t4P * t5 * t4 - 4 * t7 * t3P * t2P * k * k * t2 * t4P * t3 * t4 - 4 * t7 * t3P * t2P * k * k * t2 * t5 * t1P
                                                                                   * (t4 * t4) - 4 * t6 * t6 * t23kP * t3 * t4 * (t2 * t2) - 2 * t6 * t6 * t23kP * t3 * (t4 * t4) * (t2 * t2) - 4 * t6 * t6 * t23kP * t3 * (t4 * t4)
                                                                                   * (t2 * t2) * t1 - 8 * t7 * t3P * t2P * k * k * t2 * t4P * t1 * t4 + 4 * t7 * t7 * t3P * t2P * k * k * t2 * t4P * (t1 * t1) * t4 + 4 * t5 * t5 * t1P
                                                                                   * t24P * t2P * t4P * (t3 * t3) * t4 - 8 * t5 * t1P * t23P * t2P * t4P * t4 + 12 * t4P * t5 * t6 * t22kP * (t2 * t2) * t1 * t3
                                                                                   + 12 * t4P * t5 * t6 * t22kP * t23P * t1 * t3 - 8 * t7 * t7 * t3P * t2P * k * t23P * t4P + 4 * t6 * t6 * t23kP * k * t1 * t2 * (t3 * t3)
                                                                                   + 2 * t6 * t6 * t23kP * k * (t1 * t1) * t2 * (t3 * t3) + 8 * t6 * t6 * t23kP * k * t1 * t2 * (t3 * t3) * t4 + 4 * t6 * t6 * t23kP * k * (t1 * t1) * t2
                                                                                   * (t3 * t3) * t4 - 8 * t5 * t1P * t1 * t2P * t23P * t7 * t3P * t3 - 8 * t5 * t1P * t1 * t2P * (t2 * t2) * t7 * t3P * t4 * t3 - 4 * t7 * t3P
                                                                                   * t3 * t2P * k * t4P * t6 * t1 + 4 * t4P * t7 * t4 * t2P * (t2 * t2) * t5 * t1P - 2 * t4P * t7 * t7 * (t1 * t1) * t2P * k * t3P * t3 - 18
                                                                                   * t7 * t7 * t3P * t2P * k * (t2 * t2) * t4P * (t1 * t1) + 16 * t5 * t1P * t3 * t2P * k * t23P * t4P + 24 * t5 * t1P * t3 * t2P * k
                                                                                   * t2 * t4P - 4 * t5 * t1P * t1 * t2P * (t2 * t2) * t4P * (t3 * t3) - 3 * t6 * t22kP * t23P * t3 * t7 * t3P + 2 * t6 * t22kP * (t1
                                                                                                                                                                                                                  * t1) * (t4 * t4) * k * t7 * t3P - 12 * t6 * t22kP * t3 * k * (t2 * t2) * t4P * t7 * t4 + 8 * t4P * t7 * t7 * t3 * t2P * t24P * t3P * t1
                                                                                   * t4 + 16 * t4P * t7 * t3 * t2P * t23P * t5 * t1P * t4 + 8 * t4P * t7 * t3 * t2P * t24P * t5 * t1P * t1 - 4 * t5 * t1P * t1 * t2P
                                                                                   * (t2 * t2) * t7 * t3P * t3 + 8 * t6 * t22kP * k * t2 * t5 * t1P + 2 * t4P * t3 * t4 * t6 * t2P * k * k * t7 * t3P + 36 * t7 * t3P * t1 * t4
                                                                                   * t2P * k * t4P * (t2 * t2) - 6 * t6 * t22kP * t23P * t3 * t4P - 8 * t4P * t7 * t7 * t3 * t4 * t2P * k * t3P * t23P - 6 * t6 * t22kP
                                                                                   * t4 * (t2 * t2) * t5 * t1P - 3 * t6 * t22kP * (t4 * t4) * (t2 * t2) * t5 * t1P - 3 * t6 * t22kP * (t4 * t4) * (t2 * t2) * t7 * t3P + 6 * t6
                                                                                   * t22kP * (t4 * t4) * k * (t2 * t2) * t7 * t3P + 6 * t6 * t22kP * t1 * t23P * t4P * t7 * t4 + 6 * t6 * t22kP * t3 * (t2 * t2) * t4P
                                                                                   * t7 + 6 * t6 * t6 * t22kP * t3 * (t2 * t2) * t4P - 3 * t6 * t22kP * (t3 * t3) * (t2 * t2) * t4P * (t1 * t1) * t4 - 36 * t5 * t1P * t1 * t3
                                                                                   * t4 * t6 * t2P * k * t4P * (t2 * t2) - 16 * t5 * t1P * t1 * t3 * t4 * t6 * t2P * k * t4P * t23P + 2 * t4P * t2P * k * t5 * t1P * t4
                                                                                   * t1 + 8 * (t2 * t2) * t7 * t3P * t2P * t4P * t5 * t1 * t4 - 8 * t6 * t22kP * t1 * t3 * k * t4P * t5 * t4 - 8 * t6 * t6 * t22kP * t1 * t3 * k
                                                                                   * t4P * t4 - 8 * t6 * t22kP * t1 * t3 * k * t4P * t7 * t4 + 4 * t6 * t22kP * (t1 * t1) * t3 * k * t4P * t4 + 4 * t6 * t22kP * t1 * t3 * k * t7
                                                                                   * t3P * (t4 * t4) + 4 * t6 * t22kP * t1 * t3 * k * t5 * t1P * (t4 * t4) + 4 * t6 * t22kP * t1 * (t3 * t3) * k * t4P * t4 - 12 * t7 * t3P * t6
                                                                                   * t22kP * t1 * t4 * t23P + 4 * t4P * (t1 * t1) * t4 * t6 * t2P * t24P * t7 * t3P - 12 * t5 * t1P * t2P * k * t2 * t4P * t7 - 4 * t5
                                                                                   * t1P * t3 * t2P * k * k * t4P - 12 * t7 * t7 * t3P * t2P * k * t2 * t4P * t3 - 4 * t6 * t22kP * t3 * k * t4P * t7 - 4 * t6 * t6 * t22kP
                                                                                   * t3 * k * t4P - 4 * t6 * t22kP * t3 * k * t4P * t5 - 36 * t4P * t5 * t1 * t2P * k * (t2 * t2) * t7 * t3P * t3 + 4 * (t2 * t2) * t7 * t7 * t3P
                                                                                   * t2P * t4P * t3 + 36 * t4P * t1 * t3 * t2P * k * t7 * t3P * (t2 * t2) + 8 * (t2 * t2) * t7 * t7 * t3P * t2P * t4P * t1 + 4 * t6 * t22kP
                                                                                   * t1 * (t3 * t3) * k * t5 * t1P * t4 + 3 * t6 * t6 * t22kP * t4 * (t2 * t2) * t4P + 3 * t6 * t22kP * t4 * (t2 * t2) * t4P * t5 - 4 * t5 * t1P
                                                                                   * t3 * t2P * k * k * t4P * (t2 * t2) - 8 * t4P * t6 * t2P * k * t23P * t5 * t1P - 8 * t4P * t6 * t2P * k * t23P * t7 * t3P + 18
                                                                                   * t7 * t3P * (t1 * t1) * t4 * t2P * k * t4P * (t2 * t2) + 8 * t7 * t3P * (t1 * t1) * t4 * t2P * k * t4P * t23P - 4 * t6 * t22kP * t1
                                                                                   * t4 * k * t4P * t5 - 36 * t5 * t5 * t1P * t1 * t3 * t2P * k * (t2 * t2) * t4P + 8 * t4P * t1 * t4 * t6 * t2P * (t2 * t2) * t7 * t3P * t3 - 6
                                                                                   * t7 * t3P * t6 * t22kP * (t1 * t1) * t4 * (t2 * t2) + 4 * t4P * t4 * t6 * t2P * (t2 * t2) * t5 * t1P + 8 * t4P * t4 * t6 * t2P * (t2
                                                                                                                                                                                                                      * t2) * t5 * t1P * t3 - 2 * t5 * t1P * (t3 * t3) * t2P * k * t4P * t7 - 2 * t7 * t3P * t3 * t2P * k * k * t4P * (t1 * t1) - 6 * t6 * t22kP
                                                                                   * k * (t2 * t2) * t4P * t7 * (t3 * t3) + 8 * t4P * t3 * t2P * k * t7 * t3P * t23P - 6 * t4P * t5 * t6 * t22kP * k * (t1 * t1) * t4 * (t2
                                                                                                                                                                                                                         * t2) - 12 * t4P * t5 * t6 * t22kP * k * (t1 * t1) * t4 * (t2 * t2) * t3 + 4 * t6 * t22kP * (t1 * t1) * t4 * k * t7 * t3P + 8 * t7 * t3P * t1
                                                                                   * (t4 * t4) * t2P * k * t5 * t1P * t23P + 4 * t7 * t3P * t4 * t2P * k * t5 * t1P - 16 * t4P * t5 * t1 * t2P * k * t23P * t7 * t3P
                                                                                   * t3 + 12 * t7 * t3P * t2P * k * t2 * t4P * (t1 * t1) + 2 * t7 * t3P * t6 * t2P * k * k * (t2 * t2) * t4P * t4 - 18 * t7 * t7 * t3P * t2P
                                                                                   * k * (t2 * t2) * t4P * t3 + 4 * t4P * t7 * t2P * (t2 * t2) * t5 * t1P - 3 * t7 * t3P * t6 * t22kP * (t1 * t1) * (t2 * t2) - 3 * t6 * t22kP
                                                                                   * (t1 * t1) * (t2 * t2) * t7 * t3P * (t4 * t4) - 8 * t4P * t7 * t6 * t22kP * k * (t1 * t1) * t4 * t2 + 12 * t6 * t22kP * t1 * k * (t2 * t2) * t7
                                                                                   * t3P + 3 * t6 * t22kP * t23P * (t3 * t3) * t4P * t7 * t4 + 2 * t7 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P * t3 - 4 * t7 * t3P * t2P
                                                                                   * k * k * t2 * t4P * t4 + 12 * t6 * t22kP * t23P * t3 * t4P * t5 * t1 * t4 + 8 * t7 * t3P * t6 * t22kP * k * (t1 * t1) * t3 * t2 + 2 * t7 * t3P
                                                                                   * t2P * k * k * (t2 * t2) * t4P * t5 * (t1 * t1) * t4 + 4 * t7 * t7 * t3P * t2P * k * k * t2 * t4P * (t1 * t1) * t3 * t4 - 8 * t7 * t3P * t2P
                                                                                   * k * k * t2 * t5 * t1P * t4 * t1 + 4 * t4P * t5 * t5 * (t3 * t3) * t4 * t2P * (t2 * t2) * t1P - 2 * t4P * t1 * t2P * k * k * t5 * t1P * t4
                                                                                   + 4 * t24P * t3 * (t1 * t1) * t2P * t4P * t7 * t7 * t3P - 8 * t6 * t22kP * (t3 * t3) * k * t4P * t5 * t2 * t4 + 24 * t7 * t3P * t3 * t4
                                                                                   * t2P * k * t5 * t1P * t2 + 8 * t6 * t22kP * k * t2 * t7 * t3P + 16 * t6 * t22kP * k * t2 * t7 * t3P * t1 + 6 * t6 * t22kP * k * (t2 * t2)
                                                                                   * t7 * t3P - 6 * t6 * t22kP * t1 * t23P * t4P - 6 * t6 * t22kP * (t3 * t3) * (t2 * t2) * t5 * t1P * t4 * t1 - 8 * t6 * t22kP * t1 * t3
                                                                                   * k * t4P * t7 - 8 * t6 * t6 * t22kP * t1 * t3 * k * t4P - 8 * t6 * t22kP * t1 * t3 * k * t4P * t5 + 18 * t5 * t1P * t1 * t3 * (t4 * t4) * t2P
                                                                                   * k * t7 * t3P * (t2 * t2) - 12 * t6 * t22kP * k * (t2 * t2) * t4P * t7 * t1 * (t3 * t3) + 4 * t5 * t5 * t1P * t3 * t2P * k * k * t4P * t1 * (t2
                                                                                                                                                                                                                                 * t2) + 2 * t7 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P * (t1 * t1) * t3 - 4 * t4P * t4 * t2P * (t2 * t2) * t5 * t1P - 8 * t4P * t4 * t2P
                                                                                   * (t2 * t2) * t5 * t1P * t3 + 3 * t6 * t22kP * (t1 * t1) * (t3 * t3) * t23P * t4P * t5 + 6 * t6 * t22kP * (t1 * t1) * t3 * t23P * t4P
                                                                                   * t5 + 6 * t6 * t22kP * t1 * (t3 * t3) * t23P * t4P * t5 + 3 * t6 * t22kP * (t1 * t1) * (t3 * t3) * (t2 * t2) * t4P * t5 + 6 * t6 * t22kP
                                                                                   * (t1 * t1) * t3 * k * (t2 * t2) * t7 * t3P * (t4 * t4) + 2 * t6 * t22kP * (t1 * t1) * t3 * k * t7 * t3P * (t4 * t4) + 4 * t4P * t2P * t24P
                                                                                   * t5 * t3 * t7 * t3P + 4 * t6 * t6 * t23kP * k * t4 * t2 + 6 * t4P * t6 * t6 * t22kP * (t2 * t2) * t1 - 4 * t5 * t1P * t3 * t2P * k * t4P
                                                                                   * t7 - 3 * t6 * t22kP * t4 * (t2 * t2) * t4P - 4 * t6 * t22kP * (t1 * t1) * t3 * k * t4P * t7 - 4 * t6 * t6 * t22kP * (t1 * t1) * t3 * k * t4P - 4
                                                                                   * t6 * t6 * t22kP * t1 * (t3 * t3) * k * t4P - 4 * t6 * t22kP * t1 * (t3 * t3) * k * t4P * t5 - 4 * t6 * t22kP * (t1 * t1) * t3 * k * t4P
                                                                                   * t5 - 4 * t6 * t22kP * t1 * (t3 * t3) * k * t4P * t7 - 4 * (t2 * t2) * t7 * t3P * t2P * t4P * t4 + 4 * t7 * t7 * t3P * t3 * t4 * t2P * (t2
                                                                                                                                                                                                                            * t2) * t4P * (t1 * t1) + 2 * t4P * (t1 * t1) * t6 * t2P * k * k * t7 * t3P + 4 * t4P * t1 * t6 * t2P * k * k * t7 * t3P * t4 + 2 * t5 * t1P
                                                                                   * (t3 * t3) * t4 * t6 * t2P * k * k * t4P * (t2 * t2) + 4 * t5 * t1P * t3 * t4 * t6 * t2P * k * k * t4P * t1 * (t2 * t2) + 2 * t5 * t1P * (t3
                                                                                                                                                                                                                              * t3) * t2P * k * k * t4P * t7 * t1 + 2 * t5 * t5 * t1P * (t3 * t3) * t2P * k * k * t4P * (t2 * t2) + 2 * t5 * t1P * (t3 * t3) * t2P * k * k
                                                                                   * t4P * t6 * (t2 * t2) + 8 * t7 * t3P * t3 * t4 * t2P * (t2 * t2) * t4P * t5 * t1 + 4 * t4P * (t1 * t1) * t4 * t6 * t2P * (t2 * t2) * t7 * t3P
                                                                                   * t3 + 2 * t5 * t5 * t1P * (t3 * t3) * t2P * k * k * t4P * t1 + 4 * t5 * t5 * t1P * t3 * t2P * k * k * t4P * (t2 * t2) + 4 * t5 * t5 * t1P * (t3
                                                                                                                                                                                                                                 * t3) * t2P * k * k * t4P * t2 + 4 * t5 * t1P * t3 * t2P * k * k * t4P * t6 * (t2 * t2) + 12 * t5 * t1P * t3 * t2P * k * t2 * t7 * t3P - 36
                                                                                   * t5 * t1P * t3 * t2P * k * (t2 * t2) * t4P * t6 + 4 * t5 * t1P * t3 * t2P * k * k * t4P * t7 * (t2 * t2) + 4 * t7 * t3P * t3 * t4 * t2P
                                                                                   * (t2 * t2) * t4P * t5 * (t1 * t1) + 4 * (t2 * t2) * t7 * t3P * t2P * t4P * t5 * (t1 * t1) * t4 - 8 * t4P * t5 * t5 * t4 * t2P * k * t23P
                                                                                   * t1P + 4 * t7 * t3P * t24P * t3 * t6 * t2P * t4P * (t1 * t1) * t4 + 4 * t6 * t22kP * (t1 * t1) * t3 * k * t7 * t3P * t4 - 4 * t5 * t1P
                                                                                   * t2P * k * k * t4P * t2 + 4 * t5 * t1P * t2P * k * k * t4P * t7 * t2 - 16 * t23P * t3 * t4P * t2P * t7 * t3P * t1 - 24 * t7 * t3P
                                                                                   * t6 * t2P * k * t2 * t4P * t1 * t4 - 12 * t5 * t5 * t1P * t1 * (t3 * t3) * t2P * k * t2 * t4P + 4 * (t2 * t2) * t7 * t7 * t3P * t2P * t4P
                                                                                   * t4 + 8 * t4P * t2P * t23P * t5 * t3 * t7 * t3P * (t1 * t1) - 16 * t4P * t5 * t1 * t2P * k * t23P * t7 * t3P + 8 * t4P * t5 * t23P
                                                                                   * t4 * t2P * t7 * t3P - 2 * t5 * t1P * (t3 * t3) * t2P * k * k * t4P * (t2 * t2) - t42kP * t5 * t5 * t2P * k * k * (t2 * t2) * (t1 * t1)
                                                                                   + 18 * t42kP * t5 * t5 * t2P * k * t3 * (t2 * t2) - 4 * t23P * t7 * t7 * t32kP * t2P * (t1 * t1) - 16 * t42kP * t5 * t3 * t2P * k * k
                                                                                   * t2 * t7 * t1 + 2 * t42kP * t5 * t2P * k * k * (t2 * t2) - 4 * t5 * t5 * t12kP * t2P * (t2 * t2) * t3 + 36 * t42kP * t7 * t2P * k * t5 * (t1
                                                                                                                                                                                                                              * t1) * (t2 * t2) * t3 - 8 * t24P * t3 * t1 * t2P * t42kP * t7 * t7 - t5 * t1P * t6 * t22kP * k * k * t2 * t1 - t42kP * t5 * t5 * (t1 * t1)
                                                                                   * t2P * k * k - 8 * t42kP * t5 * (t3 * t3) * t2P * k * k * t2 * t6 * t1 - 2 * t5 * t5 * t12kP * t24P * t2P * (t3 * t3) * (t4 * t4) - 4 * t42kP
                                                                                   * t2P * t24P * t5 * t5 * t3 + 16 * t42kP * t1 * t6 * t6 * t2P * k * t23P * t3 + 16 * t42kP * t1 * t6 * t2P * k * t23P * t5 + 8 * (t2
                                                                                                                                                                                                                     * t2) * t3 * t42kP * t2P * t6 * (t1 * t1) - 4 * t42kP * t7 * (t1 * t1) * t2P * k * k * t6 * t2 - 4 * t42kP * t1 * t6 * t6 * t2P * k * k * t3
                                                                                   * (t2 * t2) + 16 * t23P * t42kP * t2P * t6 * t1 + 6 * t42kP * (t1 * t1) * t6 * t6 * t2P * k * t2 + 18 * t42kP * t5 * t2P * k * t7 * (t3
                                                                                                                                                                                                                        * t3) * (t2 * t2) - t7 * t7 * t32kP * t2P * k * k * (t2 * t2) * (t1 * t1) * (t4 * t4) + t5 * t5 * t12kP * (t3 * t3) * t2P * k - 4 * t42kP
                                                                                   * t5 * t2P * k * k * t2 * t6 + 2 * t42kP * (t1 * t1) * t6 * t2P * k * k * (t3 * t3) * (t2 * t2) + 8 * t42kP * t5 * t3 * t2P * k * k * t2 * (t1 * t1) - 2
                                                                                   * t5 * t5 * t12kP * t4 * t2P * k * k + 16 * t23P * t3 * t42kP * t2P * t7 * (t1 * t1) - 24 * t42kP * t3 * t2P * k * t5 * t2 + 8 * (t2 * t2)
                                                                                   * t3 * t42kP * t2P * t7 * (t1 * t1) + t4P * t7 * (t1 * t1) * (t3 * t3) * t22kP * k * k * t6 * t2 + 4 * t4P * t7 * t1 * t3 * t4 * t22kP * k
                                                                                   * k * t6 * t2 + 4 * t7 * t7 * t32kP * (t1 * t1) * (t4 * t4) * t2P * k * t23P + 2 * t6 * t6 * t23kP * t3 * k * (t4 * t4) + 18 * t42kP * t1 * (t3
                                                                                                                                                                                                                                * t3) * t2P * k * (t2 * t2) - 2 * t42kP * (t3 * t3) * t6 * t6 * t2P * k * k * t2 + 8 * t7 * t7 * t32kP * (t1 * t1) * t4 * t2P * k * t23P + 18
                                                                                   * t7 * t7 * t32kP * t2P * k * (t2 * t2) * t4 + 8 * t42kP * t5 * t3 * t2P * k * k * (t2 * t2) * t1 + 0.9e1 * t42kP * t7 * t7 * t2P * k * (t2
                                                                                                                                                                                                                            * t2) + t4P * t5 * t22kP * k * k * t2 * t6 - 8 * (t2 * t2) * t7 * t7 * t32kP * t2P * t1 * t4 - 8 * t42kP * t1 * t3 * t2P * k * t6 - 2 * t42kP
                                                                                   * t5 * t5 * (t3 * t3) * t2P * k * k * t2 + 18 * t42kP * t6 * t2P * k * (t2 * t2) * t7 * (t3 * t3) + 24 * t42kP * t7 * t2P * k * t5 * t1 * t2 * (t3
                                                                                                                                                                                                                                   * t3) + 2 * t42kP * t7 * (t1 * t1) * (t3 * t3) * t2P * k * k * (t2 * t2) + 8 * (t2 * t2) * (t3 * t3) * t42kP * t2P * t6 * t1 - 8 * t42kP * (t1
                                                                                                                                                                                                                                                                                                                                                                               * t1) * t6 * t2P * k * t23P * (t3 * t3) + 8 * t42kP * t5 * (t3 * t3) * t2P * k * k * t2 * t1 - 2 * t42kP * t2P * (t2 * t2) * t5 * t5 * (t1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       * t1) - 8 * t42kP * t3 * t6 * t2P * k * k * t7 * t2 - 4 * t42kP * t2P * (t2 * t2) * t5 * (t3 * t3) * t6 * (t1 * t1) - 8 * t42kP * t5 * t1 * t2P
                                                                                   * k * k * t6 * t3 + 4 * t42kP * (t1 * t1) * (t3 * t3) * t2P * k * t23P + 4 * t6 * t6 * t23kP * t1 * t4 * k + 8 * t42kP * t3 * t6 * t2P * k * k
                                                                                   * t2 - t7 * t3P * t22kP * k * k * t6 - 4 * t23P * (t3 * t3) * (t1 * t1) * t2P * t42kP * t7 * t7 - t4P * (t3 * t3) * t4 * t22kP * k * k
                                                                                   * t2 * t6 + 8 * t42kP * t7 * t7 * t2P * k * t23P * t3 - t4P * (t1 * t1) * t22kP * k * k * t6 * t2 + 0.9e1 * t5 * t5 * t12kP * t2P * k
                                                                                   * (t2 * t2) + 2 * t4P * t5 * (t1 * t1) * t22kP * k * k * t6 * t3 - 2 * t5 * t5 * t12kP * (t3 * t3) * t2P * k * k * t2 - 2 * t5 * t1P * t4 * t6 * t22kP
                                                                                   * k * k - 8 * t23P * t7 * t7 * t32kP * t2P * t1 * (t4 * t4) + 4 * t6 * t6 * t23kP * t1 * (t3 * t3) * k * t4 - 2 * t7 * t3P * t3 * (t4 * t4) * t22kP
                                                                                   * k * k * t6 * t1 - 4 * t24P * t3 * (t1 * t1) * t2P * t42kP * t7 * t7 + 8 * t42kP * t6 * t2P * k * t23P * t7 * (t3 * t3) + 2 * t4P * t5
                                                                                   * t1 * t22kP * k * k * t6 * t4 + 12 * t42kP * t7 * t7 * t2P * k * t1 * t2 * (t3 * t3) + 4 * t24P * t42kP * t2P * t5 * (t1 * t1) - 2 * t24P
                                                                                   * (t1 * t1) * t2P * t42kP * t7 * t7 - 2 * t5 * t5 * t12kP * t3 * t2P * k * k * (t2 * t2) + t4P * t7 * (t1 * t1) * (t3 * t3) * t4 * t22kP
                                                                                   * k * k * t6 * t2 + 6 * t42kP * t7 * t7 * t2P * k * (t1 * t1) * t2 * (t3 * t3) - 2 * t42kP * (t1 * t1) * t2P * k * k * t3 * (t2 * t2) + 2 * t4P
                                                                                   * t7 * t1 * t22kP * k * k * t6 * t2 + 2 * t42kP * t7 * (t1 * t1) * t2P * k * t5 * (t3 * t3) - 2 * t42kP * (t1 * t1) * t6 * t6 * t2P * k * k * (t2
                                                                                                                                                                                                                                  * t2) * t3 - 2 * t42kP * t2P * (t2 * t2) * t5 * t5 * (t3 * t3) + t42kP * t7 * t7 * (t3 * t3) * t2P * k + 8 * t42kP * t5 * t3 * t2P * k * k
                                                                                   * t2 - 4 * t7 * t7 * t32kP * t2P * k * k * (t2 * t2) * t1 * t4 - t7 * t7 * t32kP * (t4 * t4) * t2P * k * k * (t1 * t1) + 8 * t42kP * t6 * t2P
                                                                                   * k * t23P * t5 + 4 * t42kP * t5 * t2P * k * k * t3 + 4 * t42kP * t1 * t6 * t2P * k * k * (t2 * t2) - 4 * t24P * t1 * t6 * t6 * t2P * t42kP
                                                                                   * (t3 * t3) - 8 * t42kP * t2P * t23P * t5 * t5 * (t3 * t3) * t1 + 16 * t42kP * t1 * t6 * t2P * k * k * t2 * t3 - 8 * t42kP * t2P * t23P
                                                                                   * t5 * t6 + 8 * t24P * t3 * t42kP * t2P * t6 - 4 * t42kP * t5 * t5 * t3 * t2P * k * k * t2 + 6 * t42kP * t6 * t6 * t2P * k * t2 * (t3 * t3)
                                                                                   + 4 * t42kP * (t3 * t3) * t6 * t2P * k * k * t2 + 2 * t4P * t7 * (t1 * t1) * t3 * t4 * t22kP * k * k * t6 - 4 * t42kP * t5 * t5 * (t3 * t3) * t2P
                                                                                   * k * k * t2 * t1 + 2 * t4P * (t1 * t1) * t6 * t6 * t22kP * k * k * t3 + 12 * t42kP * t1 * t2P * k * t2 - t7 * t3P * t6 * t22kP * k * k * t2 * (t1
                                                                                                                                                                                                                                   * t1) + 4 * t7 * t7 * t32kP * t2P * k * t23P * (t4 * t4) + 8 * t42kP * t2P * (t2 * t2) * t5 * t3 * (t1 * t1) - 2 * t42kP * (t1 * t1) * t6
                                                                                   * t6 * t2P * k * k * (t3 * t3) * t2 - 16 * t42kP * t1 * (t3 * t3) * t2P * k * t5 * t23P - 8 * t23P * t7 * t7 * t32kP * t2P * (t1 * t1)
                                                                                   * t4 + 4 * t42kP * t7 * (t1 * t1) * t3 * t2P * k * k - 8 * t42kP * t5 * (t3 * t3) * t2P * k * k * t2 * t7 * t1 - 2 * t42kP * t2P * k * t6 - t7
                                                                                   * t7 * t32kP * t2P * k * k - 4 * (t2 * t2) * t3 * t42kP * t2P - 2 * (t2 * t2) * t42kP * t2P * (t1 * t1) + t42kP * (t3 * t3) * t2P
                                                                                   * k - 2 * t42kP * t2P * (t2 * t2) * t5 * t5 + 4 * t24P * t42kP * t2P * t5 - 4 * (t2 * t2) * t42kP * t2P * t1 - 4 * t42kP * t2P
                                                                                   * t23P * t5 * t5 - 2 * t5 * t5 * t12kP * t24P * t2P - 2 * (t2 * t2) * (t3 * t3) * t42kP * t2P + 2 * t42kP * t5 * t5 * t2P * k * t1
                                                                                   + 2 * t6 * t6 * t23kP * t1 * k - 2 * t24P * t42kP * t7 * t7 * t2P + 16 * t23P * t3 * t42kP * t2P * t7 - 4 * t42kP * t7 * t7 * t1 * t2P
                                                                                   * k * k * t2 - 8 * t42kP * t2P * (t2 * t2) * t5 * (t3 * t3) * t6 * t1 - 4 * (t2 * t2) * (t3 * t3) * (t1 * t1) * t2P * t42kP * t7 * t6 + 6 * t42kP
                                                                                   * t5 * t5 * (t1 * t1) * t2P * k * t2 - 12 * t42kP * t6 * t2P * k * t2 * (t3 * t3) - 2 * t24P * (t1 * t1) * t6 * t6 * t2P * t42kP - 18 * t42kP
                                                                                   * (t1 * t1) * (t3 * t3) * t2P * k * t5 * (t2 * t2) - 16 * t23P * (t3 * t3) * t1 * t2P * t42kP * t7 * t6 - 8 * t23P * t42kP * t6 * t6 * t2P
                                                                                   * t3 - 16 * t42kP * t6 * t2P * k * t23P * t3 - 2 * t42kP * t5 * t2P * k * k * t7 + t4P * (t1 * t1) * t6 * t6 * t22kP * k * k * (t3 * t3)
                                                                                   * t2 - 4 * t7 * t7 * t32kP * t2P * k * k * t2 * t4 + 4 * t42kP * t7 * t1 * t2P * k * k + 2 * t42kP * t7 * (t3 * t3) * t2P * k * k * (t2 * t2) - 2
                                                                                   * t5 * t5 * t12kP * (t3 * t3) * (t4 * t4) * t2P * k * k * t2 + 2 * t42kP * t5 * t2P * k * t7 - 8 * t24P * t3 * t42kP * t7 * t2P * t6 - 2
                                                                                   * t4P * t1 * t6 * t22kP * k * k * t4 * t2 + 4 * t42kP * (t1 * t1) * t6 * t2P * k * k * t3 * (t2 * t2) + 16 * t23P * (t3 * t3) * t42kP * t2P
                                                                                   * t6 * t1 - 2 * t4P * t1 * t22kP * k * k * t6 + 6 * t7 * t7 * t32kP * t2P * k * t2 * (t1 * t1) - 16 * t42kP * t2P * t23P * t5 * t5 * t3
                                                                                   * t1 + 6 * t7 * t7 * t32kP * (t1 * t1) * (t4 * t4) * t2P * k * t2 - 2 * t42kP * t5 * (t3 * t3) * t2P * k * k * (t2 * t2) * t6 + 8 * t24P * t3
                                                                                   * t42kP * t2P * t6 * (t1 * t1) - 4 * t42kP * t5 * t5 * t1 * t2P * k * k * t3 + 8 * t23P * t42kP * t2P * t7 * (t1 * t1) - 4 * t42kP
                                                                                   * t7 * t1 * t2P * k * k * t6 * (t2 * t2) - 2 * t5 * t1P * (t3 * t3) * t4 * t6 * t22kP * k * k - 16 * t42kP * t2P * t23P * t5 * t3 * t6 - 4 * t42kP
                                                                                   * t2P * (t2 * t2) * t5 * (t3 * t3) * t7 + 8 * t42kP * t1 * t6 * t2P * k * k * t3 * (t2 * t2) + 2 * t4P * t5 * t3 * t4 * t22kP * k * k * t6 * (t1
                                                                                                                                                                                                                                 * t1) + t6 * t6 * t23kP * (t3 * t3) * k * (t4 * t4) + t4P * t7 * t4 * t22kP * k * k * t2 * t6 * (t1 * t1) - 2 * t42kP * (t1 * t1) * t2P * k
                                                                                   * k * t3 - t7 * t3P * t3 * t22kP * k * k * t6 + 12 * t42kP * (t1 * t1) * t6 * t2P * k * t2 * t5 * (t3 * t3) - 2 * t42kP * t7 * t7 * t1 * (t3 * t3)
                                                                                   * t2P * k * k + 0.9e1 * t5 * t5 * t12kP * (t4 * t4) * t2P * k * (t2 * t2) - 8 * t23P * t7 * t7 * t32kP * t2P * t4 + 8 * t42kP * (t1
                                                                                                                                                                                                                    * t1) * t6 * t6 * t2P * k * t23P * t3 - 4 * t23P * (t3 * t3) * t42kP * t2P - 2 * t7 * t7 * t32kP * t4 * t2P * k * k * (t1 * t1) - 4 * t42kP
                                                                                   * t2P * (t2 * t2) * t5 * t5 * t3 * (t1 * t1) - 8 * t42kP * t2P * (t2 * t2) * t5 * t3 * t6 - 4 * t42kP * t5 * (t3 * t3) * t2P * k * k * (t2 * t2)
                                                                                   * t6 * t1 - 4 * t42kP * t3 * t2P * k * t5 + 4 * t24P * t42kP * t2P * t7 * (t1 * t1) - 4 * t42kP * t5 * t3 * t2P * k * k * (t2 * t2) * t7
                                                                                   + 36 * t42kP * t7 * t2P * k * t5 * t1 * (t2 * t2) * (t3 * t3) - 4 * t42kP * t2P * (t2 * t2) * t5 * (t3 * t3) * t7 * (t1 * t1) - t42kP * (t3
                                                                                                                                                                                                                            * t3) * t2P * k * k * (t2 * t2) - 4 * t42kP * t7 * t7 * t1 * (t3 * t3) * t2P * k * k * t2 - 2 * t5 * t5 * t12kP * t3 * t2P * k * k + 4 * t42kP
                                                                                   * t3 * t6 * t6 * t2P * k * t1 - 4 * t42kP * t5 * t3 * t2P * k * k * (t2 * t2) * t6 * (t1 * t1) + 0.9e1 * t7 * t7 * t32kP * t2P * k * (t2 * t2) - 8
                                                                                   * t42kP * t2P * t23P * t5 * t7 - 4 * t7 * t7 * t32kP * t2P * k * k * t2 * t1 * (t4 * t4) + 16 * t42kP * t1 * t6 * t2P * k * t23P * t7
                                                                                   * (t3 * t3) + 18 * t5 * t5 * t12kP * t3 * t2P * k * (t2 * t2) * (t4 * t4) - t42kP * t5 * t5 * (t1 * t1) * t2P * k * k * (t3 * t3) - 8 * t23P
                                                                                   * (t3 * t3) * (t1 * t1) * t2P * t42kP * t7 * t6 - 8 * t42kP * t2P * t23P * t5 * t5 * t3 - 36 * t42kP * (t1 * t1) * t3 * t2P * k * t5
                                                                                   * (t2 * t2) + 8 * (t2 * t2) * t3 * t42kP * t2P * t7 - 0.32e2 * t23P * t3 * t1 * t2P * t42kP * t7 * t6 - 4 * t23P * t42kP * t6 * t6
                                                                                   * t2P - 8 * t42kP * t7 * t1 * t2P * k * k * t6 * t2 - 4 * t23P * t7 * t7 * t32kP * t2P * (t4 * t4) + 4 * t42kP * t7 * t1 * (t3 * t3) * t2P
                                                                                   * k * k * (t2 * t2) + 8 * t42kP * t1 * t6 * t2P * k * k * t2 - t5 * t1P * t6 * t22kP * k * k * t2 * t1 * (t4 * t4) + 12 * t42kP * t7 * t2P * k
                                                                                   * t5 * (t1 * t1) * t2 * (t3 * t3) - 8 * (t2 * t2) * t3 * (t1 * t1) * t2P * t42kP * t7 * t6 + 4 * t42kP * t7 * t1 * t2P * k * t6 + 12 * t42kP
                                                                                   * t5 * t5 * t1 * t2P * k * t2 - 16 * t42kP * t5 * t1 * t2P * k * t23P + 2 * t4P * t7 * t1 * (t3 * t3) * t22kP * k * k * t6 * t2 - t7 * t3P
                                                                                   * t6 * t22kP * k * k * t2 * (t1 * t1) * (t4 * t4) + 2 * t4P * t5 * t3 * t4 * t22kP * k * k * t6 - 36 * t42kP * (t1 * t1) * t2P * k * (t2 * t2)
                                                                                   * t7 * t3 + 2 * t42kP * (t3 * t3) * t6 * t2P * k * k + 16 * t24P * t3 * t42kP * t2P * t7 * t1 + 0.9e1 * t42kP * (t1 * t1) * t6 * t6 * t2P
                                                                                   * k * (t2 * t2) * (t3 * t3) - 8 * t42kP * t5 * t2P * k * k * t2 * t6 * t1 - 2 * t4P * t3 * t22kP * k * k * t6 - 12 * t42kP * (t1 * t1) * t6 * t2P
                                                                                   * k * t2 - 4 * t4P * t3 * t4 * t6 * t22kP * k * k * t1 - 2 * t5 * t1P * t3 * (t4 * t4) * t6 * t22kP * k * k - 0.72e2 * t42kP * t1 * t2P * k
                                                                                   * (t2 * t2) * t7 * t3 - 2 * (t2 * t2) * t7 * t7 * t32kP * t2P - 8 * t42kP * t2P * t24P * t5 * t6 * t1 + 4 * t5 * t5 * t12kP * (t3 * t3)
                                                                                   * t2P * k * t23P * (t4 * t4) + 2 * t42kP * t7 * t7 * t1 * t2P * k + t42kP * t5 * t5 * t2P * k * (t3 * t3) + t6 * t6 * t23kP * k - 2 * t42kP
                                                                                   * t7 * (t1 * t1) * (t3 * t3) * t2P * k * k * t6 * (t2 * t2) + 18 * t42kP * t7 * t7 * t2P * k * (t1 * t1) * t3 * (t2 * t2) - 16 * t42kP * t2P
                                                                                   * t23P * t5 * (t3 * t3) * t7 * t1 - 24 * t42kP * t1 * t6 * t2P * k * t2 - 2 * t7 * t3P * t6 * t22kP * k * k * t2 * (t1 * t1) * t3 * t4 - 16 * t42kP
                                                                                   * t5 * t3 * t2P * k * k * t2 * t6 * t1 - 2 * t42kP * t2P * t24P * t5 * t5 * (t3 * t3) + 4 * t42kP * t5 * t2P * k * t6 * (t1 * t1) * t3 + 36
                                                                                   * t42kP * t5 * t5 * t1 * t2P * k * (t2 * t2) * t3 - t42kP * t7 * t7 * (t1 * t1) * (t3 * t3) * t2P * k * k - 2 * t42kP * t5 * t5 * t2P * k
                                                                                   * k * t2 * (t1 * t1) - 4 * t5 * t5 * t12kP * t4 * t2P * (t2 * t2) * (t3 * t3) + 12 * t42kP * t6 * t6 * t2P * k * t2 * t3 - 8 * t42kP * t2P
                                                                                   * t23P * t5 * (t3 * t3) * t6 * (t1 * t1) - 2 * t42kP * t5 * t5 * t1 * t2P * k * k - 4 * t42kP * t2P * t24P * t5 * (t3 * t3) * t6 + 24 * t7
                                                                                   * t7 * t32kP * t1 * t4 * t2P * k * t2 - 2 * t5 * t1P * t3 * t22kP * k * k * t6 * t1 - 2 * t42kP * t3 * t6 * t6 * t2P * k * k * (t2 * t2) + 8 * t42kP
                                                                                   * t7 * t1 * t3 * t2P * k * k - 4 * t42kP * t2P * (t2 * t2) * t5 * t5 * t1 - 2 * t4P * t3 * t4 * t22kP * k * k * t2 * t6 - t7 * t3P * (t4 * t4)
                                                                                   * t22kP * k * k * t6 * (t1 * t1) + 4 * t42kP * t5 * (t3 * t3) * t2P * k * k * t2 * (t1 * t1) + 16 * t42kP * t2P * t23P * t5 * t3 - 2 * t7
                                                                                   * t3P * t4 * t22kP * k * k * t6 + t4P * (t3 * t3) * t4 * t6 * t6 * t22kP * k * k * (t1 * t1) * t2 + 6 * t5 * t5 * t12kP * (t4 * t4) * t2P
                                                                                   * k * t2 - 8 * t5 * t5 * t12kP * t23P * t2P * t3 * (t4 * t4) - 8 * t42kP * t5 * t3 * t2P * k * k * t2 * t6 * (t1 * t1) - 6 * t5 * t1P * t6 * t22kP
                                                                                   * t1 * (t2 * t2) * t3 - 2 * t4P * t5 * t5 * t2P * k * t1P * (t3 * t3) * t4 - 18 * t7 * t3P * t3 * t4 * t6 * t2P * k * t4P * (t1 * t1) * (t2
                                                                                                                                                                                                                            * t2) - 2 * t4P * t3 * t4 * t6 * t2P * k * t7 * t3P + 4 * t4P * t6 * t2P * (t2 * t2) * t5 * t1P * (t3 * t3) - 36 * t5 * t5 * t1P * t3 * t2P
                                                                                   * k * (t2 * t2) * t4P - 12 * t5 * t5 * t1P * (t3 * t3) * t2P * k * t2 * t4P - 12 * t5 * t1P * (t3 * t3) * t2P * k * t2 * t4P * t7 + 2 * t6
                                                                                   * t6 * t23kP * k * (t4 * t4) * t2 - 2 * t4P * t1 * t2P * k * k * t5 * t1P - 4 * t4P * t1 * t2P * k * k * t7 * t3P - 16 * t4P * t5 * t6 * t22kP
                                                                                   * k * t1 * (t3 * t3) * t2 + 4 * t6 * t22kP * t1 * t4 * k * t5 * t1P + 8 * t6 * t22kP * t1 * t4 * k * t7 * t3P - 16 * t4P * t6 * t6 * t22kP * k
                                                                                   * t1 * (t3 * t3) * t4 * t2 - 8 * t4P * t6 * t6 * t22kP * k * (t1 * t1) * (t3 * t3) * t4 * t2 + 18 * t5 * t1P * (t4 * t4) * t2P * k * (t2 * t2) * t7
                                                                                   * t3P - 16 * t4P * t5 * t6 * t22kP * k * t2 * t1 - 16 * t4P * t5 * t6 * t22kP * k * t2 * t3 - 0.32e2 * t4P * t5 * t6 * t22kP * k * t2
                                                                                   * t1 * t3 - 6 * t4P * t5 * t6 * t22kP * k * (t2 * t2) - 12 * t4P * t5 * t6 * t22kP * k * (t2 * t2) * t1 - 12 * t4P * t5 * t6 * t22kP * k * (t2
                                                                                                                                                                                                                               * t2) * t3 - 24 * t4P * t5 * t6 * t22kP * k * (t2 * t2) * t1 * t3 - 6 * t6 * t6 * t22kP * t4 * k * (t2 * t2) * t4P - 16 * t7 * t3P * t6 * t2P
                                                                                   * k * t23P * t4P * t1 * t3 - 2 * t4P * (t1 * t1) * t2P * k * k * t7 * t3P + 3 * t6 * t22kP * t23P * t4P * t7 - 3 * t6 * t22kP * t23P
                                                                                   * t5 * t1P - 3 * t6 * t22kP * t23P * t7 * t3P - 6 * t4P * t6 * t22kP * (t1 * t1) * (t2 * t2) * t3 - 16 * t4P * t3 * t4 * t6 * t2P
                                                                                   * k * t23P * t5 * t1P - 36 * t4P * t3 * t4 * t6 * t2P * k * (t2 * t2) * t5 * t1P - 18 * t4P * (t3 * t3) * t4 * t6 * t2P * k * (t2 * t2)
                                                                                   * t5 * t1P + 8 * t7 * t3P * t3 * (t4 * t4) * t2P * k * t5 * t1P * t23P + 4 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P * t5 * t1 * t3 + 4
                                                                                   * t7 * t3P * t2P * k * k * (t2 * t2) * t4P * t5 * t1 + 4 * t4P * t7 * t7 * t3 * t2P * t24P * t3P * t4 + 8 * t6 * t22kP * (t4 * t4) * k
                                                                                   * t2 * t7 * t3P + 8 * t6 * t22kP * (t4 * t4) * k * t2 * t5 * t1P + 6 * t4P * t6 * t6 * t22kP * t1 * (t3 * t3) * (t2 * t2) + 12 * t6 * t22kP
                                                                                   * t1 * (t3 * t3) * k * (t2 * t2) * t5 * t1P * t4 + 8 * t5 * t1P * t1 * (t3 * t3) * t4 * t2P * k * t4P * t23P + 36 * t5 * t1P * t1 * t3 * t4
                                                                                   * t2P * k * t7 * t3P * (t2 * t2) + 18 * t5 * t1P * t1 * (t3 * t3) * t4 * t2P * k * t4P * (t2 * t2) - 16 * t4P * t1 * t4 * t2P * t23P
                                                                                   * t7 * t3P - 6 * t6 * t22kP * t3 * (t2 * t2) * t5 * t1P - 3 * t6 * t22kP * t3 * (t2 * t2) * t7 * t3P - 3 * t6 * t22kP * (t3 * t3) * (t2
                                                                                                                                                                                                                        * t2) * t4P - 3 * t6 * t22kP * (t3 * t3) * (t2 * t2) * t5 * t1P - 12 * t6 * t22kP * t3 * (t2 * t2) * t5 * t1P * t4 - 6 * t6 * t22kP * (t3
                                                                                                                                                                                                                                                                                                                                                               * t3) * (t2 * t2) * t4P * t1 - 6 * t6 * t22kP * t3 * (t2 * t2) * t7 * t3P * t1 - 6 * t6 * t22kP * t3 * (t2 * t2) * t7 * t3P * t4 - 18 * t4P
                                                                                   * t7 * t2P * k * t5 * t1P * (t2 * t2) * t1 - 18 * t4P * t7 * t2P * k * t5 * t1P * (t3 * t3) * (t2 * t2) * t1 + 12 * t6 * t22kP * (t1 * t1)
                                                                                   * k * (t2 * t2) * t7 * t3P * t4 + 12 * t6 * t22kP * t1 * k * (t2 * t2) * t4P * t4 - 8 * t7 * t7 * t3P * t2P * k * t23P * t4P * (t1 * t1)
                                                                                   * t4 - 2 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P - 2 * t7 * t3P * (t1 * t1) * t2P * k * t4P * t6 - t6 * t6 * t23kP * (t3 * t3) * (t2
                                                                                                                                                                                                                        * t2) * (t1 * t1) - 8 * t4P * t5 * (t1 * t1) * t2P * k * t23P * t7 * t3P * t3 + 8 * t4P * t2P * k * t23P * t5 * t1P + 18 * t4P * t2P
                                                                                   * k * (t2 * t2) * t5 * t1P + 16 * t6 * t22kP * t1 * t4 * k * t2 * t5 * t1P + 8 * t6 * t22kP * t1 * (t4 * t4) * k * t2 * t5 * t1P + 16 * t6 * t22kP
                                                                                   * (t1 * t1) * t4 * k * t2 * t7 * t3P + 2 * t7 * t3P * t2P * k * k * (t2 * t2) * t4P * t5 * (t1 * t1) * t3 * t4 - 12 * t42kP * t6 * t2P * k * t2
                                                                                   + 18 * t42kP * t7 * t2P * k * t5 * (t1 * t1) * (t2 * t2) + 8 * t42kP * t7 * (t1 * t1) * t3 * t2P * k * k * t2 - t42kP * t5 * t5 * (t3 * t3)
                                                                                   * t2P * k * k * (t2 * t2) * (t1 * t1) - 2 * t7 * t7 * t32kP * t1 * t2P * k * k - 2 * t42kP * t5 * t5 * t2P * k * k * t2 - 2 * t4P * t3 * t4 * t6
                                                                                   * t22kP * k * k * (t1 * t1) * t2 - 2 * t4P * (t1 * t1) * t22kP * k * k * t6 * t3 + t4P * t5 * t22kP * k * k * t6 + 8 * t42kP * t2P * t23P
                                                                                   * t5 * (t3 * t3) - 8 * t42kP * t2P * (t2 * t2) * t5 * t3 * t7 * (t1 * t1) + 18 * t7 * t7 * t32kP * (t1 * t1) * t4 * t2P * k * (t2 * t2) + 2 * t4P
                                                                                   * t5 * t1 * t22kP * k * k * t6 - 4 * t42kP * t7 * t1 * (t3 * t3) * t2P * k * k * t5 + 8 * t42kP * t1 * t6 * t6 * t2P * k * t23P - 16 * t42kP
                                                                                   * (t1 * t1) * t2P * k * t23P * t7 * t3 + 0.9e1 * t42kP * (t1 * t1) * t6 * t6 * t2P * k * (t2 * t2) + 12 * t5 * t5 * t12kP * t3 * t2P * k
                                                                                   * t2 * (t4 * t4) + 2 * t4P * t7 * t3 * t22kP * k * k * t6 * t2 - t42kP * t7 * t7 * (t3 * t3) * t2P * k * k * (t2 * t2) - t4P * (t1 * t1) * t22kP
                                                                                   * k * k * t6 * (t3 * t3) * t2 + 36 * t42kP * t5 * t2P * k * t7 * t3 * (t2 * t2) - 2 * t6 * t2P * (t3 * t3) * t1P * k * t4P * t5 + 4 * t4P * t2
                                                                                   * t5 * t1P * k * k * t6 * t2P - t4P * (t1 * t1) * t22kP * k * k * t6 - 12 * t4P * t2 * (t3 * t3) * t5 * t1P * k * t6 * t2P - 24 * t4P
                                                                                   * t2 * t3 * t5 * t1P * k * t6 * t2P * t4 - 24 * t4P * t2 * t3 * t5 * t1P * k * t6 * t2P + 4 * t6 * t2P * t3 * t1P * k * k * t1 * t4P * t5
                                                                                   * t4 + 2 * t6 * t2P * (t3 * t3) * t1P * k * k * t1 * t4P * t5 * t4 + 2 * t6 * t2P * (t3 * t3) * t1P * k * k * t1 * t4P * t5 + 24 * t42kP * t7
                                                                                   * t2P * k * t5 * t1 * t2 - t7 * t3P * (t1 * t1) * t22kP * k * k * t6 + 2 * t4P * t5 * t22kP * k * k * t2 * t6 * t1 * t4 - 8 * (t2 * t2) * t3 * t1
                                                                                   * t2P * t42kP * t7 * t7 + 6 * t42kP * (t1 * t1) * t2P * k * t2 - 4 * t42kP * t2P * t24P * t5 * t6 * (t1 * t1) + 0.9e1 * t42kP
                                                                                   * t7 * t7 * t2P * k * (t1 * t1) * (t2 * t2) + 2 * t42kP * t7 * (t1 * t1) * (t3 * t3) * t2P * k * k + t4P * t5 * (t1 * t1) * t22kP * k * k * t6
                                                                                   + 12 * t7 * t7 * t32kP * t2P * k * t2 * t1 - 24 * t42kP * (t1 * t1) * t6 * t2P * k * t2 * t3 - 8 * t5 * t5 * t12kP * t4 * t2P * (t2 * t2)
                                                                                   * t3 - 4 * t23P * (t1 * t1) * t6 * t6 * t2P * t42kP * (t3 * t3) + 18 * t42kP * t5 * t2P * k * t7 * (t2 * t2) + 8 * (t2 * t2) * t42kP * t2P
                                                                                   * t7 * t1 - t5 * t1P * t6 * t22kP * k * k * t2 * (t4 * t4) - 2 * t42kP * t1 * t2P * k * k * (t3 * t3) + 12 * t42kP * t7 * t2P * k * t5 * (t1
                                                                                                                                                                                                                             * t1) * t2 + 2 * t42kP * t5 * t2P * k * t6 * (t1 * t1) + 2 * t6 * t6 * t23kP * t1 * (t3 * t3) * k * (t4 * t4) - 2 * t42kP * t5 * (t3 * t3) * t2P
                                                                                   * k * k * (t2 * t2) * t6 * (t1 * t1) + 2 * t5 * t5 * t12kP * t3 * t2P * k + 0.9e1 * t42kP * t5 * t5 * t2P * k * (t2 * t2) + 4 * t42kP * t1 * t6
                                                                                   * t2P * k * k * (t3 * t3) + 2 * t42kP * t5 * t2P * k * t6 * (t1 * t1) * (t3 * t3) + 8 * t24P * t42kP * t2P * t6 * t1 - 8 * t42kP * t2P
                                                                                   * t24P * t5 * t3 * t7 * (t1 * t1) - 4 * (t2 * t2) * t7 * t7 * t32kP * t2P * t4 - 4 * (t2 * t2) * t42kP * t6 * t2P * t7 + 2 * t42kP * t2P
                                                                                   * k * k * t7 + t6 * t6 * t23kP * (t4 * t4) * k + t42kP * t6 * t6 * t2P * k - 2 * t5 * t5 * t12kP * t2P * k * k * t2 + t7 * t7 * t32kP * (t1
                                                                                                                                                                                                                            * t1) * (t4 * t4) * t2P * k - 8 * t24P * t7 * t7 * t32kP * t2P * t1 * t4 - 16 * t42kP * t1 * t2P * k * t23P * t7 * (t3 * t3) - 8 * t23P
                                                                                   * t42kP * t2P * t1 - t42kP * (t1 * t1) * t2P * k * k - 2 * (t2 * t2) * t42kP * t7 * t7 * t2P + 4 * t24P * t42kP * t2P * t7 + t6
                                                                                   * t6 * t23kP * (t1 * t1) * k - 18 * t42kP * t2P * k * (t2 * t2) * t7 + 2 * t4P * t5 * (t3 * t3) * t22kP * k * k * t2 * t6 * t1 + t4P * (t3
                                                                                                                                                                                                                           * t3) * t6 * t6 * t22kP * k * k + 16 * t42kP * t2P * t23P * t5 * t3 * (t1 * t1) + 8 * t42kP * (t1 * t1) * t6 * t2P * k * k * t2 * t3 + 4 * t42kP
                                                                                   * t5 * t5 * t2P * k * t23P * (t3 * t3) - 2 * t42kP * t5 * t5 * t2P * k * k * t3 + 2 * t4P * t1 * t6 * t6 * t22kP * k * k * (t3 * t3) * t2 - 2
                                                                                   * t42kP * (t3 * t3) * t2P * k * k * t2 + 0.9e1 * t42kP * t5 * t5 * (t1 * t1) * t2P * k * (t2 * t2) - 8 * t42kP * t5 * t3 * t2P * k * k * t2
                                                                                   * t7 + 18 * t7 * t7 * t32kP * t1 * (t4 * t4) * t2P * k * (t2 * t2) - 16 * t42kP * t3 * t2P * k * t7 * t23P + 2 * t42kP * t5 * t5 * t2P
                                                                                   * k * t1 * (t3 * t3) - 8 * t23P * t1 * t6 * t6 * t2P * t42kP * (t3 * t3) + 4 * t42kP * t7 * (t1 * t1) * t2P * k * k * t2 - 4 * t5 * t5 * t12kP
                                                                                   * t3 * t2P * k * k * t2 + t42kP * t6 * t6 * t2P * k * (t1 * t1) + t4P * t7 * (t1 * t1) * t22kP * k * k * t6 - 8 * t42kP * t1 * t2P * k * k
                                                                                   * t2 * t3 + 4 * t4P * t5 * t3 * t4 * t22kP * k * k * t6 * t1 + 36 * t5 * t5 * t12kP * t3 * t2P * k * (t2 * t2) * t4 + 12 * t42kP * t6 * t2P
                                                                                   * k * t2 * t7 * (t3 * t3) + 18 * t42kP * (t1 * t1) * t3 * t2P * k * (t2 * t2) + 4 * t4P * t5 * t3 * t4 * t22kP * k * k * t6 * t1 * t2 - 4 * t42kP
                                                                                   * t5 * t3 * t2P * k * k * (t2 * t2) * t6 + 8 * t7 * t7 * t32kP * t1 * (t4 * t4) * t2P * k * t23P - 8 * t23P * (t3 * t3) * t1 * t2P * t42kP
                                                                                   * t7 * t7 - 4 * t24P * (t3 * t3) * t42kP * t2P * t1 - 4 * t42kP * t2P * (t2 * t2) * t5 * t6 + 8 * t42kP * t7 * t2P * k * t5 * (t1 * t1)
                                                                                   * t23P * (t3 * t3) + 4 * t42kP * t7 * t2P * k * k * t2 + 4 * t42kP * t6 * t6 * t2P * k * t23P * (t3 * t3) + 4 * t42kP * (t1 * t1) * t6
                                                                                   * t2P * k * k * t3 - 2 * t42kP * t7 * t7 * (t1 * t1) * (t3 * t3) * t2P * k * k * t2 - 4 * t7 * t3P * t3 * t4 * t22kP * k * k * t6 * t1 - 4 * t4P
                                                                                   * t1 * t22kP * k * k * t6 * t3 * t2 - t4P * t4 * t22kP * k * k * t6 * (t3 * t3) - 16 * t42kP * t3 * t2P * k * t5 * t23P - 8 * t23P * (t3
                                                                                                                                                                                                                         * t3) * t42kP * t2P * t1 + 2 * t6 * t6 * t23kP * (t3 * t3) * k * t4 - 4 * t42kP * t2P * (t2 * t2) * t5 * t5 * (t3 * t3) * t1 + t4P * t7
                                                                                   * t4 * t22kP * k * k * t2 * t6 * (t3 * t3) - 2 * t24P * t7 * t7 * t32kP * t2P * (t1 * t1) * (t4 * t4) - t42kP * t7 * t7 * (t1 * t1) * (t3
                                                                                                                                                                                                                          * t3) * t2P * k * k * (t2 * t2) - 2 * (t2 * t2) * (t3 * t3) * (t1 * t1) * t2P * t42kP * t7 * t7 - 16 * t23P * t3 * t42kP * t2P * t1 - 2
                                                                                   * t7 * t3P * t4 * t22kP * k * k * t6 * (t1 * t1) - 36 * t42kP * t1 * t6 * t2P * k * (t2 * t2) * (t3 * t3) - 2 * (t2 * t2) * t42kP * t2P - 16
                                                                                   * t42kP * t7 * t1 * t3 * t2P * k * k * t6 * t2 + 36 * t42kP * (t1 * t1) * t6 * t2P * k * (t2 * t2) * t5 * t3 - 2 * t42kP * t7 * (t1 * t1) * t2P
                                                                                   * k * k * t5 - 8 * t42kP * t2P * t24P * t5 * t3 * t6 * (t1 * t1) + 2 * t42kP * t5 * t2P * k * t6 + 18 * t5 * t5 * t12kP * (t3 * t3) * t2P
                                                                                   * k * (t2 * t2) * t4 + 8 * t42kP * t1 * t6 * t2P * k * k * t2 * (t3 * t3) - t4P * (t3 * t3) * t22kP * k * k * t6 + 4 * t42kP * t1 * t6 * t2P
                                                                                   * k * k * (t3 * t3) * (t2 * t2) + 16 * t42kP * t7 * t2P * k * t5 * t1 * t23P * (t3 * t3) - t42kP * (t1 * t1) * t6 * t6 * t2P * k * k + 0.32e2
                                                                                   * t23P * t3 * t42kP * t2P * t6 * t1 + 4 * t24P * t42kP * t2P * t6 * (t1 * t1) + 16 * t42kP * t1 * t6 * t2P * k * t23P * t7 + 4
                                                                                   * t42kP * t5 * t1 * t2P * k * k + t42kP * t7 * t7 * (t1 * t1) * t2P * k + 0.9e1 * t42kP * t5 * t5 * t2P * k * (t3 * t3) * (t2 * t2) - 4
                                                                                   * t42kP * t5 * t2P * k * k * t2 * t6 * (t1 * t1) - 2 * t24P * t42kP * t6 * t6 * t2P * (t3 * t3) + 12 * t5 * t5 * t12kP * (t3 * t3) * t2P
                                                                                   * k * t2 * t4 - 2 * t42kP * (t1 * t1) * t6 * t6 * t2P * k * k * t3 - 4 * t24P * t3 * t42kP * t2P * (t1 * t1) - 8 * t42kP * t5 * t3 * t2P
                                                                                   * k * k * t2 * t7 * (t1 * t1) - 2 * t42kP * (t1 * t1) * t2P * k * k * t2 * (t3 * t3) + 4 * t42kP * (t1 * t1) * t6 * t2P * k * k * t2 * (t3 * t3) + 4
                                                                                   * t42kP * t7 * t7 * t2P * k * t23P * (t3 * t3) + 6 * t7 * t7 * t32kP * t2P * k * t2 - 4 * t42kP * (t1 * t1) * t3 * t2P * k * t6 + 2 * t7
                                                                                   * t7 * t32kP * t1 * t2P * k + 24 * t42kP * t7 * t7 * t2P * k * t1 * t2 * t3 + t4P * t5 * t22kP * k * k * t2 * t6 * (t1 * t1) * t4 + 4 * t42kP
                                                                                   * t5 * t3 * t2P * k * k * (t2 * t2) * (t1 * t1) - 2 * t5 * t5 * t12kP * (t3 * t3) * t4 * t2P * k * k * (t2 * t2) - 4 * t42kP * t7 * (t1 * t1) * t3
                                                                                   * t2P * k * k * t6 * (t2 * t2) + 36 * t42kP * t1 * t3 * t2P * k * (t2 * t2) + 12 * t5 * t5 * t12kP * t4 * t2P * k * t2 - t5 * t1P * (t3 * t3)
                                                                                   * (t4 * t4) * t6 * t22kP * k * k + 6 * t42kP * (t1 * t1) * t6 * t6 * t2P * k * t2 * (t3 * t3) + 4 * t42kP * t5 * t1 * t2P * k * k * (t3 * t3) - t7
                                                                                   * t3P * t3 * (t4 * t4) * t22kP * k * k * t6 * (t1 * t1) + 2 * t4P * (t3 * t3) * t4 * t6 * t6 * t22kP * k * k * t1 - 8 * t23P * (t1 * t1) * t2P
                                                                                   * t42kP * t7 * t6 + t4P * t5 * t22kP * k * k * t6 * (t3 * t3) + t4P * t7 * (t1 * t1) * (t3 * t3) * t22kP * k * k * t6 - 18 * t42kP * (t3
                                                                                                                                                                                                                         * t3) * t2P * k * t7 * (t2 * t2) - 2 * t42kP * t1 * t2P * k * k * (t2 * t2) + t42kP * (t3 * t3) * t6 * t6 * t2P * k * (t1 * t1) + t5 * t5 * t12kP
                                                                                   * t2P * k - t42kP * t2P * k * k * (t2 * t2) - 4 * t24P * t3 * t42kP * t2P - 2 * t42kP * t5 * t2P * k - t5 * t5 * t12kP * t2P
                                                                                   * k * k * (t2 * t2) * (t4 * t4) - 8 * t5 * t5 * t12kP * t3 * t4 * t2P * k * k * t2 + 6 * t5 * t5 * t12kP * t2P * k * t2 - 2 * t7 * t7 * t32kP * t2P
                                                                                   * k * k * t2 * (t1 * t1) + 4 * t4P * t2 * t5 * t1P * k * k * t6 * t2P * t1 + 8 * t4P * t2 * t5 * t1P * k * k * t6 * t2P * t3 + 16 * t23P * t42kP
                                                                                   * t2P * t5 * t1 - 12 * t4P * t2 * t5 * t1P * k * t1 * t6 * t2P + 4 * t4P * t2 * t5 * t1P * k * k * t6 * t2P * t1 * t4 + 8 * t4P * t2 * t5
                                                                                   * t1P * k * k * t6 * t2P * t1 * t3 + 8 * t4P * t2 * t5 * t1P * k * k * t6 * t2P * t3 * t4 + 18 * t5 * t5 * t12kP * t4 * t2P * k * (t2 * t2)
                                                                                   + 8 * t4P * t2 * t5 * t1P * k * k * t6 * t2P * t1 * t3 * t4 - 36 * t42kP * t3 * t2P * k * t5 * (t2 * t2) - 4 * t24P * t7 * t7 * t32kP * t2P
                                                                                   * (t1 * t1) * t4 - 4 * t42kP * t1 * (t3 * t3) * t2P * k * t7 + 16 * t42kP * (t1 * t1) * t6 * t2P * k * t23P * t7 * t3 + 6 * t42kP * t5 * t5
                                                                                   * t2P * k * t2 * (t3 * t3) + 8 * t42kP * t5 * t5 * t2P * k * t23P * t3 + 16 * t42kP * t7 * t7 * t2P * k * t1 * t23P * t3 + 4 * t42kP
                                                                                   * t5 * t2P * k * t6 * t3 + 16 * t24P * t3 * t42kP * t2P * t6 * t1 - 4 * t42kP * t7 * t1 * t2P * k * k * t5 - 8 * (t2 * t2) * t3 * t42kP
                                                                                   * t2P * t1 + 2 * t42kP * t3 * t2P * k + 2 * t4P * t1 * t6 * t6 * t22kP * k * k * (t3 * t3) - 8 * t42kP * t1 * t6 * t6 * t2P * k * k * t2 * t3
                                                                                   + 4 * t24P * (t3 * t3) * t42kP * t2P * t7 * (t1 * t1) - 8 * t23P * t7 * t7 * t32kP * t2P * t1 + 36 * t42kP * t6 * t2P * k * (t2
                                                                                                                                                                                                                * t2) * t5 * t3 - 2 * t42kP * t3 * t6 * t6 * t2P * k * k - 2 * t24P * t42kP * t6 * t6 * t2P - 2 * t24P * (t3 * t3) * t42kP * t2P - 2
                                                                                   * t42kP * t1 * t6 * t6 * t2P * k * k * (t2 * t2) - 4 * t42kP * t5 * (t3 * t3) * t2P * k * k * (t2 * t2) * t7 * t1 - 4 * t42kP * t2P * t24P
                                                                                   * t5 * t5 * (t3 * t3) * t1 - 2 * t42kP * t6 * t2P * k * k * t7 * (t2 * t2) - 2 * t42kP * t1 * t6 * t6 * t2P * k * k + 2 * t7 * t7 * t32kP * (t1
                                                                                                                                                                                                                                * t1) * t4 * t2P * k + 0.9e1 * t42kP * (t1 * t1) * (t3 * t3) * t2P * k * (t2 * t2) + 8 * t5 * t5 * t12kP * t3 * t2P * k * t23P - 16 * t42kP
                                                                                   * t2P * t23P * t5 * t3 * t7 * (t1 * t1) - t5 * t1P * t1 * t6 * t22kP * k * k * (t4 * t4) - 8 * t23P * t1 * t2P * t42kP * t7 * t7 + 4 * t24P
                                                                                   * (t3 * t3) * t42kP * t2P * t6 * (t1 * t1) - 16 * t42kP * t2P * (t2 * t2) * t5 * t3 * t6 * t1 - t4P * (t1 * t1) * t22kP * k * k * t6 * (t3
                                                                                                                                                                                                                           * t3) + 4 * t42kP * t7 * t7 * t2P * k * (t1 * t1) * t23P * (t3 * t3) + 2 * t42kP * t7 * t7 * (t1 * t1) * t2P * k * t3 - 2 * t4P * (t3 * t3)
                                                                                   * t4 * t6 * t22kP * k * k * t1 + t4P * (t3 * t3) * t4 * t6 * t6 * t22kP * k * k * (t1 * t1) - 8 * t42kP * t7 * t1 * (t3 * t3) * t2P * k * k * t6
                                                                                   * t2 - 2 * t5 * t5 * t12kP * t2P * k * k * (t2 * t2) * t4 + t4P * t7 * (t1 * t1) * t22kP * k * k * t6 * t4 + 2 * t42kP * t5 * t2P * k * t6 * (t3
                                                                                                                                                                                                                                 * t3) - 16 * t42kP * t2P * t23P * t5 * t7 * t1 - 4 * t24P * t42kP * t2P * t1 + 18 * t42kP * t1 * t6 * t6 * t2P * k * (t2 * t2) - 4
                                                                                   * (t2 * t2) * t1 * t6 * t6 * t2P * t42kP + 24 * t42kP * t1 * t3 * t2P * k * t2 - 4 * t7 * t7 * t32kP * t2P * k * k * t2 * t1 - 8 * t42kP
                                                                                   * t5 * t3 * t2P * k * k * (t2 * t2) * t7 * t1 + 24 * t42kP * t1 * t6 * t6 * t2P * k * t2 * t3 - 4 * (t2 * t2) * t3 * t42kP * t2P * (t1 * t1) - 4
                                                                                   * (t2 * t2) * t1 * t6 * t6 * t2P * t42kP * (t3 * t3) + t4P * t5 * (t3 * t3) * t4 * t22kP * k * k * t6 - 4 * t5 * t1P * t3 * t4 * t6 * t22kP
                                                                                   * k * k - 2 * t42kP * t5 * t2P * k * k * t6 - 2 * t42kP * t5 * t5 * (t3 * t3) * t2P * k * k * (t2 * t2) * t1 + t4P * t5 * (t3 * t3) * t22kP
                                                                                   * k * k * t2 * t6 - 18 * t42kP * (t1 * t1) * t2P * k * (t2 * t2) * t7 - 2 * t42kP * (t1 * t1) * t2P * k * k * t2 + 4 * t24P * (t3 * t3) * t42kP
                                                                                   * t2P * t7 - 24 * t42kP * (t1 * t1) * t3 * t2P * k * t5 * t2 + 2 * t5 * t5 * t12kP * (t4 * t4) * t2P * k * t3 + 0.9e1 * t42kP * t7 * t7
                                                                                   * t2P * k * (t2 * t2) * (t3 * t3) - 2 * t42kP * t5 * t2P * k * k * (t2 * t2) * t6 * (t1 * t1) + 4 * t5 * t5 * t12kP * (t4 * t4) * t2P * k * t23P
                                                                                   + t4P * t5 * (t3 * t3) * t22kP * k * k * t2 * t6 * (t1 * t1) - 2 * t24P * (t3 * t3) * t42kP * t7 * t7 * t2P + 4 * t4P * t3 * t4 * t6 * t6
                                                                                   * t22kP * k * k * t1 - 24 * t42kP * t5 * t1 * t2P * k * t2 + 16 * t42kP * t2P * t24P * t5 * t3 * t1 + t4P * t7 * t4 * t22kP * k * k
                                                                                   * t2 * t6 + 12 * t42kP * (t1 * t1) * t3 * t2P * k * t2 - 4 * t42kP * t1 * t2P * k * k * t2 * (t3 * t3) + 4 * t7 * t7 * t32kP * t2P * k * t23P - 4
                                                                                   * t5 * t5 * t12kP * t3 * (t4 * t4) * t2P * k * k * t2 - 2 * t42kP * (t3 * t3) * t6 * t2P * k * k * t7 * (t2 * t2) - 8 * t23P * (t1 * t1) * t6
                                                                                   * t6 * t2P * t42kP * t3 - 8 * t23P * t3 * (t1 * t1) * t2P * t42kP * t7 * t7 - 2 * t5 * t1P * t3 * (t4 * t4) * t6 * t22kP * k * k * t1
                                                                                   * t2 - 2 * (t2 * t2) * (t3 * t3) * t42kP * t7 * t7 * t2P - 16 * t42kP * t2P * t23P * t5 * t3 * t7 - t7 * t7 * t32kP * t2P * k * k * (t2
                                                                                                                                                                                                                        * t2) + 12 * t42kP * t7 * t7 * t2P * k * (t1 * t1) * t2 * t3 - 8 * t42kP * t2P * t24P * t5 * t7 * t1 + 0.9e1 * t42kP * (t3 * t3) * t2P
                                                                                   * k * (t2 * t2) - 8 * t42kP * t5 * t5 * t3 * t2P * k * k * t2 * t1 - 4 * t42kP * t2P * k * t6 * t1 + 0.9e1 * t7 * t7 * t32kP * t2P * k * (t2
                                                                                                                                                                                                                             * t2) * (t1 * t1) + 4 * t6 * t6 * t23kP * t3 * k * t4 + t4P * (t1 * t1) * t6 * t6 * t22kP * k * k * t2 + 8 * t42kP * t7 * t1 * t3 * t2P * k * k
                                                                                   * (t2 * t2) + 12 * t42kP * t3 * t2P * k * t2 - 4 * t42kP * t7 * t1 * (t3 * t3) * t2P * k * k * t6 * (t2 * t2) - t7 * t3P * t6 * t22kP * k
                                                                                   * k * t2 - 16 * t24P * t3 * t1 * t2P * t42kP * t7 * t6 + 8 * t42kP * t5 * t2P * k * t7 * t23P - 2 * (t2 * t2) * t7 * t7 * t32kP * t2P
                                                                                   * (t1 * t1) * (t4 * t4) + 0.32e2 * t23P * t3 * t42kP * t2P * t7 * t1 - 4 * t42kP * t5 * (t1 * t1) * t2P * k * k * t6 * t3 - t7 * t3P * (t4
                                                                                                                                                                                                                           * t4) * t22kP * k * k * t6 + 4 * t42kP * t7 * (t1 * t1) * t2P * k * t6 * t3 + 4 * (t2 * t2) * t42kP * t2P * t7 * (t1 * t1) + 0.9e1 * t42kP
                                                                                   * t6 * t6 * t2P * k * (t2 * t2) * (t3 * t3) - 4 * t42kP * t5 * t2P * k * k * t2 * t7 * (t1 * t1) + 8 * t42kP * t2P * (t2 * t2) * t5 * t3 + 18
                                                                                   * t42kP * t5 * t5 * t1 * t2P * k * (t2 * t2) * (t3 * t3) - 0.32e2 * t42kP * t1 * t6 * t2P * k * t23P * t3 + 2 * t4P * (t3 * t3) * t4 * t6
                                                                                   * t6 * t22kP * k * k * t1 * t2 + 36 * t42kP * t1 * t6 * t2P * k * (t2 * t2) * t7 * (t3 * t3) + 4 * t4P * t5 * t3 * t22kP * k * k * t2 * t6 * t1 - 4
                                                                                   * t5 * t5 * t12kP * t24P * t2P * t3 - 4 * t24P * t7 * t7 * t32kP * t2P * t1 + 2 * t4P * (t1 * t1) * t6 * t6 * t22kP * k * k * t3 * t2
                                                                                   + 8 * t24P * t3 * t42kP * t2P * t7 * (t1 * t1) + 2 * t42kP * t6 * t6 * t2P * k * t1 + 2 * t4P * t7 * t1 * (t3 * t3) * t22kP * k * k * t6 - 4
                                                                                   * t7 * t3P * t6 * t22kP * k * k * t2 * t1 * t4 - 8 * t42kP * t5 * t3 * t2P * k * k * t2 * t6 + 8 * t42kP * t5 * t5 * (t1 * t1) * t2P * k * t23P
                                                                                   * t3 - 4 * t42kP * t5 * t2P * k * k * (t2 * t2) * t7 * t1 + 0.9e1 * t42kP * t7 * t7 * t2P * k * (t1 * t1) * (t3 * t3) * (t2 * t2) - 4 * t4P
                                                                                   * t3 * t4 * t6 * t22kP * k * k * t1 * t2 + 18 * t42kP * t5 * t5 * (t1 * t1) * t2P * k * (t2 * t2) * t3 + t4P * t7 * t22kP * k * k * t6 - 4 * t42kP
                                                                                   * t2P * t23P * t5 * t5 * (t3 * t3) - 4 * t42kP * t1 * (t3 * t3) * t2P * k * t6 - 4 * t24P * t7 * t7 * t32kP * t2P * t1 * (t4 * t4) + t42kP
                                                                                   * (t1 * t1) * (t3 * t3) * t2P * k + 8 * t42kP * t7 * t7 * t2P * k * (t1 * t1) * t23P * t3 - 4 * t23P * (t1 * t1) * t6 * t6 * t2P * t42kP - 24
                                                                                   * t42kP * t6 * t2P * k * t2 * t3 - 4 * t5 * t5 * t12kP * t23P * t2P * (t3 * t3) + 8 * t42kP * t5 * t2P * k * t7 * t23P * (t3 * t3) - 4
                                                                                   * t42kP * t2P * (t2 * t2) * t5 * (t3 * t3) * t6 - 4 * (t2 * t2) * t3 * t42kP * t7 * t7 * t2P + 4 * t42kP * t2P * t24P * t5 * (t3 * t3)
                                                                                   + 8 * t42kP * t7 * t1 * t2P * k * k * t2 - 4 * t42kP * t5 * t3 * t2P * k * k * (t2 * t2) * t7 * (t1 * t1) + 6 * t42kP * t7 * t7 * t2P * k * (t1
                                                                                                                                                                                                                                * t1) * t2 - 4 * t24P * t42kP * t6 * t2P * t7 - 4 * t5 * t5 * t12kP * t2P * k * k * t2 * t4 - 8 * t42kP * t2P * (t2 * t2) * t5 * t3 * t6
                                                                                   * (t1 * t1) + 4 * (t2 * t2) * (t3 * t3) * t42kP * t2P * t6 - 16 * t42kP * t2P * t24P * t5 * t3 * t7 * t1 + 18 * t42kP * t6 * t6 * t2P
                                                                                   * k * (t2 * t2) * t3 + t4P * (t1 * t1) * t6 * t6 * t22kP * k * k * t4 * t2 - 4 * t42kP * t1 * (t3 * t3) * t2P * k * t5 + 4 * t7 * t7 * t32kP * t2P
                                                                                   * k * t23P * (t1 * t1) + 4 * t42kP * (t1 * t1) * t6 * t6 * t2P * k * t23P * (t3 * t3) - 2 * t42kP * t1 * t2P * k * k - 2 * t5 * t1P * t1
                                                                                   * t6 * t22kP * k * k * t4 - 8 * t42kP * t2P * t23P * t5 * (t3 * t3) * t7 * (t1 * t1) - 2 * t42kP * (t3 * t3) * t2P * k * t5 + 24 * t42kP
                                                                                   * t6 * t2P * k * t2 * t5 * t3 - 2 * t5 * t5 * t12kP * t3 * (t4 * t4) * t2P * k * k - 12 * t42kP * (t3 * t3) * t2P * k * t7 * t2 - 16 * t42kP
                                                                                   * t1 * t6 * t2P * k * t23P * (t3 * t3) - t5 * t1P * (t3 * t3) * t22kP * k * k * t6 * t1 - 2 * t5 * t1P * t6 * t22kP * k * k * t2 * t1 * t4 + 36
                                                                                   * t42kP * t1 * t6 * t2P * k * (t2 * t2) * t5 - 2 * t5 * t1P * (t3 * t3) * t4 * t6 * t22kP * k * k * t2 - t4P * (t1 * t1) * t6 * t22kP * k
                                                                                   * k * t4 + 36 * t42kP * t7 * t7 * t2P * k * t1 * t3 * (t2 * t2) - 4 * t42kP * t7 * (t1 * t1) * t3 * t2P * k * k * t6 - 2 * t42kP * (t1 * t1) * t6
                                                                                   * t6 * t2P * k * k * t2 - 4 * t24P * (t1 * t1) * t2P * t42kP * t7 * t6 + 2 * t42kP * t5 * t5 * t2P * k * (t1 * t1) * t3 - 36 * t42kP * (t1
                                                                                                                                                                                                                           * t1) * t6 * t2P * k * (t2 * t2) * t3 + 4 * t42kP * (t1 * t1) * t6 * t6 * t2P * k * t23P + 4 * t42kP * t7 * t3 * t2P * k * k * (t2 * t2) + 2
                                                                                   * t42kP * t7 * t2P * k * k * (t2 * t2) - 8 * t42kP * t2P * (t2 * t2) * t5 * t3 * t7 - 4 * t42kP * t7 * (t1 * t1) * t3 * t2P * k * k * t5 + 2
                                                                                   * t6 * t6 * t23kP * (t1 * t1) * t3 * k * (t4 * t4) + 8 * t24P * t42kP * t2P * t5 * t1 + 8 * t42kP * t5 * t5 * t1 * t2P * k * t23P * (t3
                                                                                                                                                                                                                        * t3) + t42kP * t7 * t7 * t2P * k - 2 * t42kP * t5 * (t1 * t1) * t2P * k * k * t6 * (t3 * t3) - t42kP * (t1 * t1) * t2P * k * k * (t3 * t3)
                                                                                   + 4 * (t2 * t2) * t42kP * t2P * t7 + 4 * t4P * t7 * t1 * t3 * t22kP * k * k * t6 * t2 + 16 * t42kP * t1 * t3 * t2P * k * t23P + 16 * t7
                                                                                   * t7 * t32kP * t1 * t4 * t2P * k * t23P + t5 * t5 * t12kP * (t4 * t4) * t2P * k * (t3 * t3) - 4 * (t2 * t2) * t7 * t7 * t32kP * t2P
                                                                                   * (t1 * t1) * t4 - 2 * t42kP * t5 * (t3 * t3) * t2P * k * k * (t2 * t2) * t7 - 0.32e2 * t42kP * t2P * t23P * t5 * t3 * t7 * t1 - t42kP
                                                                                   * t2P * k * k + 24 * t5 * t5 * t12kP * t3 * t2P * k * t2 * t4 + 4 * t42kP * t5 * t2P * k * t6 * t1 * (t3 * t3) + 4 * t4P * t5 * t1 * t22kP
                                                                                   * k * k * t6 * t3 + 8 * t42kP * t7 * t1 * t2P * k * t5 * t3 + 4 * t42kP * t3 * t6 * t2P * k * t7 + 6 * t42kP * (t3 * t3) * t2P * k * t2 - 2 * t42kP
                                                                                   * t5 * (t1 * t1) * t2P * k * k * t6 + t4P * t5 * t22kP * k * k * t6 * t4 - 12 * t42kP * (t1 * t1) * t6 * t2P * k * t2 * (t3 * t3) - 4 * t42kP
                                                                                   * t5 * t5 * t2P * k * k * t2 * t1 - 16 * t42kP * t1 * t6 * t2P * k * t23P + 2 * t4P * t7 * t1 * (t3 * t3) * t4 * t22kP * k * k * t6 + 8 * t42kP
                                                                                   * t2P * t24P * t5 * t3 * (t1 * t1) - t4P * (t3 * t3) * t4 * t6 * t22kP * k * k * (t1 * t1) - t5 * t5 * t12kP * t2P * k * k * (t2 * t2) - 2
                                                                                   * t4P * (t1 * t1) * t22kP * k * k * t6 * t3 * t2 + 2 * t4P * t7 * t1 * t22kP * k * k * t6 * t4 + 4 * t42kP * t5 * t2P * k * t7 * t3 + 0.32e2
                                                                                   * t42kP * t7 * t2P * k * t5 * t1 * t23P * t3 + 4 * t42kP * t7 * t7 * t2P * k * t23P + 2 * t7 * t7 * t32kP * t1 * (t4 * t4) * t2P * k
                                                                                   + 4 * t42kP * t2P * t24P * t5 * (t3 * t3) * (t1 * t1) - 2 * t7 * t3P * t3 * t4 * t22kP * k * k * t6 - 8 * (t2 * t2) * (t3 * t3) * t1 * t2P
                                                                                   * t42kP * t7 * t6 - 2 * t42kP * t5 * t5 * t1 * t2P * k * k * (t3 * t3) + 12 * t42kP * t6 * t2P * k * t2 * t7 + 24 * t42kP * t5 * t2P * k
                                                                                   * t7 * t2 * t3 + 18 * t5 * t5 * t12kP * t3 * t2P * k * (t2 * t2) + 8 * t42kP * t5 * t5 * t1 * t2P * k * t23P - 4 * t42kP * t2P * t23P
                                                                                   * t5 * t5 * (t3 * t3) * (t1 * t1) - 8 * t42kP * t2P * t24P * t5 * t3 * t6 + 0.32e2 * t42kP * t2P * t23P * t5 * t3 * t1 - 16 * t42kP
                                                                                   * (t1 * t1) * t6 * t2P * k * t23P * t3 + 2 * t42kP * (t1 * t1) * t3 * t2P * k + 4 * t42kP * t1 * t3 * t2P * k + 16 * t42kP * t2P * (t2
                                                                                                                                                                                                                       * t2) * t5 * t3 * t1 - 4 * t42kP * t2P * t24P * t5 * t5 * t3 * (t1 * t1) + 4 * t42kP * t7 * t1 * (t3 * t3) * t2P * k * k - 2 * (t2 * t2) * t7
                                                                                   * t7 * t32kP * t2P * (t1 * t1) - 16 * t42kP * t2P * t24P * t5 * t3 * t6 * t1 - 8 * t24P * t3 * (t1 * t1) * t2P * t42kP * t7 * t6
                                                                                   + 8 * t23P * (t3 * t3) * t42kP * t2P * t6 * (t1 * t1) - 4 * t24P * t3 * t42kP * t7 * t7 * t2P + 0.9e1 * t7 * t7 * t32kP * (t1 * t1)
                                                                                   * (t4 * t4) * t2P * k * (t2 * t2) - 4 * t42kP * t2P * (t2 * t2) * t5 * t7 - t42kP * t5 * t5 * t2P * k * k + 2 * t42kP * t2P * k * t1 + t6
                                                                                   * t6 * t23kP * (t3 * t3) * k - t42kP * (t3 * t3) * t2P * k * k - 2 * t42kP * t3 * t2P * k * k - 2 * t42kP * t2P * k * t7 + 2 * t42kP
                                                                                   * t5 * t2P * k * k - 4 * t42kP * t5 * t2P * k * k * (t2 * t2) * t6 * t1 - 24 * t42kP * t1 * t2P * k * t2 * t7 * (t3 * t3) + t4P * t5 * (t3 * t3)
                                                                                   * t4 * t22kP * k * k * t6 * t2 - 8 * t42kP * t1 * t3 * t2P * k * t7 - 2 * t24P * t7 * t7 * t32kP * t2P * (t1 * t1) + 0.9e1 * t42kP * t5
                                                                                   * t5 * (t1 * t1) * t2P * k * (t2 * t2) * (t3 * t3) - t5 * t1P * (t3 * t3) * (t4 * t4) * t6 * t22kP * k * k * t1 - 2 * t7 * t7 * t32kP * t2P
                                                                                   * k * k * t2 * (t1 * t1) * (t4 * t4) + t4P * t5 * t22kP * k * k * t2 * t6 * (t1 * t1) - 4 * t42kP * t1 * t6 * t6 * t2P * k * k * t2 + t4P * t7 * (t1
                                                                                                                                                                                                                                     * t1) * t22kP * k * k * t6 * t2 - 4 * t42kP * t5 * t1 * t2P * k * k * t6 + t7 * t7 * t32kP * t2P * k - 4 * t42kP * t2P * (t2 * t2) * t5
                                                                                   * t5 * t3 - 8 * t24P * t1 * t6 * t6 * t2P * t42kP * t3 + 16 * (t2 * t2) * t3 * t42kP * t2P * t6 * t1 + t6 * t6 * t23kP * (t1 * t1) * (t3
                                                                                                                                                                                                                         * t3) * k * (t4 * t4) + 2 * t6 * t6 * t23kP * t3 * k - 4 * t42kP * t2P * t24P * t5 * t7 + 8 * t23P * t42kP * t2P * t6 + 0.9e1 * t42kP
                                                                                   * t2P * k * (t2 * t2) + 4 * t42kP * t7 * t7 * t1 * t2P * k * t3 - 2 * t24P * (t3 * t3) * (t1 * t1) * t2P * t42kP * t7 * t7 - 4 * t23P
                                                                                   * (t3 * t3) * t42kP * t2P * (t1 * t1) - 2 * t4P * t1 * t6 * t22kP * k * k * t4 - 2 * t42kP * t7 * t7 * t3 * t2P * k * k * (t2 * t2) + 8 * t5
                                                                                   * t5 * t12kP * t3 * t2P * k * t23P * (t4 * t4) - 8 * t42kP * t2P * t23P * t5 * (t3 * t3) * t6 - 2 * t42kP * (t1 * t1) * (t3 * t3)
                                                                                   * t2P * k * t5 + 12 * t42kP * t6 * t2P * k * t2 * t5 * (t3 * t3) - t5 * t1P * (t3 * t3) * t22kP * k * k * t6 + t42kP * t2P * k - 4 * t5
                                                                                   * t5 * t12kP * t23P * t2P - 2 * t24P * t42kP * t2P - 2 * t7 * t3P * t6 * t22kP * k * k * t2 * t1 * t3 + 2 * t4P * t5 * t22kP
                                                                                   * k * k * t6 * t3 + 4 * t42kP * t3 * t6 * t2P * k * k * (t2 * t2) + t42kP * (t3 * t3) * t6 * t6 * t2P * k - 2 * t7 * t3P * t6 * t22kP * k * k
                                                                                   * t2 * t1 * (t4 * t4) + 36 * t42kP * t1 * t6 * t2P * k * (t2 * t2) * t7 - t5 * t1P * (t3 * t3) * t22kP * k * k * t6 * t2 + t7 * t7 * t32kP * (t4
                                                                                                                                                                                                                                 * t4) * t2P * k + 4 * t4P * t1 * t6 * t6 * t22kP * k * k * t3 - 2 * t24P * t7 * t7 * t32kP * t2P + 4 * (t2 * t2) * t42kP * t2P * t5
                                                                                   + t42kP * t5 * t5 * t2P * k * (t1 * t1) * (t3 * t3) + 16 * t42kP * t6 * t2P * k * t23P * t7 * t3 + 2 * t42kP * t6 * t2P * k * k * (t2
                                                                                                                                                                                                                      * t2) + t4P * t6 * t6 * t22kP * k * k * t2 - 0.48e2 * t42kP * t1 * t2P * k * t2 * t7 * t3 + t5 * t5 * t12kP * (t4 * t4) * t2P * k + 24 * t42kP
                                                                                   * t1 * t6 * t2P * k * t2 * t7 * (t3 * t3) - 2 * t42kP * t5 * t5 * t3 * t2P * k * k * (t2 * t2) * (t1 * t1) + 0.48e2 * t42kP * t1 * t6 * t2P
                                                                                   * k * t2 * t5 * t3 + 2 * t42kP * t5 * t2P * k * k * (t2 * t2) * (t1 * t1) - 8 * t24P * t3 * t42kP * t2P * t1 - t7 * t7 * t32kP * t2P * k
                                                                                   * k * (t2 * t2) * (t1 * t1) - 18 * t42kP * (t1 * t1) * t6 * t2P * k * (t2 * t2) - 2 * t42kP * t7 * t7 * t2P * k * k * t2 - 8 * t42kP * t7 * t1
                                                                                   * t3 * t2P * k * k * t6 * (t2 * t2) + 16 * t23P * t42kP * t2P * t7 * t1 - 16 * t42kP * t2P * (t2 * t2) * t5 * t3 * t7 * t1 + 8 * t42kP
                                                                                   * (t1 * t1) * t6 * t2P * k * t23P * t7 + 16 * t23P * t3 * t42kP * t2P * t6 + 8 * t42kP * t2P * t24P * t5 * t3 - 2 * t42kP * t7
                                                                                   * t7 * t3 * t2P * k * k - 0.72e2 * t42kP * t1 * t3 * t2P * k * t5 * (t2 * t2) - 2 * t7 * t7 * t32kP * t4 * t2P * k * k - 4 * t42kP * t7 * t7
                                                                                   * t1 * t3 * t2P * k * k * (t2 * t2) - 8 * t5 * t5 * t12kP * t23P * t2P * t3 + 2 * t4P * t7 * t1 * t22kP * k * k * t6 + 2 * t42kP * t7 * t7
                                                                                   * t3 * t2P * k + 24 * t42kP * t1 * t6 * t2P * k * t2 * t5 - 2 * t5 * t5 * t12kP * (t3 * t3) * t4 * t2P * k * k - 2 * t5 * t5 * t12kP * t24P
                                                                                   * t2P * (t3 * t3) + 2 * t42kP * (t1 * t1) * t6 * t2P * k * k - 4 * (t2 * t2) * (t1 * t1) * t6 * t6 * t2P * t42kP * t3 - 8 * t24P * t1 * t2P
                                                                                   * t42kP * t7 * t6 + 16 * t23P * (t3 * t3) * t42kP * t2P * t7 * t1 + 12 * t42kP * t5 * t5 * t1 * t2P * k * t2 * (t3 * t3) - 2 * t5 * t1P
                                                                                   * t3 * t22kP * k * k * t6 + t4P * t7 * (t3 * t3) * t22kP * k * k * t6 + 12 * t42kP * (t1 * t1) * t6 * t2P * k * t2 * t7 * (t3 * t3) - 4 * t7 * t3P
                                                                                   * t4 * t22kP * k * k * t6 * t1 + 12 * t42kP * (t1 * t1) * t6 * t2P * k * t2 * t7 - 0.48e2 * t42kP * t1 * t6 * t2P * k * t2 * t3 - 12 * t42kP
                                                                                   * (t1 * t1) * t2P * k * t2 * t7 * (t3 * t3) - 4 * t42kP * t7 * (t1 * t1) * (t3 * t3) * t2P * k * k * t6 * t2 - 2 * t5 * t1P * t3 * t6 * t22kP
                                                                                   * k * k * t1 * t2 + 4 * t42kP * t6 * t6 * t2P * k * t23P - 4 * t42kP * (t1 * t1) * t3 * t2P * k * t7 + 8 * t42kP * t7 * t2P * k * t5 * (t1
                                                                                                                                                                                                                           * t1) * t23P - 16 * t23P * t3 * (t1 * t1) * t2P * t42kP * t7 * t6 + 8 * t42kP * t5 * t2P * k * k * t2 * t1 - 2 * t7 * t7 * t32kP * (t4
                                                                                                                                                                                                                                                                                                                                                               * t4) * t2P * k * k * t1 + 16 * t42kP * (t1 * t1) * t6 * t2P * k * t23P * t5 * t3 + t4P * t7 * (t1 * t1) * (t3 * t3) * t4 * t22kP * k * k
                                                                                   * t6 + 6 * t42kP * t6 * t6 * t2P * k * t2 - 16 * t5 * t5 * t12kP * t23P * t2P * t3 * t4 - 2 * t5 * t1P * (t3 * t3) * t4 * t6 * t22kP * k
                                                                                   * k * t1 * t2 - 2 * t5 * t1P * t3 * t22kP * k * k * t6 * t2 - 12 * t42kP * t5 * t2P * k * t2 - 2 * t42kP * t1 * t6 * t6 * t2P * k * k * (t3 * t3)
                                                                                   * (t2 * t2) + t42kP * t5 * t5 * t2P * k * (t1 * t1) - 2 * t24P * (t3 * t3) * t42kP * t2P * (t1 * t1) - t5 * t1P * (t3 * t3) * t6 * t22kP
                                                                                   * k * k * t1 * t2 + 0.48e2 * t42kP * t7 * t2P * k * t5 * t1 * t2 * t3 - 4 * t42kP * t1 * t6 * t6 * t2P * k * k * t3 - 4 * t5 * t5 * t12kP * t24P
                                                                                   * t2P * t3 * (t4 * t4) - 4 * t42kP * (t1 * t1) * t2P * k * k * t2 * t3 + 24 * t42kP * t5 * t5 * t1 * t2P * k * t2 * t3 - 8 * t42kP * t2P
                                                                                   * t24P * t5 * (t3 * t3) * t6 * t1 - 2 * t5 * t5 * t12kP * t2P * k * k * t2 * (t4 * t4) - t5 * t5 * t12kP * (t4 * t4) * t2P * k * k + 4 * t42kP
                                                                                   * t5 * (t3 * t3) * t2P * k * k * (t2 * t2) * t1 - t4P * (t3 * t3) * t22kP * k * k * t6 * t2 - 4 * t42kP * t5 * t1 * t2P * k * k * t6 * (t3 * t3) - 4
                                                                                   * t5 * t5 * t12kP * (t3 * t3) * t4 * t2P * k * k * t2 - 8 * t5 * t5 * t12kP * t23P * t2P * t4 + 18 * t42kP * (t1 * t1) * t6 * t6 * t2P
                                                                                   * k * (t2 * t2) * t3 + 6 * t7 * t7 * t32kP * t2P * k * t2 * (t4 * t4) + 4 * t7 * t7 * t32kP * t1 * t4 * t2P * k + t4P * t4 * t6 * t6 * t22kP
                                                                                   * k * k - 36 * t42kP * t1 * (t3 * t3) * t2P * k * t5 * (t2 * t2) + t4P * t4 * t6 * t6 * t22kP * k * k * t2 - 4 * (t2 * t2) * t42kP * t6 * t6 * t2P
                                                                                   * t3 - 8 * t24P * (t3 * t3) * t1 * t2P * t42kP * t7 * t6 - 2 * (t2 * t2) * t42kP * t6 * t6 * t2P * (t3 * t3) - 4 * (t2 * t2) * (t3 * t3) * t42kP
                                                                                   * t7 * t2P * t6 + 4 * t42kP * t2P * (t2 * t2) * t5 * (t3 * t3) + 6 * t42kP * t5 * t5 * t2P * k * t2 - 36 * t42kP * t5 * t1 * t2P * k * (t2
                                                                                                                                                                                                                           * t2) - 4 * t42kP * t2P * t24P * t5 * (t3 * t3) * t7 - 18 * t42kP * (t1 * t1) * t6 * t2P * k * (t2 * t2) * (t3 * t3) + 8 * t42kP * t7
                                                                                   * t7 * t2P * k * t1 * t23P + 18 * t42kP * t7 * t7 * t2P * k * t1 * (t2 * t2) - 2 * t5 * t5 * t12kP * (t4 * t4) * t2P * (t2 * t2) * (t3 * t3)
                                                                                   + 2 * t42kP * t1 * (t3 * t3) * t2P * k - 4 * t24P * t1 * t2P * t42kP * t7 * t7 + 2 * t6 * t6 * t23kP * t1 * (t4 * t4) * k + 36 * t42kP
                                                                                   * (t1 * t1) * t6 * t2P * k * (t2 * t2) * t7 * t3 - 8 * t23P * (t3 * t3) * t42kP * t7 * t2P * t6 + 2 * t42kP * t7 * (t1 * t1) * t2P * k * t6
                                                                                   * (t3 * t3) + 8 * t23P * t42kP * t2P * t5 * (t1 * t1) + 4 * (t2 * t2) * (t3 * t3) * t42kP * t2P * t7 + 2 * t4P * t5 * (t3 * t3) * t4 * t22kP
                                                                                   * k * k * t6 * t1 + t4P * t5 * (t3 * t3) * t4 * t22kP * k * k * t6 * (t1 * t1) * t2 - 4 * t42kP * t2P * t24P * t5 * t7 * (t1 * t1) + 36 * t42kP
                                                                                   * t1 * t6 * t2P * k * (t2 * t2) * t5 * (t3 * t3) + 8 * t5 * t5 * t12kP * t4 * t2P * k * t23P - 4 * t42kP * t5 * t2P * k * t1 - t7 * t7 * t32kP
                                                                                   * (t4 * t4) * t2P * k * k + 4 * t42kP * t7 * t7 * t2P * k * (t1 * t1) * t23P + 4 * t6 * t6 * t23kP * t1 * t3 * k * (t4 * t4) + 2 * t42kP * t7
                                                                                   * (t1 * t1) * t2P * k * k - 8 * t42kP * t2P * (t2 * t2) * t5 * t7 * t1 - t42kP * t7 * t7 * t2P * k * k * (t2 * t2) + 8 * t42kP * t2P * (t2
                                                                                                                                                                                                                           * t2) * t5 * (t3 * t3) * t1 - 8 * t7 * t7 * t32kP * t2P * k * k * t2 * t1 * t4 + 2 * t4P * t7 * (t1 * t1) * t3 * t22kP * k * k * t6 * t2 - 2 * t5 * t1P
                                                                                   * t3 * (t4 * t4) * t6 * t22kP * k * k * t2 - 2 * (t2 * t2) * (t1 * t1) * t6 * t6 * t2P * t42kP - 2 * t42kP * t5 * t2P * k * k * t6 * (t3 * t3) - 4
                                                                                   * t5 * t5 * t12kP * t23P * t2P * (t3 * t3) * (t4 * t4) - 2 * t4P * t1 * t22kP * k * k * t6 * (t3 * t3) - 4 * t42kP * t1 * t2P * k * k
                                                                                   * t2 + 0.9e1 * t5 * t5 * t12kP * (t3 * t3) * t2P * k * (t2 * t2) - 2 * t7 * t3P * t3 * t4 * t22kP * k * k * t6 * (t1 * t1) + 4 * t42kP * (t1
                                                                                                                                                                                                                             * t1) * t2P * k * t23P)
    
    if (k > 0) test <- abs(a22/a[2,2])
    a[2,2] <- a[2,2] + a22
    k <- k+1
    
  } 
  
  
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ##A23
  test <- 100.0
  k <- 0
  while (test > criteria  &  k < maximumIteration)
  {
    t1P <- pow((t1 / (1 + t1)), k)
    t2P <- pow((t2 / (1 + t2)), k)
    t3P <- pow((t3 / (1 + t3)), k)
    t4P <- pow((t4 / (1 + t4)), k)
    
    a23 <- 1 / (1 + t2) / (1 + t3) / (t7 * t3P + t6 * t2P + t5 * t1P + t4P - t4P * t5 * t1 * t2 * t3 + t5 * t1P * t2 + t6 * t2P * t4 + t6 * t2P
                                      * t3 + t6 * t2P * t1 + t7 * t3P * t4 + t7 * t3P * t2 + t7 * t3P * t1 - t4P * t6 * t2 - t4P * t7 * t3 - t4P * t5 * t1 + t4P * t2 * t3 - t4P
                                      * t5 * t3 - t4P * t5 * t2 + t4P * t1 * t3 - t4P * t6 * t3 - t4P * t6 * t1 + t4P * t1 * t2 - t4P * t7 * t2 - t4P * t7 * t1 + t5 * t1P * t4
                                      + t5 * t1P * t3 + t4P * t3 - t4P * t5 - t4P * t6 - t4P * t7 + t4P * t2 + t4P * t1 + t5 * t1P * t2 * t4 + t5 * t1P * t3 * t4 + t5 * t1P
                                      * t2 * t3 + t6 * t2P * t1 * t4 + t6 * t2P * t3 * t4 + t6 * t2P * t1 * t3 + t7 * t3P * t2 * t4 + t7 * t3P * t1 * t2 + t7 * t3P * t1 * t4 - t4P
                                      * t7 * t1 * t2 - t4P * t6 * t2 * t3 - t4P * t6 * t1 * t2 - t4P * t7 * t2 * t3 - t4P * t7 * t1 * t3 - t4P * t6 * t1 * t3 - t4P * t5 * t1 * t2 - t4P
                                      * t5 * t1 * t3 - t4P * t5 * t2 * t3 + t4P * t1 * t2 * t3 - t4P * t6 * t1 * t2 * t3 + t5 * t1P * t2 * t3 * t4 + t6 * t2P * t1 * t3 * t4 + t7 * t3P
                                      * t1 * t2 * t4 - t4P * t7 * t1 * t2 * t3) / t2 / t3 * (1 + t1) * (1 + t4) * t6 * t2P * t7 * t3P * (k * k - k * t3 - t2 * k + (t2 * t3))
    
    if (k > 0) test <- abs(a23/a[2,3])
    a[2,3] <- a[2,3] + a23
    k <- k+1
      
  } 
    
    
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ##A24
  test <- 100.0
  k <- 0
  while (test > criteria  &  k < maximumIteration)
  {
    t1P <- pow((t1 / (1 + t1)), k)
    t2P <- pow((t2 / (1 + t2)), k)
    t3P <- pow((t3 / (1 + t3)), k)
    t4P <- pow((t4 / (1 + t4)), k)
    
    a24 <- -1 / (1 + t2) / (1 + t4) / (t7 * t3P + t6 * t2P + t5 * t1P + pow((t4
                                                                             / (1 + t4)), k) - t4P * t5 * t1 * t2 * t3 + t5 * t1P * t2 + t6 * t2P
                                       * t4 + t6 * t2P * t3 + t6 * t2P * t1 + t7 * t3P * t4 + t7 * pow((t3
                                                                                                        / (1 + t3)), k) * t2 + t7 * t3P * t1 - t4P * t6 * t2 - t4P * t7
                                       * t3 - t4P * t5 * t1 + t4P * t2 * t3 - t4P * t5 * t3 - pow((t4
                                                                                                   / (1 + t4)), k) * t5 * t2 + t4P * t1 * t3 - t4P * t6 * t3 - t4P
                                       * t6 * t1 + t4P * t1 * t2 - t4P * t7 * t2 - t4P * t7 * t1 + t5 * pow((t1
                                                                                                             / (1 + t1)), k) * t4 + t5 * t1P * t3 + t4P * t3 - t4P * t5 - pow((t4
                                                                                                                                                                               / (1 + t4)), k) * t6 - t4P * t7 + t4P * t2 + t4P * t1 + t5 * pow((t1
                                                                                                                                                                                                                                                 / (1 + t1)), k) * t2 * t4 + t5 * t1P * t3 * t4 + t5 * t1P * t2 * t3 + t6 * pow((t2 / (1
                                                                                                                                                                                                                                                                                                                                       + t2)), k) * t1 * t4 + t6 * t2P * t3 * t4 + t6 * t2P * t1 * t3 + t7 * t3P
                                       * t2 * t4 + t7 * t3P * t1 * t2 + t7 * t3P * t1 * t4 - t4P * t7 * t1
                                       * t2 - t4P * t6 * t2 * t3 - t4P * t6 * t1 * t2 - t4P * t7 * t2 * t3 - pow((t4
                                                                                                                  / (1 + t4)), k) * t7 * t1 * t3 - t4P * t6 * t1 * t3 - t4P * t5 * t1 * t2 - pow((t4 / (1
                                                                                                                                                                                                        + t4)), k) * t5 * t1 * t3 - t4P * t5 * t2 * t3 + t4P * t1 * t2 * t3 - t4P
                                       * t6 * t1 * t2 * t3 + t5 * t1P * t2 * t3 * t4 + t6 * t2P * t1 * t3 * t4 + t7 * pow((t3
                                                                                                                           / (1 + t3)), k) * t1 * t2 * t4 - t4P * t7 * t1 * t2 * t3) / t2 / t4 * (1 + t1) * (1 + t3) * t6 * t2P
    * t4P * (-k * k + t5 * k * k + k * k * t6 + k * k * t7 + k * t4 - t5 * k * t4 - k * t6 * t4 - k * t7 * t4 + t2 * k - k * t5 * t2 - t6 * k * t2 - k
             * t7 * t2 - (t2 * t4) + t5 * t2 * t4 + t6 * t2 * t4 + t7 * t2 * t4)
    
    if (k > 0) test <- abs(a24/a[2,4])
    a[2,4] <- a[2,4] + a24
    k <- k+1
      
  } 
    
    
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ##A25
  test <- 100.0
  k <- 0
  while (test > criteria  &  k < maximumIteration)
  {
    t1P <- pow((t1 / (1 + t1)), k)
    t2P <- pow((t2 / (1 + t2)), k)
    t3P <- pow((t3 / (1 + t3)), k)
    t4P <- pow((t4 / (1 + t4)), k)
    
    a25 <- (1 + t3) * t6 * t2P * (k * t1P + k * t1P * t4 - t4P * k - k * t4P * t1 - t1P * t2 - t1P * t2 * t4 + t4P * t2 + t4P * t1 * t2) / (1
                                                                                                                                            + t2) / t2 / (t7 * t3P + t6 * t2P + t5 * t1P + t4P - t4P * t5 * t1 * t2 * t3 + t5 * t1P * t2 + t6 * t2P * t4 + t6 * t2P * t3 + t6 * t2P
                                                                                                                                                          * t1 + t7 * t3P * t4 + t7 * t3P * t2 + t7 * t3P * t1 - t4P * t6 * t2 - t4P * t7 * t3 - t4P * t5 * t1 + t4P * t2 * t3 - t4P * t5 * t3 - t4P
                                                                                                                                                          * t5 * t2 + t4P * t1 * t3 - t4P * t6 * t3 - t4P * t6 * t1 + t4P * t1 * t2 - t4P * t7 * t2 - t4P * t7 * t1 + t5 * t1P * t4 + t5 * t1P * t3
                                                                                                                                                          + t4P * t3 - t4P * t5 - t4P * t6 - t4P * t7 + t4P * t2 + t4P * t1 + t5 * t1P * t2 * t4 + t5 * t1P * t3 * t4 + t5 * t1P * t2 * t3 + t6
                                                                                                                                                          * t2P * t1 * t4 + t6 * t2P * t3 * t4 + t6 * t2P * t1 * t3 + t7 * t3P * t2 * t4 + t7 * t3P * t1 * t2 + t7 * t3P * t1 * t4 - t4P * t7 * t1 * t2 - t4P
                                                                                                                                                          * t6 * t2 * t3 - t4P * t6 * t1 * t2 - t4P * t7 * t2 * t3 - t4P * t7 * t1 * t3 - t4P * t6 * t1 * t3 - t4P * t5 * t1 * t2 - t4P * t5 * t1 * t3 - t4P
                                                                                                                                                          * t5 * t2 * t3 + t4P * t1 * t2 * t3 - t4P * t6 * t1 * t2 * t3 + t5 * t1P * t2 * t3 * t4 + t6 * t2P * t1 * t3 * t4 + t7 * t3P * t1 * t2 * t4 - t4P
                                                                                                                                                          * t7 * t1 * t2 * t3)
    
    if (k > 0) test <- abs(a25/a[2,5])
    a[2,5] <- a[2,5] + a25
    k <- k+1
      
  }
    
    
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ##A26
  test <- 100.0
  k <- 0
  while (test > criteria  &  k < maximumIteration)
  {
    t1P <- pow((t1 / (1 + t1)), k)
    t2P <- pow((t2 / (1 + t2)), k)
    t3P <- pow((t3 / (1 + t3)), k)
    t4P <- pow((t4 / (1 + t4)), k)
    
    t12kP <- pow((t1 / (1 + t1)), (2 * k))
    t22kP <- pow((t2 / (1 + t2)), (2 * k))
    t32kP <- pow((t3 / (1 + t3)), (2 * k))
    t42kP <- pow((t4 / (1 + t4)), (2 * k))
    
    a26 <- (-t4P * t5 * t3 * t3 * t22kP * t2 * t6 - 4 * t4P * t5 * t5 * t1 * t2P * t2 * t1P * t3 - 2 * t4P * t5 * t5 * t1 * t2P * t2 * t1P
            * t4 + 4 * t42kP * t1 * t3 * t3 * t2P * k * t5 * t2 + 4 * t5 * t1P * t3 * t4 * t22kP * t2 * t6 * t1 - 2 * (t2 * t2) * t3 * t42kP * t2P * t6
            + 2 * t5 * t5 * t12kP * t4 * t2P * (t2 * t2) + 4 * t42kP * t7 * t1 * t2P * k - 4 * t42kP * t7 * (t1 * t1) * t2P * k * t5 * t3 - 8 * t42kP
            * t3 * t2P * t2 * t7 * t1 + 2 * t42kP * (t1 * t1) * t3 * t3 * t2P * k * t7 - 2 * t42kP * t1 * t6 * t2P * k * t2 * t5 * t3 * t3 + 2 * t42kP * t2P
            * k * t7 * t2 - 4 * t4P * t7 * t1 * t22kP * t2 * t6 * t3 * t4 + (t2 * t2) * t7 * t7 * t32kP * t2P * (t4 * t4) - 2 * t42kP * t7 * (t1 * t1) * t2P
            * k * t5 + 2 * t42kP * (t1 * t1) * t2P * k * t2 * t7 + 2 * t5 * t5 * t12kP * t3 * t3 * t4 * t2P * t2 - 2 * t5 * t5 * t12kP * t4 * t2P * k * t3
            * t3 + 2 * t5 * t1P * t22kP * t2 * t6 * t4 + 2 * (t2 * t2) * t1 * t2P * t42kP * t7 * t7 - t42kP * t7 * t7 * t2P * k * t2 * t3 * t3 + t42kP
            * t7 * t3 * t3 * t6 * t2P * t2 - (t2 * t2) * t3 * t3 * t42kP * t2P * t6 * (t1 * t1) - 2 * t42kP * t5 * t5 * t2P * k * t2 * t3 + t42kP * t5
            * t5 * t3 * t3 * t2P * t2 - 2 * t42kP * t6 * t2P * k * t2 * t7 * t3 - t5 * t5 * t12kP * t3 * t3 * t2P * k * t2 * (t4 * t4) - 2 * (t2 * t2) * t42kP
            * t2P * t5 * (t1 * t1) + 2 * t7 * t3P * (t4 * t4) * t2P * t2 * t5 * t1P + 2 * t42kP * t5 * t5 * (t1 * t1) * t2P * t2 * t3 + t5 * t5 * t12kP
            * (t4 * t4) * t2P * (t2 * t2) - 2 * t7 * t7 * t32kP * t1 * (t4 * t4) * t2P * k * t2 - 2 * t42kP * t3 * t3 * t2P * t2 * t7 - 2 * t42kP * t7
            * t7 * t2P * k * t1 * t2 - (t2 * t2) * t42kP * t2P * t6 * (t1 * t1) + t4P * t3 * t3 * t22kP * t2 * t6 * t4 + 8 * t42kP * t1 * t3 * t2P
            * k * t5 * t2 - t4P * t7 * (t1 * t1) * t22kP * t2 * t6 - 2 * t42kP * t7 * t7 * t1 * t2P * k * t3 * t3 - 2 * (t2 * t2) * t42kP * t2P * t6 * t1
            + 2 * t5 * t5 * t12kP * t2P * t2 * t4 + 4 * t7 * t3P * t4 * t2P * t2 * t5 * t1P * t1 + t42kP * t3 * t3 * t2P * k * t6 - 4 * t7 * t3P * t4
            * t2P * t2 * t4P * t5 * t1 - 2 * t7 * t7 * t3P * t4 * t2P * t2 * t4P * t3 - 2 * t4P * t2P * k * t7 * t3P * (t1 * t1) - 2 * t4P * t2P
            * k * t5 * t1P * t1 - t6 * t22kP * t1 * t3 * t3 * k * t5 * t1P * (t4 * t4) + 2 * t6 * t22kP * t1 * t3 * t3 * k * t4P * t7 * t4 - 4 * t6 * t22kP
            * t1 * t3 * k * t4P + t6 * t22kP * k * t4P * t7 + t6 * t22kP * k * t4P * t5 - t7 * t3P * t3 * t4 * t2P * (t2 * t2) * t4P * t6 - 2 * t7
            * t3P * t3 * t4 * t2P * (t2 * t2) * t4P * t5 + t4P * t3 * t4 * t6 * t2P * k * t2 * t7 * t3P - 2 * t5 * t1P * t3 * t3 * t2P * k * t2 * t4P - 2
            * t42kP * t3 * t3 * t2P * t2 * t5 * (t1 * t1) - 2 * t5 * t1P * t1 * t6 * t2P * (t2 * t2) * t4P * t3 * t4 + 4 * t4P * t1 * t4 * t2P * (t2
                                                                                                                                                  * t2) * t7 * t3P + 2 * t4P * (t1 * t1) * t4 * t2P * (t2 * t2) * t7 * t3P + 2 * t4P * t5 * t2P * k * t7 * t3P * t3 + 2 * t4P * t5 * t5
            * t2P * k * t1P * t3 * t3 - 2 * t7 * t3P * t3 * t2P * k * t5 * t1P - 2 * t6 * t22kP * t1 * t3 * k * t5 * t1P - 2 * t6 * t22kP * t1 * t3
            * k * t7 * t3P - 2 * t6 * t22kP * t1 * t3 * t3 * k * t4P - t6 * t22kP * t1 * t3 * t3 * k * t5 * t1P - 4 * t6 * t22kP * t1 * t3 * k * t5 * t1P
            * t4 + 2 * t7 * t3P * t1 * t2P * k * t4P * t6 - 2 * t5 * t1P * t1 * t2P * k * t7 * t3P - t5 * t1P * t1 * t6 * t2P * (t2 * t2) * t4P - 2
            * t4P * (t1 * t1) * t3 * t2P * k * t7 * t3P * t2 + t6 * t22kP * (t1 * t1) * t3 * t3 * k * t4P * t5 - 4 * (t2 * t2) * t3 * t1 * t2P * t4P
            * t7 * t7 * t3P - 2 * t6 * t22kP * t4 * k * t5 * t1P - 2 * t6 * t22kP * t4 * k * t7 * t3P - t6 * t22kP * (t4 * t4) * k * t5 * t1P - t6 * t22kP
            * (t4 * t4) * k * t7 * t3P - 4 * t5 * t1P * t1 * t3 * t2P * k * t2 * t4P - 2 * t5 * t1P * t1 * t6 * t2P * (t2 * t2) * t4P * t3 + 2 * t5 * t1P
            * t4 * t2P * k * t4P * t7 - t4P * t4 * t6 * t2P * (t2 * t2) * t5 * t1P * t3 * t3 + 2 * t4P * t7 * t2P * k * t5 * t1P * t2 * t3 * t3 * t1 - 2
            * t6 * t22kP * t3 * k * t4P * t4 - 2 * t6 * t22kP * t3 * k * t7 * t3P * t4 - 2 * t4P * t2P * (t2 * t2) * t5 * t3 * t7 * t3P - t5 * t1P
            * t1 * t6 * t2P * (t2 * t2) * t4P * t3 * t3 * t4 + 2 * t7 * t3P * t6 * t2P * k * t2 * t4P * t1 * t3 + 4 * t4P * t5 * t5 * t2P * k * t1P
            * t3 * t4 + 2 * t5 * t5 * t1P * t1 * t2P * k * t2 * t4P + 2 * t5 * t1P * t1 * t3 * t3 * t4 * t2P * k * t4P * t7 * t2 + 2 * t7 * t7 * t3P * t2P
            * k * t2 * t4P * (t1 * t1) + 2 * t5 * t1P * t1 * t2P * (t2 * t2) * t4P * t4 - 4 * t4P * t7 * t3 * t2P * (t2 * t2) * t5 * t1P - 2 * (t2 * t2)
            * t3 * (t1 * t1) * t2P * t4P * t7 * t7 * t3P + 4 * t4P * t7 * t3 * t4 * t2P * k * t5 * t1P + 2 * t7 * t3P * t6 * t2P * k * t2 * t4P
            * t1 + t7 * t3P * t6 * t2P * k * t2 * t4P * (t1 * t1) * t4 - 2 * t7 * t3P * t2P * k * t2 * t4P * t4 - 4 * t4P * t3 * t2P * k * t5 * t1P - 2
            * t4P * t3 * t2P * k * t7 * t3P + 2 * t4P * t7 * t7 * t3 * t4 * t2P * k * t3P - 4 * t42kP * t7 * t1 * t2P * k * t5 * t3 * t3 + 2 * (t2 * t2)
            * t3 * (t1 * t1) * t2P * t42kP * t7 * t7 + 2 * t42kP * t3 * t3 * t2P * k * t5 * t2 - 2 * t42kP * t1 * t3 * t3 * t2P * k * t2 + 2 * t42kP
            * (t1 * t1) * t3 * t3 * t2P * k * t5 * t2 + 2 * t4P * t4 * t2P * t2 * t5 * t1P - 2 * t7 * t3P * t4 * t2P * t2 * t4P * t5 + t42kP * t2P
            * (t2 * t2) * t5 * t5 * t3 * t3 * (t1 * t1) - t6 * t22kP * k * t4P + t42kP * t2P * t2 - 4 * t42kP * t1 * t6 * t2P * k * t2 * t7 * t3 - t4P
            * t7 * t4 * t22kP * t2 * t6 - 2 * t42kP * t5 * t5 * (t1 * t1) * t2P * k * t2 * t3 + 2 * t42kP * t5 * t5 * t1 * t2P * t2 * t3 * t3 - 4 * t4P
            * t5 * t1 * t22kP * t2 * t6 * t3 + t5 * t1P * t3 * t3 * t22kP * t2 * t6 * t1 - 4 * t4P * t7 * t1 * t2P * t2 * t5 * t1P * t3 - 2 * t4P * t7
            * t1 * t2P * t2 * t5 * t1P * t4 + t4P * t4 * t22kP * t2 * t6 * (t1 * t1) + 2 * t7 * t3P * t4 * t22kP * t2 * t6 * t3 + 4 * t4P * t3 * t2P
            * t2 * t5 * t1P * t1 + 2 * t4P * t3 * t2P * t2 * t7 * t3P + 2 * t4P * t3 * t3 * t2P * t2 * t5 * t1P * t1 + 4 * t4P * t3 * t2P * t2 * t5
            * t1P - 2 * t42kP * t5 * t2P * k * t7 * t2 + 2 * (t2 * t2) * t7 * t7 * t32kP * t2P * t1 * (t4 * t4) - t42kP * t7 * t7 * t2P * k * t2 + 4
            * t42kP * t3 * t2P * k * t7 * t2 - 2 * t42kP * t5 * t2P * k * t7 * t3 * t3 - t42kP * t6 * t2P * t2 - 2 * t42kP * t7 * t2P * t2 - t42kP
            * t5 * t5 * t2P * k - t42kP * t7 * t7 * (t1 * t1) * t2P * k * t3 * t3 - 8 * (t2 * t2) * t3 * t42kP * t2P * t7 * t1 + 4 * t7 * t3P * t1 * t2P
            * t2 * t4P - t42kP * (t1 * t1) * t6 * t2P * t2 * t3 * t3 - 2 * (t2 * t2) * t3 * t3 * t42kP * t2P * t7 * (t1 * t1) + 4 * t42kP * t7 * (t1
                                                                                                                                                  * t1) * t2P * t2 * t5 * t3 - 2 * t42kP * t1 * t6 * t2P * k * t2 * t7 - t42kP * (t1 * t1) * t6 * t2P * t2 - t42kP * (t1 * t1) * t3 * t3 * t2P
            * k * t2 + 2 * t42kP * t1 * t6 * t2P * k * t2 * t3 * t3 + t42kP * t2P * (t2 * t2) * t5 * t6 * (t1 * t1) - 2 * t42kP * t7 * t1 * t2P * k * t6
            * t3 * t3 - 4 * t42kP * t7 * t1 * t2P * k * t5 - 2 * t4P * t5 * t1 * t22kP * t2 * t6 * t3 * t3 * t4 - 2 * t42kP * (t1 * t1) * t6 * t2P * t2
            * t3 - 2 * t7 * t7 * t3P * t3 * t2P * t2 * t4P + 2 * t42kP * t5 * t1 * t3 * t3 * t6 * t2P * t2 - 4 * t4P * t5 * t1 * t2P * t2 * t7 * t3P
            + 4 * t7 * t3P * t4 * t2P * t2 * t5 * t1P * t3 + 2 * t7 * t7 * t32kP * (t4 * t4) * t2P * t2 * t1 + 2 * t42kP * t2P * (t2 * t2) * t5 * t7
            * (t1 * t1) + t5 * t1P * t1 * (t4 * t4) * t22kP * t2 * t6 + 2 * t42kP * t7 * (t1 * t1) * t2P * k + 2 * (t2 * t2) * t3 * t3 * t42kP * t2P
            * t1 + 2 * t4P * t3 * t2P * t2 * t7 * t3P * (t1 * t1) + t42kP * t2P * k * t6 * (t1 * t1) + 2 * (t2 * t2) * t7 * t7 * t32kP * t2P * t1
            + 2 * t42kP * t2P * (t2 * t2) * t5 * t6 * t1 + 4 * t42kP * (t1 * t1) * t3 * t2P * k * t5 - 2 * t42kP * t3 * t3 * t2P * t2 * t7 * (t1 * t1) - t7
            * t3P * t4 * t2P * t2 * t4P * t6 * (t1 * t1) * t3 - 2 * t7 * t7 * t3P * t4 * t2P * t2 * t4P * (t1 * t1) * t3 - 2 * t7 * t3P * t4 * t2P
            * t2 * t4P * t5 * (t1 * t1) * t3 - t42kP * t7 * (t1 * t1) * t2P * k * t6 + 4 * t7 * t7 * t32kP * t4 * t2P * t2 * t1 + 4 * t42kP * t7 * t1
            * t2P * t2 * t5 - 2 * t42kP * t5 * t2P * k * t6 * t1 - 2 * t7 * t3P * t4 * t2P * t2 * t4P * t6 * t1 * t3 - 2 * t42kP * (t1 * t1) * t6 * t2P
            * k * t2 * t7 * t3 + 2 * t42kP * t3 * t2P * k * t6 - t42kP * t6 * t2P * k * t2 * t5 - 2 * t7 * t7 * t32kP * t4 * t2P * k - 2 * t42kP * t7
            * t7 * t2P * k * t2 * t3 - 2 * t42kP * t5 * t2P * k * t7 * t2 * t3 * t3 - t7 * t3P * t4 * t2P * t2 * t4P * t6 - 2 * t7 * t3P * t2P * t2
            * t4P * t5 - t42kP * t3 * t3 * t6 * t2P * t2 - 2 * t42kP * t3 * t3 * t6 * t2P * t2 * t1 + t5 * t5 * t12kP * t3 * t3 * (t4 * t4) * t2P
            * t2 + t7 * t7 * t32kP * (t1 * t1) * t2P * t2 + 2 * t5 * t5 * t12kP * t3 * (t4 * t4) * t2P * t2 + 2 * (t2 * t2) * t3 * t42kP * t7 * t2P
            * t6 + t7 * t3P * (t4 * t4) * t22kP * t2 * t6 * (t1 * t1) * t3 + 2 * (t2 * t2) * t3 * t3 * t1 * t2P * t42kP * t7 * t7 - 2 * t7 * t7 * t32kP
            * t2P * k * t2 * t4 + 2 * t7 * t7 * t32kP * t4 * t2P * t2 * (t1 * t1) - t42kP * t2P * k * (t1 * t1) + 2 * (t2 * t2) * t1 * t6 * t2P * t42kP
            * t7 - 2 * t5 * t5 * t12kP * t4 * t2P * k + 4 * t5 * t1P * t1 * t2P * (t2 * t2) * t4P * t3 + 2 * t4P * t4 * t2P * (t2 * t2) * t5 * t1P
            * t3 * t3 - 2 * t5 * t1P * t1 * t3 * (t4 * t4) * t2P * k * t7 * t3P * t2 - 2 * t4P * t1 * t3 * t3 * t2P * k * t5 * t1P - t6 * t22kP * t3 * t3
            * k * t5 * t1P * (t4 * t4) + 4 * t4P * t5 * t1 * t2P * k * t2 * t7 * t3P + 2 * t4P * t5 * (t1 * t1) * t2P * k * t2 * t7 * t3P + t4P * t6
            * t2P * k * t2 * t7 * t3P + 4 * (t2 * t2) * t7 * t3P * t2P * t4P * t1 + t6 * t22kP * (t1 * t1) * t3 * t3 * k * t4P * t5 * t4 - 4 * t7 * t3P
            * t2P * k * t2 * t4P * t1 - 2 * (t2 * t2) * t7 * t7 * t3P * t2P * t4P * (t1 * t1) + 4 * t5 * t5 * t1P * t1 * t3 * t4 * t2P * k * t4P * t2
            + 2 * t5 * t5 * t1P * t1 * t3 * t3 * t4 * t2P * k * t4P - 2 * t4P * t3 * t3 * t2P * k * t5 * t1P - 4 * t4P * t3 * t2P * k * t5 * t1P * t4 - 2
            * t4P * t3 * t2P * k * t7 * t3P * t2 - 2 * t4P * t3 * t2P * k * t7 * t3P * t4 + 2 * (t2 * t2) * t7 * t3P * t2P * t4P + 4 * t4P * t5
            * t5 * t2P * k * t1P * t3 * t1 + 2 * t7 * t3P * t1 * t4 * t2P * k * t4P * t6 + 4 * t7 * t3P * t1 * t4 * t2P * k * t4P * t5 + 4 * t7 * t7 * t3P
            * t1 * t4 * t2P * k * t4P + 2 * t5 * t1P * t1 * t2P * (t2 * t2) * t7 * t3P + 2 * t7 * t3P * t3 * t4 * t6 * t2P * k * t4P * t1 * t2 - (t2
                                                                                                                                                  * t2) * t7 * t3P * t2P * t4P * t6 * (t1 * t1) - 4 * t5 * t1P * t1 * t3 * t4 * t2P * k * t7 * t3P - 2 * t5 * t1P * t1 * t3 * t3 * t4 * t2P
            * k * t4P - 2 * (t2 * t2) * t7 * t3P * t2P * t4P * t5 * (t1 * t1) - (t2 * t2) * t7 * t3P * t2P * t4P * t6 * t3 - 2 * (t2 * t2) * t7 * t3P
            * t2P * t4P * t6 * t1 + 2 * t7 * t3P * t3 * t2P * (t2 * t2) * t5 * t1P * (t4 * t4) - 4 * (t2 * t2) * t7 * t3P * t2P * t4P * t5 * t1
            + 2 * t6 * t22kP * t1 * k * t4P * t7 + 2 * t6 * t22kP * t1 * k * t4P * t5 + 2 * t4P * t5 * (t1 * t1) * t2P * k * t2 * t7 * t3P * t3 - 4 * t7
            * t3P * t1 * t4 * t2P * k * t5 * t1P * t2 - 4 * t7 * t3P * t1 * t4 * t2P * k * t5 * t1P - 4 * t5 * t1P * t4 * t2P * k * t2 * t7 * t3P - t6
            * t22kP * (t1 * t1) * t3 * t3 * k * t4P * t4 - 4 * t6 * t22kP * t1 * t3 * k * t4P * t4 + 2 * t5 * t1P * t4 * t2P * k * t2 * t4P * t7 + 4 * t4P
            * t5 * t4 * t2P * k * t2 * t7 * t3P * t1 - 2 * t7 * t3P * (t1 * t1) * t4 * t2P * k * t4P * t2 - 2 * t7 * t3P * (t1 * t1) * t4 * t2P * k * t4P
            * t3 - 2 * t7 * t3P * t1 * (t4 * t4) * t2P * k * t5 * t1P + 4 * t4P * t5 * t5 * t4 * t2P * k * t1P * t2 * t3 - 2 * t4P * t7 * t3 * t3 * t2P
            * (t2 * t2) * t5 * t1P * t4 * t1 + 2 * t7 * t7 * t3P * t3 * t4 * t2P * k * t4P * (t1 * t1) * t2 - 4 * t7 * t3P * t1 * t4 * t2P * k * t4P * t3 - 4
            * t4P * t7 * t3 * t2P * (t2 * t2) * t5 * t1P * t4 * t1 + 2 * t7 * t3P * (t4 * t4) * t2P * (t2 * t2) * t5 * t1P + 4 * t5 * t1P * t1 * t3
            * t4 * t2P * k * t4P * t7 + 4 * t5 * t5 * t1P * t1 * t3 * t4 * t2P * k * t4P + t4P * t6 * t2P * k * t2 * t7 * t3P * t3 - 2 * t6 * t22kP
            * t3 * t3 * k * t5 * t1P * t4 + 2 * t4P * t5 * t5 * t2P * k * t1P * t4 * t1 + 4 * t4P * t5 * t2P * k * t7 * t3P * t1 * t3 - 2 * t5 * t5 * t12kP
            * t3 * t2P * k * t2 + 2 * t7 * t3P * t4 * t2P * t2 * t4P - 2 * t4P * t7 * t1 * t2P * t2 * t5 * t1P - 2 * t5 * t1P * t1 * t3 * t2P * k
            * t2 * t7 * t3P + 2 * t4P * t5 * t5 * t4 * t2P * k * t2 * t1P + 2 * t7 * t3P * (t1 * t1) * t4 * t2P * k * t4P * t5 * t3 + 2 * t6 * t22kP
            * (t1 * t1) * t3 * k * t4P * t7 * t4 + 2 * t6 * t22kP * t1 * t3 * t3 * k * t4P * t5 * t4 + 2 * t6 * t22kP * (t1 * t1) * t3 * k * t4P * t5 * t4 - 2
            * t6 * t2P * t2 * t3 * t1P * t1 * t4P * t5 - 4 * t4P * t1 * t3 * t2P * k * t5 * t1P - 4 * t4P * t1 * t3 * t2P * k * t7 * t3P - 2 * t5
            * t1P * t1 * t3 * t3 * t2P * k * t2 * t4P - 2 * (t2 * t2) * t7 * t3P * t2P * t4P * t6 * t1 * t3 - 2 * t4P * t2P * (t2 * t2) * t5 * t3
            * t7 * t3P * (t1 * t1) - t6 * t22kP * t3 * t3 * k * t4P - t6 * t22kP * t3 * t3 * k * t5 * t1P - 4 * t6 * t22kP * t3 * k * t5 * t1P * t4 - (t2
                                                                                                                                                       * t2) * t7 * t3P * t2P * t4P * t6 * (t1 * t1) * t3 - t4P * t6 * t2P * (t2 * t2) * t5 * t1P - 2 * t4P * t6 * t2P * (t2 * t2) * t5 * t1P
            * t3 - 2 * t7 * t3P * t3 * t2P * k * t5 * t1P * (t4 * t4) - t4P * t7 * t22kP * t2 * t6 + 2 * t42kP * t5 * (t1 * t1) * t6 * t2P * t2 * t3 - t4P
            * t5 * (t1 * t1) * t22kP * t2 * t6 * t3 * t3 * t4 + 4 * t42kP * t2P * (t2 * t2) * t5 * t5 * t3 * t1 - 2 * t42kP * t2P * t2 * t5 + t42kP
            * (t1 * t1) * t3 * t3 * t2P * k * t6 + 4 * t42kP * (t1 * t1) * t2P * k * t2 * t7 * t3 - 4 * t42kP * t7 * t2P * k * t5 * (t1 * t1) * t2 * t3 - 2
            * t4P * t7 * (t1 * t1) * t22kP * t2 * t6 * t3 + t5 * t5 * t12kP * t2P * (t2 * t2) - t42kP * t2P * k * t2 + 4 * (t2 * t2) * t3 * t1 * t2P
            * t42kP * t7 * t6 - t4P * t5 * (t1 * t1) * t22kP * t2 * t6 * t4 + (t2 * t2) * t3 * t3 * t42kP * t2P * (t1 * t1) - t7 * t3P * t4 * t2P
            * t2 * t4P * t6 * (t1 * t1) - 4 * t4P * t7 * t7 * t1 * t2P * t2 * t3P + 4 * t42kP * t3 * t2P * k * t7 + t42kP * t5 * t6 * t2P * t2 + 2
            * t42kP * t5 * t6 * t2P * t2 * t1 + 2 * t42kP * t5 * t6 * t2P * t2 * t3 + 4 * t42kP * t5 * t6 * t2P * t2 * t1 * t3 + 2 * t7 * t3P * t3 * t2P
            * t2 * t5 * t1P + 4 * t7 * t3P * t4 * t2P * t2 * t4P * t1 * t3 + 2 * t4P * t3 * t3 * t22kP * t2 * t6 * t1 + t6 * t2P * t1P * k * t1 * t4P
            * t5 - t5 * t5 * t12kP * t3 * t3 * t2P * k * t2 - 2 * t6 * t2P * t3 * t4 * t1P * t1 * t4P * t5 * t2 + 2 * t5 * t5 * t1P * t1 * t3 * t3 * t4 * t2P
            * k * t4P * t2 + 4 * t7 * t3P * t4 * t2P * (t2 * t2) * t5 * t1P - 2 * t5 * t1P * t1 * t4 * t2P * k * t2 * t4P + t6 * t22kP * (t1 * t1)
            * t4 * k * t4P * t5 - 4 * t7 * t3P * t1 * t4 * t2P * k * t4P - 2 * t5 * t1P * t1 * t2P * k * t2 * t7 * t3P - 2 * t7 * t3P * t4 * t2P * k
            * t4P - 2 * t5 * t1P * t1 * t3 * t2P * k * t7 * t3P + 2 * t7 * t7 * t3P * t4 * t2P * k * t4P - t5 * t1P * t1 * t6 * t2P * (t2 * t2)
            * t4P * t4 + 2 * t6 * t22kP * t3 * k * t4P * t5 * t4 + 2 * t6 * t22kP * t3 * k * t4P * t7 * t4 - t6 * t22kP * t4 * k * t4P - 2 * t4P * t3
            * t3 * t2P * k * t5 * t1P * t4 + 2 * t7 * t3P * t2P * (t2 * t2) * t5 * t1P + t6 * t22kP * (t1 * t1) * t3 * t3 * k * t4P * t7 + 2 * t4P
            * t7 * t2P * k * t5 * t1P * t2 * t3 * t3 * t4 - 2 * t5 * t5 * t1P * t1 * t2P * (t2 * t2) * t4P * t4 + 4 * t5 * t1P * t1 * t2P * (t2 * t2)
            * t4P * t3 * t4 + 2 * t5 * t1P * t1 * t2P * (t2 * t2) * t7 * t3P * (t4 * t4) + 4 * t7 * t7 * t3P * t2P * k * t2 * t4P * t1 * t4 + 2 * t4P
            * t5 * t4 * t2P * k * t2 * t7 * t3P * (t1 * t1) + t7 * t3P * t4 * t2P * k * t4P * t6 + 2 * t4P * t7 * t1 * t2P * k * t5 * t1P + 4 * t4P
            * t7 * t7 * t1 * t2P * k * t3P + 4 * t4P * t7 * t1 * t2P * k * t5 * t1P * t3 - 4 * t5 * t5 * t1P * t1 * t2P * (t2 * t2) * t4P * t3 + t6
            * t22kP * t4 * k * t4P * t7 + t6 * t22kP * t4 * k * t4P * t5 - 2 * t5 * t5 * t1P * t1 * t2P * (t2 * t2) * t4P * t3 * t3 - 2 * t5 * t1P
            * t1 * t2P * (t2 * t2) * t4P * t7 * t4 - 2 * t5 * t5 * t1P * t1 * t2P * (t2 * t2) * t4P + 2 * t6 * t22kP * t1 * t4 * k * t4P * t7 + 2 * t7
            * t7 * t3P * t2P * k * t4P - 2 * (t2 * t2) * t7 * t3P * t2P * t4P * t5 - (t2 * t2) * t7 * t3P * t2P * t4P * t6 + 4 * t5 * t1P
            * t3 * t2P * k * t2 * t4P * t7 + 4 * t5 * t5 * t1P * t3 * t2P * k * t2 * t4P - 2 * t7 * t3P * t2P * k * t2 * t4P + 2 * t4P * t5 * t5 * t4
            * t2P * k * t2 * t1P * t1 - 2 * t4P * t1 * t4 * t6 * t2P * (t2 * t2) * t7 * t3P - t4P * (t1 * t1) * t4 * t6 * t2P * (t2 * t2) * t7 * t3P - 2
            * t7 * t3P * t2P * k * t2 * t5 * t1P + t4P * t6 * t2P * k * t2 * t7 * t3P * t4 + t7 * t3P * t6 * t2P * k * t2 * t4P * (t1 * t1) * t3
            + t7 * t3P * t6 * t2P * k * t2 * t4P * (t1 * t1) - 2 * t4P * (t1 * t1) * t3 * t2P * k * t7 * t3P - 4 * t4P * t1 * t3 * t2P * k * t7 * t3P
            * t2 + 2 * t5 * t1P * t1 * t2P * (t2 * t2) * t4P * t3 * t3 * t4 + 4 * t5 * t1P * t1 * t2P * (t2 * t2) * t7 * t3P * t4 - 4 * t4P * t5 * t5
            * t3 * t4 * t2P * (t2 * t2) * t1P + 4 * t4P * t7 * t7 * t1 * t2P * k * t3P * t3 + 2 * t4P * t7 * t1 * t2P * k * t5 * t1P * t3 * t3 - 2 * t7
            * t7 * t3P * t3 * t4 * t2P * (t2 * t2) * t4P + 2 * t7 * t3P * t3 * t4 * t2P * (t2 * t2) * t4P * (t1 * t1) - 2 * t4P * t7 * t3 * t3 * t2P
            * (t2 * t2) * t5 * t1P - 4 * t4P * t7 * t3 * t2P * (t2 * t2) * t5 * t1P * t4 - t6 * t22kP * k * t5 * t1P - t6 * t22kP * k * t7 * t3P - 2
            * t6 * t22kP * t1 * (t4 * t4) * k * t7 * t3P + 2 * t5 * t1P * t1 * t3 * t3 * t4 * t2P * k * t4P * t7 + 2 * (t2 * t2) * t7 * t3P * t2P * t4P
            * (t1 * t1) - 2 * t6 * t22kP * t3 * k * t5 * t1P - t6 * t22kP * t3 * k * t7 * t3P + 2 * t7 * t3P * t3 * t4 * t2P * (t2 * t2) * t4P - 2 * t7
            * t3P * t1 * (t4 * t4) * t2P * k * t5 * t1P * t2 + 2 * t4P * t5 * t4 * t2P * k * t2 * t7 * t3P + t6 * t22kP * (t1 * t1) * t3 * t3 * k * t4P
            * t7 * t4 + t7 * t3P * t3 * t4 * t6 * t2P * k * t4P * (t1 * t1) * t2 + t6 * t22kP * t3 * t3 * k * t4P * t5 * t4 - t5 * t1P * t1 * t6 * t2P
            * (t2 * t2) * t4P * t3 * t3 + t6 * t22kP * t3 * t3 * k * t4P * t7 * t4 - 2 * t6 * t22kP * t1 * t4 * k * t4P - t6 * t22kP * (t1 * t1) * t4
            * k * t4P - 4 * t7 * t3P * t3 * t2P * k * t5 * t1P * t4 - 2 * t6 * t22kP * t3 * k * t4P - 2 * t7 * t3P * (t1 * t1) * t4 * t2P * k * t4P
            * t2 * t3 - 2 * t5 * t1P * t4 * t2P * k * t2 * t4P - 2 * t5 * t1P * t1 * t2P * k * t2 * t4P + 4 * t5 * t5 * t1P * t1 * t3 * t2P * k * t2 * t4P
            + 4 * t4P * t5 * t4 * t2P * k * t2 * t7 * t3P * t1 * t3 + 2 * t7 * t7 * t3P * t3 * t2P * k * t4P + t7 * t3P * t2P * k * t4P * t6 - t6
            * t22kP * (t1 * t1) * t3 * t3 * k * t4P - t6 * t22kP * (t1 * t1) * t3 * k * t7 * t3P - 4 * t6 * t22kP * t1 * t3 * k * t7 * t3P * t4 - 2 * t6
            * t22kP * (t1 * t1) * t3 * k * t4P + 2 * (t2 * t2) * t3 * t4P * t2P * t7 * t3P * (t1 * t1) + 2 * t4P * t5 * t4 * t2P * k * t2 * t7 * t3P
            * (t1 * t1) * t3 - 2 * t7 * t3P * (t4 * t4) * t2P * k * t5 * t1P + 4 * t7 * t3P * t3 * t4 * t2P * (t2 * t2) * t4P * t1 + 2 * t7 * t7 * t3P
            * t2P * k * t2 * t4P + 2 * t4P * t7 * t1 * t2P * k * t5 * t1P * t4 + 2 * t4P * t7 * t7 * (t1 * t1) * t2P * k * t3P + t7 * t3P * (t1
                                                                                                                                             * t1) * t4 * t2P * k * t4P * t6 * t3 - 2 * t7 * t3P * t2P * k * t5 * t1P - 4 * t7 * t7 * t3P * t3 * t4 * t2P * (t2 * t2) * t4P * t1 + 2
            * t7 * t7 * t3P * (t1 * t1) * t4 * t2P * k * t4P * t3 + t7 * t3P * t3 * t2P * k * t4P * t6 + t42kP * t2P * t2 * (t1 * t1) - 2 * t7 * t7
            * t32kP * (t1 * t1) * t4 * t2P * k * t2 - 4 * (t2 * t2) * t3 * t3 * t42kP * t2P * t7 * t1 - t42kP * (t1 * t1) * t6 * t2P * k * t2 * t5 + 2
            * t7 * t3P * t2P * t2 * t5 * t1P + 2 * t7 * t3P * t2P * t2 * t4P + 2 * t42kP * t5 * t2P * k * (t1 * t1) + t4P * t4 * t5 * t1P
            * k * t1 * t6 * t2P * t2 + t4P * t4 * t5 * t1P * k * t1 * t6 * t2P * t3 * t3 * t2 + t4P * t4 * t5 * t1P * k * t1 * t6 * t2P * t3 * t3 + t4P
            * t4 * t5 * t1P * k * t6 * t2P + 2 * t4P * t4 * t5 * t1P * k * t6 * t2P * t3 + t4P * t4 * t5 * t1P * k * t6 * t2P * t2 + t4P * t4 * t5
            * t1P * k * t6 * t2P * t3 * t3 + 2 * t6 * t2P * t3 * t1P * k * t4P * t5 + 2 * t42kP * t5 * (t1 * t1) * t2P * k * t2 + t4P * t3 * t3 * t5
            * t1P * k * t1 * t6 * t2P + 2 * t4P * t3 * t5 * t1P * k * t1 * t6 * t2P + 2 * t4P * t4 * t5 * t1P * k * t1 * t6 * t2P * t3 + t4P * t4
            * t5 * t1P * k * t1 * t6 * t2P + t4P * t3 * t3 * t22kP * t2 * t6 * (t1 * t1) * t4 - t42kP * t3 * t3 * t6 * t2P * k * t7 + 2 * t7 * t3P * t1
            * t22kP * t2 * t6 * t3 + 2 * t42kP * t5 * t3 * t3 * t2P * t2 * t7 + 2 * t4P * t2P * t2 * t5 * t1P * t1 + t42kP * t5 * (t1 * t1) * t3 * t3
            * t6 * t2P * t2 + 4 * t7 * t3P * t4 * t2P * t2 * t4P * t1 + t42kP * t5 * t6 * t2P * t2 * t3 * t3 - 4 * t42kP * t5 * t5 * t2P * k * t1
            * t3 - 2 * t42kP * (t1 * t1) * t6 * t2P * k * t2 * t5 * t3 + (t2 * t2) * (t1 * t1) * t2P * t42kP * t7 * t7 + t7 * t7 * t32kP * t2P * t2
            + 4 * t4P * t3 * t2P * t2 * t7 * t3P * t1 - t42kP * t6 * t2P * k * t7 - 4 * t42kP * t5 * t2P * k * t6 * t1 * t3 + 4 * t42kP * t2P
            * (t2 * t2) * t5 * t3 * t3 * t7 * t1 - 2 * t42kP * t5 * t5 * t2P * k * t3 - t7 * t7 * t32kP * (t1 * t1) * t2P * k - 4 * t5 * t5 * t12kP * t4 * t2P
            * k * t3 + 8 * t42kP * t1 * t3 * t2P * k * t5 + 2 * t4P * t4 * t5 * t1P * k * t1 * t6 * t2P * t3 * t2 + t4P * t5 * t1P * k * t6 * t2P + 2
            * t4P * t3 * t5 * t1P * k * t1 * t6 * t2P * t2 + t4P * t3 * t3 * t5 * t1P * k * t1 * t6 * t2P * t2 + t6 * t2P * t2 * t1P * k * t4P * t5
            + t4P * t2 * t3 * t3 * t5 * t1P * k * t6 * t2P * t4 + 2 * t42kP * t3 * t3 * t2P * k * t7 + 2 * t5 * t1P * t3 * t3 * t4 * t22kP * t2 * t6 - 4
            * t42kP * t7 * t1 * t2P * k * t6 * t3 - t4P * t6 * t3 * t2P * t2 * t7 * t3P * (t1 * t1) + 2 * t5 * t5 * t12kP * (t4 * t4) * t2P * (t2
                                                                                                                                               * t2) * t3 - t4P * t6 * t3 * t3 * t2P * t2 * t5 * t1P - (t2 * t2) * t42kP * t2P * t6 + t42kP * t5 * (t1 * t1) * t6 * t2P * t2 - 4 * t42kP
            * t3 * t6 * t2P * t2 * t1 - 4 * (t2 * t2) * t42kP * t2P * t5 * t1 + 4 * t42kP * t5 * t5 * t1 * t2P * t2 * t3 - t42kP * t5 * t5 * (t1 * t1)
            * t2P * k * t2 * t3 * t3 - 4 * t4P * t5 * t5 * t3 * t2P * t2 * t1P - 2 * t4P * t5 * t3 * t2P * t2 * t7 * t3P + 4 * t42kP * t1 * t2P
            * k * t2 * t7 + t5 * t5 * t12kP * t2P * (t2 * t2) * t3 * t3 - 2 * t42kP * t3 * t3 * t2P * t2 * t5 - t4P * t5 * t3 * t3 * t22kP * t2 * t6 * t4 - 2
            * t42kP * t2P * (t2 * t2) * t5 * t3 * t3 * (t1 * t1) + t42kP * t5 * t5 * t2P * t2 + (t2 * t2) * (t1 * t1) * t6 * t2P * t42kP * t7 - 4
            * (t2 * t2) * t7 * t7 * t3P * t2P * t4P * t1 * t4 + 2 * t7 * t7 * t3P * t2P * k * t2 * t4P * (t1 * t1) * t4 + t6 * t22kP * t3 * t3 * k * t4P
            * t5 + t6 * t22kP * t3 * t3 * k * t4P * t7 - 4 * t5 * t1P * t1 * t3 * t4 * t2P * k * t4P * t2 + 4 * t5 * t1P * t1 * t3 * t4 * t2P * k * t4P
            * t7 * t2 - 2 * t4P * t5 * t5 * t2P * (t2 * t2) * t1P - 4 * t4P * t5 * t5 * t2P * (t2 * t2) * t1P * t3 - 2 * t4P * t5 * t5 * t2P * (t2
                                                                                                                                                * t2) * t1P * t4 - 4 * t7 * t3P * t1 * t4 * t2P * k * t4P * t2 * t3 - 2 * t7 * t3P * t3 * (t4 * t4) * t2P * k * t5 * t1P * t2 + 4 * t7 * t7
            * t3P * t3 * t4 * t2P * k * t4P * t1 * t2 + 2 * t4P * t5 * t5 * t4 * t2P * k * t1P * t2 * t3 * t3 - 2 * t4P * t7 * t3 * t3 * t2P * (t2 * t2)
            * t5 * t1P * t1 - 2 * t4P * t7 * t3 * t3 * t2P * (t2 * t2) * t5 * t1P * t4 + 4 * t4P * t5 * t1 * t2P * k * t2 * t7 * t3P * t3 - 4 * t5 * t5
            * t1P * t1 * t2P * (t2 * t2) * t4P * t3 * t4 - 2 * t7 * t3P * (t1 * t1) * t4 * t2P * k * t4P - 4 * t7 * t3P * t1 * t4 * t2P * k * t4P
            * t2 - 2 * (t2 * t2) * t7 * t7 * t3P * t2P * t4P * (t1 * t1) * t4 - 4 * t4P * t3 * t4 * t2P * k * t2 * t5 * t1P - 2 * t4P * t3 * t3 * t4 * t2P
            * k * t2 * t5 * t1P + 4 * t7 * t7 * t3P * t2P * k * t2 * t4P * t1 - t6 * t22kP * t3 * k * t7 * t3P * (t4 * t4) - 2 * t6 * t22kP * t3 * k * t5
            * t1P * (t4 * t4) - t6 * t22kP * t3 * t3 * k * t4P * t4 + 4 * t7 * t7 * t3P * t2P * k * t2 * t4P * t1 * t3 - 2 * t5 * t1P * t1 * t2P * (t2
                                                                                                                                                    * t2) * t4P * t7 + 2 * t7 * t3P * t3 * t2P * (t2 * t2) * t5 * t1P + 4 * t7 * t3P * t3 * t2P * (t2 * t2) * t5 * t1P * t4 - 2 * t5 * t5 * t1P
            * t1 * t2P * (t2 * t2) * t4P * t3 * t3 * t4 + 2 * t5 * t1P * t1 * t2P * (t2 * t2) * t7 * t3P * (t4 * t4) * t3 - (t2 * t2) * t7 * t3P * t2P
            * t4P * t6 * t4 - 2 * (t2 * t2) * t7 * t3P * t2P * t4P * t5 * t4 + 2 * t7 * t7 * t3P * t2P * k * t2 * t4P * (t1 * t1) * t3 + 2 * t7 * t7 * t3P
            * t2P * k * t2 * t4P * t4 - 2 * t5 * t1P * (t4 * t4) * t2P * k * t2 * t7 * t3P + 2 * t5 * t1P * t1 * t2P * (t2 * t2) * t4P + 2 * t4P
            * t5 * t2P * k * t7 * t3P * (t1 * t1) * t3 + 2 * t4P * t5 * t5 * t2P * k * t1P * t3 * t3 * t1 + 2 * t4P * t5 * t2P * k * t7 * t3P * t2 * t3
            + 2 * t4P * t5 * t5 * t2P * k * t1P * t1 + 2 * t4P * t5 * t2P * k * t7 * t3P * (t1 * t1) + 2 * t4P * t7 * t2P * k * t5 * t1P * t2 * t4
            * t1 + 4 * t4P * t7 * t2P * k * t5 * t1P * t2 * t3 * t1 + 2 * t7 * t7 * t3P * (t1 * t1) * t4 * t2P * k * t4P + 2 * t4P * t2P * (t2 * t2)
            * t5 * t1P * t3 * t3 + 2 * t4P * t2P * (t2 * t2) * t5 * t1P + 4 * t4P * t2P * (t2 * t2) * t5 * t1P * t3 - 2 * t4P * t3 * t4 * t2P
            * k * t2 * t7 * t3P + 2 * t4P * t5 * t2P * k * t7 * t3P * t4 * t3 - 4 * t5 * t1P * t1 * t3 * t4 * t2P * k * t7 * t3P * t2 - 2 * t5 * t1P * t1
            * t3 * (t4 * t4) * t2P * k * t7 * t3P + t7 * t3P * (t1 * t1) * t4 * t2P * k * t4P * t6 + 2 * t7 * t3P * t1 * t4 * t2P * k * t4P * t6 * t3
            + 2 * t7 * t3P * (t1 * t1) * t4 * t2P * k * t4P * t5 + 4 * t7 * t7 * t3P * t1 * t4 * t2P * k * t4P * t3 + 4 * t7 * t3P * t1 * t4 * t2P * k
            * t4P * t5 * t3 + 2 * t4P * t5 * t4 * t2P * k * t2 * t7 * t3P * t3 - 2 * t4P * t2P * k * t5 * t1P * t2 - 2 * t4P * t2P * k * t5 * t1P - 2
            * t4P * t2P * k * t7 * t3P - 2 * t4P * t2P * k * t5 * t1P * t4 + 2 * t4P * t7 * t2P * k * t5 * t1P - 4 * t4P * t2P * k * t7 * t3P
            * t1 - t6 * t22kP * t1 * k * t5 * t1P - 2 * t6 * t22kP * t1 * k * t7 * t3P - t6 * t22kP * (t1 * t1) * k * t7 * t3P - t6 * t22kP * (t1
                                                                                                                                               * t1) * k * t4P + t6 * t22kP * (t1 * t1) * k * t4P * t7 + t6 * t22kP * (t1 * t1) * k * t4P * t5 - 2 * t6 * t22kP * t1 * k * t4P - 2 * t7
            * t3P * t4 * t2P * t2 * t4P * t5 * (t1 * t1) - 4 * t4P * t2P * (t2 * t2) * t5 * t3 * t7 * t3P * t1 + 2 * t4P * t7 * t3 * t3 * t4 * t2P
            * k * t5 * t1P + t6 * t22kP * (t1 * t1) * t4 * k * t4P * t7 - 2 * (t2 * t2) * t7 * t7 * t3P * t2P * t4P + t7 * t3P * t3 * t2P * k * t4P
            * t6 * (t1 * t1) - 4 * t5 * t1P * t1 * t3 * t4 * t2P * k * t4P + 2 * t4P * t7 * t2P * k * t5 * t1P * t2 * t1 + 4 * t4P * t7 * t2P * k * t5
            * t1P * t2 * t3 * t4 - 4 * t4P * t7 * t3 * t2P * (t2 * t2) * t5 * t1P * t1 - t6 * t22kP * t1 * (t4 * t4) * k * t5 * t1P + 2 * t4P * t7 * t7
            * t3 * t4 * t2P * k * t3P * t2 + 2 * t4P * t5 * t5 * t2P * k * t1P * t2 + 2 * t4P * t5 * t5 * t2P * k * t1P + 2 * t4P * t5 * t2P * k
            * t7 * t3P + 4 * t4P * t5 * t5 * t2P * k * t1P * t3 + 2 * t4P * t5 * t5 * t2P * k * t1P * t4 + 4 * t4P * t5 * t2P * k * t7 * t3P * t1
            + 2 * t4P * t5 * t2P * k * t7 * t3P * t2 + 2 * t4P * t5 * t2P * k * t7 * t3P * t4 + 2 * (t2 * t2) * t3 * t4P * t2P * t7 * t3P - 2 * t4P
            * t5 * t5 * t2P * (t2 * t2) * t1P * t3 * t3 - 2 * t5 * t1P * t1 * t3 * t3 * t4 * t2P * k * t4P * t2 + 4 * (t2 * t2) * t3 * t4P * t2P * t7
            * t3P * t1 + 4 * t5 * t1P * t1 * t2P * (t2 * t2) * t7 * t3P * t4 * t3 + 2 * t7 * t3P * t3 * t2P * k * t4P * t6 * t1 - 2 * t4P * t7 * t4
            * t2P * (t2 * t2) * t5 * t1P + 2 * t4P * t7 * t7 * (t1 * t1) * t2P * k * t3P * t3 - 4 * t5 * t1P * t3 * t2P * k * t2 * t4P + 2 * t5 * t1P
            * t1 * t2P * (t2 * t2) * t4P * t3 * t3 - t6 * t22kP * (t1 * t1) * (t4 * t4) * k * t7 * t3P + 2 * t5 * t1P * t1 * t2P * (t2 * t2) * t7 * t3P
            * t3 - 2 * t4P * t2P * k * t5 * t1P * t4 * t1 - 4 * (t2 * t2) * t7 * t3P * t2P * t4P * t5 * t1 * t4 + 4 * t6 * t22kP * t1 * t3 * k * t4P
            * t5 * t4 + 4 * t6 * t22kP * t1 * t3 * k * t4P * t7 * t4 - 2 * t6 * t22kP * (t1 * t1) * t3 * k * t4P * t4 - 2 * t6 * t22kP * t1 * t3 * k * t7 * t3P
            * (t4 * t4) - 2 * t6 * t22kP * t1 * t3 * k * t5 * t1P * (t4 * t4) - 2 * t6 * t22kP * t1 * t3 * t3 * k * t4P * t4 + 2 * t5 * t1P * t2P * k * t2
            * t4P * t7 + 2 * t7 * t7 * t3P * t2P * k * t2 * t4P * t3 + 2 * t6 * t22kP * t3 * k * t4P * t7 + 2 * t6 * t22kP * t3 * k * t4P * t5 - 2 * (t2
                                                                                                                                                      * t2) * t7 * t7 * t3P * t2P * t4P * t3 - 4 * (t2 * t2) * t7 * t7 * t3P * t2P * t4P * t1 - 2 * t6 * t22kP * t1 * t3 * t3 * k * t5 * t1P
            * t4 + 2 * t6 * t22kP * t1 * t4 * k * t4P * t5 - 2 * t4P * t1 * t4 * t6 * t2P * (t2 * t2) * t7 * t3P * t3 - t4P * t4 * t6 * t2P * (t2 * t2)
            * t5 * t1P - 2 * t4P * t4 * t6 * t2P * (t2 * t2) * t5 * t1P * t3 + 2 * t5 * t1P * t3 * t3 * t2P * k * t4P * t7 - 2 * t6 * t22kP * (t1
                                                                                                                                               * t1) * t4 * k * t7 * t3P - 4 * t7 * t3P * t4 * t2P * k * t5 * t1P - 2 * t7 * t3P * t2P * k * t2 * t4P * (t1 * t1) - 2 * t4P * t7 * t2P
            * (t2 * t2) * t5 * t1P - 2 * t4P * t5 * t5 * t3 * t3 * t4 * t2P * (t2 * t2) * t1P - 4 * t7 * t3P * t3 * t4 * t2P * k * t5 * t1P * t2 + 4 * t6
            * t22kP * t1 * t3 * k * t4P * t7 + 4 * t6 * t22kP * t1 * t3 * k * t4P * t5 + 2 * t4P * t4 * t2P * (t2 * t2) * t5 * t1P + 4 * t4P * t4
            * t2P * (t2 * t2) * t5 * t1P * t3 - t6 * t22kP * (t1 * t1) * t3 * k * t7 * t3P * (t4 * t4) + 4 * t5 * t1P * t3 * t2P * k * t4P * t7 + 2
            * t6 * t22kP * (t1 * t1) * t3 * k * t4P * t7 + 2 * t6 * t22kP * t1 * t3 * t3 * k * t4P * t5 + 2 * t6 * t22kP * (t1 * t1) * t3 * k * t4P * t5
            + 2 * t6 * t22kP * t1 * t3 * t3 * k * t4P * t7 + 2 * (t2 * t2) * t7 * t3P * t2P * t4P * t4 - 2 * t7 * t7 * t3P * t3 * t4 * t2P * (t2 * t2)
            * t4P * (t1 * t1) - 4 * t7 * t3P * t3 * t4 * t2P * (t2 * t2) * t4P * t5 * t1 - t4P * (t1 * t1) * t4 * t6 * t2P * (t2 * t2) * t7 * t3P
            * t3 - 2 * t5 * t1P * t3 * t2P * k * t2 * t7 * t3P - 2 * t7 * t3P * t3 * t4 * t2P * (t2 * t2) * t4P * t5 * (t1 * t1) - 2 * (t2 * t2) * t7 * t3P
            * t2P * t4P * t5 * (t1 * t1) * t4 - 2 * t6 * t22kP * (t1 * t1) * t3 * k * t7 * t3P * t4 - t4P * t7 * (t1 * t1) * t22kP * t2 * t6 * t3 * t3
            * t4 + 2 * t7 * t3P * t6 * t2P * k * t2 * t4P * t1 * t4 + 2 * t5 * t5 * t1P * t1 * t3 * t3 * t2P * k * t2 * t4P - 2 * (t2 * t2) * t7 * t7 * t3P
            * t2P * t4P * t4 + 2 * t7 * t3P * t4 * t2P * t2 * t4P * t3 - 2 * t4P * t5 * t5 * t4 * t2P * t2 * t1P - 2 * t4P * t7 * t4 * t2P * t2
            * t5 * t1P - 2 * t5 * t5 * t1P * t3 * t3 * t4 * t2P * t2 * t4P - 4 * t5 * t1P * t3 * t4 * t2P * t2 * t4P * t7 * t1 - 4 * t5 * t5 * t1P * t3
            * t4 * t2P * t2 * t4P * t1 - 2 * t5 * t5 * t1P * t2P * t2 * t4P + 2 * t5 * t1P * t3 * t3 * t4 * t22kP * t2 * t6 * t1 + 2 * t5 * t5 * t12kP
            * t2P * (t2 * t2) * t3 - 2 * t5 * t1P * t3 * t3 * t2P * t2 * t4P * t7 - 4 * t5 * t1P * t3 * t4 * t2P * t2 * t4P * t7 - 2 * (t2 * t2) * t3
            * t42kP * t2P * t6 * (t1 * t1) - t4P * t7 * t3 * t3 * t4 * t22kP * t2 * t6 - t5 * t5 * t12kP * t3 * t3 * t2P * k - t4P * t5 * t22kP
            * t2 * t6 + 4 * t42kP * t3 * t2P * k * t5 * t2 - 4 * (t2 * t2) * t3 * t42kP * t2P * t7 * (t1 * t1) + t7 * t3P * t22kP * t2 * t6 - 4 * t42kP
            * t3 * t3 * t2P * t2 * t7 * t1 + 4 * (t2 * t2) * t7 * t7 * t32kP * t2P * t1 * t4 + 4 * t7 * t3P * t4 * t22kP * t2 * t6 * t1 * t3 - 4 * t42kP
            * t7 * t1 * t2P * t2 + 4 * t42kP * t1 * t3 * t2P * k * t6 - 4 * t42kP * t7 * t2P * k * t5 * t1 * t2 * t3 * t3 - 2 * (t2 * t2) * t3 * t3 * t42kP
            * t2P * t6 * t1 + t42kP * t2P * (t2 * t2) * t5 * t5 * (t1 * t1) + t42kP * t2P * (t2 * t2) * t5 * t3 * t3 * t6 * (t1 * t1) - 2 * t42kP
            * t7 * t7 * t2P * k * t1 * t2 * t3 * t3 - t42kP * t7 * t7 * t2P * k * (t1 * t1) * t2 * t3 * t3 - 2 * t42kP * t7 * (t1 * t1) * t2P * k * t5 * t3
            * t3 + t42kP * t2P * (t2 * t2) * t5 * t5 * t3 * t3 - t42kP * t7 * t7 * t3 * t3 * t2P * k + t7 * t3P * (t4 * t4) * t22kP * t2 * t6 - 2 * t4P
            * t7 * t1 * t22kP * t2 * t6 * t3 * t3 - 2 * t42kP * t1 * t2P * k * t2 - 4 * t42kP * t2P * (t2 * t2) * t5 * t3 * (t1 * t1) - 2 * t4P * t5 * t1
            * t22kP * t2 * t6 * t3 * t3 + 2 * t42kP * t7 * t2P * t2 * t5 - 2 * t4P * t7 * t1 * t22kP * t2 * t6 * t3 * t3 * t4 + t5 * t1P * t22kP * t2
            * t6 * (t4 * t4) + t42kP * t2P * k * t6 + 2 * (t2 * t2) * t3 * t42kP * t2P + (t2 * t2) * t42kP * t2P * (t1 * t1) - t42kP * t3 * t3
            * t2P * k + t42kP * t2P * (t2 * t2) * t5 * t5 + 2 * (t2 * t2) * t42kP * t2P * t1 + (t2 * t2) * t3 * t3 * t42kP * t2P - 2 * t42kP
            * t5 * t5 * t2P * k * t1 + 2 * t42kP * t2P * (t2 * t2) * t5 * t3 * t3 * t6 * t1 + (t2 * t2) * t3 * t3 * (t1 * t1) * t2P * t42kP * t7 * t6 - t42kP
            * t5 * t5 * (t1 * t1) * t2P * k * t2 + t42kP * t6 * t2P * k * t2 * t3 * t3 - 2 * t42kP * t5 * t2P * k * t7 - 2 * t4P * t7 * (t1 * t1) * t22kP
            * t2 * t6 * t3 * t4 - t7 * t7 * t32kP * t2P * k * t2 * (t1 * t1) - 4 * t4P * t7 * t1 * t22kP * t2 * t6 * t3 - 4 * t42kP * t3 * t2P * t2 * t5 - t7
            * t7 * t32kP * (t1 * t1) * (t4 * t4) * t2P * k * t2 + 2 * t42kP * t2P * (t2 * t2) * t5 * t3 * t3 * t7 - t42kP * (t1 * t1) * t6 * t2P * k
            * t2 * t5 * t3 * t3 + 2 * t42kP * t2P * (t2 * t2) * t5 * t5 * t3 * (t1 * t1) + 2 * t42kP * t2P * (t2 * t2) * t5 * t3 * t6 + 4 * t42kP * t3 * t2P
            * k * t5 + 2 * t42kP * t2P * (t2 * t2) * t5 * t3 * t3 * t7 * (t1 * t1) + 2 * t5 * t1P * t3 * t3 * t4 * t2P * t2 * t4P * t1 + 2 * t4P * t2P
            * t2 * t5 * t1P + 2 * t7 * t3P * t4 * t22kP * t2 * t6 * (t1 * t1) * t3 + 2 * t5 * t5 * t12kP * t3 * t2P * t2 + 2 * t42kP * t7 * (t1 * t1)
            * t2P * t2 * t5 * t3 * t3 - 4 * (t2 * t2) * t3 * t42kP * t2P * t7 + t42kP * t7 * t7 * (t1 * t1) * t2P * t2 * t3 * t3 + t7 * t3P * (t4 * t4)
            * t22kP * t2 * t6 * t3 - 2 * t42kP * t7 * t2P * k * t5 * (t1 * t1) * t2 * t3 * t3 - 4 * t42kP * t3 * t2P * t2 * t7 + 2 * (t2 * t2) * t3 * (t1
                                                                                                                                                       * t1) * t2P * t42kP * t7 * t6 - 2 * t42kP * t7 * t1 * t2P * k * t6 - 2 * t42kP * t5 * t5 * t1 * t2P * k * t2 + 2 * t7 * t7 * t32kP * t1
            * t2P * t2 + 2 * t7 * t3P * t4 * t2P * t2 * t4P * (t1 * t1) * t3 - 4 * t42kP * t5 * t1 * t2P * t2 + t42kP * (t1 * t1) * t6 * t2P * k
            * t2 + (t2 * t2) * t7 * t7 * t32kP * t2P - 2 * t4P * t5 * t5 * t3 * t3 * t2P * t2 * t1P + t5 * t5 * t12kP * t2P * t2 - 2 * t42kP * t7
            * t7 * t1 * t2P * k - t42kP * t5 * t5 * t2P * k * t3 * t3 + 4 * t7 * t3P * t4 * t22kP * t2 * t6 * t1 + 2 * t42kP * t2P * t2 * t1 + 2 * t42kP
            * t1 * t6 * t2P * k * t2 - 2 * t42kP * t5 * t2P * k * t6 * (t1 * t1) * t3 + 2 * t42kP * t5 * t5 * t1 * t2P * t2 + t5 * t1P * t22kP * t2
            * t6 * t1 + 2 * t5 * t5 * t12kP * t4 * t2P * (t2 * t2) * t3 * t3 + t42kP * t3 * t3 * t2P * t2 * (t1 * t1) - 4 * t7 * t7 * t32kP * t1 * t4 * t2P
            * k * t2 + 2 * t42kP * t2P * (t2 * t2) * t5 * t5 * t1 - 2 * t4P * t7 * t3 * t4 * t22kP * t2 * t6 - t5 * t5 * t12kP * (t4 * t4) * t2P * k * t2
            + 2 * t4P * t5 * t5 * t2P * k * t1P * t3 * t3 * t4 + t4P * t3 * t4 * t6 * t2P * k * t7 * t3P - t4P * t6 * t2P * (t2 * t2) * t5 * t1P
            * t3 * t3 + 2 * t5 * t5 * t1P * t3 * t3 * t2P * k * t2 * t4P + 2 * t5 * t1P * t3 * t3 * t2P * k * t2 * t4P * t7 - 2 * t6 * t22kP * t1 * t4 * k
            * t5 * t1P - 4 * t6 * t22kP * t1 * t4 * k * t7 * t3P + t7 * t3P * (t1 * t1) * t2P * k * t4P * t6 + t42kP * t6 * t2P * k * t2 + t42kP
            * t5 * t5 * (t1 * t1) * t2P * t2 * t3 * t3 - 2 * t4P * t5 * t1 * t22kP * t2 * t6 * t4 + t7 * t3P * t3 * t22kP * t2 * t6 + t42kP * t7 * t7
            * t2P * t2 + 4 * t42kP * t2P * (t2 * t2) * t5 * t3 * t7 * (t1 * t1) - 4 * t42kP * t3 * t3 * t2P * t2 * t5 * t1 + 2 * t42kP * t7 * t7 * (t1
                                                                                                                                                    * t1) * t2P * t2 * t3 - 2 * t5 * t5 * t12kP * t3 * t2P * k * t2 * (t4 * t4) + t7 * t3P * (t1 * t1) * t22kP * t2 * t6 + 2 * t42kP * t3 * t2P
            * t2 - 8 * t42kP * t3 * t2P * t2 * t5 * t1 + 2 * t42kP * t7 * t6 * t2P * t2 * t3 + 2 * t42kP * t7 * t6 * t2P * t2 * t1 + t6 * t2P * t3 * t3
            * t1P * k * t4P * t5 + 2 * t42kP * t7 * t7 * t1 * t2P * t2 * t3 * t3 + t4P * t2 * t3 * t3 * t5 * t1P * k * t6 * t2P + 2 * t4P * t2 * t3
            * t5 * t1P * k * t6 * t2P * t4 + 2 * t4P * t2 * t3 * t5 * t1P * k * t6 * t2P - 4 * t42kP * t7 * t2P * k * t5 * t1 * t2 + 4 * t4P * t3 * t22kP
            * t2 * t6 * t1 + 4 * (t2 * t2) * t3 * t1 * t2P * t42kP * t7 * t7 - t42kP * (t1 * t1) * t2P * k * t2 + t42kP * t7 * (t1 * t1) * t6 * t2P
            * t2 + 4 * t42kP * t7 * t1 * t3 * t6 * t2P * t2 - t4P * t7 * t22kP * t2 * t6 * t3 * t3 - t6 * t2P * t2 * t3 * t3 * t1P * t1 * t4P * t5 - 2
            * t7 * t7 * t32kP * t2P * k * t2 * t1 + 4 * t5 * t1P * t3 * t4 * t2P * t2 * t4P * t1 - 2 * t4P * t7 * t2P * t2 * t5 * t1P - 2 * t4P
            * t7 * t7 * t2P * t2 * t3P - 4 * t42kP * t3 * t2P * t2 * t5 * (t1 * t1) + 2 * t42kP * (t1 * t1) * t6 * t2P * k * t2 * t3 + 4 * t5 * t5 * t12kP
            * t4 * t2P * (t2 * t2) * t3 + t42kP * t3 * t3 * t2P * t2 - 4 * (t2 * t2) * t42kP * t2P * t7 * t1 + t42kP * t5 * t5 * (t1 * t1) * t2P
            * t2 - 2 * t42kP * t7 * t2P * k * t5 * (t1 * t1) * t2 - t42kP * t5 * t2P * k * t6 * (t1 * t1) - 2 * t5 * t5 * t12kP * t3 * t2P * k - t42kP
            * t5 * t2P * k * t6 * (t1 * t1) * t3 * t3 - 2 * t4P * t7 * t7 * (t1 * t1) * t2P * t2 * t3P + 2 * t7 * t3P * (t4 * t4) * t2P * t2 * t5 * t1P
            * t3 + 2 * (t2 * t2) * t7 * t7 * t32kP * t2P * t4 + (t2 * t2) * t42kP * t6 * t2P * t7 - t7 * t7 * t32kP * (t1 * t1) * (t4 * t4) * t2P
            * k + (t2 * t2) * t42kP * t7 * t7 * t2P - 2 * t42kP * t5 * t5 * t2P * k * t1 * t3 * t3 - 2 * t5 * t1P * t3 * t3 * t4 * t2P * t2 * t4P * t7
            + 2 * t4P * t3 * t22kP * t2 * t6 * (t1 * t1) - t42kP * t6 * t2P * k * t2 * t7 * t3 * t3 - t6 * t2P * t2 * t4 * t1P * t1 * t4P * t5 + 4 * t5
            * t5 * t12kP * t3 * t4 * t2P * t2 + t42kP * t2P * (t2 * t2) * t5 * t6 + 2 * t42kP * t3 * t3 * t2P * t2 * t1 + 2 * t42kP * t7 * t7 * t2P
            * t2 * t3 + 4 * t42kP * t3 * t2P * t2 * t1 + 2 * t5 * t1P * t1 * t4 * t22kP * t2 * t6 + 2 * t42kP * t2P * (t2 * t2) * t5 * t5 * t3 * t3 * t1
            + (t2 * t2) * t3 * t3 * (t1 * t1) * t2P * t42kP * t7 * t7 + (t2 * t2) * t42kP * t2P - t42kP * t5 * t2P * k * t6 - t4P * t5 * (t1 * t1)
            * t22kP * t2 * t6 * t3 * t3 - 2 * t5 * t1P * t3 * t3 * t4 * t2P * t2 * t4P * t7 * t1 - 2 * t5 * t5 * t1P * t3 * t3 * t4 * t2P * t2 * t4P * t1 - t42kP
            * t7 * t7 * (t1 * t1) * t2P * k - 2 * t5 * t5 * t12kP * t3 * t3 * t2P * k * t2 * t4 - t7 * t7 * t32kP * t2P * k * t2 + 2 * t42kP * (t1 * t1)
            * t3 * t2P * k * t6 - 2 * t7 * t7 * t32kP * t1 * t2P * k - 4 * t42kP * t7 * t7 * t2P * k * t1 * t2 * t3 - 2 * t5 * t5 * t12kP * t4 * t2P * k
            * t2 - 4 * t5 * t1P * t3 * t2P * t2 * t4P * t7 - t5 * t5 * t12kP * t2P * k + 2 * t42kP * t5 * t2P * k - t5 * t5 * t12kP * t2P * k
            * t2 - 2 * t4P * t6 * t3 * t2P * t2 * t7 * t3P * t1 - t6 * t2P * t4P * t2 * t5 * t1P - t6 * t2P * t4P * t2 * t7 * t3P - t6 * t2P
            * t4P * t2 * t5 * t1P * t4 + t4P * t2 * t5 * t1P * k * t1 * t6 * t2P + t7 * t3P * (t4 * t4) * t22kP * t2 * t6 * (t1 * t1) + 4 * t42kP
            * t1 * t3 * t3 * t2P * k * t7 - t42kP * t5 * t5 * t2P * k * t2 * t3 * t3 + 2 * t4P * t4 * t2P * t2 * t5 * t1P * t1 - 2 * t42kP * t5 * t2P
            * k * t6 * t3 + 4 * (t2 * t2) * t3 * t42kP * t2P * t1 - 2 * t42kP * t3 * t2P * k - 2 * t7 * t7 * t32kP * (t1 * t1) * t4 * t2P * k + 4 * t42kP
            * t2P * (t2 * t2) * t5 * t3 * t6 * t1 - 2 * t42kP * t7 * t7 * (t1 * t1) * t2P * k * t3 - 2 * t42kP * t7 * (t1 * t1) * t2P * t2 - t42kP * t5
            * t2P * k * t6 * t3 * t3 + 2 * t7 * t3P * (t4 * t4) * t22kP * t2 * t6 * t1 * t3 + t5 * t1P * t22kP * t2 * t6 - 4 * t42kP * t1 * t3 * t2P
            * k * t2 + 2 * (t2 * t2) * t3 * t42kP * t2P * (t1 * t1) + 2 * t5 * t1P * t3 * t22kP * t2 * t6 + 2 * t7 * t7 * t32kP * t4 * t2P * t2 + 2 * t7
            * t3P * t4 * t22kP * t2 * t6 * (t1 * t1) + t4P * t4 * t22kP * t2 * t6 + 4 * t42kP * (t1 * t1) * t3 * t2P * k * t5 * t2 - 2 * t5 * t5 * t12kP
            * (t4 * t4) * t2P * k * t3 + 2 * t42kP * t7 * (t1 * t1) * t3 * t6 * t2P * t2 + 2 * t42kP * t7 * t1 * t3 * t3 * t6 * t2P * t2 + t42kP * t7
            * (t1 * t1) * t3 * t3 * t6 * t2P * t2 + 4 * t42kP * t5 * t1 * t2P * k * t2 + t4P * t3 * t3 * t22kP * t2 * t6 - 2 * t42kP * (t1 * t1) * t3
            * t2P * k * t2 - 2 * t4P * t5 * (t1 * t1) * t2P * t2 * t7 * t3P * t3 + (t2 * t2) * t3 * t3 * t42kP * t7 * t7 * t2P - 2 * t4P * t7 * t22kP
            * t2 * t6 * t3 - 2 * t42kP * t7 * t7 * t2P * k * (t1 * t1) * t2 * t3 + 2 * t42kP * t2P * k * t6 * t1 - 2 * t4P * t7 * t1 * t2P * t2 * t5 * t1P
            * t3 * t3 - 4 * t4P * t7 * t7 * t1 * t2P * t2 * t3P * t3 - t4P * t7 * (t1 * t1) * t22kP * t2 * t6 * t4 + 2 * t7 * t3P * (t4 * t4) * t2P
            * t2 * t5 * t1P * t3 * t1 + 4 * t7 * t3P * t4 * t2P * t2 * t5 * t1P * t3 * t1 + 2 * t7 * t3P * (t4 * t4) * t2P * t2 * t5 * t1P * t1 + 4 * t7
            * t3P * t4 * t2P * t2 * t5 * t1P - 2 * t42kP * t3 * t2P * k * t2 + (t2 * t2) * t7 * t7 * t32kP * t2P * (t1 * t1) * (t4 * t4) + 2 * t7
            * t3P * t1 * t2P * t2 * t5 * t1P * t3 + 2 * t7 * t3P * (t1 * t1) * t2P * t2 * t4P - 2 * t42kP * t7 * (t1 * t1) * t2P * k * t6 * t3 - 2
            * (t2 * t2) * t42kP * t2P * t7 * (t1 * t1) + 4 * t5 * t1P * t3 * t4 * t2P * t2 * t4P - 4 * t42kP * t2P * (t2 * t2) * t5 * t3 + 2 * t42kP
            * t1 * t3 * t3 * t2P * k * t6 - t42kP * (t1 * t1) * t3 * t3 * t2P * k + t5 * t1P * t3 * t3 * (t4 * t4) * t22kP * t2 * t6 + 2 * t42kP * t6
            * t2P * k * t2 * t3 - t4P * t5 * (t1 * t1) * t22kP * t2 * t6 + t42kP * t2P * (t2 * t2) * t5 * t3 * t3 * t6 + 4 * t42kP * t7 * t1 * t2P
            * t2 * t5 * t3 * t3 + 2 * (t2 * t2) * t3 * t42kP * t7 * t7 * t2P - 2 * t4P * t5 * (t1 * t1) * t22kP * t2 * t6 * t3 - t42kP * t7 * t7 * t2P
            * k * (t1 * t1) * t2 + t5 * t5 * t12kP * t2P * t2 * (t4 * t4) + 2 * t42kP * t2P * (t2 * t2) * t5 * t3 * t6 * (t1 * t1) - (t2 * t2) * t3 * t3 * t42kP
            * t2P * t6 + t4P * t22kP * t2 * t6 + 4 * t42kP * t1 * t3 * t3 * t2P * k * t5 + 2 * t7 * t3P * t1 * t2P * t2 * t5 * t1P + 2 * t42kP
            * t3 * t3 * t2P * k * t5 - 2 * t42kP * t6 * t2P * k * t2 * t5 * t3 + 2 * t42kP * t3 * t3 * t2P * k * t7 * t2 - 2 * t42kP * t5 * t5 * t2P * k
            * (t1 * t1) * t3 + 4 * t42kP * t2P * (t2 * t2) * t5 * t3 * t7 + 2 * t7 * t3P * (t4 * t4) * t22kP * t2 * t6 * t1 - t42kP * t7 * t7 * t2P
            * k - 2 * t4P * t7 * t1 * t22kP * t2 * t6 - 2 * (t2 * t2) * t42kP * t2P * t7 - t5 * t5 * t12kP * (t4 * t4) * t2P * k * t3 * t3 + 2 * (t2 * t2)
            * t7 * t7 * t32kP * t2P * (t1 * t1) * t4 + t5 * t1P * t3 * t3 * t22kP * t2 * t6 + 2 * t7 * t3P * t4 * t22kP * t2 * t6 + 2 * t4P * t3 * t3
            * t22kP * t2 * t6 * t1 * t4 - 4 * t5 * t5 * t12kP * t3 * t2P * k * t2 * t4 - 2 * t42kP * t5 * t2P * k * t6 * t1 * t3 * t3 - 8 * t42kP * t7 * t1
            * t2P * k * t5 * t3 - 2 * t42kP * t3 * t6 * t2P * k * t7 - t42kP * t3 * t3 * t2P * k * t2 - 2 * t7 * t3P * t4 * t2P * t2 * t4P * t5 * t3
            + t42kP * (t1 * t1) * t6 * t2P * k * t2 * t3 * t3 + 2 * t4P * t3 * t22kP * t2 * t6 + 2 * t4P * t4 * t22kP * t2 * t6 * t1 - 4 * t42kP * t5
            * t2P * k * t7 * t3 - 2 * t4P * t6 * t3 * t2P * t2 * t5 * t1P - t4P * t6 * t3 * t2P * t2 * t7 * t3P - 2 * t4P * t5 * t5 * t1 * t2P * t2
            * t1P * t3 * t3 - 4 * t4P * t5 * t1 * t2P * t2 * t7 * t3P * t3 - 2 * t7 * t7 * t32kP * t1 * (t4 * t4) * t2P * k + 2 * (t2 * t2) * t3 * t3 * t1
            * t2P * t42kP * t7 * t6 - t42kP * t6 * t2P * k * t2 * t7 - 4 * t42kP * t5 * t2P * k * t7 * t2 * t3 - 2 * t42kP * (t1 * t1) * t3 * t2P
            * k - 4 * t42kP * t1 * t3 * t2P * k - 8 * t42kP * t2P * (t2 * t2) * t5 * t3 * t1 - t5 * t1P * t3 * t3 * t4 * t2P * t2 * t4P * t6 - 2 * t4P
            * t7 * t7 * (t1 * t1) * t2P * t2 * t3P * t3 + 4 * t5 * t1P * t3 * t4 * t22kP * t2 * t6 + (t2 * t2) * t7 * t7 * t32kP * t2P * (t1 * t1) - 2
            * t4P * t5 * (t1 * t1) * t2P * t2 * t7 * t3P + 2 * t42kP * t2P * (t2 * t2) * t5 * t7 - 2 * t42kP * t2P * k * t1 + 2 * t42kP * t2P
            * k * t7 + 4 * t42kP * t1 * t2P * k * t2 * t7 * t3 * t3 + 8 * t42kP * t1 * t3 * t2P * k * t7 - 2 * t42kP * t3 * t6 * t2P * t2 + t5 * t5 * t12kP
            * t3 * t3 * t2P * t2 - t7 * t7 * t32kP * t2P * k + 2 * t42kP * t2P * (t2 * t2) * t5 * t5 * t3 - 4 * (t2 * t2) * t3 * t42kP * t2P * t6
            * t1 - 2 * t4P * t5 * t3 * t22kP * t2 * t6 * t4 - 4 * t42kP * t7 * t7 * t1 * t2P * k * t3 - 2 * t4P * t7 * t1 * t22kP * t2 * t6 * t4 + 2 * t42kP
            * (t1 * t1) * t3 * t3 * t2P * k * t5 - t42kP * t6 * t2P * k * t2 * t5 * t3 * t3 - t42kP * t2P * k + t4P * t3 * t3 * t22kP * t2 * t6 * (t1
                                                                                                                                                   * t1) + 2 * t4P * t3 * t22kP * t2 * t6 * (t1 * t1) * t4 + 2 * t7 * t3P * t1 * t22kP * t2 * t6 + 4 * t4P * t3 * t22kP * t2 * t6 * t1 * t4 - t7
            * t7 * t32kP * (t4 * t4) * t2P * k - 2 * (t2 * t2) * t42kP * t2P * t5 - t42kP * t5 * t5 * t2P * k * (t1 * t1) * t3 * t3 + 8 * t42kP
            * t1 * t2P * k * t2 * t7 * t3 + 2 * t5 * t1P * t3 * t22kP * t2 * t6 * t1 - t5 * t5 * t12kP * (t4 * t4) * t2P * k - 2 * t42kP * t1 * t6 * t2P
            * k * t2 * t7 * t3 * t3 - 4 * t42kP * t1 * t6 * t2P * k * t2 * t5 * t3 + t42kP * t7 * t7 * t2P * t2 * t3 * t3 - 2 * t7 * t7 * t3P * t4 * t2P * t2
            * t4P + 2 * t5 * t1P * t3 * t3 * t4 * t2P * t2 * t4P - 2 * t7 * t3P * t1 * t6 * t2P * t4P * t2 - t7 * t3P * (t1 * t1) * t6 * t2P * t4P
            * t2 + t42kP * t7 * t6 * t2P * t2 + 2 * t42kP * t3 * t2P * t2 * (t1 * t1) + 2 * t42kP * t7 * (t1 * t1) * t2P * t2 * t5 + t7 * t3P * (t1
                                                                                                                                                 * t1) * t22kP * t2 * t6 * t3 + 8 * t42kP * t2P * (t2 * t2) * t5 * t3 * t7 * t1 - 2 * t4P * t5 * t5 * t1 * t2P * t2 * t1P - 2 * t7 * t3P
            * t4 * t2P * t2 * t4P * t6 * t1 - 4 * t7 * t7 * t3P * t4 * t2P * t2 * t4P * t1 + 4 * t42kP * t5 * t3 * t2P * t2 * t7 - 2 * t5 * t1P * t3
            * t4 * t2P * t2 * t4P * t6 - 4 * t5 * t5 * t1P * t3 * t4 * t2P * t2 * t4P - 2 * t42kP * t1 * t6 * t2P * t2 + 2 * t7 * t3P * t4 * t2P
            * t2 * t4P * (t1 * t1) - 2 * t4P * t5 * (t1 * t1) * t22kP * t2 * t6 * t3 * t4 + t4P * t22kP * t2 * t6 * (t1 * t1) + 4 * t42kP * t7 * t7
            * t1 * t2P * t2 * t3 - 4 * t4P * t5 * t1 * t22kP * t2 * t6 * t3 * t4 - 2 * t42kP * t5 * (t1 * t1) * t2P * t2 + 2 * t5 * t1P * t3 * (t4 * t4)
            * t22kP * t2 * t6 - 2 * t42kP * t7 * t7 * t3 * t2P * k - 2 * t42kP * t1 * t6 * t2P * k * t2 * t5 + t7 * t7 * t32kP * (t4 * t4) * t2P * t2 - 2
            * t4P * t5 * t1 * t22kP * t2 * t6 + t7 * t7 * t32kP * (t4 * t4) * t2P * t2 * (t1 * t1) - 2 * t42kP * t5 * t5 * t1 * t2P * k * t2 * t3 * t3 - t42kP
            * (t1 * t1) * t6 * t2P * k * t2 * t7 * t3 * t3 - t42kP * (t1 * t1) * t6 * t2P * k * t2 * t7 + 4 * t42kP * t1 * t6 * t2P * k * t2 * t3 + 2 * t42kP
            * (t1 * t1) * t2P * k * t2 * t7 * t3 * t3 - t6 * t2P * t3 * t3 * t4 * t1P * t1 * t4P * t5 * t2 - t4P * t5 * t2 * t1P * t1 * t6 * t2P + 4 * t42kP
            * (t1 * t1) * t3 * t2P * k * t7 + 2 * t42kP * t5 * t2P * k * t2 - t42kP * t5 * t5 * t2P * k * (t1 * t1) - 8 * t42kP * t7 * t2P * k * t5
            * t1 * t2 * t3 - 4 * t42kP * t5 * t5 * t1 * t2P * k * t2 * t3 + t42kP * t7 * t7 * (t1 * t1) * t2P * t2 - t4P * t7 * (t1 * t1) * t22kP * t2
            * t6 * t3 * t3 - t7 * t7 * t32kP * t2P * k * t2 * (t4 * t4) - 4 * t7 * t7 * t32kP * t1 * t4 * t2P * k + 8 * t42kP * t7 * t1 * t2P * t2 * t5
            * t3 + 2 * t5 * t1P * t3 * (t4 * t4) * t22kP * t2 * t6 * t1 + (t2 * t2) * t3 * t3 * t42kP * t7 * t2P * t6 - t4P * t5 * t4 * t22kP * t2 * t6 - 2
            * t42kP * t2P * (t2 * t2) * t5 * t3 * t3 + 2 * t42kP * t7 * t7 * t1 * t2P * t2 - t42kP * t5 * t5 * t2P * k * t2 + t5 * t1P * t3 * t3 * (t4
                                                                                                                                                    * t4) * t22kP * t2 * t6 * t1 - 4 * t42kP * t3 * t2P * t2 * t7 * (t1 * t1) + t5 * t5 * t12kP * (t4 * t4) * t2P * (t2 * t2) * t3 * t3 - 2 * t42kP
            * t1 * t3 * t3 * t2P * k - 4 * t7 * t7 * t3P * t4 * t2P * t2 * t4P * t1 * t3 + 2 * t42kP * t5 * t5 * t3 * t2P * t2 - t42kP * t7 * (t1 * t1)
            * t2P * k * t6 * t3 * t3 - 2 * (t2 * t2) * t3 * t3 * t42kP * t2P * t7 + 2 * t4P * t3 * t3 * t2P * t2 * t5 * t1P - 2 * t7 * t7 * t3P * t4 * t2P
            * t2 * t4P * (t1 * t1) + 4 * t42kP * t5 * t2P * k * t1 - t7 * t3P * t4 * t2P * t2 * t4P * t6 * t3 - 4 * t7 * t3P * t4 * t2P * t2 * t4P
            * t5 * t1 * t3 + 4 * t42kP * t2P * (t2 * t2) * t5 * t7 * t1 - 4 * t42kP * t2P * (t2 * t2) * t5 * t3 * t3 * t1 + 2 * t4P * t22kP * t2 * t6
            * t1 + 2 * t4P * t3 * t22kP * t2 * t6 * t4 - 2 * t4P * t5 * t3 * t22kP * t2 * t6) / (1 + t2) / t2 * pow(t7 * t3P + t6 * t2P + t5
                                                                                                                    * t1P + t4P - t4P * t5 * t1 * t2 * t3 + t5 * t1P * t2 + t6 * t2P * t4 + t6 * t2P * t3 + t6 * t2P * t1 + t7 * t3P * t4 + t7 * t3P * t2
                                                                                                                    + t7 * t3P * t1 - t4P * t6 * t2 - t4P * t7 * t3 - t4P * t5 * t1 + t4P * t2 * t3 - t4P * t5 * t3 - t4P * t5 * t2 + t4P * t1 * t3 - t4P
                                                                                                                    * t6 * t3 - t4P * t6 * t1 + t4P * t1 * t2 - t4P * t7 * t2 - t4P * t7 * t1 + t5 * t1P * t4 + t5 * t1P * t3 + t4P * t3 - t4P * t5 - t4P
                                                                                                                    * t6 - t4P * t7 + t4P * t2 + t4P * t1 + t5 * t1P * t2 * t4 + t5 * t1P * t3 * t4 + t5 * t1P * t2 * t3 + t6 * t2P * t1 * t4 + t6 * t2P * t3
                                                                                                                    * t4 + t6 * t2P * t1 * t3 + t7 * t3P * t2 * t4 + t7 * t3P * t1 * t2 + t7 * t3P * t1 * t4 - t4P * t7 * t1 * t2 - t4P * t6 * t2 * t3 - t4P * t6
                                                                                                                    * t1 * t2 - t4P * t7 * t2 * t3 - t4P * t7 * t1 * t3 - t4P * t6 * t1 * t3 - t4P * t5 * t1 * t2 - t4P * t5 * t1 * t3 - t4P * t5 * t2 * t3 + t4P
                                                                                                                    * t1 * t2 * t3 - t4P * t6 * t1 * t2 * t3 + t5 * t1P * t2 * t3 * t4 + t6 * t2P * t1 * t3 * t4 + t7 * t3P * t1 * t2 * t4 - t4P * t7 * t1 * t2 * t3, -2)
    
    
    if (k > 0) test <- abs(a26/a[2,6])
    a[2,6] <- a[2,6] + a26
    k <- k+1
      
  } 
    
    
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
  
  ##A27
  test <- 100.0
  k <- 0
  while (test > criteria  &  k < maximumIteration)
  {
    t1P <- pow((t1 / (1 + t1)), k)
    t2P <- pow((t2 / (1 + t2)), k)
    t3P <- pow((t3 / (1 + t3)), k)
    t4P <- pow((t4 / (1 + t4)), k)
    
    a27 <- -(1 + t1) * t6 * t2P * (-k * t3P - k * t3P * t4 + t4P * k + t4P * k * t3 + t3P * t2 + t3P * t2 * t4 - t4P * t2 - t4P * t2 * t3) / (1
                                                                                                                                              + t2) / t2 / (t7 * t3P + t6 * t2P + t5 * t1P + t4P - t4P * t5 * t1 * t2 * t3 + t5 * t1P * t2 + t6 * t2P * t4 + t6 * t2P * t3 + t6 * t2P
                                                                                                                                                            * t1 + t7 * t3P * t4 + t7 * t3P * t2 + t7 * t3P * t1 - t4P * t6 * t2 - t4P * t7 * t3 - t4P * t5 * t1 + t4P * t2 * t3 - t4P * t5 * t3 - t4P
                                                                                                                                                            * t5 * t2 + t4P * t1 * t3 - t4P * t6 * t3 - t4P * t6 * t1 + t4P * t1 * t2 - t4P * t7 * t2 - t4P * t7 * t1 + t5 * t1P * t4 + t5 * t1P * t3
                                                                                                                                                            + t4P * t3 - t4P * t5 - t4P * t6 - t4P * t7 + t4P * t2 + t4P * t1 + t5 * t1P * t2 * t4 + t5 * t1P * t3 * t4 + t5 * t1P * t2 * t3 + t6
                                                                                                                                                            * t2P * t1 * t4 + t6 * t2P * t3 * t4 + t6 * t2P * t1 * t3 + t7 * t3P * t2 * t4 + t7 * t3P * t1 * t2 + t7 * t3P * t1 * t4 - t4P * t7 * t1 * t2 - t4P
                                                                                                                                                            * t6 * t2 * t3 - t4P * t6 * t1 * t2 - t4P * t7 * t2 * t3 - t4P * t7 * t1 * t3 - t4P * t6 * t1 * t3 - t4P * t5 * t1 * t2 - t4P * t5 * t1 * t3 - t4P
                                                                                                                                                            * t5 * t2 * t3 + t4P * t1 * t2 * t3 - t4P * t6 * t1 * t2 * t3 + t5 * t1P * t2 * t3 * t4 + t6 * t2P * t1 * t3 * t4 + t7 * t3P * t1 * t2 * t4 - t4P
                                                                                                                                                            * t7 * t1 * t2 * t3)
    
    if (k > 0) test <- abs(a27/a[2,7])
    a[2,7] <- a[2,7] + a27
    k <- k+1
      
  }
    
    
  if (k == maximumIteration) {
    return(list("flag"=1))
    break
  }
#}

}