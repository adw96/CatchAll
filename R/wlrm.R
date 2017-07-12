wlrm_untransformed <- function(frequency_table) {
  
  input_table <- frequency_table@table
  n <- frequency_table@n
  f1 <- frequency_table@f1
  
  if ("SplitFrequencyTable" %in% class(frequency_table)) {
    cutoff <- frequency_table@cutoff
  } else {
    cutoff <- length(input_table[,1])
  }
  
  my_table <- input_table[1:cutoff,]
  
  ys <- (my_table[1:(cutoff-1),1]+1)*my_table[2:cutoff,2]/my_table[1:(cutoff-1),2]
  xs <- 1:(cutoff-1)
  xbar <- mean(xs)
  lhs <- list("x"=xs-xbar,"y"=my_table[2:cutoff,2]/my_table[1:(cutoff-1),2])
  
  weights_untrans <- (my_table[-1,1]^2*my_table[-1,2]*my_table[-cutoff,2]^-2*(my_table[-1,2]*my_table[-cutoff,2]^-1+1))^-1
  
  lm2 <- lm(ys~xs,weights=weights_untrans)
  b0_hat <- summary(lm2)$coef[1,1]
  
  if(b0_hat > 0) {
    b0_se <- summary(lm2)$coef[1,2]
    f0 <- f1 / b0_hat
    diversity <- f0 + n
    f0_se <- sqrt( f1*(1-f1/n)*b0_hat^-2 + f1^2*b0_hat^-4*b0_se^2   ) #1st order d.m.
    diversity_se <- sqrt(f0_se^2+n*f0/(n+f0))
    d <- exp(1.96*sqrt(log(1+diversity_se^2/f0)))
    
    new("Estimate", 
        estimate = diversity, 
        se = diversity_se, 
        model = "UntransformedWLRM",
        ci = c(n+f0/d, n+f0*d), 
        success = TRUE,
        cutoff = cutoff)
    
  } else {
    new("Estimate", 
        model = "UntransformedWLRM",
        cutoff = cutoff,
        success = FALSE)
  }
  
}

wlrm_transformed <- function(frequency_table) {
  
  input_table <- frequency_table@table
  n <- frequency_table@n
  f1 <- frequency_table@f1
  
  if ("SplitFrequencyTable" %in% class(frequency_table)) {
    cutoff <- frequency_table@cutoff
  } else {
    # TODO: make maximum contiguous
    cutoff <- length(input_table[,1])
  }
  
  my_table <- input_table[1:cutoff,]
  
  ys <- (my_table[1:(cutoff-1),1]+1)*my_table[2:cutoff,2]/my_table[1:(cutoff-1),2]
  xs <- 1:(cutoff-1)
  xbar <- mean(xs)
  lhs <- list("x"=xs-xbar,"y"=my_table[2:cutoff,2]/my_table[1:(cutoff-1),2])
  
  weights_trans <- (1/my_table[-1,2]+1/my_table[-cutoff,2])^-1
  lm1 <- lm(log(ys)~xs,weights=weights_trans)
  b0_hat <- summary(lm1)$coef[1,1]
  b0_se <- summary(lm1)$coef[1,2]
  f0 <- f1*exp(-b0_hat)
  diversity <- f0 + n
  f0_se <- sqrt( (exp(-b0_hat))^2*f1*(b0_se^2*f1+1) )
  diversity_se <- sqrt(f0_se^2+n*f0/(n+f0))
  d <- exp(1.96*sqrt(log(1+diversity_se^2/f0)))

  new("Estimate", 
      estimate = diversity, 
      se = diversity_se, 
      model = "TransformedWLRM",
      ci = c(n+f0/d, n+f0*d), 
      success = TRUE,
      cutoff = cutoff)

}
