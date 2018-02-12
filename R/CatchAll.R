#' Species richness estimation with CatchAll
#'
#' This function implements the CatchAll species richness estimate in R. The packages currently in beta mode!
#'
#'
#' @param frequency_table The sample frequency count table for the population of
#' interest. The first row must correspond to the singletons. Acceptable
#' formats include a matrix, data frame, or file path (csv or txt). The
#' standard frequency count table format is used: two columns, the first of
#' which contains the frequency of interest (eg. 1 for singletons, species
#' observed once, 2 for doubletons, species observed twice, etc.) and the
#' second of which contains the number of species observed this many times.
#' Frequencies (first column) should be ordered least to greatest.
#' @return All models and their species richness estimates (beta version)
#' @author Amy Willis
#' @author Teresa Zhan
#' @export CatchAll
CatchAll <- function(frequency_table) {
  output <- data.frame()
  ## global variables that were declared:
  # obsMax, freqMax, freqTau10
  # ACE1Tau10Rule,
  # freq, observedCount, s
  # n, lnSFactorial, sumlnFFactorial, sumFlnFFactorial
  # w, y, lnW, lnY
  # WLRMSwitch, WLRMGOF0
  # GOF5Test, bestGOF0, bestAICc
  BigChiSq = 1000000000.0
  Criteria = 0.0000000000000001
  maxIter = 100000
  
  
  # Frequency Range for Analysis, model dependent
  # 1=Poisson, 2=1-expl, 3=2-expl, 4=3-exp, 5=4-exp, 6=TWLRM, 7=UWLRM, 8=nonparametric
  fMin <- c(4, 4, 6, 8, 10, 5, 5)
  #numParameters <- c(1, 1, 3, 5, 7, 2, 2)
  fMinFlag <- rep(NA, 7)
  modelDescription <- c(
    "Poisson",
    "SingleExponential",
    "TwoMixedExponential",
    "ThreeMixedExponential",
    "FourMixedExponential",
    "LogTransformedWeightedLinearRegression",
    "UntransformedWeightedLinearRegression",
    "Chao1",
    "ACE",
    "ACE1"
  )
  maxGoodnessOfFit <- 10
  bestCount <- rep(NA, 4)
  
  ################################
  ## Create input data
  ################################
  
  positive_frequency_table <-
    frequency_table[frequency_table[, 2] > 0,]
  
  numberOfRows <- dim(positive_frequency_table)[1]
  if (numberOfRows < 2) {
    stop("Not enough rows?")
    break
  }
  maximumObservation <- numberOfRows # yes, this is correct
  
  bestGOF0  <- array(dim = c(4, 9, maximumObservation))
  bestAICc <- array(dim = c(4, 9, maximumObservation))
  GOFTest <- array(dim = c(9, maximumObservation))
  
  
  frequency <- positive_frequency_table[, 1]
  observedCount <- positive_frequency_table[, 2]
 
  frequencyTau10 <-
    max(which(frequency <= 10 & observedCount > 0)) + 1
  
  ################################
  ## Basic Statistics
  ################################
  
  if (all(positive_frequency_table[, 1] == 1:length(positive_frequency_table[, 1]))) {
    a <- maximumObservation
  } else {
    a <-
      min(which(
        positive_frequency_table[, 1] != 1:length(positive_frequency_table[, 1])
      ))
  }
  frequencyMaximum <- a - 1
  fMinFlag[maximumObservation >= fMin &
             c(rep(TRUE, 5), rep(FALSE, 2))] <- 1
  fMinFlag[frequencyMaximum >= fMin &
             c(rep(FALSE, 5), rep(TRUE, 2))] <- 1
  
  observedCount

  s <- cumsum(observedCount)
  n <- cumsum(frequency * observedCount)
  
  #hmmm-double check these
  logFactorial <- function(x)
    ifelse(x == 0, 0, sum(log(1:x)))
  
  lnSFactorial <- mapply(logFactorial, s)
  lnFFactorial <- mapply(logFactorial, observedCount)
  sumlnFFactorial <-
    cumsum(lnFFactorial)# NA#mapply(logFactorial, lnFFactorial)
  #
  lnIFactorial <- observedCount *  mapply(logFactorial, frequency)
  sumFlnFFactorial <- cumsum(lnIFactorial)
  
  # ratios
  
  # y <- (1 + 1:(frequencyMaximum-1)) *
  #   observedCount[c(2:frequencyMaximum+1)] /
  #   observedCount[c(1:(frequencyMaximum-1))]
  
  y <-  rep(NA, times = frequencyMaximum)
  
  w <- rep(NA, times = frequencyMaximum)
  
  lnW <- rep(NA, times = frequencyMaximum)
  
  w <- observedCount[c(1:(frequencyMaximum - 1))] ^ 3 /
    ((2:(frequencyMaximum)) ^ 2 *
       observedCount[c(2:(frequencyMaximum))] *
       (observedCount[c(1:(frequencyMaximum - 1))] +
          observedCount[c(2:(frequencyMaximum))]))
  
  for (i in 1:frequencyMaximum) {
    lnW[i] = (observedCount[i] * observedCount[i + 1]) /
      (observedCount[i] + observedCount[i + 1])
    y[i] = (i + 1) * observedCount[i + 1] / observedCount[i]
    w[i] = pow(observedCount[i], 3) / #integer overflow
      ((i + 1.0) * (i + 1.0) * observedCount[i + 1] *
         (observedCount[i] + observedCount[i + 1]))
  }
  
  lnY <- log(y)
  WLRMSwitch <- rep(0, frequencyMaximum - 1)
  WLRMGOF0 <- rep(0, frequencyMaximum - 1)
  
  ## TODO
  #  ACE1Tau10Rule <- ACE1Tau10()
  
  bestCount <- rep(NA, 4)
  
  ################################
  ## Non-Parametric Statistics
  ################################
  singletons <- 0.0
  if (frequency[1] == 1) {
    singletons <- observedCount[1]
  }
  
  GTEstimate <- rep(NA, maximumObservation + 1)
  gammaSqRare <- rep(NA, maximumObservation + 1)
  G <- rep(NA, maximumObservation + 1)
  cvRare <- rep(NA, maximumObservation + 1)
  
  
  
  #these could be put in the funtion too for both ACE and ACE1
  # but then you would calculate it twice? seems kinda weird to put it here
  # though
  
  
  ################################
  ## Poisson -works now
  # ################################
  # modelNumber <- 1
  # if (fMinFlag[modelNumber]==1) {
  #   frequencyMinimum <- 1 + max(which(frequency < fMin[modelNumber]))
  #   for (r in frequencyMinimum:maximumObservation) {
  #     poisson_results <- PoissonModel(s, r, observedCount, n,
  #                                  s0Init, frequency,
  #                                  lnSFactorial, sumlnFFactorial, sumFlnFFactorial,
  #                                  maximumObservation)
  #     output <- rbind(output, poisson_results)
  #   }
  # }
  
  ################################
  ## Single Exponential -works now
  ################################
  # modelNumber <- 2
  # if (fMinFlag[modelNumber]==1) {
  #     frequencyMinimum <- 1 + max(which(frequency < fMin[modelNumber]))
  #   for (r in frequencyMinimum:maximumObservation) {
  #     single_exponential_results <- SingleExponentialModel(s, r, observedCount, n,
  #                                   s0Init, frequency,
  #                                   lnSFactorial, sumlnFFactorial,
  #                                   maximumObservation)
  #     output <- rbind(output, single_exponential_results)
  #   }
  # }
  
  
  ################################
  ## Double Exponential -stimate       SE      LCB      UCB incorrect. only returns 1 too
  ################################
  # modelNumber <- 3
  # if (fMinFlag[modelNumber]==1) {
  #   frequencyMinimum <- 1 + max(which(frequency < fMin[modelNumber]))
  #   #change back to maximumObservation!!!
  #   for (r in frequencyMinimum:maximumObservation) {
  #     double_exponential_results <- DoubleExponentialModel(s, r, observedCount, n,
  #                                                          s0Init, frequency,
  #                                                          lnSFactorial, sumlnFFactorial,
  #                                                          maximumObservation)
  #     output <- rbind(output, double_exponential_results)
  #   }
  # }
  
  ################################
  ## Triple Exponential
  ################################
  # modelNumber <- 4
  # if (fMinFlag[modelNumber]==1) {
  #   frequencyMinimum <- 1 + max(which(frequency < fMin[modelNumber]))
  #   #maximumObservation <- frequencyMinimum
  # for (r in frequencyMinimum:maximumObservation) {
  #     triple_exponential_results <- TripleExponentialModel(s, r, observedCount, n,
  #                                                          s0Init, frequency,
  #                                                          lnSFactorial, sumlnFFactorial,
  #                                                          maximumObservation)
  #     output <- rbind(output, triple_exponential_results)
  #   }
  # }
  
  
  ################################
  ## Four Exponential
  ################################
  # modelNumber <- 5
  # if (fMinFlag[modelNumber]==1) {
  #   frequencyMinimum <- 1 + max(which(frequency < fMin[modelNumber]))
  #   #maximumObservation <- frequencyMinimum
  #   for (r in frequencyMinimum:maximumObservation) {
  #     four_exponential_results <- FourExponentialModel(s, r, observedCount, n,
  #                                                          s0Init, frequency,
  #                                                          lnSFactorial, sumlnFFactorial,
  #                                                          maximumObservation)
  #     output <- rbind(output, four_exponential_results)
  #   }
  # }
  # output
  
  ################################
  ## LogTransfWLR
  ################################
  # modelNumber <- 6
  #  if (fMinFlag[modelNumber]==1) {
  #     frequencyMinimum <- 1 + max(which(frequency < fMin[modelNumber]))
  #     for (r in frequencyMinimum:maximumObservation) {
  #       log_transfWLR_results <- LogTWLRModel(lnW, lnY,  WLRMGOF0, WLRMSwitch, s, r, observedCount, n,
  #                                                            s0Init, frequency,
  #                                                            lnSFactorial, sumlnFFactorial,
  #                                                            maximumObservation)
  #       head(output)
  #       output <- rbind(output, log_transfWLR_results)
  #
  #     }
#}
#  output


################################
## WLR
################################
#  modelNumber <- 7
#  if (fMinFlag[modelNumber]==1) {
#    frequencyMinimum <- 1 + max(which(frequency < fMin[modelNumber]))
#    for (r in frequencyMinimum:maximumObservation) {
#      WLR_results <- WLRModel(w, y,  WLRMGOF0, WLRMSwitch, s, r, observedCount, n,
#                                            s0Init, frequency,
#                                            lnSFactorial, sumlnFFactorial,
#                                            maximumObservation)
#      head(output)
#      output <- rbind(output, WLR_results)
# 
#    }
#  }
# output
  
  
  ################################
  ## NonParametric: Chao1
  ################################
  modelNumber <- 8
  # frequencyMinimum <- 1 + max(which(frequency < fMin[1]))
  # for (r in frequencyMinimum:maximumObservation) {
  #   Chao_results <- Chao1Model(s, r, observedCount, n, frequency, singletons, maximumObservation)
  #   output <- rbind(output, Chao_results)
  # }
  # output
  
  
  ################################
  ## NonParametric: ACE
  ################################
  frequencyMinimum <- 1 + max(which(frequency < fMin[1]))
  for (r in frequencyMinimum:maximumObservation) {
    ACE_results <- ACEModel(s, r, observedCount, n, frequency, singletons, maximumObservation,
                              GTEstimate, G, gammaSqRare, cvRare)
    output <- rbind(output, ACE_results)
  }
  output

}
