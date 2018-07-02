library(CatchAll)
library(breakaway)
context("Test PoissonModel works")


test_that("PoissonModel runs", {
  
  data(apples)
  
  expect_is(PoissonModel(cbind(c(20:30), 40:30)), "alpha_estimate")
  expect_is(PoissonModel(apples), "alpha_estimate")
  expect_is(PoissonModel(rpois(5000, 2)), "alpha_estimate")
  
})


test_that("PoissonModel gives estimates to within 10% accuracy under correct specification", {
  
  lambdas <- c(50, 500, 2000, 5000)
  for (lambda in lambdas) {
    poisson_data <- PoissonModel(rpois(lambda, 2))
    expect_equal(poisson_data$estimate, lambda, tol = 0.1*lambda)
  }
  
})

