library(CatchAll)
context("Test output")

counts <- rpois(5000, 2)
counts <- counts[counts>0] 
x <- data.frame(table(counts))
x
y <- x
y[,2] <- y[,2]/sum(y[,2])

test_that("Tests are working", {
  expect_true(TRUE)  
  expect_is(TRUE, "logical")
})

test_that("Class is richnessEstimate", {
  expect_true(TRUE)  
  expect_is(PoissonModel(x), "alpha_estimate")
  expect_is(PoissonModel(x, cutoff = 3), "alpha_estimate")
})

test_that("Cutoffs are implemented", {
  expect_equal(PoissonModel(x, cutoff = 3)$other$cutoff, 3)
})

test_that("Catches weird data structres", {
  expect_error(PoissonModel(y))
})

