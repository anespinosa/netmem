context("Test degree")

test_that("Wheter degree give us the results of the paper", {
  
  A3 <- matrix(c(0,4,4,0,0,0,
                 4,0,2,1,1,0,
                 4,2,0,0,0,0,
                 0,1,0,0,0,0,
                 0,1,0,0,0,7,
                 0,0,0,0,7,0), byrow=TRUE, ncol=6)
  
  gen  <- gen_degree(A3, digraph = FALSE, weighted=TRUE)
  expect_equal(gen[1], 4)
  
})
