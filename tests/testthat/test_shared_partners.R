context("Test edgewise shared patners")

library(testthat)
test_that("Whether we find the same number of edges with the original matrix", {
  n <- sample(5:30, 1)
  A <- ind_rand_matrix(n,
    type = "probability",
    digraph = TRUE, loops = FALSE
  )
  number_edges <- sum(A)

  esp <- shared_partners(A, type = "esp", directed = TRUE)
  nsp <- shared_partners(A, type = "nsp", directed = TRUE)
  dsp <- shared_partners(A, type = "dsp", directed = TRUE)
  number_edges2 <- sum(esp)

  expect_equal(number_edges, number_edges2)
  expect_equal(sum(esp, nsp), sum(dsp))
})
