context("Test dyad census")

test_that("Whether we find the same number of dyads", {
  # 1 <-> 2 and 2 <-> 4 are mutual; 1 -> 3, 3 -> 4, 4 -> 5 and 5 -> 1 are
  # asymmetric; the other four pairs are null. Same as igraph::dyad_census
  g <- matrix(c(
    0, 1, 1, 0, 0,
    1, 0, 0, 1, 0,
    0, 0, 0, 1, 0,
    0, 1, 0, 0, 1,
    1, 0, 0, 0, 0
  ), byrow = TRUE, ncol = 5)
  dyad <- dyadic_census(g)
  expect_equal(as.numeric(dyad), c(2, 4, 4))
  expect_equal(sum(dyad), (NCOL(g) * (NCOL(g) - 1)) / 2)

  # The census of a random network always adds up to the number of pairs
  g <- ind_rand_matrix(5, type = "edges", l = 3, digraph = TRUE, loops = TRUE)
  diag(g) <- 0
  expect_equal(sum(dyadic_census(g)), (NCOL(g) * (NCOL(g) - 1)) / 2)
})
