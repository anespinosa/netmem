context("Test dyad census")

test_that("Whether we find the same number of dyads", {
  library(netmem)
  library(igraph)
  # set.seed(18051889)
  g <- ind_rand_matrix(5, type = "edges", l = 3, digraph = TRUE, loops = TRUE)
  diag(g) <- 0
  t <- igraph::dyad_census(igraph::graph_from_adjacency_matrix(g))
  dyad <- netmem::dyadic_census(g)
  expect_equal(sum(unlist(dyad)), (NCOL(g) * (NCOL(g) - 1)) / 2)
  expect_equal(unlist(as.numeric(t)), as.numeric(dyad))
})
