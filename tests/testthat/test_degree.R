context("Test degree")

test_that("Wheter degree give us the results of the paper", {
  A3 <- matrix(c(
    0, 4, 4, 0, 0, 0,
    4, 0, 2, 1, 1, 0,
    4, 2, 0, 0, 0, 0,
    0, 1, 0, 0, 0, 0,
    0, 1, 0, 0, 0, 7,
    0, 0, 0, 0, 7, 0
  ), byrow = TRUE, ncol = 6)

  gen <- gen_degree(A3, digraph = FALSE, weighted = TRUE)
  expect_equal(gen[1], 4)
})

# The example of the README: three levels, without A3 and without B3
A1 <- matrix(c(
  0, 1, 0, 0, 1,
  1, 0, 0, 1, 1,
  0, 0, 0, 1, 1,
  0, 1, 1, 0, 1,
  1, 1, 1, 1, 0
), byrow = TRUE, ncol = 5)
B1 <- matrix(c(
  1, 0, 0,
  1, 1, 0,
  0, 1, 0,
  0, 1, 0,
  0, 1, 1
), byrow = TRUE, ncol = 3)
A2 <- matrix(c(
  0, 1, 1,
  1, 0, 0,
  1, 0, 0
), byrow = TRUE, nrow = 3)
B2 <- matrix(c(
  1, 1, 0, 0,
  0, 0, 1, 0,
  0, 0, 1, 1
), byrow = TRUE, ncol = 4)
A3 <- matrix(c(
  0, 1, 1, 1,
  1, 0, 0, 0,
  1, 0, 0, 1,
  1, 0, 1, 0
), byrow = TRUE, ncol = 4)
B3 <- matrix(c(
  1, 0, 0, 0, 0,
  0, 1, 0, 1, 0,
  0, 0, 0, 0, 0,
  0, 0, 1, 0, 0
), byrow = TRUE, ncol = 5)

test_that("The multilevel degree counts each level with its own ties and its ties to other levels", {
  res <- multilevel_degree(A1, B1, A2, B2, complete = TRUE)
  levels <- substr(rownames(res), 1, 1)
  # A3 is not given, so the third level only has its ties with the second level
  expect_equal(res$multilevel[levels == "k"], colSums(B2))
  expect_equal(res$multilevel[levels == "n"], unname(rowSums(A1) + rowSums(B1)))
  expect_equal(res$multilevel[levels == "m"], unname(colSums(B1) + rowSums(A2) + rowSums(B2)))

  # Each level is the column that emphasises it
  expect_equal(res$multilevel[levels == "n"], res$low_multilevel[levels == "n"])
  expect_equal(res$multilevel[levels == "m"], res$meso_multilevel[levels == "m"])
  expect_equal(res$multilevel[levels == "k"], res$high_multilevel[levels == "k"])
})

test_that("The multilevel degree with the three incidence matrices", {
  res <- multilevel_degree(A1, B1, A2, B2, A3, B3, complete = TRUE)
  levels <- substr(rownames(res), 1, 1)
  expect_equal(res$multilevel[levels == "n"], unname(rowSums(A1) + rowSums(B1) + colSums(B3)))
  expect_equal(res$multilevel[levels == "k"], unname(colSums(B2) + rowSums(A3) + rowSums(B3)))
  expect_equal(res$multilevel[levels == "k"], res$high_multilevel[levels == "k"])
  expect_equal(res$bipartiteB3[levels == "n"], colSums(B3))
})

test_that("The normalized multilevel degree", {
  res <- multilevel_degree(A1, B1, A2, B2, A3, normalized = TRUE)
  levels <- substr(rownames(res), 1, 1)
  n <- 5
  m <- 3
  k <- 4
  expect_equal(res$multilevel[levels == "k"], unname((colSums(B2) + rowSums(A3)) / ((k - 1) + m)), tolerance = 1e-3)
  expect_equal(res$multilevel[levels == "m"], unname((colSums(B1) + rowSums(A2) + rowSums(B2)) / ((m - 1) + n + k)), tolerance = 1e-3)
})

test_that("The weights follow the loops and the symmetrization of the ties", {
  W <- matrix(c(
    5, 3, 2,
    3, 0, 1,
    2, 1, 0
  ), byrow = TRUE, ncol = 3)
  # With alpha = 1 the generalized degree is the strength
  expect_equal(unname(gen_degree(W, weighted = TRUE, alpha = 1, loops = FALSE, digraph = FALSE)), c(5, 4, 3))
  asymmetric <- matrix(c(
    0, 4, 0,
    1, 0, 2,
    0, 2, 0
  ), byrow = TRUE, ncol = 3)
  expect_equal(unname(suppressWarnings(gen_degree(asymmetric, weighted = TRUE, alpha = 1, digraph = FALSE))), c(4, 6, 2))
})

test_that("k_core of binary networks agrees with igraph::coreness", {
  # Expected values computed with igraph 2.2.1 and hard-coded
  # A clique of four nodes (1-4), with a tail 3 - 5 - 6
  A <- matrix(c(
    0, 1, 1, 1, 0, 0,
    1, 0, 1, 1, 0, 0,
    1, 1, 0, 1, 1, 0,
    1, 1, 1, 0, 0, 0,
    0, 0, 1, 0, 0, 1,
    0, 0, 0, 0, 1, 0
  ), byrow = TRUE, ncol = 6)
  expect_equal(k_core(A), c(3, 3, 3, 3, 1, 1))
  # Three ties between 5 and 6 count as three
  V <- A
  V[5, 6] <- V[6, 5] <- 3
  expect_equal(k_core(V), c(3, 3, 3, 3, 3, 3))
  # A loop counts twice, and only when loops = TRUE
  L <- A
  L[6, 6] <- 1
  expect_equal(k_core(L, loops = TRUE), c(3, 3, 3, 3, 2, 2))
  expect_equal(k_core(L), c(3, 3, 3, 3, 1, 1))
  # Directed cycle 1 -> 2 -> 3 -> 1 with 3 -> 4 -> 2
  D <- matrix(c(
    0, 1, 0, 0,
    0, 0, 1, 0,
    1, 0, 0, 1,
    0, 1, 0, 0
  ), byrow = TRUE, ncol = 4)
  expect_equal(k_core(D, digraph = TRUE, type = "in"), c(1, 1, 1, 1))
  expect_equal(k_core(D, digraph = TRUE, type = "out"), c(1, 1, 1, 1))
  expect_equal(k_core(D, digraph = TRUE, type = "all"), c(2, 2, 2, 2))
})

test_that("zone_sample returns the adjacency matrices of the zones", {
  # Expected values from the igraph version of zone_sample (netmem 1.0-3)
  A <- matrix(0, 8, 8, dimnames = list(1:8, 1:8))
  A[rbind(c(1, 2), c(2, 3), c(3, 2), c(3, 4), c(6, 4))] <- 1
  X <- matrix(c(
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 1, 0,
    0, 1, 1, 0,
    0, 1, 1, 1,
    0, 1, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 1
  ), byrow = TRUE, ncol = 4, dimnames = list(1:8, c("a", "b", "c", "d")))
  Z <- zone_sample(A, X, core = TRUE)
  expect_equal(names(Z), c("a", "b", "c", "d"))
  expect_equal(rownames(Z$c), c("2", "3", "4", "5", "6", "a", "b", "c", "d"))
  expect_equal(sum(Z$c), 22)
  expect_equal(unname(attr(Z$c, "core")), c(0, 1, 1, 1, 0, 0, 0, 0, 0))
  expect_equal(Z$c["6", ], c("2" = 0, "3" = 0, "4" = 1, "5" = 0, "6" = 0, a = 0, b = 1, c = 0, d = 0))
})
