context("Test cliques")

test_that("Whether we find the same number of cliques using matrices and eigenvalues", {
  A <- matrix(c(
    0, 1, 1, 0, 0, 0, 0, 1, 0,
    1, 0, 1, 0, 0, 0, 0, 0, 0,
    1, 1, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 1, 1, 0, 0, 0,
    0, 0, 0, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 1, 0, 0, 1, 1, 0,
    0, 0, 0, 0, 0, 1, 0, 1, 0,
    1, 0, 0, 0, 0, 1, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0
  ), byrow = TRUE, ncol = 9)
  rownames(A) <- letters[1:nrow(A)]
  colnames(A) <- letters[1:ncol(A)]

  eigen_cliques <- sum(eigen(A)$values^3) / 6 # number of cliques
  matrix_cliques <- clique_table(A, number = TRUE)$n_triangles

  expect_equal(eigen_cliques, matrix_cliques)
})

test_that("percolation_clique when all nodes are in a clique", {
  A <- matrix(1, 4, 4)
  diag(A) <- 0
  rownames(A) <- letters[1:4]
  colnames(A) <- rownames(A)
  expect_equal(dim(percolation_clique(A)), c(4, 4))
})

test_that("Clique percolation with a node outside every clique", {
  U <- matrix(0, 5, 5)
  U[1:3, 1:3] <- 1
  diag(U) <- 0
  U[3, 4] <- 1
  U[4, 3] <- 1
  rownames(U) <- letters[1:5]
  colnames(U) <- rownames(U)
  expect_equal(nrow(percolation_clique(U)), 5)
})

test_that("clique_table says when there are no cliques", {
  empty <- matrix(0, 4, 4, dimnames = list(letters[1:4], letters[1:4]))
  expect_error(clique_table(empty), "no cliques")
})
