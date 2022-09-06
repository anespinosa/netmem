context("Test cliques")

test_that("Wheter we find the same number of cliques using matrices and eigenvalues", {
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
