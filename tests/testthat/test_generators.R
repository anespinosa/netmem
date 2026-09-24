context("Test random network generators")

test_that("Watts-Strogatz without rewiring is a ring lattice", {
  A <- small_world(30, neighbours = 2, p = 0)
  expect_true(all(rowSums(A) == 4))
  expect_true(isSymmetric(A))
  expect_equal(sum(A) / 2, 60)
  # Transitivity of a ring lattice: 3(k - 2) / (4(k - 1)) with k = 4
  expect_equal(trans_coef(A, method = "global"), 3 * (4 - 2) / (4 * (4 - 1)))

  # Rewiring keeps the number of ties and shortens the distances
  set.seed(18051889)
  B <- small_world(30, neighbours = 2, p = 1)
  expect_equal(sum(B) / 2, sum(A) / 2)
  expect_lt(geo_summary(B, digraph = FALSE)$average_distance,
            geo_summary(A, digraph = FALSE)$average_distance)
  expect_error(small_world(3, neighbours = 2), "larger")
})

test_that("Preferential attachment concentrates the ties in the first nodes", {
  set.seed(18051889)
  A <- pref_attachment(100, m = 2)
  expect_equal(sum(A) / 2, 2 * (100 - 2))
  expect_true(all(diag(A) == 0))
  expect_true(isSymmetric(A))
  # The nodes that arrive first end up with more ties
  expect_lt(cor(1:100, rowSums(A), method = "spearman"), 0)
  # With no preference the degrees are much less concentrated
  set.seed(18051889)
  random <- pref_attachment(100, m = 2, power = 0)
  expect_lt(max(rowSums(random)), max(rowSums(A)))
  expect_error(pref_attachment(2, m = 2), "larger")
})

test_that("The G(n,m) model places exactly the ties asked for", {
  set.seed(18051889)
  for (digraph in c(TRUE, FALSE)) {
    for (loops in c(TRUE, FALSE)) {
      A <- ind_rand_matrix(12, type = "edges", l = 9, digraph = digraph, loops = loops)
      # An undirected tie is in both cells, and a loop in a single one
      ties <- if (digraph) sum(A) else (sum(A) + sum(diag(A))) / 2
      expect_equal(ties, 9)
      expect_true(all(A %in% c(0, 1)))
      if (!loops) expect_true(all(diag(A) == 0))
      if (!digraph) expect_true(isSymmetric(A))
    }
  }
  expect_error(ind_rand_matrix(5, type = "edges"), "not specified")
})

test_that("The G(n,p) model gives the complete network with p = 1 and none with p = 0", {
  # The cells that each model can fill: the whole matrix, without the diagonal,
  # or one triangle placed in both halves
  expect_equal(sum(ind_rand_matrix(5, type = "probability", p = 1, loops = TRUE)), 25)
  expect_equal(sum(ind_rand_matrix(5, type = "probability", p = 1)), 20)
  expect_equal(sum(ind_rand_matrix(5, type = "probability", p = 1, digraph = FALSE)), 20)
  expect_equal(sum(ind_rand_matrix(5, type = "probability", p = 1, digraph = FALSE, loops = TRUE)), 25)
  expect_equal(sum(ind_rand_matrix(5, type = "probability", p = 0)), 0)

  set.seed(18051889)
  # Without p the ties follow a uniform distribution, and with trials the
  # cells count how many of them succeeded
  expect_true(sum(ind_rand_matrix(30, type = "probability")) > 0)
  counts <- ind_rand_matrix(10, type = "probability", p = 0.5, trials = 3)
  expect_true(max(counts) <= 3)
  expect_true(all(counts == round(counts)))
})

test_that("Two-mode and multilevel random matrices have the two sets of nodes", {
  set.seed(18051889)
  X <- ind_rand_matrix(n = 5, m = 4, type = "edges", l = 6)
  # The rows are the second set and the columns the first one
  expect_equal(dim(X), c(4, 5))
  expect_equal(sum(X), 6)
  expect_equal(sum(ind_rand_matrix(n = 5, m = 4, type = "probability", p = 1)), 20)

  M <- ind_rand_matrix(n = 5, m = 3, type = "probability", p = 0.5, multilevel = TRUE)
  expect_equal(dim(M), c(8, 8))
  expect_equal(rownames(M), c(paste0("n", 1:5), paste0("m", 1:3)))
  expect_equal(colnames(M), rownames(M))
  expect_equal(dim(ind_rand_matrix(n = 5, m = 3, type = "edges", l = 4, multilevel = TRUE, digraph = FALSE)), c(8, 8))
})

test_that("The sparse matrix follows the same models as the dense one", {
  set.seed(18051889)
  for (digraph in c(TRUE, FALSE)) {
    for (loops in c(TRUE, FALSE)) {
      A <- ind_rand_matrix(40, type = "edges", l = 25, digraph = digraph, loops = loops, sparse = TRUE)
      expect_s4_class(A, "Matrix")
      ties <- if (digraph) sum(A) else (sum(A) + sum(Matrix::diag(A))) / 2
      expect_equal(ties, 25)
      if (!loops) expect_true(all(Matrix::diag(A) == 0))
      if (!digraph) expect_true(isSymmetric(as.matrix(A)))
    }
  }
  # Every cell that the model allows can hold a tie, and no other
  expect_equal(sum(ind_rand_matrix(6, type = "probability", p = 1, sparse = TRUE)), 30)
  expect_equal(sum(ind_rand_matrix(6, type = "probability", p = 1, digraph = FALSE, loops = TRUE, sparse = TRUE)), 36)
  expect_equal(dim(ind_rand_matrix(n = 5, m = 4, type = "edges", l = 7, sparse = TRUE)), c(4, 5))

  expect_error(ind_rand_matrix(10, type = "edges", l = 100, digraph = FALSE, sparse = TRUE), "larger")
  expect_error(ind_rand_matrix(10, m = 2, type = "edges", l = 3, multilevel = TRUE, sparse = TRUE), "multilevel")
  expect_error(ind_rand_matrix(10, type = "probability", p = 0.5, trials = 3, sparse = TRUE), "trials")

  # A large network is built without the dense matrix
  big <- ind_rand_matrix(20000, type = "edges", l = 1000, digraph = FALSE, sparse = TRUE)
  expect_equal(sum(big) / 2, 1000)
  expect_lt(as.numeric(object.size(big)), 1e6)
})
