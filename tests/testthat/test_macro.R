context("Test macro structure")

test_that("Maximal cliques", {
  A <- matrix(c(
    0, 1, 1, 0, 0, 0,
    1, 0, 1, 1, 0, 0,
    1, 1, 0, 1, 0, 0,
    0, 1, 1, 0, 1, 1,
    0, 0, 0, 1, 0, 1,
    0, 0, 0, 1, 1, 0
  ), byrow = TRUE, ncol = 6)
  rownames(A) <- letters[1:nrow(A)]
  colnames(A) <- rownames(A)

  cliques <- clique_max(A)
  expect_equal(length(cliques), 3)
  expect_true(all(sapply(cliques, length) == 3))
  expect_true(any(sapply(cliques, function(x) identical(x, c("a", "b", "c")))))
  expect_true(any(sapply(cliques, function(x) identical(x, c("b", "c", "d")))))
  expect_true(any(sapply(cliques, function(x) identical(x, c("d", "e", "f")))))

  # The complete network is a single clique, and cliques can be larger than a triangle
  K <- matrix(1, 5, 5)
  diag(K) <- 0
  rownames(K) <- letters[1:5]
  colnames(K) <- rownames(K)
  expect_equal(clique_max(K), list(letters[1:5]))
  expect_equal(length(clique_max(A, min = 4)), 0)
})

test_that("Krackhardt's dimensions of a perfect out-tree", {
  tree <- matrix(0, 7, 7)
  tree[1, 2:3] <- 1
  tree[2, 4:5] <- 1
  tree[3, 6:7] <- 1
  rownames(tree) <- letters[1:7]
  colnames(tree) <- rownames(tree)

  expect_equal(unlist(krackhardt_index(tree)), c(connectedness = 1, hierarchy = 1, efficiency = 1, lubness = 1))
  expect_equal(unlist(krackhardt_index(tree, lubness = "least")), c(connectedness = 1, hierarchy = 1, efficiency = 1, lubness = 1))
})

test_that("Krackhardt's dimensions of a cycle and of isolates", {
  # Every node reaches every other node in both directions, so there is no hierarchy
  cycle <- matrix(0, 4, 4)
  cycle[cbind(1:4, c(2:4, 1))] <- 1
  k <- krackhardt_index(cycle)
  expect_equal(k$connectedness, 1)
  expect_equal(k$hierarchy, 0)
  expect_equal(k$efficiency, 1 - (4 - 3) / (12 - 3))

  # Two separate dyads: half of the pairs are in different components
  dyads <- matrix(0, 4, 4)
  dyads[1, 2] <- 1
  dyads[3, 4] <- 1
  expect_equal(krackhardt_index(dyads)$connectedness, 4 / 12)
  expect_equal(krackhardt_index(dyads)$hierarchy, 1)
})

test_that("The least upper bound condition is more demanding than the upper bound one", {
  # x and y have three upper bounds, none of which reaches the others
  A <- matrix(0, 5, 5)
  A[3, 1] <- 1
  A[3, 2] <- 1
  A[4, 1] <- 1
  A[4, 2] <- 1
  A[5, 1] <- 1
  A[5, 2] <- 1
  rownames(A) <- letters[1:5]
  colnames(A) <- rownames(A)
  expect_gt(krackhardt_index(A)$lubness, krackhardt_index(A, lubness = "least")$lubness)
})

test_that("Core-periphery", {
  A <- matrix(c(
    0, 1, 1, 1, 1, 0,
    1, 0, 1, 1, 0, 1,
    1, 1, 0, 1, 0, 0,
    1, 1, 1, 0, 0, 0,
    1, 0, 0, 0, 0, 0,
    0, 1, 0, 0, 0, 0
  ), byrow = TRUE, ncol = 6)
  rownames(A) <- letters[1:nrow(A)]
  colnames(A) <- rownames(A)

  set.seed(1)
  cp <- core_periphery(A)
  expect_equal(sort(c(cp$core, cp$periphery)), letters[1:6])
  expect_true(cp$fit > 0)
  # The most connected nodes are in the core
  expect_true(all(c("a", "b") %in% cp$core))

  # An ideal core-periphery structure is fitted perfectly
  ideal <- matrix(1, 6, 6)
  ideal[4:6, 4:6] <- 0
  diag(ideal) <- 0
  rownames(ideal) <- letters[1:6]
  colnames(ideal) <- rownames(ideal)
  set.seed(1)
  expect_equal(core_periphery(ideal)$fit, 1)
  expect_equal(sort(core_periphery(ideal)$core), c("a", "b", "c"))

  cont <- core_periphery(A, method = "continuous")
  expect_equal(length(cont$coreness), 6)
  expect_equal(sum(cont$coreness^2), 1)
})

test_that("trans_coef returns the mean local transitivity", {
  A <- matrix(c(
    0, 1, 0, 1, 0,
    1, 0, 1, 1, 0,
    0, 1, 0, 0, 0,
    1, 1, 0, 0, 1,
    0, 0, 0, 1, 0
  ), byrow = TRUE, ncol = 5)
  rownames(A) <- letters[1:ncol(A)]
  colnames(A) <- rownames(A)
  expect_equal(trans_coef(A, method = "mean"), mean(c(1, 1 / 3, 1 / 3))) # c and e have one neighbour
})

test_that("The transitivity matrix marks every transitive triple", {
  # An undirected triangle: the three nodes are in a transitive triple
  U <- matrix(0, 4, 4)
  U[1:3, 1:3] <- 1
  diag(U) <- 0
  rownames(U) <- letters[1:4]
  colnames(U) <- rownames(U)
  B <- trans_matrix(U)
  expect_equal(unname(B[1:3, 1:3]), matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3))
  expect_equal(unname(B[4, ]), c(0, 0, 0, 0))
})
