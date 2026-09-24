context("Test equivalence and blockmodels")

# Two cliques joined by a single tie
A <- matrix(0, 8, 8)
A[1:4, 1:4] <- 1
A[5:8, 5:8] <- 1
diag(A) <- 0
A[4, 5] <- 1
A[5, 4] <- 1
rownames(A) <- letters[1:8]
colnames(A) <- rownames(A)

# Core-periphery: a and b are tied to everybody, the periphery only to the core
B <- matrix(c(
  0, 1, 1, 1, 1, 1,
  1, 0, 1, 1, 1, 1,
  1, 1, 0, 0, 0, 0,
  1, 1, 0, 0, 0, 0,
  1, 1, 0, 0, 0, 0,
  1, 1, 0, 0, 0, 0
), byrow = TRUE, ncol = 6)
rownames(B) <- letters[1:6]
colnames(B) <- rownames(B)

test_that("CONCOR splits structurally equivalent nodes", {
  partition <- concor(A, splits = 1)$partition
  expect_equal(length(unique(partition)), 2)
  expect_equal(unname(partition[1:4]), rep(partition[[1]], 4))
  expect_equal(unname(partition[5:8]), rep(partition[[5]], 4))

  partition <- concor(B, splits = 1)$partition
  expect_equal(unname(partition), c(1, 1, 2, 2, 2, 2))
  expect_equal(concor(B, splits = 1)$positions, 2)
})

test_that("Block densities and image matrix", {
  blocks <- block_density(B, partition = c(1, 1, 2, 2, 2, 2))
  expect_equal(unname(blocks$densities), matrix(c(1, 1, 1, 0), 2, 2))
  expect_equal(unname(blocks$image), matrix(c(1, 1, 1, 0), 2, 2))
  expect_equal(blocks$density, 18 / 30)
  expect_error(block_density(B, partition = c(1, 2)), "one position")

  # With a cutoff of one, only the complete blocks are ones
  expect_equal(unname(block_density(A, partition = c(rep(1, 4), rep(2, 4)), cutoff = 1)$image),
               matrix(c(1, 0, 0, 1), 2, 2))
})

test_that("Regular equivalence of a role structure", {
  # a manages b and c, which manage two subordinates each
  T3 <- matrix(0, 7, 7)
  T3[1, 2:3] <- 1
  T3[2, 4:5] <- 1
  T3[3, 6:7] <- 1
  rownames(T3) <- letters[1:7]
  colnames(T3) <- rownames(T3)

  similarity <- rege(T3)
  expect_equal(similarity["b", "c"], 1) # same role, different subordinates
  expect_equal(similarity["d", "g"], 1) # every subordinate plays the same role
  expect_lt(similarity["a", "d"], 1)
  expect_equal(unname(diag(similarity)), rep(1, 7))
  expect_true(isSymmetric(similarity))
})
