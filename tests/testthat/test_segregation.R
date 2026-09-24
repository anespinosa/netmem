context("Test segregation")

# Two groups that only relate among themselves, plus one tie between them
A <- matrix(c(
  0, 1, 1, 0, 0, 0,
  1, 0, 1, 0, 0, 0,
  1, 1, 0, 1, 0, 0,
  0, 0, 1, 0, 1, 1,
  0, 0, 0, 1, 0, 1,
  0, 0, 0, 1, 1, 0
), byrow = TRUE, ncol = 6)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)
att <- c("a", "a", "a", "b", "b", "b")

test_that("Segregation of two groups joined by a single tie", {
  # Six of the seven ties are within groups
  expect_equal(segregation(A, att), 0.7142857, tolerance = 1e-6) # netseg::assort
  expect_equal(segregation(A, att, method = "gam"), 0.5, tolerance = 1e-6) # netseg::gamix
  expect_equal(segregation(A, att, method = "freeman"), 0.7619048, tolerance = 1e-6) # netseg::freeman
  # Every pair within a group is tied, so the odds of a tie within a group are infinite
  expect_equal(segregation(A, att, method = "orwg"), Inf)

  # A network where the groups are not complete inside
  B <- matrix(0, 8, 8)
  B[1, 2] <- 1
  B[2, 3] <- 1
  B[5, 6] <- 1
  B[6, 7] <- 1
  B[3, 5] <- 1
  B <- pmax(B, t(B))
  group <- c(rep("a", 4), rep("b", 4))
  expect_equal(segregation(B, group, method = "orwg"), 7.5, tolerance = 1e-6) # netseg::orwg
})

test_that("A completely segregated network and a random one", {
  # Without the tie between the groups everything is within groups
  B <- A
  B[3, 4] <- 0
  B[4, 3] <- 0
  expect_equal(segregation(B, att), 1)
  expect_equal(segregation(B, att, method = "gam"), 1)
  expect_equal(segregation(B, att, method = "freeman"), 1)

  # Every tie between the groups and none within
  C <- matrix(0, 6, 6)
  C[1:3, 4:6] <- 1
  C[4:6, 1:3] <- 1
  expect_lt(segregation(C, att), 0)
  expect_equal(segregation(C, att, method = "gam"), -1)
  expect_equal(segregation(C, att, method = "orwg"), 0)
})

test_that("Coleman index by group", {
  # Directed: the first group only sends ties to itself
  D <- matrix(0, 6, 6)
  D[1, 2] <- 1
  D[2, 3] <- 1
  D[3, 1] <- 1
  D[4, 1] <- 1
  D[5, 6] <- 1
  coleman <- segregation(D, att, method = "coleman", digraph = TRUE)
  expect_equal(length(coleman), 2)
  expect_equal(unname(coleman["a"]), 1)
  expect_lt(coleman[["b"]], 1)
  expect_error(segregation(A, att[1:3]), "one group for each node")
})

test_that("The mixing matrix counts every tie", {
  # Two groups of three, all ties within the groups
  G <- matrix(0, 6, 6)
  G[1:3, 1:3] <- 1
  G[4:6, 4:6] <- 1
  diag(G) <- 0
  rownames(G) <- letters[1:6]
  colnames(G) <- rownames(G)
  group <- c(1, 1, 1, 2, 2, 2)

  # Six edges within the groups
  expect_equal(unname(mix_matrix(G, group)), matrix(c(3, 0, 0, 3), 2, 2))
  expect_equal(sum(mix_matrix(G, group)), 6)

  # A group that never sends a tie still has its row and column
  B <- matrix(0, 6, 6)
  B[1:3, 4:6] <- 1
  B[4:6, 1:3] <- 1
  rownames(B) <- letters[1:6]
  colnames(B) <- rownames(B)
  expect_equal(dim(mix_matrix(B, group)), c(2, 2))
  expect_equal(sum(mix_matrix(B, group)), 9)

  # Directed: the entries are the arcs
  D <- matrix(0, 6, 6)
  D[1, 2] <- 1
  D[2, 1] <- 1
  D[1, 4] <- 1
  D[5, 3] <- 1
  rownames(D) <- letters[1:6]
  colnames(D) <- rownames(D)
  expect_equal(sum(mix_matrix(D, group)), 4)
  expect_equal(unname(mix_matrix(D, group)), matrix(c(2, 1, 1, 0), 2, 2))
})

test_that("The E-I index uses the attribute", {
  # Two groups of three, all ties within the groups
  G <- matrix(0, 6, 6)
  G[1:3, 1:3] <- 1
  G[4:6, 4:6] <- 1
  diag(G) <- 0
  rownames(G) <- letters[1:6]
  colnames(G) <- rownames(G)
  group <- c(1, 1, 1, 2, 2, 2)

  expect_equal(ei_index(G, att = group), -1) # every tie is within a group
  B <- matrix(0, 6, 6)
  B[1:3, 4:6] <- 1
  B[4:6, 1:3] <- 1
  rownames(B) <- letters[1:6]
  colnames(B) <- rownames(B)
  expect_equal(ei_index(B, att = group), 1) # every tie is between the groups

  C <- G
  C[1, 4] <- 1
  C[4, 1] <- 1
  expect_equal(ei_index(C, att = group), (1 - 6) / 7)

  # Without an attribute the matrix should already be a mixing matrix
  expect_error(ei_index(G, mixed = FALSE), "attribute")
})
