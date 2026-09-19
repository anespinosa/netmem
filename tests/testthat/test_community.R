context("Test communities")

# Two triangles joined by one tie. Expected values computed with igraph 2.2.1
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

# Three cliques in a chain
C3 <- matrix(0, 12, 12)
C3[1:4, 1:4] <- 1
C3[5:8, 5:8] <- 1
C3[9:12, 9:12] <- 1
diag(C3) <- 0
C3[4, 5] <- 1
C3[5, 4] <- 1
C3[8, 9] <- 1
C3[9, 8] <- 1
rownames(C3) <- letters[1:12]
colnames(C3) <- rownames(C3)

test_that("Modularity of a partition", {
  expect_equal(modularity_score(A, c(1, 1, 1, 2, 2, 2)), 0.3571429, tolerance = 1e-6)
  expect_equal(modularity_score(A, rep(1, 6)), 0) # one group gives zero
  expect_lt(modularity_score(A, c(1, 2, 1, 2, 1, 2)), 0) # a partition against the structure

  # Directed modularity uses the out-degree of the sender and the in-degree of the receiver
  D <- matrix(0, 6, 6)
  D[1, 2] <- 1
  D[2, 3] <- 1
  D[3, 1] <- 1
  D[4, 5] <- 1
  D[5, 6] <- 1
  D[6, 4] <- 1
  expect_equal(modularity_score(D, c(1, 1, 1, 2, 2, 2), digraph = TRUE), 0.5)
})

test_that("LinkRank modularity", {
  # The flows and their expectation both add up to one, so a single group gives zero
  expect_equal(modularity_score(A, rep(1, 6), method = "linkrank", digraph = TRUE), 0)
  expect_gt(modularity_score(C3, rep(1:3, each = 4), method = "linkrank", digraph = TRUE), 0)
  expect_lt(modularity_score(C3, rep(1:3, 4), method = "linkrank", digraph = TRUE), 0)
})

test_that("Leading eigenvector finds the two triangles", {
  res <- leading_eigen(A)
  expect_equal(res$groups, 2)
  # The labels of the groups are arbitrary, the split is not
  expect_equal(length(unique(res$partition[1:3])), 1)
  expect_equal(length(unique(res$partition[4:6])), 1)
  expect_true(res$partition[["a"]] != res$partition[["d"]])
  expect_equal(res$modularity, modularity_score(A, res$partition))
})

test_that("Leiden finds the three cliques", {
  set.seed(18051889)
  res <- leiden(C3)
  expect_equal(res$groups, 3)
  expect_equal(as.numeric(factor(res$partition, levels = unique(res$partition))), rep(1:3, each = 4))
  expect_equal(res$modularity, modularity_score(C3, rep(1:3, each = 4)))

  # Every group is internally connected, which is what Leiden guarantees
  for (group in unique(res$partition)) {
    members <- which(res$partition == group)
    expect_equal(length(unique(components_id(C3[members, members])$components)), 1)
  }

  # The constant Potts model does not compare with the degrees
  set.seed(18051889)
  expect_equal(leiden(C3, objective = "cpm", resolution = 0.5)$groups, 3)
})

test_that("Edge betweenness of a chain", {
  # In a chain a - b - c - d, the middle tie carries the geodesics of both sides
  chain <- matrix(0, 4, 4)
  chain[1, 2] <- 1
  chain[2, 3] <- 1
  chain[3, 4] <- 1
  chain <- pmax(chain, t(chain))
  eb <- netmem:::edge_betweenness(chain)
  expect_equal(eb[1, 2], 3) # a-b, a-c and a-d
  expect_equal(eb[2, 3], 4) # a-c, a-d, b-c and b-d
  expect_equal(eb[3, 4], 3)
  expect_true(isSymmetric(eb))
})

test_that("Greedy, label propagation and edge betweenness find the communities", {
  greedy <- community_greedy(C3)
  expect_equal(greedy$groups, 3)
  expect_equal(greedy$modularity, modularity_score(C3, rep(1:3, each = 4)))

  betweenness <- community_betweenness(C3)
  expect_equal(betweenness$groups, 3)
  expect_equal(unname(betweenness$partition), rep(1:3, each = 4))

  set.seed(18051889)
  label <- community_label(C3)
  expect_gt(label$modularity, 0.4)

  # Without the refinement the algorithm is the one of Louvain
  set.seed(18051889)
  expect_equal(leiden(C3, refine = FALSE)$groups, 3)
})
