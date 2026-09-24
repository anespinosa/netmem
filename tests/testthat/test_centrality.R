context("Test centrality")

# Expected values were computed with igraph 2.2.1 (weights as 1/w for the
# valued network) and are hard-coded to avoid depending on igraph

# Krackhardt's kite
A <- matrix(c(
  0, 1, 1, 1, 0, 0, 0, 0, 0,
  1, 0, 1, 1, 1, 0, 0, 0, 0,
  1, 1, 0, 1, 0, 1, 0, 0, 0,
  1, 1, 1, 0, 1, 1, 0, 0, 0,
  0, 1, 0, 1, 0, 1, 1, 0, 0,
  0, 0, 1, 1, 1, 0, 1, 0, 0,
  0, 0, 0, 0, 1, 1, 0, 1, 0,
  0, 0, 0, 0, 0, 0, 1, 0, 1,
  0, 0, 0, 0, 0, 0, 0, 1, 0
), byrow = TRUE, ncol = 9)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)

# Directed network with a disconnected node
D <- matrix(c(
  0, 1, 1, 0, 0, 0,
  0, 0, 0, 1, 1, 0,
  0, 1, 0, 0, 1, 0,
  1, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 0,
  0, 0, 0, 0, 0, 0
), byrow = TRUE, ncol = 6)
rownames(D) <- letters[1:nrow(D)]
colnames(D) <- rownames(D)

# Valued network
W <- matrix(c(
  0, 4, 4, 0, 0, 0,
  4, 0, 2, 1, 1, 0,
  4, 2, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 7,
  0, 0, 0, 0, 7, 0
), byrow = TRUE, ncol = 6)

test_that("Betweenness of binary, directed and valued networks", {
  expect_equal(unname(betweenness_centrality(A, digraph = FALSE)),
               c(0, 1.583333, 1.583333, 3.166667, 6.333333, 6.333333, 12, 7, 0), tolerance = 1e-6)
  expect_equal(unname(betweenness_centrality(A, digraph = FALSE, normalized = TRUE)),
               c(0, 1.583333, 1.583333, 3.166667, 6.333333, 6.333333, 12, 7, 0) / 28, tolerance = 1e-6)
  expect_equal(unname(betweenness_centrality(D)), c(6, 3, 1, 6, 1, 0))
  expect_equal(unname(betweenness_centrality(W, digraph = FALSE, weighted = TRUE)), c(2, 8, 0, 0, 4, 0))
})

test_that("Closeness of binary, directed and valued networks", {
  expect_equal(unname(closeness_centrality(A, digraph = FALSE)),
               c(0.052632, 0.066667, 0.066667, 0.071429, 0.076923, 0.076923, 0.071429, 0.052632, 0.038462), tolerance = 1e-5)
  expect_equal(unname(closeness_centrality(A, digraph = FALSE, normalized = TRUE)),
               8 * c(0.052632, 0.066667, 0.066667, 0.071429, 0.076923, 0.076923, 0.071429, 0.052632, 0.038462), tolerance = 1e-5)
  expect_equal(unname(closeness_centrality(A, digraph = FALSE, harmonic = TRUE)),
               c(4.783333, 5.583333, 5.583333, 6.083333, 5.833333, 5.833333, 5.333333, 4.25, 3.116667), tolerance = 1e-6)
  expect_equal(unname(closeness_centrality(W, digraph = FALSE, weighted = TRUE)),
               c(0.227642, 0.256881, 0.18543, 0.126697, 0.169697, 0.154696), tolerance = 1e-5)
  expect_warning(clo <- closeness_centrality(D, type = "in"), "disconnected")
  expect_equal(unname(clo), c(0.125, 0.142857, 0.111111, 0.166667, 0.142857, NaN), tolerance = 1e-5)
})

test_that("Eigenvector of an undirected network", {
  expect_equal(unname(eigenvector_centrality(A, digraph = FALSE)$vector),
               c(0.684567, 0.842283, 0.842283, 1, 0.776211, 0.776211, 0.425458, 0.116038, 0.02959), tolerance = 1e-5)
  expect_equal(sum(eigenvector_centrality(A, digraph = FALSE, scale = "unit")$vector^2), 1)
})

test_that("Eigenvector stops for acyclic directed networks", {
  B <- matrix(c(
    0, 1, 1,
    0, 0, 1,
    0, 0, 0
  ), byrow = TRUE, ncol = 3)
  expect_error(eigenvector_centrality(B), "leading eigenvalue")
})

test_that("Katz, Bonacich power and PageRank", {
  expect_equal(unname(katz_centrality(A, alpha = 0.1, digraph = FALSE)),
               c(1.514806, 1.665717, 1.665717, 1.816628, 1.660021, 1.660021, 1.457841, 1.258368, 1.125837), tolerance = 1e-6)
  expect_equal(unname(bonacich_power(A, beta = 0.1, digraph = FALSE, scale = "ssq")),
               c(0.894399, 1.156584, 1.156584, 1.418769, 1.146687, 1.146687, 0.79543, 0.448875, 0.218622), tolerance = 1e-6)
  expect_equal(unname(bonacich_power(A, beta = 0, digraph = FALSE)), unname(rowSums(A)))
  expect_equal(unname(page_rank_centrality(A, digraph = FALSE)),
               c(0.092888, 0.120334, 0.120334, 0.147525, 0.125643, 0.125643, 0.111623, 0.097785, 0.058225), tolerance = 1e-5)
  expect_equal(unname(page_rank_centrality(D)),
               c(0.240148, 0.186944, 0.131189, 0.24826, 0.164333, 0.029126), tolerance = 1e-5)
  expect_equal(sum(page_rank_centrality(D)), 1)
  expect_warning(katz_centrality(A, alpha = 0.9, digraph = FALSE), "converge")
})

test_that("Centralization", {
  expect_equal(centrality_centralization(A, "degree", digraph = FALSE)$centralization, 0.267857, tolerance = 1e-6)
  expect_equal(centrality_centralization(A, "closeness", digraph = FALSE)$centralization, 0.254028, tolerance = 1e-6)
  expect_equal(centrality_centralization(A, "betweenness", digraph = FALSE)$centralization, 0.3125, tolerance = 1e-6)
  expect_equal(centrality_centralization(A, "eigenvector", digraph = FALSE)$centralization, 0.501051, tolerance = 1e-6)

  # The star network is completely centralized, and the complete network is not centralized at all
  star <- matrix(0, 6, 6)
  star[1, ] <- 1
  star[, 1] <- 1
  diag(star) <- 0
  expect_equal(centrality_centralization(star, "degree", digraph = FALSE)$centralization, 1)
  complete <- matrix(1, 6, 6)
  diag(complete) <- 0
  expect_equal(centrality_centralization(complete, "degree", digraph = FALSE)$centralization, 0)
})

test_that("Geodesic distances and summary", {
  expect_equal(geo_summary(A, digraph = FALSE)$diameter, 5)
  expect_equal(geo_summary(A, digraph = FALSE)$average_distance, 2.055556, tolerance = 1e-6)
  expect_equal(geo_summary(A, digraph = FALSE)$prop_reachable, 1)

  expect_equal(unname(geo_distances(D)), matrix(c(
    0, 1, 1, 2, 2, Inf,
    2, 0, 3, 1, 1, Inf,
    3, 1, 0, 2, 1, Inf,
    1, 2, 2, 0, 3, Inf,
    2, 3, 3, 1, 0, Inf,
    Inf, Inf, Inf, Inf, Inf, 0
  ), byrow = TRUE, ncol = 6))
  expect_equal(geo_summary(D)$prop_reachable, 20 / 30)
})
