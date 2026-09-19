context("Test constraint of Everett and Borgatti (2020)")

# Ego "e" and N alters. In the complete network all the alters are tied; in the
# shadow network the alter x1 is tied to all the others and there are no other
# ties among alters. In the directed version the ties from ego to the alters are
# not reciprocated and the others are (Everett and Borgatti, 2020: 53)
ego_network <- function(N, type, directed = FALSE) {
  labels <- c("e", paste0("x", 1:N))
  A <- matrix(0, N + 1, N + 1, dimnames = list(labels, labels))
  A["e", -1] <- 1
  if (!directed) A[-1, "e"] <- 1
  if (type == "complete") A[-1, -1] <- 1
  if (type == "shadow") A["x1", -1] <- A[-1, "x1"] <- 1
  if (type == "shadow" && directed) A["x1", "e"] <- 1
  diag(A) <- 0
  A
}

test_that("eb_constraint reproduces Table 1 (undirected)", {
  complete <- c(1.125, 0.926, 0.766, 0.648, 0.560, 0.493, 0.439, 0.396, 0.361)
  shadow <- c(1.125, 0.840, 0.684, 0.590, 0.529, 0.486, 0.455, 0.431, 0.411)
  for (N in 2:10) {
    expect_equal(eb_constraint(ego_network(N, "complete"), "e")$results$constraint, complete[N - 1], tolerance = 2e-3)
    expect_equal(eb_constraint(ego_network(N, "shadow"), "e")$results$constraint, shadow[N - 1], tolerance = 2e-3)
    # The maximum is the larger of the two, so both networks have the maximum
    # normalized constraint of one when they are the larger
    expect_equal(unname(eb_constraint(ego_network(N, "complete"), "e")$maximum), max(complete[N - 1], shadow[N - 1]), tolerance = 2e-3)
  }
  # With 7 alters the complete network has the maximum
  expect_equal(eb_constraint(ego_network(7, "complete"), "e")$results$normalization, 1)
})

test_that("eb_constraint reproduces Table 2 (directed)", {
  complete <- c(1.388889, 1.08, 0.862245, 0.71358, 0.607438, 0.528318, 0.467222, 0.418685, 0.379224)
  shadow <- c(1.234568, 1.041667, 0.91, 0.822716, 0.761905, 0.717474, 0.683728, 0.657284, 0.636033)
  for (N in 2:10) {
    expect_equal(eb_constraint(ego_network(N, "complete", TRUE), "e", digraph = TRUE)$results$constraint, complete[N - 1], tolerance = 1e-3)
    expect_equal(eb_constraint(ego_network(N, "shadow", TRUE), "e", digraph = TRUE)$results$constraint, shadow[N - 1], tolerance = 1e-3)
  }
})

test_that("a directed network has the constraint of A + t(A)", {
  A <- matrix(c(
    0, 1, 0, 0, 1, 0,
    0, 0, 1, 0, 0, 1,
    1, 0, 0, 1, 0, 0,
    0, 0, 1, 0, 1, 1,
    0, 0, 0, 0, 0, 1,
    1, 1, 0, 1, 0, 0
  ), byrow = TRUE, ncol = 6)
  rownames(A) <- colnames(A) <- letters[1:6]
  directed <- eb_constraint(A, ego = "f", digraph = TRUE)
  valued <- eb_constraint(A + t(A), ego = "f", weighted = TRUE)
  expect_equal(directed$results[, 1:4], valued$results[, 1:4])
  # Same as igraph::constraint on the ego network (igraph 2.2.1)
  expect_equal(directed$results$constraint, 0.525)
  # Reversing the direction of the arcs does not change the constraint
  expect_equal(eb_constraint(t(A), ego = "f", digraph = TRUE)$results, directed$results)
})

test_that("eb_constraint of valued networks agrees with igraph::constraint", {
  # Value computed with igraph 2.2.1 on the ego network of f
  W <- matrix(c(
    0, 2, 1, 0, 0, 3,
    2, 0, 1, 0, 0, 1,
    1, 1, 0, 0, 0, 2,
    0, 0, 0, 0, 4, 1,
    0, 0, 0, 4, 0, 1,
    3, 1, 2, 1, 1, 0
  ), byrow = TRUE, ncol = 6)
  rownames(W) <- colnames(W) <- letters[1:6]
  expect_equal(eb_constraint(W, ego = "f", weighted = TRUE)$results$constraint, 0.567)
  # The binary network ignores the values
  expect_equal(eb_constraint(W, ego = "f")$results$constraint, eb_constraint(1 * (W > 0), ego = "f")$results$constraint)
})

test_that("eb_constraint names the results after ego, as in version 1.0-3", {
  C <- eb_constraint(ego_network(4, "complete"), "e")
  expect_equal(rownames(C$results), "e")
  expect_equal(names(C$maximum), "e")
})

test_that("eb_constraint without ties among alters and with one alter", {
  A <- matrix(c(
    0, 1, 1, 1,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0
  ), byrow = TRUE, ncol = 4)
  rownames(A) <- letters[1:4]
  colnames(A) <- rownames(A)
  expect_equal(eb_constraint(A, ego = "a")$results$constraint, 0.333) # igraph::constraint
  expect_equal(eb_constraint(A, ego = "b")$results$constraint, 1)

  A["d", "a"] <- 0
  A["a", "d"] <- 0
  expect_error(eb_constraint(A, ego = "d"), "isolate")
})

test_that("redundancy of an isolate is not defined", {
  empty <- matrix(0, 4, 4, dimnames = list(letters[1:4], letters[1:4]))
  expect_error(redundancy(empty, ego = "a"), "isolate")
})
