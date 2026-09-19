context("Test the extensions of the existing functions")

# Expected values computed with igraph 2.2.1 or by hand

test_that("Components are weak or strong, and cover both modes", {
  # A chain belongs to a single weak component, and each node is its own
  # strong component
  chain <- matrix(0, 4, 4)
  chain[1, 2] <- 1
  chain[2, 3] <- 1
  chain[3, 4] <- 1
  rownames(chain) <- letters[1:4]
  colnames(chain) <- rownames(chain)

  expect_equal(length(unique(components_id(chain)$components)), 1)
  expect_equal(length(unique(components_id(chain, mode = "strong")$components)), 4)

  # A cycle is a single strong component
  cycle <- chain
  cycle[4, 1] <- 1
  expect_equal(length(unique(components_id(cycle, mode = "strong")$components)), 1)

  # Two separate triangles
  two <- matrix(0, 6, 6)
  two[1:3, 1:3] <- 1
  two[4:6, 4:6] <- 1
  diag(two) <- 0
  expect_equal(length(unique(components_id(two)$components)), 2)
  expect_equal(as.numeric(components_id(two)$size), c(3, 3))

  # Two-mode: the rows and the columns that they share are in the same component
  B <- matrix(c(
    1, 1, 0, 0,
    1, 0, 0, 0,
    0, 0, 1, 1,
    0, 0, 0, 1
  ), byrow = TRUE, ncol = 4)
  rownames(B) <- paste0("r", 1:4)
  colnames(B) <- paste0("c", 1:4)
  components <- components_id(B, bipartite = TRUE)$components
  expect_equal(length(components), 8) # the nodes of both modes
  expect_equal(length(unique(components)), 2)
  expect_equal(unname(components["r1"]), unname(components["c2"]))
  expect_false(components[["r1"]] == components[["r3"]])
})

test_that("Eigenvector centrality with negative ties", {
  # Three nodes that like each other, and one that everybody dislikes
  S <- matrix(c(
    0, 1, 1, -1,
    1, 0, 1, -1,
    1, 1, 0, -1,
    -1, -1, -1, 0
  ), byrow = TRUE, ncol = 4)
  rownames(S) <- letters[1:4]
  colnames(S) <- rownames(S)

  signed <- eigenvector_centrality(S, digraph = FALSE, signed = TRUE)
  expect_equal(unname(signed$vector), c(1, 1, 1, -1))
  expect_equal(signed$value, 3)

  # Without the argument the negative ties would be read as ties
  expect_true(all(eigenvector_centrality(abs(S), digraph = FALSE)$vector > 0))
})

test_that("Barrat's weighted transitivity", {
  W <- matrix(c(
    0, 4, 0, 2, 0,
    4, 0, 1, 3, 0,
    0, 1, 0, 0, 0,
    2, 3, 0, 0, 5,
    0, 0, 0, 5, 0
  ), byrow = TRUE, ncol = 5)
  expect_equal(unname(trans_coef(W, method = "barrat")), c(1, 0.4375, NaN, 0.25, NaN))

  # On a binary network it is the local transitivity
  A <- 1 * (W > 0)
  rownames(A) <- letters[1:5]
  colnames(A) <- rownames(A)
  expect_equal(unname(trans_coef(A, method = "barrat"))[1:2], unname(unlist(trans_coef(A, method = "local")))[1:2])
})

test_that("Density of weighted networks", {
  W <- matrix(c(
    0, 4, 0, 2, 0,
    4, 0, 1, 3, 0,
    0, 1, 0, 0, 0,
    2, 3, 0, 0, 5,
    0, 0, 0, 5, 0
  ), byrow = TRUE, ncol = 5)
  # The average strength over the possible ties
  expect_equal(gen_density(W, directed = FALSE, weighted = TRUE), 15 / 10)
  expect_equal(gen_density(W, directed = TRUE, weighted = TRUE), 30 / 20)

  B <- matrix(c(
    2, 0, 1,
    0, 3, 0
  ), byrow = TRUE, ncol = 3)
  expect_equal(gen_density(B, bipartite = TRUE, weighted = TRUE), 6 / 6)
})

test_that("Edge lists keep the direction and the value of the ties", {
  E <- rbind(
    c("a", "b"),
    c("c", "b"),
    c("b", "c")
  )
  A <- edgelist_to_matrix(E, digraph = TRUE)
  expect_equal(A["c", "b"], 1)
  expect_equal(A["b", "c"], 1)
  expect_equal(A["a", "b"], 1)
  expect_equal(A["b", "a"], 0)

  V <- rbind(
    c("a", "b", 3),
    c("b", "c", 1),
    c("c", "a", 7)
  )
  W <- edgelist_to_matrix(V, valued = TRUE)
  expect_equal(W["a", "b"], 3)
  expect_equal(W["c", "a"], 7)
  expect_equal(W["b", "a"], 0)
  expect_error(edgelist_to_matrix(E, valued = TRUE), "third column")

  # The loops are dropped unless they are asked for
  L <- rbind(c("a", "a"), c("a", "b"))
  expect_equal(edgelist_to_matrix(L)["a", "a"], 0)
  expect_equal(edgelist_to_matrix(L, loops = TRUE)["a", "a"], 1)
})

test_that("Distances with more than one matrix", {
  A <- matrix(c(
    0, 0, 3, 0, 5,
    0, 0, 2, 0, 4,
    5, 4, 0, 4, 0,
    0, 3, 0, 1, 0,
    0, 0, 0, 0, 2
  ), nrow = 5, byrow = TRUE)
  B <- matrix(c(
    0, 1, 0, 0, 1,
    1, 0, 0, 0, 1,
    0, 0, 0, 1, 0,
    0, 0, 1, 0, 0,
    1, 1, 0, 0, 0
  ), nrow = 5, byrow = TRUE)

  # The rows and the columns of both matrices are stacked into one profile
  expect_equal(
    unname(dist_sim_matrix(list(A, B), method = "euclidean")),
    unname(as.matrix(dist(cbind(A, B, t(A), t(B)))))
  )
  expect_error(dist_sim_matrix(list(A, B[1:3, ]), method = "euclidean"), "same number of rows")
})

test_that("gen_density agrees with igraph::edge_density", {
  # Expected values computed with igraph 2.2.1 and hard-coded
  # Two-mode: the diagonal of an incidence matrix holds real ties
  B <- matrix(c(
    1, 1, 0,
    0, 0, 1,
    0, 1, 1,
    0, 0, 1
  ), byrow = TRUE, ncol = 3)
  expect_equal(gen_density(B, bipartite = TRUE), 6 / 12)
  expect_equal(gen_density(B, bipartite = TRUE, loops = TRUE), 6 / 12)
  D <- matrix(c(
    0, 1, 1, 0,
    0, 0, 1, 0,
    0, 0, 0, 1,
    1, 0, 0, 0
  ), byrow = TRUE, ncol = 4)
  expect_equal(gen_density(D), 5 / 12)
  expect_equal(unname(unlist(gen_density(list(D, B), multilayer = TRUE))), c(5 / 12, 6 / 12))
  # Underlying graph: the five arcs are five different edges
  expect_equal(suppressWarnings(gen_density(D, directed = FALSE)), 5 / 6)
  # With loops the diagonal is among the possible ties
  L <- D
  L[1, 1] <- 1
  expect_equal(gen_density(L, loops = TRUE), 6 / 16)
  expect_equal(suppressWarnings(gen_density(L, directed = FALSE, loops = TRUE)), 6 / 10)
})

test_that("Edge lists of an empty network and of a single column", {
  empty <- matrix(0, 4, 4, dimnames = list(letters[1:4], letters[1:4]))
  expect_equal(dim(matrix_to_edgelist(empty)), c(0, 2))
  E <- cbind(c("a", "b", "c"), c("1", "1", "1"))
  expect_equal(dim(edgelist_to_matrix(E, bipartite = TRUE)), c(3, 1))
})
