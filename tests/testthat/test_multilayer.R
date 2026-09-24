context("Test supra-adjacency matrices and the aggregation of layers")

A1 <- matrix(c(
  0, 1, 0,
  1, 0, 1,
  0, 1, 0
), byrow = TRUE, ncol = 3, dimnames = list(letters[1:3], letters[1:3]))

A2 <- matrix(c(
  0, 1, 1,
  1, 0, 0,
  1, 0, 0
), byrow = TRUE, ncol = 3, dimnames = list(letters[1:3], letters[1:3]))

test_that("The supra-adjacency matrix has the layers in its diagonal blocks", {
  S <- supra_adjacency(list(advice = A1, friendship = A2))
  expect_equal(dim(S), c(6, 6))
  expect_equal(rownames(S), c("a_advice", "b_advice", "c_advice", "a_friendship", "b_friendship", "c_friendship"))
  expect_equal(unname(S[1:3, 1:3]), unname(A1))
  expect_equal(unname(S[4:6, 4:6]), unname(A2))
  expect_true(isSymmetric(S))

  # Without names, the actors and the layers are numbered
  expect_equal(rownames(supra_adjacency(list(unname(A1), unname(A2)))), c(paste0("n", 1:3, "_L1"), paste0("n", 1:3, "_L2")))
})

test_that("The coupling joins the copies of the same actor", {
  # Categorical: every pair of layers, so three actors in three pairs of layers
  S <- supra_adjacency(list(A1, A2, A1))
  couples <- (sum(S) - 2 * sum(A1) - sum(A2)) / 2
  expect_equal(couples, 3 * 3)
  expect_equal(S["a_L1", "a_L3"], 1)

  # Ordinal: only the layers that follow each other
  O <- supra_adjacency(list(A1, A2, A1), coupling = "ordinal")
  expect_equal((sum(O) - 2 * sum(A1) - sum(A2)) / 2, 3 * 2)
  expect_equal(O["a_L1", "a_L2"], 1)
  expect_equal(O["a_L1", "a_L3"], 0)

  # None: the layers are apart, and the matrix is block diagonal
  N <- supra_adjacency(list(A1, A2), coupling = "none")
  expect_equal(sum(N), sum(A1) + sum(A2))

  # The weight of the coupling is the value of those ties
  W <- supra_adjacency(list(A1, A2), weight = 0.5)
  expect_equal(W["a_L1", "a_L2"], 0.5)

  # Different actors are never coupled
  expect_equal(S["a_L1", "b_L2"], 0)
})

test_that("The layers are aggregated by sum, binary or mean", {
  # The tie a-b is in both layers, b-c and a-c in one each
  A <- aggregate_layers(list(A1, A2))
  expect_equal(A["a", "b"], 2)
  expect_equal(A["b", "c"], 1)
  expect_equal(A["a", "c"], 1)
  expect_equal(aggregate_layers(list(A1, A2), method = "binary")["a", "b"], 1)
  expect_equal(aggregate_layers(list(A1, A2), method = "mean")["a", "b"], 1)
  expect_equal(aggregate_layers(list(A1))["a", "b"], 1)

  # The same result from the supra-adjacency matrix, whose coupling is ignored
  expect_equal(aggregate_layers(supra_adjacency(list(A1, A2))), A)
  expect_equal(aggregate_layers(supra_adjacency(list(A1, A2), coupling = "none"), l = 2), A)
  expect_equal(aggregate_layers(supra_adjacency(list(A1, A2), sparse = TRUE)), A)
})

test_that("The layers should be square matrices with the same actors", {
  expect_error(supra_adjacency(A1), "list")
  expect_error(supra_adjacency(list()), "empty")
  expect_error(supra_adjacency(list(A1, matrix(0, 2, 3))), "square")
  expect_error(supra_adjacency(list(A1, matrix(0, 2, 2))), "same actors")

  S <- supra_adjacency(list(A1, A2))
  attributes(S)[c("actors", "layers")] <- NULL
  expect_error(aggregate_layers(S), "number of layers")
  expect_error(aggregate_layers(S, l = 4), "multiple")

  # Missing values are absent ties, as in the rest of the package
  N <- A1
  N[1, 2] <- NA
  expect_equal(aggregate_layers(list(N, A2))["a", "b"], 1)
})
