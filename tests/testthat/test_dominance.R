context("Test dominance")

# Star: the neighbourhood-inclusion preorder is complete
S <- matrix(c(
  0, 1, 1, 1, 1,
  1, 0, 0, 0, 0,
  1, 0, 0, 0, 0,
  1, 0, 0, 0, 0,
  1, 0, 0, 0, 0
), byrow = TRUE, ncol = 5)
rownames(S) <- letters[1:nrow(S)]
colnames(S) <- rownames(S)

# Path a - b - c - d
L <- matrix(c(
  0, 1, 0, 0,
  1, 0, 1, 0,
  0, 1, 0, 1,
  0, 0, 1, 0
), byrow = TRUE, ncol = 4)
rownames(L) <- letters[1:nrow(L)]
colnames(L) <- rownames(L)

# Example of the hyper-event chain (001dominance.R)
X <- matrix(c(
  1, 1, 1, 0,
  1, 1, 0, 0,
  1, 0, 0, 0,
  0, 1, 0, 0,
  0, 0, 1, 0
), byrow = TRUE, ncol = 4)
rownames(X) <- c("a1", "a2", "a3", "a4", "a5")
colnames(X) <- c("w1", "w2", "w3", "w4")

W <- matrix(c(
  0, 1, 1, 0,
  0, 0, 1, 0,
  0, 0, 0, 1,
  0, 0, 0, 0
), byrow = TRUE, ncol = 4)
rownames(W) <- colnames(X)
colnames(W) <- colnames(X)

test_that("Neighbourhood inclusion of a star and a path", {
  P <- neigh_inclusion(S)
  expect_equal(sum(P), 4 + 4 * 3) # leaves under the centre, and leaves equivalent among them
  expect_true(all(P[-1, "a"] == 1))
  expect_equal(dominance_pairs(P)$prop_comparable, 1)

  P <- neigh_inclusion(L)
  expect_equal(P["a", "c"], 1) # N(a) = {b} is included in N[c] = {b, c, d}
  expect_equal(P["a", "b"], 1)
  expect_equal(P["b", "c"], 0) # N(b) = {a, c} is not included in N[c]
  expect_equal(P["a", "d"], 0)
  expect_equal(dominance_pairs(P)$incomparable, 2)
})

test_that("Open neighbourhoods are more demanding", {
  expect_equal(neigh_inclusion(L, closed = FALSE)["a", "b"], 0)
  expect_equal(neigh_inclusion(L, closed = FALSE)["a", "c"], 1)
})

test_that("Degree and betweenness preserve neighbourhood inclusion", {
  P <- neigh_inclusion(L)
  expect_true(preserved_order(P, rowSums(L))$preserved)
  expect_true(preserved_order(P, betweenness_centrality(L, digraph = FALSE))$preserved)
  expect_false(preserved_order(P, c(4, 3, 2, 1))$preserved)
  expect_equal(nrow(preserved_order(P, c(4, 3, 2, 1))$violations), 2)
})

test_that("Pareto dominance requires strict inclusion", {
  P <- set_inclusion(X)
  D <- pareto_dominance(list(P))
  expect_equal(D["a2", "a1"], 1)
  expect_equal(D["a3", "a2"], 1)
  expect_equal(D["a1", "a2"], 0)
  expect_equal(sum(diag(D)), 0)
  expect_error(pareto_dominance(list(P), tau = 2), "tau")
})

test_that("Hyper-event dominance and layers", {
  D <- hyperevent_dominance(X, W, tau = 2)
  # a5 only authored w3, which cites w4 without authors, so a5 has no chain
  expect_equal(rownames(D), c("a1", "a2", "a3", "a4"))
  expect_equal(unname(rowSums(D)), c(0, 0, 2, 3))
  expect_equal(hyperevent_dominance(X, W, tau = 2, direction = "dominates"), t(D))

  layers <- dominance_layers(D, reduction = TRUE)
  expect_equal(unname(layers$layer_id), c(1, 1, 2, 3))
  expect_equal(unname(layers$status), c("dominant", "dominant", "dominated", "dominated"))
  expect_equal(unname(layers$net_dominance), c(2, 2, -1, -3))
  expect_equal(layers$reduction["a4", "a1"], 0) # a4 < a3 < a1
  expect_equal(layers$reduction["a4", "a3"], 1)
})

test_that("Hyper-event dominance options", {
  # Proper inclusion is less demanding, and a1 and a2 dominate each other
  P <- hyperevent_dominance(X, W, tau = 2, strict = "proper")
  expect_equal(P["a1", "a2"], 1)
  expect_equal(P["a2", "a1"], 1)
  expect_warning(dominance_layers(P), "cycles")

  expect_equal(unname(rowSums(hyperevent_dominance(X, W, tau = 1, dimensions = "cited_authors"))), c(0, 0, 0, 3))
  expect_error(hyperevent_dominance(X, W, tau = 2, dimensions = "cited_authors"), "tau")
  expect_error(hyperevent_dominance(X, W, closed = TRUE), "closed")

  # Excluding w2 (three authors) leaves a4 without hyper-events
  M <- hyperevent_dominance(X, W, tau = 2, max_authors = 2, team_size = c(1, 3, 1, 1))
  expect_equal(rownames(M), c("a1", "a2", "a3"))
  expect_equal(sum(M), 0)
})

test_that("Layers are incomplete for cyclic relations", {
  expect_warning(layers <- dominance_layers(neigh_inclusion(S)), "cycles")
  expect_equal(layers$layers[[1]], "a")
  expect_true(all(is.na(layers$layer_id[-1])))
})

test_that("hyperevent_dominance closes the cited papers with the citing or all the papers", {
  # a1 wrote p1, which cites p3 of a3; a2 wrote p2, which cites p4 of a1.
  # p4 cites nothing, so it is a paper of a1 but not one of its citing papers
  X <- matrix(0, 3, 4, dimnames = list(c("a1", "a2", "a3"), c("p1", "p2", "p3", "p4")))
  X["a1", "p1"] <- X["a2", "p2"] <- 1
  Xb <- X * 0
  Xb["a3", "p3"] <- Xb["a1", "p4"] <- 1
  W <- matrix(0, 4, 4, dimnames = list(colnames(X), colnames(X)))
  W["p1", "p3"] <- W["p2", "p4"] <- 1
  # Cited authors: a2 cites a1, which is in the closed neighbourhood of a1
  citing <- hyperevent_dominance(X, W, Xb, tau = 2)
  expect_equal(sum(citing), 0)
  # With every paper of a1, the paper p4 cited by a2 is also in the closure
  authored <- hyperevent_dominance(X, W, Xb, tau = 2, closure_papers = "authored")
  expect_equal(authored["a2", "a1"], 1)
  expect_equal(sum(authored), 1)
})
