context("Test simplicial complexes")

# A clique of four nodes, a pendant node e and an isolated node f
G <- matrix(0, 6, 6, dimnames = list(letters[1:6], letters[1:6]))
G[1:4, 1:4] <- 1
G["d", "e"] <- G["e", "d"] <- 1
diag(G) <- 0

test_that("the clique complex has one simplex for each maximal clique", {
  S <- simplicial_complexes(G)
  expect_equal(colnames(S), c("a-b-c-d", "d-e", "f"))
  expect_equal(unname(colSums(S)), c(4, 2, 1))
  expect_equal(rownames(S), letters[1:6])
  # Without the isolated nodes
  expect_equal(colnames(simplicial_complexes(G, zero_simplex = FALSE)), c("a-b-c-d", "d-e"))
})

test_that("simplicial_complexes works without triangles and without ties", {
  H <- matrix(0, 3, 3, dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  expect_equal(colnames(simplicial_complexes(H)), c("a", "b", "c"))
  expect_equal(dim(simplicial_complexes(H, zero_simplex = FALSE)), c(3, 0))
  H["a", "b"] <- H["b", "a"] <- 1
  expect_equal(colnames(simplicial_complexes(H)), c("a-b", "c"))
})

test_that("the simplices are the maximal cliques of clique_max()", {
  set.seed(9)
  for (r in 1:10) {
    n <- sample(6:12, 1)
    U <- matrix(rbinom(n * n, 1, 0.4), n)
    U[lower.tri(U)] <- t(U)[lower.tri(U)]
    diag(U) <- 0
    dimnames(U) <- list(paste0("v", 1:n), paste0("v", 1:n))
    S <- simplicial_complexes(U, zero_simplex = FALSE)
    cliques <- sapply(clique_max(U), paste, collapse = "-")
    expect_setequal(colnames(S), cliques)
  }
})

test_that("the neighbourhood complex uses the out-neighbours", {
  # a -> b, a -> c, b -> c
  D <- matrix(0, 3, 3, dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  D["a", c("b", "c")] <- 1
  D["b", "c"] <- 1
  open <- simplicial_complexes(D, complex = "neighbourhood")
  expect_equal(colnames(open), c("N(a)", "N(b)"))
  expect_equal(unname(open[, "N(a)"]), c(0, 1, 1))
  closed <- simplicial_complexes(D, complex = "neighbourhood", closed = TRUE)
  expect_equal(colnames(closed), c("N[a]", "N[b]", "N[c]"))
  expect_equal(unname(colSums(closed)), c(3, 2, 1))
})

test_that("the projections link simplices and nodes", {
  P <- simplicial_complexes(G, projection = TRUE, valued = TRUE)
  # The two cliques share d, and f shares nothing
  expect_equal(P$projection1["a-b-c-d", "d-e"], 1)
  expect_equal(sum(P$projection1["f", ]), 0)
  # d shares one simplex with a and one with e
  expect_equal(P$projection2["d", c("a", "e")], c(a = 1, e = 1))
  expect_equal(unname(diag(P$projection2)), rep(0, 6))
  binary <- simplicial_complexes(G, projection = TRUE)
  expect_true(all(binary$projection2 %in% c(0, 1)))
})

test_that("q_analysis uses the complex of simplicial_complexes()", {
  expect_equal(q_analysis(G)$simplices, t(simplicial_complexes(G)))
  expect_equal(
    q_analysis(G, complex = "neighbourhood", closed = TRUE)$simplices,
    t(simplicial_complexes(G, complex = "neighbourhood", closed = TRUE))
  )
})
