context("Test the functions that had no test before the audit")

# Values checked by hand, from the definitions, or computed with the package
# used as reference in dev/validation and hard-coded

test_that("bonacich_norm gives 1 to identical and 0 to disjoint profiles", {
  X <- matrix(c(
    1, 1, 0, 0,
    1, 1, 0, 0,
    0, 0, 1, 1
  ), byrow = TRUE, ncol = 4, dimnames = list(c("a", "b", "c"), NULL))
  B <- bonacich_norm(X)
  expect_equal(B["a", "b"], 1)
  expect_equal(B["a", "c"], 0)
  expect_true(isSymmetric(B))
  expect_equal(unname(diag(B)), c(1, 1, 1))
})

test_that("compound_relation composes the relations", {
  A <- matrix(c(0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0), 4, byrow = TRUE, dimnames = list(letters[1:4], letters[1:4]))
  B <- matrix(c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0), 4, byrow = TRUE, dimnames = list(letters[1:4], letters[1:4]))
  compound <- compound_relation(list(A, B), comp = 2, matrices = TRUE)
  expect_equal(names(compound$compound_matrices), c("a", "b", "ab", "aa", "ba", "bb"))
  expect_equal(compound$compound_matrices$ab, 1 * ((A %*% B) > 0))
  expect_equal(compound$compound_matrices$ba, 1 * ((B %*% A) > 0))
})

test_that("cumulativeSumMatrices adds the matrices one by one", {
  M <- list(diag(2), matrix(1, 2, 2), matrix(2, 2, 2))
  sums <- cumulativeSumMatrices(M)
  expect_length(sums, 3)
  expect_equal(sums[[3]], diag(2) + matrix(3, 2, 2))
})

test_that("dist_geographic gives the haversine distance with the mean radius of the Earth", {
  # Santiago and London; geosphere::distHaversine gives 11685.96 km with the
  # equatorial radius (6378.137 km), and netmem uses the mean radius (6371 km)
  D <- dist_geographic(c(-33.45, 51.50), c(-70.67, -0.12), method = "harvesine", dd_to_radians = TRUE)
  expect_equal(D[1, 2], 11685.96 * 6371 / 6378.137, tolerance = 1e-4)
})

test_that("dyad_triad_table lists the forbidden and closed triads of each node", {
  A <- matrix(c(
    0, 1, 1, 1, 0,
    1, 0, 1, 0, 0,
    1, 1, 0, 0, 0,
    1, 0, 0, 0, 1,
    0, 0, 0, 1, 0
  ), byrow = TRUE, ncol = 5, dimnames = list(letters[1:5], letters[1:5]))
  table <- dyad_triad_table(A)
  # a is the centre of the forbidden triads a-b-d and a-c-d, and of the closed a-b-c
  expect_equal(table$type[table$node == "a"], c("300", "201", "201"))
  # The closed triad has the same number for its three nodes
  expect_equal(length(unique(table$triad[table$members == "a|b|c"])), 1)
  expect_equal(sort(table$node[table$members == "a|b|c"]), c("a", "b", "c"))
  expect_equal(table$type[table$node == "e"], "102")
  # Nodes at the centre of at least two forbidden triads
  expect_equal(unique(dyad_triad_table(A, min = 2)$node), "a")
  expect_equal(unique(dyad_triad_table(A, max = 0)$node), c("b", "c", "e"))
})

test_that("expand_matrix copies the ties of each node to its duplicated labels", {
  E <- matrix(c(0, 1, 1, 0, 0, 1, 1, 0, 0), 3, byrow = TRUE, dimnames = list(letters[1:3], letters[1:3]))
  X <- expand_matrix(E, sort(rep(letters[1:3], 2)))
  expect_equal(dim(X), c(6, 6))
  expect_equal(unname(X[1, ]), c(0, 0, 1, 1, 1, 1))
  expect_equal(unname(X[5, ]), c(1, 1, 0, 0, 0, 0))
})

test_that("fractional_approach follows Batagelj (2020)", {
  Ci <- matrix(c(
    0, 1, 1, 0, 0,
    0, 0, 0, 0, 0,
    0, 0, 0, 0, 0,
    0, 1, 1, 0, 1,
    0, 0, 1, 0, 0
  ), byrow = TRUE, ncol = 5, dimnames = list(paste0("w", 1:5), paste0("w", 1:5)))
  WA <- matrix(c(1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1), 5, byrow = TRUE,
    dimnames = list(rownames(Ci), c("a1", "a2", "a3"))
  )
  # The fractional networks redistribute one unit per citation or per citing work
  expect_equal(sum(fractional_approach(Ci, WA)), sum(Ci))
  expect_equal(sum(fractional_approach(Ci, approach = "cocitation")), 3)
  expect_equal(fractional_approach(Ci, WA, fractional = FALSE), t(WA) %*% Ci %*% WA)
  expect_equal(fractional_approach(Ci, approach = "cocitation", fractional = FALSE), t(Ci) %*% Ci)
  # w1 and w4 share two references; w1 has two and w4 three
  b <- fractional_approach(Ci, approach = "bcoupling")
  expect_equal(c(b["w1", "w4"], b["w4", "w1"]), c(1, 2 / 3))
  expect_equal(fractional_approach(Ci, approach = "bcoupling", symmetric = "geometric")["w1", "w4"], 2 / sqrt(6))
  expect_equal(fractional_approach(Ci, approach = "bcoupling", symmetric = "harmonic")["w1", "w4"], 4 / 5)
  expect_equal(fractional_approach(Ci, approach = "bcoupling", symmetric = "jaccard")["w1", "w4"], 2 / 3)
  expect_equal(fractional_approach(Ci, approach = "bcoupling", symmetric = "minimum")["w1", "w4"], 2 / 3)
  expect_error(fractional_approach(Ci), "authorship")
})

test_that("heterogeneity gives Blau's index and the IQV", {
  att <- c(1, 1, 2, 3)
  expect_equal(unname(unlist(heterogeneity(att)))[1], 1 - (0.5^2 + 0.25^2 + 0.25^2))
  expect_equal(heterogeneity(att, normalized = TRUE)$iqv, (1 - (0.5^2 + 0.25^2 + 0.25^2)) / (1 - 1 / 3))
})

test_that("hypergraph lists the events of each node and the nodes of each event", {
  X <- matrix(c(1, 0, 1, 1, 0, 1), 3, byrow = TRUE, dimnames = list(c("x", "y", "z"), c("e1", "e2")))
  H <- hypergraph(X)
  expect_equal(H$hypergraph$y, c("e1", "e2"))
  expect_equal(H$dual_hypergraph$e2, c("y", "z"))
})

test_that("matrix_projection multiplies the incidence matrix by its transpose", {
  X <- matrix(c(1, 0, 1, 1, 0, 1), 3, byrow = TRUE)
  P <- matrix_projection(X)
  expect_equal(P$matrix1, t(X) %*% X)
  expect_equal(P$matrix2, X %*% t(X))
})

test_that("matrix_report counts the nodes and the ties", {
  U <- matrix(c(0, 1, 1, 1, 0, 0, 1, 0, 0), 3)
  report <- NULL
  invisible(capture.output(report <- matrix_report(U)))
  expect_equal(unname(report[1, ]), c(3, 2))
  X <- matrix(c(1, 0, 1, 1, 0, 1), 3, byrow = TRUE)
  invisible(capture.output(report <- matrix_report(X)))
  expect_equal(unname(report[1, ]), c(2, 3, 4))
})

test_that("perm_matrix and perm_label give permutations", {
  set.seed(1)
  P <- perm_matrix(5)[[1]]
  expect_equal(unname(rowSums(P)), rep(1, 5))
  expect_equal(unname(colSums(P)), rep(1, 5))
  A <- matrix(0, 4, 4, dimnames = list(letters[1:4], letters[1:4]))
  expect_setequal(perm_label(A)[1, ], letters[1:4])
})

test_that("recip_coef agrees with igraph::reciprocity", {
  # igraph 2.2.1: reciprocity(mode = "ratio") = 0.5 and reciprocity() = 2/3
  D <- matrix(c(0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0), 4, byrow = TRUE)
  expect_equal(unname(recip_coef(D, method = "ratio_nonnull")), 0.5)
  expect_equal(unname(recip_coef(D, method = "global")), 2 / 3)
})

test_that("spatial_cor agrees with ape::Moran.I", {
  # ape 5.8: Moran.I(v, A)$observed = 0.5
  A <- matrix(c(0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0), 5, byrow = TRUE)
  expect_equal(unname(spatial_cor(A, 1:5, measures = "moran", rowstand = TRUE)), 0.5)
})

test_that("structural_na adds the unobserved nodes as missing", {
  N <- matrix(c(0, 1, 1, 1, 0, 1, 0, 0, 0), 3, byrow = TRUE, dimnames = list(c("A", "C", "D"), c("A", "C", "D")))
  expect_silent(X <- structural_na(N, label = c("A", "B", "C", "D", "E")))
  # A node of the matrix that is not in the labels is dropped with a warning
  expect_warning(structural_na(N, label = c("A", "C")), "dropped")
  expect_equal(dim(X), c(5, 5))
  expect_true(all(is.na(X["B", ])))
  expect_equal(X["C", "D"], 1)
})

test_that("ego_net asks for ego", {
  A <- matrix(c(0, 1, 1, 0), 2, dimnames = list(c("a", "b"), c("a", "b")))
  expect_error(ego_net(A), "Provide the name of ego")
  expect_error(eb_constraint(A), "Provide the name of ego")
})

test_that("z_arctest gives the Z test of the number of arcs", {
  # Wasserman and Faust (1994): z = (L - g(g - 1)p) / sqrt(g(g - 1)p(1 - p))
  data(krackhardt_friends)
  G <- krackhardt_friends
  g <- nrow(G)
  L <- sum(G > 0) - sum(diag(G) > 0)
  z <- (L - g * (g - 1) * 0.5) / sqrt(g * (g - 1) * 0.25)
  expect_equal(unname(z_arctest(G)["z"]), round(z, 3))
})
