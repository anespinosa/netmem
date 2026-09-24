context("Test Q-analysis")

# Freeman (1980): 29 researchers (simplices) and 19 linking events (vertices),
# given by the positions of the ones of the incidence matrix
freeman <- matrix(0, 29, 19, dimnames = list(1:29, letters[1:19]))
freeman[cbind(
  c(
    2, 2, 2, 3, 3, 3, 3, 4, 4, 5, 6, 8, 9, 9, 10, 11, 11, 12, 13, 13, 13, 13, 14, 15, 15, 16, 17, 18,
    19, 19, 19, 20, 20, 20, 21, 21, 21, 23, 24, 25, 25, 26, 28, 28, 29
  ),
  c(
    12, 13, 14, 11, 13, 15, 16, 3, 4, 17, 12, 16, 6, 7, 15, 7, 19, 18, 2, 8, 9, 19, 5, 1, 8, 1, 14, 17,
    8, 9, 10, 2, 3, 6, 8, 9, 11, 18, 10, 4, 5, 9, 12, 13, 16
  )
)] <- 1

test_that("q_analysis reproduces the counts of Freeman (1980)", {
  Q <- q_analysis(freeman, simplicial_complex = TRUE)
  expect_equal(Q$q_table$q, 3:0)
  # 25 of the 29 researchers took part in an event: two 3-simplices, four
  # 2-simplices, six 1-simplices and thirteen 0-simplices
  expect_equal(Q$q_table$n, c(2, 6, 12, 25))
  expect_equal(Q$q_table$Q, c(2, 6, 9, 3))
  expect_equal(Q$q_table$Qbar, 1 - c(2, 6, 9, 3) / c(2, 6, 12, 25))
  expect_equal(Q$q_table$obstruction, c(1, 5, 8, 2))
  expect_equal(Q$components$q3$simplex, c("3", "13"))
})

test_that("q_analysis reports every q, also those without a simplex of that dimension", {
  # s1 and s2 are 4-simplices that share a face of dimension 2; s3 is an edge
  X <- matrix(0, 3, 10, dimnames = list(c("s1", "s2", "s3"), letters[1:10]))
  X["s1", c("a", "b", "c", "d", "e")] <- 1
  X["s2", c("c", "d", "e", "f", "g")] <- 1
  X["s3", c("i", "j")] <- 1
  Q <- q_analysis(X, simplicial_complex = TRUE)
  expect_equal(Q$q_table$q, 4:0)
  expect_equal(Q$q_table$Q, c(2, 2, 1, 2, 2))
  expect_equal(Q$q_table$n, c(2, 2, 2, 3, 3))
  # Atkin: (4 - 2) / (2 + 1) for s1 and s2, infinite for s3
  expect_equal(Q$eccentricity$eccentricity, c(2 / 3, 2 / 3, Inf))
  # Johnson: 2 of the 5 vertices of s1 are not in s2
  johnson <- q_analysis(X, simplicial_complex = TRUE, eccentricity = "johnson")
  expect_equal(johnson$eccentricity$eccentricity, c(0.4, 0.4, 1))
})

test_that("q_analysis accepts square incidence matrices", {
  Z <- matrix(c(
    1, 1, 0,
    0, 1, 1,
    1, 0, 1
  ), byrow = TRUE, ncol = 3, dimnames = list(c("s1", "s2", "s3"), c("a", "b", "c")))
  expect_equal(q_analysis(Z, simplicial_complex = TRUE)$q_table$Q, c(3, 1))
})

test_that("q_analysis builds the clique and the neighbourhood complex of a network", {
  # A clique of four nodes and a pendant node
  G <- matrix(0, 5, 5, dimnames = list(letters[1:5], letters[1:5]))
  G[1:4, 1:4] <- 1
  G["d", "e"] <- G["e", "d"] <- 1
  diag(G) <- 0
  clique <- q_analysis(G)
  expect_equal(unname(rowSums(clique$simplices)), c(4, 2))
  expect_equal(clique$q_table$Q, c(1, 1, 2, 1))
  # Open neighbourhoods: a, b and c share two vertices, and d has four
  expect_equal(q_analysis(G, complex = "neighbourhood")$q_table$Q, c(1, 4, 1, 1))
  # Closed neighbourhoods: a, b, c and d share the four nodes of the clique
  expect_equal(q_analysis(G, complex = "neighbourhood", closed = TRUE)$q_table$Q, c(1, 1, 1, 1, 1))
  # An isolated node is a simplex of dimension 0 of the clique complex
  H <- matrix(0, 3, 3, dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  H["a", "b"] <- H["b", "a"] <- 1
  expect_equal(q_analysis(H)$q_table$Q, c(1, 2))
})

test_that("q_analysis agrees with the Python package of Smirnov et al. (2025)", {
  # Values computed with q-analysis (github.com/pakrentos/q-analysis) and
  # hard-coded: FSV, SSV, TSV and family eccentricity of this complex
  X <- matrix(c(
    1, 1, 1, 0, 0, 0,
    0, 1, 1, 1, 0, 0,
    0, 0, 0, 1, 1, 0,
    0, 0, 0, 0, 1, 1,
    1, 0, 0, 0, 0, 0
  ), byrow = TRUE, ncol = 6, dimnames = list(paste0("s", 1:5), paste0("v", 1:6)))
  Q <- q_analysis(X, simplicial_complex = TRUE, eccentricity = "johnson")
  expect_equal(rev(Q$q_table$Q), c(1, 3, 2))
  expect_equal(rev(Q$q_table$n), c(5, 4, 2))
  expect_equal(rev(Q$q_table$Qbar), c(0.8, 0.25, 0))
  expect_equal(Q$eccentricity$eccentricity, c(1 / 3, 1 / 3, 0.5, 0.5, 0))
})
