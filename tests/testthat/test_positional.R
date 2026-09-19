context("Test positional dominance")

# Directed example: a cites b and c, b cites c, c cites d and e, d cites e
A <- matrix(c(
  0, 1, 1, 0, 0,
  0, 0, 1, 0, 0,
  0, 0, 0, 1, 1,
  0, 0, 0, 0, 1,
  0, 0, 0, 0, 0
), byrow = TRUE, ncol = 5)
rownames(A) <- letters[1:5]
colnames(A) <- rownames(A)

test_that("Directed neighbourhood inclusion follows Definition 1 of Marmulla and Brandes", {
  # b sends to c only, and a sends to b and c, so b is dominated by a outwards
  out <- dir_inclusion(A, type = "radial_out")
  expect_equal(out["b", "a"], 1)
  expect_equal(out["a", "b"], 0)

  # d receives from c, and e receives from c and d, so d is dominated by e inwards
  into <- dir_inclusion(A, type = "radial_in")
  expect_equal(into["d", "e"], 1)
  expect_equal(into["e", "d"], 0)

  # The weak relations use the closed neighbourhoods
  expect_equal(dim(dir_inclusion(A, type = "radial_out", strength = "weak")), c(5, 5))
  expect_equal(sum(diag(dir_inclusion(A, type = "medial"))), 0)

  # Hierarchical downwards asks for more sent and less received at the same time
  down <- dir_inclusion(A, type = "hierarchical_down")
  up <- dir_inclusion(A, type = "hierarchical_up")
  expect_true(all(down * t(up) >= 0))
  expect_equal(down, t(up)) # one is the reverse of the other

  expect_equal(dir_inclusion(A, type = "radial_out", direction = "dominates"), t(out))
})

test_that("Indirect relations", {
  U <- pmax(A, t(A))
  D <- indirect_rel(U, type = "distance", digraph = FALSE)
  expect_equal(D["a", "e"], 2) # a - c - e
  expect_equal(unname(diag(D)), rep(0, 5))
  expect_equal(indirect_rel(U, type = "adjacency", digraph = FALSE), U)
  # Shared neighbours: a and b are both tied to c
  expect_equal(indirect_rel(U, type = "shared", digraph = FALSE)["a", "b"], 1)
})

test_that("Positional dominance on distances", {
  U <- pmax(A, t(A))
  D <- indirect_rel(U, type = "distance", digraph = FALSE)
  # With distances, being closer to everybody is better
  P <- pos_dominance(D, benefit = FALSE)
  expect_true(all(diag(P) == 0))
  expect_true(all(P %in% c(0, 1)))
  # Sorting the distances makes the comparison less demanding
  expect_gte(sum(pos_dominance(D, benefit = FALSE, map = TRUE)), sum(P))
})

test_that("Rank intervals", {
  # In a star, the centre is ranked above every leaf, and the leaves are equivalent
  star <- matrix(0, 5, 5)
  star[1, ] <- 1
  star[, 1] <- 1
  diag(star) <- 0
  rownames(star) <- letters[1:5]
  colnames(star) <- rownames(star)

  intervals <- dominance_ranks(neigh_inclusion(star))
  expect_equal(intervals$min_rank[1], 5) # the centre dominates the four leaves
  expect_equal(intervals$max_rank[1], 5)
  expect_equal(intervals$width[1], 0)
  # The leaves are equivalent to each other, so they share the lowest rank
  expect_true(all(intervals$min_rank[2:5] == 1))
  expect_true(all(intervals$max_rank[2:5] == 1))

  # In a path a - b - c the ends are equivalent and the middle is above them
  path <- matrix(0, 3, 3)
  path[1, 2] <- 1
  path[2, 3] <- 1
  path <- pmax(path, t(path))
  rownames(path) <- letters[1:3]
  colnames(path) <- rownames(path)
  intervals <- dominance_ranks(neigh_inclusion(path))
  expect_equal(intervals$min_rank, c(1, 3, 1))
  expect_equal(intervals$max_rank, c(1, 3, 1))
})
