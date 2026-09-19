context("Test random network generators")

test_that("Watts-Strogatz without rewiring is a ring lattice", {
  A <- small_world(30, neighbours = 2, p = 0)
  expect_true(all(rowSums(A) == 4))
  expect_true(isSymmetric(A))
  expect_equal(sum(A) / 2, 60)
  # Transitivity of a ring lattice: 3(k - 2) / (4(k - 1)) with k = 4
  expect_equal(trans_coef(A, method = "global"), 3 * (4 - 2) / (4 * (4 - 1)))

  # Rewiring keeps the number of ties and shortens the distances
  set.seed(18051889)
  B <- small_world(30, neighbours = 2, p = 1)
  expect_equal(sum(B) / 2, sum(A) / 2)
  expect_lt(geo_summary(B, digraph = FALSE)$average_distance,
            geo_summary(A, digraph = FALSE)$average_distance)
  expect_error(small_world(3, neighbours = 2), "larger")
})

test_that("Preferential attachment concentrates the ties in the first nodes", {
  set.seed(18051889)
  A <- pref_attachment(100, m = 2)
  expect_equal(sum(A) / 2, 2 * (100 - 2))
  expect_true(all(diag(A) == 0))
  expect_true(isSymmetric(A))
  # The nodes that arrive first end up with more ties
  expect_lt(cor(1:100, rowSums(A), method = "spearman"), 0)
  # With no preference the degrees are much less concentrated
  set.seed(18051889)
  random <- pref_attachment(100, m = 2, power = 0)
  expect_lt(max(rowSums(random)), max(rowSums(A)))
  expect_error(pref_attachment(2, m = 2), "larger")
})
