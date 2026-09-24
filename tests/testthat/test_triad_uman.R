context("Test triad census under U|MAN")

# Krackhardt's friendship relation, as reported by Wasserman and Faust (1994),
# Table 14.3 (expected values and standard deviations) and Table 14.4 (covariances)

data(krackhardt_friends)
res <- suppressWarnings(triad_uman(krackhardt_friends, ztest = TRUE, covar = TRUE))
cv <- res$covariance

test_that("Expected values and standard deviations of Wasserman and Faust (1994: 582)", {
  expect_equal(res$results$OBS[c(1, 4, 5, 6, 7, 8, 9, 10)], c(376, 114, 34, 35, 39, 101, 23, 0))
  expect_equal(res$results$EXP[c(1, 4, 5, 6, 7, 8, 9, 10)],
               c(320.06, 44.09, 44.09, 88.17, 73.74, 73.74, 18.17, 6.06), tolerance = 1e-3)
  expect_equal(res$results$STD[c(1, 4, 5, 6, 7, 8, 9, 10)],
               c(9.39, 6.22, 6.22, 8.17, 7.78, 7.78, 3.86, 2.39), tolerance = 1e-3)
})

test_that("Covariances of Wasserman and Faust (1994: 583)", {
  expect_equal(unname(cv["111D", c("003", "012", "102", "021D", "021U", "021C")]),
               c(7.87, -8.65, -19.6, -2.09, -2.09, -4.18), tolerance = 1e-2)
  expect_equal(unname(cv["111U", "111D"]), -13.3, tolerance = 1e-2)
  expect_equal(unname(cv["030T", c("003", "012", "102", "021D", "021U", "021C", "111D", "111U")]),
               c(6.65, -5.68, 3.56, -3.57, -3.57, -7.14, -1.01, -1.01), tolerance = 1e-2)
  expect_equal(unname(cv["030C", c("003", "012", "102", "021D")]), c(2.22, -1.89, 1.19, -1.19), tolerance = 1e-2)
  # This covariance was positive before: the p2 term used (m + n + 4) instead of (m + n - 4)
  expect_equal(unname(cv["201", c("003", "012", "102", "021D", "111D")]),
               c(3.09, 7.29, -18.4, 1.12, -6.02), tolerance = 1e-2)
})

test_that("The covariance matrix is symmetric and the total number of triads is fixed", {
  expect_true(isSymmetric(cv))
  # Var(sum of the sixteen counts) = 0, as the sum is always choose(g, 3)
  expect_equal(sum(cv), 0, tolerance = 1e-2)
  expect_equal(sum(res$results$OBS), choose(nrow(krackhardt_friends), 3))
})

test_that("Test of a linear combination of the census", {
  # 030T, 120D, 120U and 300 are the transitive triads
  l <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1)
  lin <- suppressWarnings(triad_uman(krackhardt_friends, l = l))$z_test
  expect_equal(unname(lin["observed"]), 70)
  expect_equal(unname(lin["expected"]), 35.201, tolerance = 1e-3)
  expect_equal(unname(lin["z"]), 6.613, tolerance = 1e-3)
  expect_error(suppressWarnings(triad_uman(krackhardt_friends, l = c(1, 0))), "sixteen")
})

test_that("The triad census of an empty network is all empty triads", {
  empty <- matrix(0, 4, 4, dimnames = list(letters[1:4], letters[1:4]))
  expect_equal(unname(triad_uman(empty)$OBS[1]), choose(4, 3))
  expect_equal(sum(triad_uman(empty)$OBS[-1]), 0)
})
