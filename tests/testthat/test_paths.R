context("Test path distances")

# Expected values were checked with sna::geodist and igraph 2.2.1

test_that("count_geodesics adds the geodesics of every parent", {
  A <- matrix(c(
    0, 1, 1, 0,
    0, 0, 0, 1,
    0, 0, 0, 1,
    0, 0, 0, 0
  ), byrow = TRUE, ncol = 4)
  expect_equal(count_geodesics(A)$counts[1, 4], 2) # a -> b -> d and a -> c -> d
  expect_equal(count_geodesics(A)$distances[1, 4], 2)
})

test_that("Weighted shortest paths and all the pairs", {
  A <- matrix(c(
    0, 3, 3, 10, 15, 0, 0, 0,
    1, 0, 5, 2, 7, 0, 0, 0,
    3, 5, 0, 0, 0, 0, 0, 0,
    10, 2, 0, 0, 2, 7, 12, 0,
    11, 3, 0, 3, 0, 11, 2, 0,
    0, 0, 0, 7, 11, 0, 3, 2,
    0, 0, 0, 12, 2, 3, 0, 2,
    0, 0, 0, 0, 0, 2, 2, 0
  ), byrow = TRUE, ncol = 8)
  rownames(A) <- c("a", "b", "s", "c", "d", "e", "f", "z")
  colnames(A) <- rownames(A)

  expect_equal(wlocal_distances(A, from = "a", to = "z")$path, c("a", "b", "c", "d", "f", "z"))
  paths <- wall_distances(A, select = "all")
  expect_equal(length(paths$fromTo), 8)
  expect_equal(length(paths$fromTo$a), 8)
  expect_equal(paths$fromTo$a$z, c("a", "b", "c", "d", "f", "z"))
  expect_equal(paths$toFrom$z$a, c("a", "b", "c", "d", "f", "z"))
})

test_that("short_path without a path", {
  A <- matrix(c(
    0, 1,
    0, 0
  ), byrow = TRUE, ncol = 2)
  rownames(A) <- c("a", "b")
  colnames(A) <- rownames(A)
  expect_warning(path <- short_path(A, from = "b", to = "a"), "no path")
  expect_null(path)
})

test_that("power_function for large powers", {
  A <- matrix(c(
    1, 0, 0, 0,
    1, 1, 0, 0,
    1, 0, 1, 0,
    0, 1, 1, 1
  ), byrow = TRUE, ncol = 4)
  expect_equal(power_function(A, 1000)[4, 1], 999000)
  expect_equal(power_function(A, 1), A)
})
