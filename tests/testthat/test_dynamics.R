context("Test social influence and diffusion")

A <- matrix(c(
  0, 1, 1, 0, 0, 0,
  1, 0, 1, 0, 0, 0,
  1, 1, 0, 1, 0, 0,
  0, 0, 1, 0, 1, 1,
  0, 0, 0, 1, 0, 1,
  0, 0, 0, 1, 1, 0
), byrow = TRUE, ncol = 6)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)
W <- A / rowSums(A)
opinion <- c(0.1, 0.2, 0.3, 0.7, 0.8, 0.9)

test_that("Assimilation leads to consensus", {
  res <- social_influence(W, opinion, rule = "assimilation", steps = 200)
  expect_equal(var(res$final), 0, tolerance = 1e-8)
  expect_equal(res$groups, 1)
  expect_equal(dim(res$trajectory), c(201, 6))
  expect_equal(unname(res$trajectory[1, ]), opinion)
  # One step by hand: a is influenced by b and c
  one <- social_influence(W, opinion, rule = "assimilation", mu = 0.3, steps = 1)
  expect_equal(unname(one$final[1]), 0.1 + 0.3 * (mean(c(0.2, 0.3)) - 0.1))
})

test_that("Friedkin and Johnsen converges to its fixed point", {
  res <- social_influence(W, opinion, rule = "friedkin", susceptibility = 0.6, steps = 500)
  fixed <- solve(diag(6) - 0.6 * W) %*% ((1 - 0.6) * opinion)
  expect_equal(unname(res$final), as.numeric(fixed))
})

test_that("Bounded confidence only listens to similar others", {
  # Nobody is close enough, so the opinions do not change
  expect_equal(unname(social_influence(W, opinion, rule = "bounded", epsilon = 0, steps = 50)$final), opinion)
  # A wide enough window behaves like assimilation
  wide <- social_influence(W, opinion, rule = "bounded", epsilon = 1, steps = 200)
  expect_equal(wide$groups, 1)
})

test_that("Repulsion keeps the opinions between zero and one", {
  res <- social_influence(W, opinion, rule = "repulsion", steps = 100)
  expect_true(all(res$trajectory >= 0 & res$trajectory <= 1))
})

test_that("Threshold diffusion", {
  # a adopts first, b needs one of its two neighbours, c two of its three
  res <- threshold_diffusion(A, seeds = "a", threshold = 0.5)
  expect_equal(unname(res$time), c(0, 1, 2, NA, NA, NA))
  expect_equal(res$adopters, 0.5)

  expect_equal(threshold_diffusion(A, seeds = "a", threshold = 0)$adopters, 1)
  expect_equal(threshold_diffusion(A, seeds = "a", threshold = 1.1)$adopters, 1 / 6)

  # With counts instead of proportions, one adopting neighbour is enough
  res <- threshold_diffusion(A, seeds = "a", threshold = 1, mode = "count")
  expect_equal(res$adopters, 1)
  expect_error(threshold_diffusion(A, seeds = "z"), "do not match")
})
