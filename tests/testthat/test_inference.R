context("Test conditional uniform graphs and QAP")

set.seed(18051889)
A <- 1 * (matrix(runif(400), 20) < 0.2)
diag(A) <- 0
B <- 1 * (matrix(runif(400), 20) < 0.2)
diag(B) <- 0

test_that("The random networks keep what is conditioned", {
  edges <- replicate(20, sum(netmem:::rand_cug(A, "edges", TRUE)))
  expect_true(all(edges == sum(A)))

  census <- replicate(20, paste(dyadic_census(netmem:::rand_cug(A, "dyad", TRUE)), collapse = "|"))
  expect_true(all(census == paste(dyadic_census(A), collapse = "|")))

  U <- pmax(A, t(A))
  random <- netmem:::rand_cug(U, "edges", FALSE)
  expect_true(isSymmetric(random))
  expect_equal(sum(random), sum(U))
})

test_that("CUG test of the density conditioned on the number of ties", {
  # Conditioning on the ties, the density is always the observed one
  res <- cug_test(A, FUN = function(x) sum(x) / (nrow(x) * (nrow(x) - 1)), cmode = "edges", reps = 50)
  expect_equal(res$observed, res$mean)
  expect_equal(res$sd, 0)
  expect_equal(res$p_greater, 1)

  # Conditioning only on the size, the expected density is one half, which is
  # higher than the observed density of 0.2
  res <- cug_test(A, FUN = function(x) sum(x) / (nrow(x) * (nrow(x) - 1)), cmode = "size", reps = 200)
  expect_equal(res$mean, 0.5, tolerance = 0.05)
  expect_equal(res$p_greater, 1)
  expect_equal(res$p_lower, 0)
  expect_error(cug_test(A, FUN = function(x) x, reps = 10), "single number")
})

test_that("QAP correlation", {
  # A matrix correlates perfectly with itself, and no permutation reaches that
  same <- qap_cor(A, A, reps = 100)
  expect_equal(same$correlation, 1)
  expect_lt(same$p_greater, 0.05)

  res <- qap_cor(A, B, reps = 100)
  expect_equal(res$correlation, cor(A[row(A) != col(A)], B[row(B) != col(B)]))
  expect_true(res$p_two_sided >= 0 & res$p_two_sided <= 1)
})

test_that("QAP regression coefficients are the ones of the regression of the ties", {
  Y <- 0.5 * A + 0.3 * B + matrix(rnorm(400, 0, 0.2), 20)
  diag(Y) <- 0
  cells <- row(Y) != col(Y)
  expected <- unname(coef(lm(Y[cells] ~ A[cells] + B[cells])))

  res <- qap_lm(Y, list(a = A, b = B), reps = 50, method = "y")
  expect_equal(unname(res$coefficients$coefficient), expected)
  expect_equal(rownames(res$coefficients), c("intercept", "a", "b"))
  expect_equal(res$fit, summary(lm(Y[cells] ~ A[cells] + B[cells]))$r.squared)

  # The intercept is not permuted by the double semi-partialling
  res <- qap_lm(Y, list(a = A, b = B), reps = 50, method = "dsp")
  expect_true(is.na(res$coefficients["intercept", "p_two_sided"]))
  expect_false(is.na(res$coefficients["a", "p_two_sided"]))

  Yb <- 1 * (Y > 0.5)
  diag(Yb) <- 0
  logit <- qap_lm(Yb, list(a = A, b = B), reps = 50, family = "binomial", method = "y")
  expect_equal(unname(logit$coefficients$coefficient),
               unname(coef(glm(Yb[cells] ~ A[cells] + B[cells], family = binomial()))))
})
