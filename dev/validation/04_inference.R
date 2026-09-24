# ============================================================
# 04_inference.R
# Author: Alejandro Espinosa-Rada, Instituto de Sociología,
#         P. Universidad Católica de Chile
# Last update: 2026-09-18
#
# Compares the conditional uniform graph tests and the QAP
# regressions of netmem with sna. The coefficients should be
# identical, while the p-values only agree within the error of
# the simulation.
#
# Output: printed table of agreements and a comparison of p-values
# ============================================================

rm(list = ls())

library(here)
library(sna)

pkgload::load_all(here::here(), quiet = TRUE)

set.seed(77)
checks <- list()

for (r in 1:15) {
  n <- sample(8:15, 1)
  A <- 1 * (matrix(runif(n * n), n) < 0.3)
  diag(A) <- 0

  # The random networks keep what is conditioned
  ties <- replicate(20, sum(netmem:::rand_cug(A, "edges", TRUE)))
  checks$edges_conditioned <- c(checks$edges_conditioned, all(ties == sum(A)))
  census <- replicate(20, paste(dyadic_census(netmem:::rand_cug(A, "dyad", TRUE)), collapse = "|"))
  checks$dyad_census_conditioned <- c(checks$dyad_census_conditioned, all(census == paste(dyadic_census(A), collapse = "|")))

  U <- pmax(A, t(A))
  undirected <- replicate(20, {
    B <- netmem:::rand_cug(U, "edges", FALSE)
    c(sum(B), isSymmetric(B))
  })
  checks$undirected_conditioned <- c(checks$undirected_conditioned, all(undirected[1, ] == sum(U)) && all(undirected[2, ] == 1))

  # The coefficients are the ones of the regression of the ties
  X1 <- 1 * (matrix(runif(n * n), n) < 0.3)
  diag(X1) <- 0
  X2 <- matrix(runif(n * n), n)
  diag(X2) <- 0
  Y <- 0.5 * X1 + 0.3 * X2 + matrix(rnorm(n * n, 0, 0.2), n)
  diag(Y) <- 0

  linear <- qap_lm(Y, list(x1 = X1, x2 = X2), reps = 20, method = "y")
  reference <- sna::netlm(Y, list(X1, X2), reps = 20, nullhyp = "qapy")
  checks$netlm_coefficients <- c(
    checks$netlm_coefficients,
    isTRUE(all.equal(unname(linear$coefficients$coefficient), unname(reference$coefficients)))
  )

  Yb <- 1 * (Y > 0.5)
  diag(Yb) <- 0
  logit <- qap_lm(Yb, list(x1 = X1, x2 = X2), reps = 20, family = "binomial", method = "y")
  reference <- sna::netlogit(Yb, list(X1, X2), reps = 20, nullhyp = "qapy")
  checks$netlogit_coefficients <- c(
    checks$netlogit_coefficients,
    isTRUE(all.equal(unname(logit$coefficients$coefficient), unname(reference$coefficients)))
  )
}

results <- data.frame(
  check = names(checks),
  agreements = sapply(checks, sum),
  comparisons = sapply(checks, length),
  reference = c(rep("invariants", 3), rep("sna", 2)),
  row.names = NULL
)
results

# The p-values of the tests only agree within the error of the simulation
set.seed(3)
A <- 1 * (matrix(runif(400), 20) < 0.2)
diag(A) <- 0
B <- 1 * (matrix(runif(400), 20) < 0.2)
diag(B) <- 0

data.frame(
  test = c("CUG, transitivity", "QAP, correlation"),
  netmem = c(
    cug_test(A, FUN = function(x) sna::gtrans(x), cmode = "edges", reps = 500)$p_greater,
    qap_cor(A, B, reps = 500)$p_greater
  ),
  sna = c(
    sna::cug.test(A, FUN = sna::gtrans, cmode = "edges", reps = 500)$pgteobs,
    sna::qaptest(list(A, B), sna::gcor, g1 = 1, g2 = 2, reps = 500)$pgreq
  )
)
