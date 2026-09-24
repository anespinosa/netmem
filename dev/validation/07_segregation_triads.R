# ============================================================
# 07_segregation_triads.R
# Author: Alejandro Espinosa-Rada, Instituto de Sociología,
#         P. Universidad Católica de Chile
# Last update: 2026-09-18
#
# Compares segregation() with netseg, and triad_uman() with the
# table of Wasserman and Faust (1994: 582-583) and with networks
# drawn from the U|MAN distribution.
#
# Output: printed tables of agreements and of the covariances
# ============================================================

rm(list = ls())

library(here)
library(igraph)
library(netseg)
library(sna)

pkgload::load_all(here::here(), quiet = TRUE)

set.seed(91)
checks <- list()
same <- function(a, b) isTRUE(all.equal(as.numeric(a), as.numeric(b)))

for (r in 1:25) {
  n <- sample(10:25, 1)
  att <- sample(letters[1:sample(2:3, 1)], n, replace = TRUE)
  if (length(unique(att)) < 2) next
  U <- 1 * (matrix(runif(n * n), n) < 0.25)
  U[lower.tri(U)] <- t(U)[lower.tri(U)]
  diag(U) <- 0
  g <- graph_from_adjacency_matrix(U, "undirected")
  V(g)$att <- att

  checks$assortativity <- c(checks$assortativity, same(segregation(U, att), netseg::assort(g, "att")))
  if (all(tapply(rowSums(U), att, sum) > 0)) {
    checks$gupta_anderson_may <- c(checks$gupta_anderson_may, same(segregation(U, att, "gam"), netseg::gamix(g, "att")))
  }
  checks$odds_ratio <- c(checks$odds_ratio, same(segregation(U, att, "orwg"), netseg::orwg(g, "att")))
  if (length(unique(att)) == 2) {
    checks$freeman <- c(checks$freeman, same(segregation(U, att, "freeman"), netseg::freeman(g, "att")))
  }

  D <- 1 * (matrix(runif(n * n), n) < 0.2)
  diag(D) <- 0
  gd <- graph_from_adjacency_matrix(D, "directed")
  V(gd)$att <- att
  checks$coleman <- c(
    checks$coleman,
    same(unname(segregation(D, att, "coleman", digraph = TRUE)), unname(netseg::coleman(gd, "att")))
  )
}

data.frame(
  check = names(checks),
  agreements = sapply(checks, sum),
  comparisons = sapply(checks, length),
  reference = "netseg",
  row.names = NULL
)

#### Triad census under U|MAN ####

data(krackhardt_friends)
observed <- suppressWarnings(triad_uman(krackhardt_friends, ztest = TRUE, covar = TRUE))

# Table 14.3 of Wasserman and Faust (1994: 582)
book <- data.frame(
  label = c("003", "021D", "021U", "021C", "111D", "111U", "030T", "030C"),
  expected = c(320.06, 44.09, 44.09, 88.17, 73.74, 73.74, 18.17, 6.06),
  sd = c(9.39, 6.22, 6.22, 8.17, 7.78, 7.78, 3.86, 2.39)
)
book$netmem_expected <- round(observed$results$EXP[match(book$label, observed$results$label)], 2)
book$netmem_sd <- round(observed$results$STD[match(book$label, observed$results$label)], 2)
book

# The covariance matrix should add up to zero, as the total number of triads is fixed
cat("sum of the covariance matrix:", round(sum(observed$covariance), 4), "\n")

# The covariances that the issue reported, against networks drawn from U|MAN
A <- as.matrix(krackhardt_friends)
diag(A) <- 0
g <- nrow(A)
mutual <- sum(A * t(A)) / 2
asymmetric <- sum(A) - 2 * mutual
null <- g * (g - 1) / 2 - mutual - asymmetric
pairs <- t(combn(g, 2))

set.seed(2026)
reps <- 20000
census <- matrix(0, reps, 16)
for (r in 1:reps) {
  state <- sample(rep(c("m", "a", "n"), times = c(mutual, asymmetric, null)))
  B <- matrix(0, g, g)
  both <- pairs[state == "m", , drop = FALSE]
  B[both] <- 1
  B[both[, c(2, 1), drop = FALSE]] <- 1
  single <- pairs[state == "a", , drop = FALSE]
  flip <- runif(nrow(single)) < 0.5
  single[flip, ] <- single[flip, c(2, 1)]
  B[single] <- 1
  census[r, ] <- sna::triad.census(B)
}
colnames(census) <- observed$results$label
simulated <- cov(census)

data.frame(
  pair = c("201 - 102", "300 - 030T"),
  book = c(-18.4, -0.01),
  netmem = c(observed$covariance["201", "102"], observed$covariance["300", "030T"]),
  simulated = c(simulated["201", "102"], simulated["300", "030T"]),
  standard_error = c(
    sd((census[, "201"] - mean(census[, "201"])) * (census[, "102"] - mean(census[, "102"]))) / sqrt(reps),
    sd((census[, "300"] - mean(census[, "300"])) * (census[, "030T"] - mean(census[, "030T"]))) / sqrt(reps)
  )
)
