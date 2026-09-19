# ============================================================
# 01_centrality.R
# Author: Alejandro Espinosa-Rada, Instituto de Sociología,
#         P. Universidad Católica de Chile
# Last update: 2026-09-18
#
# Compares the centrality and distance functions of netmem with
# igraph on random networks. Every row of the printed table should
# show as many agreements as comparisons.
#
# Output: printed table of agreements
# ============================================================

rm(list = ls())

library(here)
library(igraph)

pkgload::load_all(here::here(), quiet = TRUE)

set.seed(12)
checks <- list()
same <- function(a, b, tol = 1e-6) {
  isTRUE(all.equal(unname(as.numeric(a)), unname(as.numeric(b)), tolerance = tol))
}

for (r in 1:25) {
  n <- sample(6:18, 1)
  U <- 1 * (matrix(runif(n * n), n) < 0.35)
  U[lower.tri(U)] <- t(U)[lower.tri(U)]
  diag(U) <- 0
  D <- 1 * (matrix(runif(n * n), n) < 0.25)
  diag(D) <- 0
  W <- D * matrix(sample(2:9, n * n, TRUE), n)
  dimnames(U) <- dimnames(D) <- dimnames(W) <- list(paste0("v", 1:n), paste0("v", 1:n))

  gu <- graph_from_adjacency_matrix(U, "undirected")
  gd <- graph_from_adjacency_matrix(D, "directed")
  gw <- graph_from_adjacency_matrix(W, "directed", weighted = TRUE)

  checks$betweenness <- c(checks$betweenness, same(betweenness_centrality(U, digraph = FALSE), betweenness(gu)))
  checks$betweenness_directed <- c(checks$betweenness_directed, same(betweenness_centrality(D), betweenness(gd)))
  checks$betweenness_weighted <- c(
    checks$betweenness_weighted,
    same(betweenness_centrality(W, weighted = TRUE), betweenness(gw, weights = 1 / E(gw)$weight))
  )
  checks$closeness <- c(checks$closeness, same(suppressWarnings(closeness_centrality(U, digraph = FALSE)), closeness(gu)))
  checks$closeness_in <- c(checks$closeness_in, same(suppressWarnings(closeness_centrality(D, type = "in")), closeness(gd, mode = "in")))
  checks$harmonic <- c(checks$harmonic, same(closeness_centrality(U, digraph = FALSE, harmonic = TRUE), harmonic_centrality(gu)))
  if (is_connected(gu)) {
    checks$eigenvector <- c(checks$eigenvector, same(eigenvector_centrality(U, digraph = FALSE)$vector, eigen_centrality(gu)$vector))
    checks$centralization_closeness <- c(
      checks$centralization_closeness,
      same(centrality_centralization(U, "closeness", digraph = FALSE)$centralization, centr_clo(gu)$centralization)
    )
    checks$centralization_eigen <- c(
      checks$centralization_eigen,
      same(centrality_centralization(U, "eigenvector", digraph = FALSE)$centralization, centr_eigen(gu)$centralization)
    )
  }
  checks$katz <- c(checks$katz, same(katz_centrality(U, alpha = 0.1, digraph = FALSE), alpha_centrality(gu, alpha = 0.1)))
  checks$bonacich_power <- c(
    checks$bonacich_power,
    same(bonacich_power(U, beta = 0.1, digraph = FALSE, scale = "ssq"), power_centrality(gu, exponent = 0.1, rescale = FALSE))
  )
  checks$page_rank <- c(checks$page_rank, same(page_rank_centrality(D), page_rank(gd)$vector, tol = 1e-5))
  checks$centralization_degree <- c(
    checks$centralization_degree,
    same(centrality_centralization(U, "degree", digraph = FALSE)$centralization, centr_degree(gu, loops = FALSE)$centralization)
  )
  checks$centralization_betweenness <- c(
    checks$centralization_betweenness,
    same(centrality_centralization(U, "betweenness", digraph = FALSE)$centralization, centr_betw(gu)$centralization)
  )
  checks$distances <- c(checks$distances, same(geo_distances(D), distances(gd, mode = "out")))
  checks$diameter <- c(checks$diameter, same(geo_summary(U, digraph = FALSE)$diameter, diameter(gu, unconnected = TRUE)))
  checks$average_distance <- c(checks$average_distance, same(geo_summary(U, digraph = FALSE)$average_distance, mean_distance(gu)))
}

data.frame(
  check = names(checks),
  agreements = sapply(checks, sum),
  comparisons = sapply(checks, length),
  reference = "igraph",
  row.names = NULL
)
