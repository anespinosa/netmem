# ============================================================
# 08_existing_functions.R
# Author: Alejandro Espinosa-Rada, Instituto de Sociología,
#         P. Universidad Católica de Chile
# Last update: 2026-09-18
#
# Compares the functions that already existed in netmem with the
# packages that implement the same measures, and with formulas that
# can be checked by hand. Several bugs of version 1.0-3 were found
# with these comparisons.
#
# Output: printed table of agreements
# ============================================================

rm(list = ls())

library(here)
library(igraph)
library(sna)
library(ergm)
library(network)
library(netseg)
library(signnet)
library(ape)
library(geosphere)

pkgload::load_all(here::here(), quiet = TRUE)

set.seed(20)
checks <- list()
same <- function(a, b, tol = 1e-6) {
  isTRUE(all.equal(unname(as.numeric(a)), unname(as.numeric(b)), tolerance = tol))
}

for (r in 1:15) {
  n <- sample(8:16, 1)
  U <- 1 * (matrix(runif(n * n), n) < 0.3)
  U[lower.tri(U)] <- t(U)[lower.tri(U)]
  diag(U) <- 0
  D <- 1 * (matrix(runif(n * n), n) < 0.25)
  diag(D) <- 0
  dimnames(U) <- dimnames(D) <- list(letters[1:n], letters[1:n])
  att <- sample(c("x", "y"), n, TRUE)
  gu <- graph_from_adjacency_matrix(U, "undirected")
  gd <- graph_from_adjacency_matrix(D, "directed")

  checks$gen_density <- c(checks$gen_density, same(gen_density(D), igraph::edge_density(gd)))
  checks$gen_degree <- c(checks$gen_degree, same(gen_degree(D, type = "all"), igraph::degree(gd, mode = "all")))
  checks$k_core <- c(checks$k_core, same(k_core(U), igraph::coreness(gu)))
  checks$recip_coef <- c(checks$recip_coef, same(recip_coef(D, method = "global"), igraph::reciprocity(gd)))
  checks$dyadic_census <- c(checks$dyadic_census, same(dyadic_census(D)[1:3], sna::dyad.census(D)[1, ]))
  checks$triad_census <- c(checks$triad_census, same(suppressWarnings(triad_uman(D))$OBS, sna::triad.census(D)[1, ]))

  # Shared partners against ergm, comparing the counts of every size
  net <- network::network(U, directed = FALSE)
  esp <- summary(net ~ esp(1:(n - 2)))
  mine <- shared_partners(U, type = "esp", directed = FALSE)
  sizes <- as.character(1:(n - 2))
  mine_counts <- ifelse(sizes %in% names(mine), mine[sizes], 0)
  checks$shared_partners <- c(checks$shared_partners, same(mine_counts, esp[paste0("esp", sizes)]))

  # The E-I index and the mixing matrix against netseg
  V(gu)$att <- att
  checks$ei_index <- c(checks$ei_index, same(ei_index(U, att = att), netseg::ei(gu, "att")))
  checks$mix_matrix <- c(checks$mix_matrix, same(sum(mix_matrix(U, att)), sum(U) / 2))
  V(gd)$att <- att
  checks$ei_index_directed <- c(checks$ei_index_directed, same(ei_index(D, att = att), netseg::ei(gd, "att")))

  # Blau's index, and the relations by hand
  p <- as.numeric(table(att)) / n
  checks$heterogeneity <- c(checks$heterogeneity, same(heterogeneity(att)[[1]], 1 - sum(p^2)))
  checks$power_function <- c(checks$power_function, same(power_function(U, 3), U %*% U %*% U))
  # The compound relations are binary, and "aa" is the composition of the first
  # matrix with itself
  compound <- compound_relation(list(U, D), comp = 2, matrices = TRUE)$compound_matrices
  checks$compound_relation <- c(
    checks$compound_relation,
    same(compound[["aa"]], 1 * ((U %*% U) > 0)) && same(compound[["ab"]], 1 * ((U %*% D) > 0))
  )
  checks$cumulative_sum <- c(checks$cumulative_sum, same(cumulativeSumMatrices(list(U, D))[[2]], U + D))

  # The projection of the rows against igraph
  B <- 1 * (matrix(runif(n * 6), n) < 0.3)
  dimnames(B) <- list(letters[1:n], LETTERS[1:6])
  projection <- matrix_projection(B)$matrix2
  igraph_projection <- igraph::as_adjacency_matrix(
    igraph::bipartite_projection(graph_from_biadjacency_matrix(B))[[1]],
    attr = "weight", sparse = FALSE
  )
  common <- intersect(rownames(projection), rownames(igraph_projection))
  checks$matrix_projection <- c(
    checks$matrix_projection,
    same(projection[common, common][lower.tri(projection[common, common])],
         igraph_projection[common, common][lower.tri(igraph_projection[common, common])])
  )

  # The edge list and the matrix are the same network
  E <- matrix_to_edgelist(D, digraph = TRUE)
  if (nrow(E) > 1) {
    back <- edgelist_to_matrix(E, digraph = TRUE, label = letters[1:n])
    checks$edgelist_round_trip <- c(checks$edgelist_round_trip, same(back[letters[1:n], letters[1:n]], D))
  }
}

# The positive-negative centrality against signnet
set.seed(7)
for (r in 1:10) {
  n <- 10
  S <- matrix(0, n, n)
  S[upper.tri(S)] <- sample(c(0, 1, -1), sum(upper.tri(S)), TRUE, prob = c(.6, .25, .15))
  S <- S + t(S)
  dimnames(S) <- list(letters[1:n], letters[1:n])
  gs <- graph_from_adjacency_matrix(S, "undirected", weighted = TRUE)
  E(gs)$sign <- E(gs)$weight
  checks$posneg_index <- c(checks$posneg_index, same(posneg_index(S, select = "all"), signnet::pn_index(gs), tol = 1e-4))
}

# Moran's I against ape, with the ties as weights
set.seed(5)
for (r in 1:10) {
  n <- 15
  A <- 1 * (matrix(runif(n * n), n) < 0.3)
  A[lower.tri(A)] <- t(A)[lower.tri(A)]
  diag(A) <- 0
  v <- rnorm(n)
  if (any(rowSums(A) == 0)) next
  checks$spatial_cor <- c(
    checks$spatial_cor,
    same(spatial_cor(A, v, measures = "moran", rowstand = TRUE), ape::Moran.I(v, A)$observed)
  )
}

# Geographic distances against geosphere, in radians as the function expects
latitude <- c(-33.45, 51.50, 40.71)
longitude <- c(-70.67, -0.12, -74.01)
mine <- dist_geographic(latitude, longitude, method = "harvesine", dd_to_radians = TRUE)
reference <- geosphere::distm(cbind(longitude, latitude), fun = geosphere::distHaversine) / 1000
checks$dist_geographic <- same(mine[lower.tri(mine)], reference[lower.tri(reference)], tol = 0.01)

data.frame(
  check = names(checks),
  agreements = sapply(checks, sum),
  comparisons = sapply(checks, length),
  row.names = NULL
)
