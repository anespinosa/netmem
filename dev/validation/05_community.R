# ============================================================
# 05_community.R
# Author: Alejandro Espinosa-Rada, Instituto de Sociología,
#         P. Universidad Católica de Chile
# Last update: 2026-09-18
#
# Compares the community functions of netmem with igraph on
# networks with planted communities. Leiden should reach the same
# modularity as igraph and return groups that are internally
# connected, which is the guarantee of Traag et al. (2019).
#
# Output: printed table of agreements
# ============================================================

rm(list = ls())

library(here)
library(igraph)

pkgload::load_all(here::here(), quiet = TRUE)

set.seed(44)
checks <- list()

for (r in 1:15) {
  groups <- sample(2:4, 1)
  sizes <- sample(5:10, groups, replace = TRUE)
  n <- sum(sizes)
  planted <- rep(seq_len(groups), sizes)
  probability <- ifelse(outer(planted, planted, "=="), 0.7, 0.05)
  U <- 1 * (matrix(runif(n * n), n) < probability)
  U[lower.tri(U)] <- t(U)[lower.tri(U)]
  diag(U) <- 0
  dimnames(U) <- list(paste0("v", 1:n), paste0("v", 1:n))
  g <- graph_from_adjacency_matrix(U, "undirected")

  checks$modularity <- c(checks$modularity, isTRUE(all.equal(modularity_score(U, planted), modularity(g, planted))))
  D <- 1 * (matrix(runif(n * n), n) < 0.2)
  diag(D) <- 0
  checks$modularity_directed <- c(
    checks$modularity_directed,
    isTRUE(all.equal(
      modularity_score(D, planted, digraph = TRUE),
      modularity(graph_from_adjacency_matrix(D, "directed"), planted, directed = TRUE)
    ))
  )

  leiden_result <- leiden(U)
  checks$leiden_vs_igraph <- c(
    checks$leiden_vs_igraph,
    isTRUE(all.equal(
      leiden_result$modularity,
      modularity(g, membership(cluster_leiden(g, objective_function = "modularity", resolution = 1, n_iterations = 5)))
    ))
  )
  # Every group of Leiden is internally connected
  connected <- sapply(unique(leiden_result$partition), function(group) {
    members <- which(leiden_result$partition == group)
    length(unique(components_id(U[members, members, drop = FALSE])$components)) == 1
  })
  checks$leiden_connected <- c(checks$leiden_connected, all(connected))
  checks$louvain <- c(checks$louvain, leiden(U, refine = FALSE)$modularity >= modularity(cluster_louvain(g)) - 1e-8)
  checks$leading_eigen <- c(checks$leading_eigen, leading_eigen(U)$modularity >= modularity(cluster_leading_eigen(g)) - 1e-8)
  checks$greedy <- c(checks$greedy, community_greedy(U)$modularity >= modularity(cluster_fast_greedy(g)) - 1e-8)
  checks$edge_betweenness <- c(
    checks$edge_betweenness,
    isTRUE(all.equal(community_betweenness(U)$modularity, modularity(cluster_edge_betweenness(g))))
  )

  edges <- as_edgelist(g, names = FALSE)
  checks$edge_betweenness_scores <- c(
    checks$edge_betweenness_scores,
    isTRUE(all.equal(netmem:::edge_betweenness(U)[edges], igraph::edge_betweenness(g)))
  )
  # The flows and their expectation both add up to one, so one group gives zero
  checks$linkrank_single_group <- c(
    checks$linkrank_single_group,
    isTRUE(all.equal(modularity_score(D, rep(1, n), method = "linkrank", digraph = TRUE), 0))
  )
}

data.frame(
  check = names(checks),
  agreements = sapply(checks, sum),
  comparisons = sapply(checks, length),
  reference = ifelse(names(checks) == "linkrank_single_group", "identity", "igraph"),
  row.names = NULL
)
