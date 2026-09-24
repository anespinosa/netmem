# ============================================================
# 03_structure.R
# Author: Alejandro Espinosa-Rada, Instituto de Sociología,
#         P. Universidad Católica de Chile
# Last update: 2026-09-18
#
# Compares the macro-structure and equivalence functions of netmem
# with sna and igraph, and with structures whose answer is known.
# The core-periphery partition is compared with every possible
# partition of the nodes.
#
# Output: printed table of agreements
# ============================================================

rm(list = ls())

library(here)
library(igraph)
library(sna)

pkgload::load_all(here::here(), quiet = TRUE)

set.seed(9)
checks <- list()
same <- function(a, b) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = 1e-8))

for (r in 1:20) {
  n <- sample(5:14, 1)
  D <- 1 * (matrix(runif(n * n), n) < runif(1, 0.1, 0.4))
  diag(D) <- 0
  U <- 1 * (matrix(runif(n * n), n) < 0.35)
  U[lower.tri(U)] <- t(U)[lower.tri(U)]
  diag(U) <- 0
  dimnames(U) <- dimnames(D) <- list(letters[1:n], letters[1:n])

  krackhardt <- krackhardt_index(D)
  checks$connectedness <- c(checks$connectedness, same(krackhardt$connectedness, sna::connectedness(D)))
  checks$hierarchy <- c(checks$hierarchy, same(krackhardt$hierarchy, sna::hierarchy(D, measure = "krackhardt")))
  checks$efficiency <- c(checks$efficiency, same(krackhardt$efficiency, sna::efficiency(D)))

  # The upper bound condition is less demanding than the least upper bound one
  checks$lubness_order <- c(
    checks$lubness_order,
    krackhardt$lubness >= krackhardt_index(D, lubness = "least")$lubness - 1e-12
  )

  # The components, in the three ways they can be asked for
  same_partition <- function(a, b) {
    length(unique(a)) == length(unique(b)) && all(rowSums(table(a, b) > 0) == 1)
  }
  gd <- graph_from_adjacency_matrix(D, "directed")
  checks$components_weak <- c(
    checks$components_weak,
    same_partition(components_id(D)$components, igraph::components(gd, "weak")$membership)
  )
  checks$components_strong <- c(
    checks$components_strong,
    same_partition(components_id(D, "strong")$components, igraph::components(gd, "strong")$membership)
  )
  B <- 1 * (matrix(runif(n * 8), n) < 0.2)
  dimnames(B) <- list(paste0("r", 1:n), paste0("c", 1:8))
  checks$components_bipartite <- c(
    checks$components_bipartite,
    same_partition(
      components_id(B, bipartite = TRUE)$components,
      igraph::components(graph_from_biadjacency_matrix(B))$membership
    )
  )

  key <- function(x) paste(sort(as.character(x)), collapse = "|")
  mine <- sapply(clique_max(U, min = 1), key)
  theirs <- sapply(igraph::max_cliques(graph_from_adjacency_matrix(U, "undirected")), function(x) key(names(x)))
  checks$maximal_cliques <- c(checks$maximal_cliques, setequal(mine, theirs) && length(mine) == length(theirs))
}

# The core-periphery partition against every possible partition
set.seed(21)
for (r in 1:15) {
  n <- sample(6:11, 1)
  U <- 1 * (matrix(runif(n * n), n) < 0.3)
  U[lower.tri(U)] <- t(U)[lower.tri(U)]
  diag(U) <- 0
  dimnames(U) <- list(letters[1:n], letters[1:n])

  best <- -Inf
  for (m in 1:(2^n - 2)) {
    core <- as.logical(bitwAnd(m, 2^(0:(n - 1))) > 0)
    fit <- netmem:::pattern_fit(U, netmem:::ideal_core(core))
    if (is.finite(fit) && fit > best) best <- fit
  }
  checks$core_periphery <- c(checks$core_periphery, same(core_periphery(U)$fit, best))
}

# Structures whose partition is known
cliques <- matrix(0, 8, 8)
cliques[1:4, 1:4] <- 1
cliques[5:8, 5:8] <- 1
diag(cliques) <- 0
cliques[4, 5] <- 1
cliques[5, 4] <- 1
rownames(cliques) <- letters[1:8]
colnames(cliques) <- rownames(cliques)
partition <- concor(cliques, splits = 1)$partition
checks$concor_two_cliques <- length(unique(partition)) == 2 &&
  length(unique(partition[1:4])) == 1 && length(unique(partition[5:8])) == 1

blocks <- block_density(cliques, partition = c(rep(1, 4), rep(2, 4)))
checks$block_density <- same(blocks$densities[1, 1], 1) && same(blocks$densities[1, 2], 1 / 16)

# Two managers with different subordinates play the same role
roles <- matrix(0, 7, 7)
roles[1, 2:3] <- 1
roles[2, 4:5] <- 1
roles[3, 6:7] <- 1
rownames(roles) <- letters[1:7]
colnames(roles) <- rownames(roles)
similarity <- rege(roles)
checks$rege_roles <- same(similarity["b", "c"], 1) && same(similarity["d", "g"], 1) && similarity["a", "d"] < 1

# A perfect out-tree scores one on the four dimensions
checks$krackhardt_out_tree <- all(unlist(krackhardt_index(roles)) == 1)

reference <- rep("sna", length(checks))
reference[names(checks) %in% c("maximal_cliques", "components_weak", "components_strong", "components_bipartite")] <- "igraph"
reference[names(checks) %in% c("core_periphery")] <- "every partition"
reference[names(checks) %in% c("lubness_order", "concor_two_cliques", "block_density", "rege_roles", "krackhardt_out_tree")] <- "known structure"

data.frame(
  check = names(checks),
  agreements = sapply(checks, sum),
  comparisons = sapply(checks, length),
  reference = reference,
  row.names = NULL
)
