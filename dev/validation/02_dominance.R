# ============================================================
# 02_dominance.R
# Author: Alejandro Espinosa-Rada, Instituto de Sociología,
#         P. Universidad Católica de Chile
# Last update: 2026-09-18
#
# Compares the dominance functions of netmem with netrankr, with the
# directed criteria of Marmulla and Brandes (2026) written directly
# with sets, and with the loop used in the analysis of
# astro_qap_citations/analysis_dominance/010dominance_coauthor_core_tau1.R
#
# Output: printed table of agreements
# ============================================================

rm(list = ls())

library(here)
library(igraph)
library(netrankr)

pkgload::load_all(here::here(), quiet = TRUE)

# Definition 1 of Marmulla and Brandes (2026), written with sets
directed_criteria <- function(A, type, strength) {
  n <- nrow(A)
  out <- lapply(1:n, function(i) which(A[i, ] > 0))
  into <- lapply(1:n, function(i) which(A[, i] > 0))
  out_closed <- lapply(1:n, function(i) union(out[[i]], i))
  in_closed <- lapply(1:n, function(i) union(into[[i]], i))

  P <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) next
      if (type == "medial") {
        holds <- all(out[[i]] %in% out_closed[[j]]) && all(into[[i]] %in% in_closed[[j]])
        if (holds && A[i, j] > 0) holds <- all(into[[i]] %in% out_closed[[j]])
        if (holds && A[j, i] > 0) holds <- all(out[[i]] %in% in_closed[[j]])
        P[i, j] <- 1 * holds
      } else {
        out_i <- if (strength == "strong") out[[i]] else out_closed[[i]]
        out_j <- if (strength == "strong") out[[j]] else out_closed[[j]]
        in_i <- if (strength == "strong") into[[i]] else in_closed[[i]]
        in_j <- if (strength == "strong") into[[j]] else in_closed[[j]]
        P[i, j] <- 1 * switch(type,
          radial_out = all(out_i %in% out_j),
          radial_in = all(in_i %in% in_j),
          hierarchical_down = all(out_i %in% out_j) && all(in_j %in% in_i),
          hierarchical_up = all(in_i %in% in_j) && all(out_j %in% out_i)
        )
      }
    }
  }
  P
}

# The dominance of the authors, as it was computed in the analysis scripts
hyperevent_loop <- function(X, W, Xb, tau) {
  chains <- NULL
  for (a1 in rownames(X)) {
    for (cp in colnames(X)[X[a1, ] > 0]) {
      for (cd in colnames(W)[W[cp, ] > 0]) {
        for (a2 in rownames(Xb)[Xb[, cd] > 0]) {
          chains <- rbind(chains, data.frame(a1, cp, cd, a2))
        }
      }
    }
  }
  authors <- unique(chains$a1)
  citing <- lapply(authors, function(a) unique(chains$cp[chains$a1 == a]))
  cited <- lapply(authors, function(a) unique(chains$cd[chains$a1 == a]))
  cited_authors <- lapply(authors, function(a) unique(chains$a2[chains$a1 == a]))

  M <- matrix(FALSE, length(authors), length(authors), dimnames = list(authors, authors))
  for (i in seq_along(authors)) {
    for (j in seq_along(authors)) {
      if (i == j) next
      d1_ij <- all(citing[[j]] %in% citing[[i]])
      d1_ji <- all(citing[[i]] %in% citing[[j]])
      d2_ij <- all(cited[[j]] %in% union(cited[[i]], citing[[i]]))
      d2_ji <- all(cited[[i]] %in% union(cited[[j]], citing[[j]]))
      d3_ij <- all(cited_authors[[j]] %in% union(cited_authors[[i]], authors[i]))
      d3_ji <- all(cited_authors[[i]] %in% union(cited_authors[[j]], authors[j]))
      M[i, j] <- (sum(d1_ij, d2_ij, d3_ij) >= tau) &
        ((d1_ij & !d1_ji) | (d2_ij & !d2_ji) | (d3_ij & !d3_ji))
    }
  }
  M
}

set.seed(63)
checks <- list()

for (r in 1:20) {
  n <- sample(5:12, 1)
  D <- 1 * (matrix(runif(n * n), n) < runif(1, 0.15, 0.45))
  diag(D) <- 0
  dimnames(D) <- list(letters[1:n], letters[1:n])

  for (type in c("radial_out", "radial_in", "hierarchical_down", "hierarchical_up")) {
    for (strength in c("strong", "weak")) {
      key <- paste("dir_inclusion", type, strength)
      checks[[key]] <- c(checks[[key]], all(dir_inclusion(D, type, strength) == directed_criteria(D, type, strength)))
    }
  }
  checks[["dir_inclusion medial"]] <- c(
    checks[["dir_inclusion medial"]],
    all(dir_inclusion(D, "medial") == directed_criteria(D, "medial"))
  )

  U <- pmax(D, t(D))
  g <- graph_from_adjacency_matrix(U, "max")
  P <- neigh_inclusion(U)
  checks$neigh_inclusion <- c(checks$neigh_inclusion, all(P == as.matrix(neighborhood_inclusion(g))))
  checks$comparable_pairs <- c(checks$comparable_pairs, isTRUE(all.equal(dominance_pairs(P)$prop_comparable, comparable_pairs(P))))
  checks$preserved_order <- c(
    checks$preserved_order,
    preserved_order(P, rowSums(U))$preserved == is_preserved(P, degree(g))
  )

  distances <- indirect_rel(U, type = "distance", digraph = FALSE)
  reference <- netrankr::indirect_relations(g, type = "dist_sp")
  checks$indirect_distance <- c(checks$indirect_distance, isTRUE(all.equal(unname(distances), unname(reference))))
  if (all(is.finite(distances))) {
    checks$positional_heterogeneity <- c(
      checks$positional_heterogeneity,
      all(pos_dominance(distances, benefit = FALSE) == netrankr::positional_dominance(reference, benefit = FALSE))
    )
    checks$positional_homogeneity <- c(
      checks$positional_homogeneity,
      all(pos_dominance(distances, benefit = FALSE, map = TRUE) ==
        netrankr::positional_dominance(reference, benefit = FALSE, map = TRUE))
    )
  }
  mine <- dominance_ranks(P)
  theirs <- netrankr::rank_intervals(P)
  checks$rank_intervals <- c(checks$rank_intervals, all(mine$min_rank == theirs$min_rank) && all(mine$max_rank == theirs$max_rank))
}

# Hyper-event dominance against the loop of the analysis scripts
set.seed(11)
for (r in 1:20) {
  authors <- sample(4:12, 1)
  papers <- sample(5:20, 1)
  X <- 1 * (matrix(runif(authors * papers), authors) < 0.25)
  W <- 1 * (matrix(runif(papers * papers), papers) < 0.15)
  W[lower.tri(W, diag = TRUE)] <- 0
  dimnames(X) <- list(paste0("a", 1:authors), paste0("w", 1:papers))
  dimnames(W) <- list(colnames(X), colnames(X))
  if (sum((X %*% W) > 0) == 0) next

  for (tau in 1:3) {
    reference <- hyperevent_loop(X, W, X, tau)
    mine <- hyperevent_dominance(X, W, tau = tau, direction = "dominates")
    checks$hyperevent_dominance <- c(
      checks$hyperevent_dominance,
      identical(dim(reference), dim(mine)) && all(reference[rownames(mine), colnames(mine)] == (mine == 1))
    )
  }
}

reference <- rep("netrankr", length(checks))
reference[grepl("^dir_inclusion", names(checks))] <- "Marmulla and Brandes (2026)"
reference[names(checks) == "hyperevent_dominance"] <- "analysis scripts"

data.frame(
  check = names(checks),
  agreements = sapply(checks, sum),
  comparisons = sapply(checks, length),
  reference = reference,
  row.names = NULL
)
