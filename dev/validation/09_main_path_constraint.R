# ============================================================
# 09_main_path_constraint.R
# Author: Alejandro Espinosa-Rada, Instituto de Sociología,
#         P. Universidad Católica de Chile
# Last update: 2026-09-18
#
# Compares the traversal weights of the main path analysis with
# the published values of Liu and Lu (2012, Fig. 1) and Kuan
# (2020, Table 3), and with the enumeration of every path of
# small random DAGs. Checks that dag_check() leaves a DAG and
# removes only arcs that close a cycle, and compares the number
# of removed arcs with the minimum of igraph. Compares k_core()
# with igraph::coreness() for directed networks and loops, and
# the directed and valued constraint with Everett and Borgatti
# (2020, Tables 1 and 2) and igraph::constraint().
#
# Output: printed table of agreements, and the number of arcs
#         removed by dag_check() above the minimum
# ============================================================

rm(list = ls())

library(here)
library(igraph)

pkgload::load_all(here::here(), quiet = TRUE)

checks <- list()
same <- function(a, b, tol = 1e-6) {
  isTRUE(all.equal(unname(as.numeric(a)), unname(as.numeric(b)), tolerance = tol))
}
# eb_constraint() rounds to three decimals
same_rounded <- function(a, b) {
  abs(a - b) <= 0.0005 + 1e-9
}

#### Published traversal weights ####

# Liu and Lu (2012), Fig. 1, arcs in the direction of the flow of knowledge
liu_arcs <- rbind(
  c("A", "H"), c("B", "H"), c("H", "J"), c("H", "D"), c("J", "C"), c("J", "D"),
  c("B", "I"), c("I", "F"), c("I", "G"), c("I", "E"), c("G", "D"), c("G", "E")
)
L <- matrix(0, 10, 10, dimnames = list(LETTERS[1:10], LETTERS[1:10]))
L[liu_arcs] <- 1
checks$liu_lu_spc <- same(traversal_weights(L, "spc")$edge_weights[liu_arcs], c(3, 3, 4, 2, 2, 2, 4, 1, 2, 1, 1, 1))

# Kuan (2020), Fig. 2 and Table 3
K <- matrix(0, 13, 13, dimnames = list(1:13, 1:13))
K[rbind(
  c(1, 4), c(2, 4), c(3, 4), c(4, 5), c(4, 6), c(5, 7), c(6, 8),
  c(7, 8), c(7, 9), c(7, 10), c(7, 11), c(8, 12), c(8, 13)
)] <- 1
table3 <- list(
  spc = cbind(c(0, 0, 0, 21, 15, 6, 15, 12, 3, 3, 3, 6, 6), c(7, 7, 7, 21, 15, 6, 15, 12, 0, 0, 0, 0, 0)),
  splc = cbind(c(0, 0, 0, 21, 20, 8, 25, 22, 6, 6, 6, 12, 12), c(7, 7, 7, 28, 25, 10, 30, 24, 0, 0, 0, 0, 0)),
  spnp = cbind(c(0, 0, 0, 39, 32, 16, 35, 33, 6, 6, 6, 12, 12), c(13, 13, 13, 48, 35, 15, 36, 24, 0, 0, 0, 0, 0))
)
for (method in names(table3)) {
  w <- traversal_weights(K, method)
  checks$kuan_table3 <- c(
    checks$kuan_table3,
    same(cbind(w$weighted_indegree, w$weighted_outdegree), table3[[method]])
  )
}

#### Traversal weights against the enumeration of every path ####

# Every path with at least one arc, starting from each node
all_paths <- function(A) {
  paths <- list()
  frontier <- as.list(seq_len(nrow(A)))
  while (length(frontier) > 0) {
    longer <- list()
    for (p in frontier) {
      for (j in which(A[p[length(p)], ] > 0)) {
        longer[[length(longer) + 1]] <- c(p, j)
      }
    }
    paths <- c(paths, longer)
    frontier <- longer
  }
  paths
}

set.seed(2026)
for (r in 1:25) {
  n <- sample(6:10, 1)
  M <- matrix(0, n, n)
  M[upper.tri(M)] <- rbinom(n * (n - 1) / 2, 1, 0.35)
  order <- sample(n)
  M <- M[order, order]
  dimnames(M) <- list(letters[1:n], letters[1:n])
  if (sum(M) == 0) next

  sources <- colSums(M) == 0
  sinks <- rowSums(M) == 0
  SPC <- SPLC <- SPNP <- M * 0
  for (p in all_paths(M)) {
    arcs <- cbind(p[-length(p)], p[-1])
    # SPC: from a source to a sink; SPLC: from any node to a sink; SPNP: any path
    if (sources[p[1]] && sinks[p[length(p)]]) SPC[arcs] <- SPC[arcs] + 1
    if (sinks[p[length(p)]]) SPLC[arcs] <- SPLC[arcs] + 1
    SPNP[arcs] <- SPNP[arcs] + 1
  }
  checks$spc_enumeration <- c(checks$spc_enumeration, same(traversal_weights(M, "spc")$edge_weights, SPC))
  checks$splc_enumeration <- c(checks$splc_enumeration, same(traversal_weights(M, "splc")$edge_weights, SPLC))
  checks$spnp_enumeration <- c(checks$spnp_enumeration, same(traversal_weights(M, "spnp")$edge_weights, SPNP))
}

#### Cycles of a citation network ####

set.seed(1)
excess <- NULL
for (r in 1:30) {
  n <- sample(8:15, 1)
  M <- matrix(rbinom(n * n, 1, 0.2), n, n)
  dimnames(M) <- list(paste0("v", 1:n), paste0("v", 1:n))
  result <- suppressWarnings(dag_check(M))

  # The result is a DAG, and dag_sort() orders every arc forwards
  position <- match(rownames(M), dag_sort(result$A))
  arcs <- which(result$A > 0, arr.ind = TRUE)
  checks$dag_sort_order <- c(checks$dag_sort_order, all(position[arcs[, 1]] < position[arcs[, 2]]))

  # Putting back any of the removed arcs closes a cycle
  removed <- which(M > 0 & result$A == 0 & row(M) != col(M), arr.ind = TRUE)
  needed <- TRUE
  for (k in seq_len(nrow(removed))) {
    B <- result$A
    B[removed[k, 1], removed[k, 2]] <- 1
    needed <- needed && !is_dag(graph_from_adjacency_matrix(B))
  }
  checks$dag_check_needed <- c(checks$dag_check_needed, needed)

  g <- graph_from_adjacency_matrix(M, mode = "directed", diag = FALSE)
  # A loop is a cycle for dag_check(), and igraph ignores it here
  checks$dag_check_is_dag <- c(checks$dag_check_is_dag, result$is_dag == (is_dag(g) && !any(diag(M) > 0)))
  minimum <- length(feedback_arc_set(g, algo = "exact_ip"))
  excess <- c(excess, result$n_removed - sum(diag(M) > 0) - minimum)
}

#### k-core of directed networks and loops ####

set.seed(7)
for (r in 1:20) {
  n <- sample(8:20, 1)
  D <- matrix(rbinom(n * n, 1, runif(1, 0.1, 0.5)), n, n)
  diag(D) <- rbinom(n, 1, 0.5)
  for (type in c("in", "out", "all")) {
    checks$k_core_directed <- c(
      checks$k_core_directed,
      same(k_core(D, digraph = TRUE, type = type, loops = TRUE), coreness(graph_from_adjacency_matrix(D), mode = type))
    )
  }
  U <- pmax(D, t(D))
  checks$k_core_loops <- c(
    checks$k_core_loops,
    same(k_core(U, loops = TRUE), coreness(graph_from_adjacency_matrix(U, mode = "undirected")))
  )
}

#### Constraint ####

# Ego "e" and N alters, in a complete or a shadow ego network. In the directed
# version the ties from ego to the alters are not reciprocated
ego_network <- function(N, type, directed = FALSE) {
  labels <- c("e", paste0("x", 1:N))
  A <- matrix(0, N + 1, N + 1, dimnames = list(labels, labels))
  A["e", -1] <- 1
  if (!directed) A[-1, "e"] <- 1
  if (type == "complete") A[-1, -1] <- 1
  if (type == "shadow") A["x1", -1] <- A[-1, "x1"] <- 1
  if (type == "shadow" && directed) A["x1", "e"] <- 1
  diag(A) <- 0
  A
}
table1 <- cbind(
  c(1.125, 0.926, 0.766, 0.648, 0.560, 0.493, 0.439, 0.396, 0.361),
  c(1.125, 0.840, 0.684, 0.590, 0.529, 0.486, 0.455, 0.431, 0.411)
)
table2 <- cbind(
  c(1.388889, 1.08, 0.862245, 0.71358, 0.607438, 0.528318, 0.467222, 0.418685, 0.379224),
  c(1.234568, 1.041667, 0.91, 0.822716, 0.761905, 0.717474, 0.683728, 0.657284, 0.636033)
)
for (N in 2:10) {
  checks$constraint_table1 <- c(
    checks$constraint_table1,
    same(eb_constraint(ego_network(N, "complete"), "e")$results$constraint, table1[N - 1, 1], tol = 2e-3),
    same(eb_constraint(ego_network(N, "shadow"), "e")$results$constraint, table1[N - 1, 2], tol = 2e-3)
  )
  checks$constraint_table2 <- c(
    checks$constraint_table2,
    same(eb_constraint(ego_network(N, "complete", TRUE), "e", digraph = TRUE)$results$constraint, table2[N - 1, 1], tol = 1e-3),
    same(eb_constraint(ego_network(N, "shadow", TRUE), "e", digraph = TRUE)$results$constraint, table2[N - 1, 2], tol = 1e-3)
  )
}

# igraph::constraint on the ego network, which netmem rounds to three decimals
set.seed(5)
for (r in 1:30) {
  n <- sample(5:12, 1)
  A <- matrix(rbinom(n * n, 1, 0.35), n)
  diag(A) <- 0
  dimnames(A) <- list(letters[1:n], letters[1:n])
  if (sum(A["a", ] + A[, "a"]) < 2) next
  E <- ego_net(A, "a", addEgo = TRUE)
  reference <- constraint(graph_from_adjacency_matrix(E, mode = "directed"), nodes = "a")
  checks$constraint_directed <- c(
    checks$constraint_directed,
    same_rounded(eb_constraint(A, "a", digraph = TRUE)$results$constraint, reference)
  )

  W <- A * sample(1:5, n * n, TRUE)
  W[lower.tri(W)] <- t(W)[lower.tri(W)]
  if (sum(W["a", ]) == 0) next
  Ew <- ego_net(W, "a", addEgo = TRUE)
  reference <- constraint(graph_from_adjacency_matrix(Ew, mode = "undirected", weighted = TRUE), nodes = "a")
  checks$constraint_valued <- c(
    checks$constraint_valued,
    same_rounded(eb_constraint(W, "a", weighted = TRUE)$results$constraint, reference)
  )
}

data.frame(
  check = names(checks),
  agreements = sapply(checks, sum),
  comparisons = sapply(checks, length),
  row.names = NULL
)

# Arcs removed by dag_check() above the minimum of igraph, over 30 networks
table(excess)
