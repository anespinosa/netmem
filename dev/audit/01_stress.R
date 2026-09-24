# ============================================================
# 01_stress.R
# Author: Alejandro Espinosa-Rada, Instituto de Sociología,
#         P. Universidad Católica de Chile
# Last update: 2026-09-18
#
# First layer of the audit. Calls every exported function on a set
# of awkward inputs (undirected, directed, valued, with an isolate,
# empty, two nodes, with a loop, without names, with a missing
# value, two-mode) and records the errors, the warnings, the NaN or
# Inf values, and the outputs whose size does not match the
# network. Then checks that relabelling the nodes permutes the
# result of the node-level measures in the same way.
#
# Output (dev/audit/):
#   stress_results.csv       one row per function and input
#   permutation_results.csv  one row per function and input
# ============================================================

rm(list = ls())

library(here)

pkgload::load_all(here::here(), quiet = TRUE)

#### Inputs ####

nodes <- letters[1:6]
U <- matrix(c(
  0, 1, 1, 0, 0, 0,
  1, 0, 1, 1, 0, 0,
  1, 1, 0, 1, 0, 0,
  0, 1, 1, 0, 1, 0,
  0, 0, 0, 1, 0, 1,
  0, 0, 0, 0, 1, 0
), byrow = TRUE, ncol = 6, dimnames = list(nodes, nodes))
D <- matrix(c(
  0, 1, 0, 0, 0, 0,
  0, 0, 1, 1, 0, 0,
  1, 0, 0, 1, 0, 0,
  0, 0, 0, 0, 1, 0,
  0, 1, 0, 0, 0, 1,
  0, 0, 0, 0, 1, 0
), byrow = TRUE, ncol = 6, dimnames = list(nodes, nodes))
W <- U * c(2, 1, 3, 1, 2, 4)
W[lower.tri(W)] <- t(W)[lower.tri(W)]
ISO <- U
ISO["f", ] <- ISO[, "f"] <- 0
EMPTY <- U * 0
TWO <- matrix(c(0, 1, 1, 0), 2, dimnames = list(c("a", "b"), c("a", "b")))
LOOP <- U
LOOP["a", "a"] <- 1
NONAME <- unname(U)
MISSING <- U
MISSING["a", "b"] <- NA
one_mode <- list(
  undirected = U, directed = D, valued = W, isolate = ISO, empty = EMPTY,
  two_nodes = TWO, loop = LOOP, no_names = NONAME, missing = MISSING
)

X <- matrix(c(
  1, 1, 0,
  0, 1, 0,
  1, 0, 1,
  0, 0, 1,
  1, 1, 1
), byrow = TRUE, ncol = 3, dimnames = list(letters[1:5], c("e1", "e2", "e3")))
XZ <- X
XZ["d", ] <- 0
two_mode <- list(incidence = X, incidence_empty_row = XZ, incidence_square = X[1:3, ], incidence_no_names = unname(X))

DAG <- matrix(c(
  0, 1, 1, 0, 0,
  0, 0, 0, 1, 0,
  0, 0, 0, 1, 1,
  0, 0, 0, 0, 0,
  0, 0, 0, 0, 0
), byrow = TRUE, ncol = 5, dimnames = list(letters[1:5], letters[1:5]))

# Attributes of the nodes, given by name so that they follow the nodes when
# they are relabelled (by position for the matrices without names)
att_of <- function(M) {
  if (is.null(rownames(M))) {
    return(rep(c(1, 2), length.out = nrow(M)))
  }
  unname(c(a = 1, b = 2, c = 1, d = 2, e = 1, f = 2)[rownames(M)])
}
group_of <- function(M) {
  if (is.null(rownames(M))) {
    return(rep(c("x", "y", "z"), length.out = nrow(M)))
  }
  unname(c(a = "x", b = "y", c = "z", d = "x", e = "y", f = "z")[rownames(M)])
}

#### Calls ####

# Each template receives a one-mode matrix M
one_mode_calls <- list(
  adj_to_incidence = function(M) adj_to_incidence(M),
  alter_composition = function(M) alter_composition(M, group_of(M)),
  alter_heterogeneity = function(M) alter_heterogeneity(M, group_of(M)),
  alter_homophily = function(M) alter_homophily(M, group_of(M)),
  alter_homophily_yule = function(M) alter_homophily(M, group_of(M), method = "yule"),
  betweenness_centrality = function(M) betweenness_centrality(M),
  betweenness_undirected = function(M) betweenness_centrality(M, digraph = FALSE),
  bfs_ugraph = function(M) bfs_ugraph(M),
  block_density = function(M) block_density(M, att_of(M)),
  bonacich_power = function(M) bonacich_power(M, beta = 0.1),
  brokerage_roles = function(M) brokerage_roles(M, group_of(M)),
  centrality_centralization = function(M) centrality_centralization(M),
  centralization_betweenness = function(M) centrality_centralization(M, "betweenness"),
  clique_max = function(M) clique_max(M),
  clique_table = function(M) clique_table(M),
  closeness_centrality = function(M) closeness_centrality(M),
  closeness_harmonic = function(M) closeness_centrality(M, harmonic = TRUE),
  community_betweenness = function(M) community_betweenness(M),
  community_greedy = function(M) community_greedy(M),
  community_label = function(M) community_label(M),
  components_id = function(M) components_id(M),
  components_strong = function(M) components_id(M, mode = "strong"),
  concor = function(M) concor(M),
  core_periphery = function(M) core_periphery(M),
  count_geodesics = function(M) count_geodesics(M),
  cug_test = function(M) cug_test(M, gen_density, reps = 20),
  cumulativeSumMatrices = function(M) cumulativeSumMatrices(list(M, M)),
  dir_inclusion = function(M) dir_inclusion(M),
  dist_sim_matrix = function(M) dist_sim_matrix(M),
  dominance_layers = function(M) dominance_layers(neigh_inclusion(M)),
  dominance_pairs = function(M) dominance_pairs(neigh_inclusion(M)),
  dominance_ranks = function(M) dominance_ranks(neigh_inclusion(M)),
  dyad_triad_table = function(M) dyad_triad_table(M),
  dyadic_census = function(M) dyadic_census(M),
  eb_constraint = function(M) eb_constraint(M, ego = rownames(M)[1]),
  edgelist_roundtrip = function(M) edgelist_to_matrix(matrix_to_edgelist(M, digraph = TRUE), digraph = TRUE),
  ego_net = function(M) ego_net(M, ego = rownames(M)[1]),
  ei_index = function(M) ei_index(M, att = att_of(M)),
  eigenvector_centrality = function(M) eigenvector_centrality(M),
  expand_matrix = function(M) expand_matrix(M),
  extract_component = function(M) extract_component(M),
  gen_degree = function(M) gen_degree(M),
  gen_degree_undirected = function(M) gen_degree(M, digraph = FALSE),
  gen_degree_weighted = function(M) gen_degree(M, digraph = FALSE, weighted = TRUE),
  closeness_undirected = function(M) closeness_centrality(M, digraph = FALSE),
  eigenvector_undirected = function(M) eigenvector_centrality(M, digraph = FALSE),
  katz_undirected = function(M) katz_centrality(M, alpha = 0.1, digraph = FALSE),
  page_rank_undirected = function(M) page_rank_centrality(M, digraph = FALSE),
  k_core_directed = function(M) k_core(M, digraph = TRUE, type = "all"),
  k_core_weighted = function(M) k_core(M, weighted = TRUE),
  structural_holes_whole = function(M) structural_holes(M, ego_network = FALSE),
  gen_density = function(M) gen_density(M),
  geo_distances = function(M) geo_distances(M),
  geo_summary = function(M) geo_summary(M),
  indirect_rel = function(M) indirect_rel(M),
  k_core = function(M) k_core(M),
  katz_centrality = function(M) katz_centrality(M, alpha = 0.1),
  kp_reciprocity = function(M) kp_reciprocity(M),
  krackhardt_index = function(M) krackhardt_index(M),
  leading_eigen = function(M) leading_eigen(M),
  leiden = function(M) leiden(M),
  matrix_adjlist = function(M) matrix_adjlist(M),
  matrix_report = function(M) matrix_report(M),
  matrix_to_edgelist = function(M) matrix_to_edgelist(M),
  minmax_overlap = function(M) minmax_overlap(M),
  mix_matrix = function(M) mix_matrix(M, att_of(M)),
  modularity_score = function(M) modularity_score(M, att_of(M)),
  multiplex_census = function(M) multiplex_census(M, M),
  supra_adjacency = function(M) supra_adjacency(list(M, M)),
  supra_adjacency_ordinal = function(M) supra_adjacency(list(M, M), coupling = "ordinal", sparse = TRUE),
  aggregate_layers = function(M) aggregate_layers(list(M, M)),
  aggregate_layers_supra = function(M) aggregate_layers(supra_adjacency(list(M, M)), method = "binary"),
  neigh_inclusion = function(M) neigh_inclusion(M),
  page_rank_centrality = function(M) page_rank_centrality(M),
  pareto_dominance = function(M) pareto_dominance(list(neigh_inclusion(M))),
  partition_centrality = function(M) partition_centrality(M, group_of(M)),
  percolation_clique = function(M) percolation_clique(M),
  pos_dominance = function(M) pos_dominance(indirect_rel(M)),
  power_function = function(M) power_function(M, 3),
  preserved_order = function(M) preserved_order(neigh_inclusion(M), gen_degree(M)),
  q_analysis = function(M) q_analysis(M),
  qap_cor = function(M) qap_cor(M, M, reps = 20),
  recip_coef = function(M) recip_coef(M),
  redundancy = function(M) redundancy(M, ego = rownames(M)[1]),
  rege = function(M) rege(M),
  segregation = function(M) segregation(M, att_of(M)),
  set_inclusion = function(M) set_inclusion(M),
  shared_partners = function(M) shared_partners(M),
  short_path = function(M) short_path(M, from = rownames(M)[1], to = rownames(M)[2]),
  simplicial_complexes = function(M) simplicial_complexes(M),
  social_influence = function(M) social_influence(M, seq(0, 1, length.out = nrow(M))),
  spatial_cor = function(M) spatial_cor(M, seq_len(nrow(M))),
  structural_holes = function(M) structural_holes(M),
  structural_na = function(M) structural_na(M),
  threshold_diffusion = function(M) threshold_diffusion(M, seeds = 1),
  trans_coef = function(M) trans_coef(M),
  trans_matrix = function(M) trans_matrix(M),
  triad_uman = function(M) triad_uman(M),
  wall_distances = function(M) wall_distances(M),
  wlocal_distances = function(M) wlocal_distances(M, from = rownames(M)[1], to = rownames(M)[2]),
  z_arctest = function(M) z_arctest(M),
  zone_sample = function(M) zone_sample(M, X = matrix(rep(c(1, 0), length.out = nrow(M) * 2), nrow(M), dimnames = list(rownames(M), c("u", "v"))))
)

two_mode_calls <- list(
  bonacich_norm = function(M) bonacich_norm(M),
  co_occurrence = function(M) co_occurrence(M),
  components_bipartite = function(M) components_id(M, bipartite = TRUE),
  gen_degree_bipartite = function(M) gen_degree(M, bipartite = TRUE),
  gen_density_bipartite = function(M) gen_density(M, bipartite = TRUE),
  hypergraph = function(M) hypergraph(M),
  matrix_projection = function(M) matrix_projection(M),
  q_analysis_incidence = function(M) q_analysis(M, simplicial_complex = TRUE),
  dist_sim_bipartite = function(M) dist_sim_matrix(M, bipartite = TRUE),
  jaccard_bipartite = function(M) jaccard(M, M, bipartite = TRUE),
  alter_composition_rect = function(M) alter_composition(M, rep(c("x", "y"), length.out = ncol(M)))
)

dag_calls <- list(
  dag_check = function(M) dag_check(M),
  dag_sort = function(M) dag_sort(M),
  traversal_weights = function(M) traversal_weights(M, "splc"),
  main_path = function(M) main_path(M),
  main_path_key_route = function(M) main_path(M, method = "key_route", k = 2),
  main_path_diag = function(M) main_path_diag(M, k_values = 1:2, k_jaccard = 1),
  citation_decay = function(M) citation_decay(M, setNames(2000 + seq_len(nrow(M)), rownames(M)))
)

#### Run ####

# A call that takes more than 20 seconds on these small inputs is stopped and
# recorded as an error, as it is probably stuck in a loop
run_one <- function(call, M) {
  warnings <- character(0)
  result <- withCallingHandlers(
    tryCatch(
      {
        setTimeLimit(elapsed = 20, transient = TRUE)
        on.exit(setTimeLimit(elapsed = Inf))
        call(M)
      },
      error = function(e) structure(conditionMessage(e), class = "stress_error")
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  error <- if (inherits(result, "stress_error")) as.character(result) else ""
  # Every number inside the result, however nested
  numbers <- tryCatch(
    as.numeric(rapply(list(result), function(x) as.numeric(x), classes = c("numeric", "integer", "matrix"), how = "unlist")),
    error = function(e) numeric(0)
  )
  data.frame(
    error = substr(gsub("\n", " ", error), 1, 120),
    warning = substr(gsub("\n", " ", paste(unique(warnings), collapse = " | ")), 1, 120),
    nan = any(is.nan(numbers)),
    inf = any(is.infinite(numbers)),
    stringsAsFactors = FALSE
  )
}

results <- NULL
set.seed(1)
for (f in names(one_mode_calls)) {
  message(f)
  for (input in names(one_mode)) {
    results <- rbind(results, cbind(fn = f, input = input, run_one(one_mode_calls[[f]], one_mode[[input]])))
  }
}
for (f in names(two_mode_calls)) {
  for (input in names(two_mode)) {
    results <- rbind(results, cbind(fn = f, input = input, run_one(two_mode_calls[[f]], two_mode[[input]])))
  }
}
for (f in names(dag_calls)) {
  results <- rbind(results, cbind(fn = f, input = "dag", run_one(dag_calls[[f]], DAG)))
}
write.csv(results, here::here("dev", "audit", "stress_results.csv"), row.names = FALSE)

#### Permutation ####

# A node-level result (a vector of length n or a matrix with n rows) should
# follow the nodes when they are relabelled
permuted <- NULL
order <- c(4, 1, 6, 2, 5, 3)
for (f in names(one_mode_calls)) {
  for (input in c("undirected", "directed", "valued")) {
    M <- one_mode[[input]]
    P <- M[order, order]
    set.seed(1)
    a <- tryCatch(suppressWarnings(one_mode_calls[[f]](M)), error = function(e) NULL)
    set.seed(1)
    b <- tryCatch(suppressWarnings(one_mode_calls[[f]](P)), error = function(e) NULL)
    if (is.null(a) || is.null(b)) next
    node_level <- (is.atomic(a) && is.null(dim(a)) && length(a) == 6 && is.numeric(a)) ||
      (is.matrix(a) && nrow(a) == 6 && is.numeric(a))
    if (!node_level) next
    # The results are aligned by the names of the nodes; columns that are not
    # nodes (edges, cliques, categories) are compared as a set when their
    # order depends on the order of the nodes
    if (is.matrix(a)) {
      b <- b[rownames(a), , drop = FALSE]
      if (all(colnames(a) %in% nodes)) {
        b <- b[, colnames(a), drop = FALSE]
        same <- isTRUE(all.equal(unname(a), unname(b), tolerance = 1e-6))
      } else {
        key <- function(m) sort(apply(round(m, 6), 2, paste, collapse = ","))
        same <- isTRUE(all.equal(unname(a[, colnames(a) %in% colnames(b), drop = FALSE]), unname(b[, colnames(a)[colnames(a) %in% colnames(b)], drop = FALSE]), tolerance = 1e-6)) &&
          ncol(a) == ncol(b)
        if (!same) same <- identical(key(a), key(b))
      }
    } else {
      same <- isTRUE(all.equal(unname(a), unname(b[names(a)]), tolerance = 1e-6))
    }
    permuted <- rbind(permuted, data.frame(fn = f, input = input, follows_nodes = same))
  }
}
write.csv(permuted, here::here("dev", "audit", "permutation_results.csv"), row.names = FALSE)

#### Summary ####

problems <- results[results$error != "" | results$nan | results$inf, ]
table(input = results$input, error = results$error != "")
problems[, c("fn", "input", "error", "nan", "inf")]
permuted[!permuted$follows_nodes, ]
