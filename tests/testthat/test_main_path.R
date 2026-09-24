context("Test main path analysis")

# A[i, j] = 1 means that knowledge flows from i to j (j cites i)
# P1 is cited by P2 and P3, which are both cited by P4
# Source: P1  Sink: P4   Two equal paths: P1->P2->P4 and P1->P3->P4
A4 <- matrix(c(
  0, 1, 1, 0,
  0, 0, 0, 1,
  0, 0, 0, 1,
  0, 0, 0, 0
), byrow = TRUE, nrow = 4)
rownames(A4) <- c("P1", "P2", "P3", "P4")
colnames(A4) <- c("P1", "P2", "P3", "P4")

# 6-node network with one dominant path a->b->e->f
A6 <- matrix(c(
  0, 1, 1, 0, 0, 0,
  0, 0, 0, 1, 1, 0,
  0, 0, 0, 0, 1, 0,
  0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 1,
  0, 0, 0, 0, 0, 0
), byrow = TRUE, nrow = 6)
rownames(A6) <- letters[1:6]
colnames(A6) <- letters[1:6]

# Fig. 2 of Kuan (2020), Scientometrics 124: 775-782
K13 <- matrix(0, 13, 13, dimnames = list(1:13, 1:13))
K13[rbind(
  c(1, 4), c(2, 4), c(3, 4), c(4, 5), c(4, 6), c(5, 7), c(6, 8),
  c(7, 8), c(7, 9), c(7, 10), c(7, 11), c(8, 12), c(8, 13)
)] <- 1

# Fig. 1 of Liu and Lu (2012), JASIST 63: 528-542, with the SPC of each arc
liu_arcs <- rbind(
  c("A", "H"), c("B", "H"), c("H", "J"), c("H", "D"), c("J", "C"), c("J", "D"),
  c("B", "I"), c("I", "F"), c("I", "G"), c("I", "E"), c("G", "D"), c("G", "E")
)
liu_spc <- c(3, 3, 4, 2, 2, 2, 4, 1, 2, 1, 1, 1)
L10 <- matrix(0, 10, 10, dimnames = list(LETTERS[1:10], LETTERS[1:10]))
L10[liu_arcs] <- 1


# ---- dag_check ---------------------------------------------------------------

test_that("dag_check passes a clean DAG", {
  res <- dag_check(A4)
  expect_true(res$is_dag)
  expect_equal(res$n_removed, 0L)
})

test_that("dag_check removes back-edges from a cyclic graph", {
  A_cyc <- A4
  A_cyc["P4", "P1"] <- 1L  # P1 also cites P4, which closes two cycles
  expect_warning(res <- dag_check(A_cyc), "removed")
  expect_false(res$is_dag)
  expect_equal(res$n_removed, 1L)
  expect_equal(res$A["P4", "P1"], 0)
  expect_equal(sum(res$A), sum(A4))
})

test_that("dag_check removes self-citations and the arcs of each cycle", {
  A_cyc <- A6
  A_cyc["a", "a"] <- 1  # self-citation
  A_cyc["e", "b"] <- 1  # b <-> e
  A_cyc["f", "c"] <- 1  # c -> e -> f -> c
  expect_warning(res <- dag_check(A_cyc), "3 arc")
  expect_equal(res$n_removed, 3L)
  expect_equal(res$A["a", "a"], 0)
  expect_setequal(dag_sort(res$A), letters[1:6])
})

test_that("dag_check stops on unnamed matrix", {
  B <- matrix(0, 2, 2)
  expect_error(dag_check(B), "named")
})


# ---- dag_sort ----------------------------------------------------------------

test_that("dag_sort returns all node names", {
  ord <- dag_sort(A4)
  expect_setequal(ord, rownames(A4))
})

test_that("dag_sort places P1 before P2 and P3, and those before P4", {
  ord <- dag_sort(A4)
  expect_lt(which(ord == "P1"), which(ord == "P2"))
  expect_lt(which(ord == "P1"), which(ord == "P3"))
  expect_lt(which(ord == "P2"), which(ord == "P4"))
  expect_lt(which(ord == "P3"), which(ord == "P4"))
})

test_that("dag_sort stops on a cycle", {
  A_cyc <- A4
  A_cyc["P4", "P1"] <- 1
  expect_error(dag_sort(A_cyc), "dag_check")
})


# ---- traversal_weights -------------------------------------------------------

test_that("traversal_weights (SPC) returns correct sources and sinks", {
  w <- traversal_weights(A4, method = "spc")
  expect_equal(w$sources, "P1")
  expect_equal(w$sinks, "P4")
})

test_that("SPC agrees with Fig. 1 of Liu and Lu (2012)", {
  w <- traversal_weights(L10, method = "spc")
  expect_equal(w$edge_weights[liu_arcs], liu_spc)
})

test_that("SPC, SPLC and SPNP agree with Kuan (2020)", {
  spc <- traversal_weights(K13, method = "spc")
  splc <- traversal_weights(K13, method = "splc")
  spnp <- traversal_weights(K13, method = "spnp")
  # Arcs discussed in the text
  expect_equal(spc$edge_weights["8", "12"], 6)
  expect_equal(splc$edge_weights["8", "12"], 12)
  expect_equal(spnp$edge_weights["8", "12"], 12)
  expect_equal(spc$edge_weights["1", "4"], 7)
  expect_equal(splc$edge_weights["1", "4"], 7)
  expect_equal(spnp$edge_weights["1", "4"], 13)
  # Table 3
  expect_equal(unname(spc$weighted_indegree), c(0, 0, 0, 21, 15, 6, 15, 12, 3, 3, 3, 6, 6))
  expect_equal(unname(spc$weighted_outdegree), c(7, 7, 7, 21, 15, 6, 15, 12, 0, 0, 0, 0, 0))
  expect_equal(unname(splc$weighted_indegree), c(0, 0, 0, 21, 20, 8, 25, 22, 6, 6, 6, 12, 12))
  expect_equal(unname(splc$weighted_outdegree), c(7, 7, 7, 28, 25, 10, 30, 24, 0, 0, 0, 0, 0))
  expect_equal(unname(spnp$weighted_indegree), c(0, 0, 0, 39, 32, 16, 35, 33, 6, 6, 6, 12, 12))
  expect_equal(unname(spnp$weighted_outdegree), c(13, 13, 13, 48, 35, 15, 36, 24, 0, 0, 0, 0, 0))
  # Table 4, SPNP-WxD
  expect_equal(
    unname(spnp$weighted_indegree + spnp$weighted_outdegree) / 2,
    c(6.5, 6.5, 6.5, 43.5, 33.5, 15.5, 35.5, 28.5, 3, 3, 3, 6, 6)
  )
})

test_that("SPC edge weights are symmetric for the two-path example", {
  w <- traversal_weights(A4, method = "spc")
  expect_equal(w$edge_weights["P1", "P2"], w$edge_weights["P1", "P3"])
  expect_equal(w$edge_weights["P2", "P4"], w$edge_weights["P3", "P4"])
})

test_that("SPC edge weights on non-edges are zero", {
  w <- traversal_weights(A4, method = "spc")
  expect_equal(w$edge_weights["P1", "P4"], 0)
  expect_equal(w$edge_weights["P2", "P3"], 0)
})

test_that("traversal weights satisfy SPC <= SPLC <= SPNP", {
  spc <- traversal_weights(A6, method = "spc")$edge_weights
  splc <- traversal_weights(A6, method = "splc")$edge_weights
  spnp <- traversal_weights(A6, method = "spnp")$edge_weights
  expect_true(all(spc <= splc))
  expect_true(all(splc <= spnp))
})

test_that("normalized weights are the proportion of search paths", {
  # Two source-to-sink paths in A4, and each arc is on one of them
  w <- traversal_weights(A4, method = "spc", normalized = TRUE)
  expect_equal(w$edge_weights["P1", "P2"], 0.5)
  expect_equal(exp(w$total_log_paths), 2)
  # Search paths of Liu and Lu (2012) that leave a source for a sink
  w <- traversal_weights(L10, method = "spc", normalized = TRUE)
  expect_equal(sum(w$edge_weights[c("A", "B"), ]), 1)
})

test_that("traversal_weights cutoff zeroes post-cutoff outgoing edges", {
  years <- c(P1 = 2000, P2 = 2005, P3 = 2006, P4 = 2010)
  # P3 (2006) is after the cutoff, so the arc P3 -> P4 is not counted
  w_cut <- traversal_weights(A4, method = "spc", years = years, cutoff = 2005)
  expect_true("P3" %in% w_cut$sinks)
  expect_equal(w_cut$edge_weights["P3", "P4"], 0)
  expect_equal(w_cut$edge_weights["P1", "P3"], 1)
  expect_error(traversal_weights(A4, cutoff = 2005), "years")
})

test_that("traversal_weights stops on a cycle", {
  A_cyc <- A4
  A_cyc["P4", "P1"] <- 1
  expect_error(traversal_weights(A_cyc), "dag_check")
})


# ---- citation_decay ----------------------------------------------------------

test_that("citation_decay returns matrix of same dims as A", {
  years <- c(P1 = 2000, P2 = 2005, P3 = 2006, P4 = 2010)
  W <- citation_decay(A4, years)
  expect_equal(dim(W), dim(A4))
  expect_equal(dimnames(W), dimnames(A4))
})

test_that("citation_decay zero-weights non-edges", {
  years <- c(P1 = 2000, P2 = 2005, P3 = 2006, P4 = 2010)
  W <- citation_decay(A4, years)
  expect_equal(W["P2", "P3"], 0)
  expect_equal(W["P1", "P4"], 0)
})

test_that("citation_decay references of each citing paper sum to 1", {
  years <- c(P1 = 2000, P2 = 2005, P3 = 2006, P4 = 2010)
  W <- citation_decay(A4, years, normalize = TRUE)
  cs <- colSums(W)
  # P1 cites nobody (column sum = 0), the others should sum to 1
  expect_equal(unname(cs), c(0, 1, 1, 1))
  # P4 cites P2 (5 years before) and P3 (4 years before)
  expect_equal(W["P2", "P4"], exp(-0.2 * 5) / (exp(-0.2 * 5) + exp(-0.2 * 4)))
})

test_that("citation_decay larger lambda produces smaller weights", {
  years <- c(P1 = 2000, P2 = 2005, P3 = 2006, P4 = 2010)
  W_lo <- citation_decay(A4, years, lambda = 0.1, normalize = FALSE)
  W_hi <- citation_decay(A4, years, lambda = 0.5, normalize = FALSE)
  # P1->P2: dt = 5; exp(-0.1*5) > exp(-0.5*5)
  expect_gt(W_lo["P1", "P2"], W_hi["P1", "P2"])
  expect_equal(W_lo["P1", "P2"], exp(-0.1 * 5))
})


# ---- main_path: global -------------------------------------------------------

test_that("main_path global returns a single route from source to sink", {
  mp <- main_path(A6, method = "global")
  expect_length(mp$routes, 1L)
  route <- mp$routes[[1]]
  expect_equal(route[1], "a")
  expect_equal(route[length(route)], "f")
})

test_that("main_path global selects the highest-weight path a->b->e->f", {
  mp <- main_path(A6, method = "global")
  expect_equal(mp$routes[[1]], c("a", "b", "e", "f"))
})

test_that("main_path global nodes match route nodes", {
  mp <- main_path(A6, method = "global")
  expect_setequal(mp$nodes, mp$routes[[1]])
})

test_that("main_path global edge matrix is a subgraph of A", {
  mp <- main_path(A6, method = "global")
  # Every edge in path matrix must exist in A
  path_edges <- which(mp$edges > 0, arr.ind = TRUE)
  for (i in seq_len(nrow(path_edges))) {
    r <- rownames(mp$edges)[path_edges[i, 1]]
    cc <- colnames(mp$edges)[path_edges[i, 2]]
    expect_gt(A6[r, cc], 0)
  }
})


# ---- main_path: local --------------------------------------------------------

test_that("main_path local traces forward and backward from seed", {
  mp <- main_path(A6, method = "local", seeds = "b")
  expect_length(mp$routes, 1L)
  route <- mp$routes[[1]]
  expect_true("b" %in% route)
  expect_true("a" %in% route)                          # traced back to source
  expect_true(length(intersect(route, c("d", "f"))) > 0) # reached a sink
})

test_that("main_path local stops on missing seeds", {
  expect_error(main_path(A6, method = "local", seeds = "z"), "seeds match")
})


# ---- main_path: key_route ----------------------------------------------------

test_that("main_path key_route k=1 returns one route", {
  kr <- main_path(A6, method = "key_route", k = 1L)
  expect_length(kr$routes, 1L)
})

test_that("main_path key_route k=2 returns at most 2 routes", {
  kr <- main_path(A6, method = "key_route", k = 2L)
  expect_lte(length(kr$routes), 2L)
})

test_that("key_route k=1 node set is a subset of k=2 node set", {
  kr1 <- main_path(A6, method = "key_route", k = 1L)
  kr2 <- main_path(A6, method = "key_route", k = 2L)
  expect_true(all(kr1$nodes %in% kr2$nodes))
})

test_that("key_route edges submatrix has correct dimensions", {
  kr <- main_path(A6, method = "key_route", k = 2L)
  expect_equal(nrow(kr$edges), length(kr$nodes))
  expect_equal(ncol(kr$edges), length(kr$nodes))
})


# ---- main_path_diag ----------------------------------------------------------

test_that("main_path_diag returns k_sensitivity with correct columns", {
  diag <- main_path_diag(A6, k_values = c(1L, 2L, 3L))
  expect_true(all(c("K", "n_nodes", "n_edges", "new_nodes") %in%
                    names(diag$k_sensitivity)))
})

test_that("main_path_diag n_nodes is non-decreasing in K", {
  diag <- main_path_diag(A6, k_values = c(1L, 2L, 3L))
  nn <- diag$k_sensitivity$n_nodes
  expect_true(all(diff(nn) >= 0))
})

test_that("main_path_diag Jaccard is in [0, 1]", {
  diag <- main_path_diag(A6, k_values = c(1L, 2L), k_jaccard = 1L)
  expect_gte(diag$splc_spc_jaccard, 0)
  expect_lte(diag$splc_spc_jaccard, 1)
})

test_that("main_path_diag reports correct source and sink counts", {
  diag <- main_path_diag(A6)
  expect_equal(diag$n_sources, 1L)  # only 'a' has no incoming edges
  expect_equal(diag$n_sinks,   2L)  # 'd' and 'f' have no outgoing edges
})
