context("Test the bugs found in the audit of version 1.1-0")

# Each test reproduces a bug found by dev/audit (layer 1: awkward inputs;
# layer 2: comparison with other implementations or with the definitions)

U <- matrix(c(
  0, 1, 1, 0, 0,
  1, 0, 1, 1, 0,
  1, 1, 0, 1, 0,
  0, 1, 1, 0, 1,
  0, 0, 0, 1, 0
), byrow = TRUE, ncol = 5, dimnames = list(letters[1:5], letters[1:5]))

test_that("missing values are treated as absent ties", {
  M <- U
  M["a", "b"] <- NA
  expect_equal(k_core(M), k_core(ifelse(is.na(M), 0, M))) # used to loop forever
  expect_equal(bfs_ugraph(M), bfs_ugraph(ifelse(is.na(M), 0, M)))
  expect_equal(gen_degree(M, digraph = FALSE), gen_degree(ifelse(is.na(M), 0, M), digraph = FALSE))
  expect_equal(nrow(matrix_to_edgelist(M)), 6)
  expect_equal(short_path(M, from = "a", to = "e"), short_path(ifelse(is.na(M), 0, M), from = "a", to = "e"))
  expect_error(short_path(unname(U), from = "a", to = "e"), "No label")
})

test_that("a tie in either direction is an edge of the underlying graph", {
  D <- matrix(0, 3, 3, dimnames = list(letters[1:3], letters[1:3]))
  D["b", "a"] <- 1 # only in the lower triangle
  expect_equal(unname(gen_degree(D, digraph = FALSE)), c(1, 1, 0))
  expect_equal(sum(clique_max(pmax(D, t(D)), min = 2)[[1]] %in% c("a", "b")), 2)
  S <- matrix(0, 3, 3)
  S[1, 2] <- 1
  S[2, 1] <- -1 # the negative tie is kept when both have the same magnitude
  expect_equal(underlying_signed(S)[1, 2], -1)
  expect_equal(underlying_signed(S)[2, 1], -1)
})

test_that("edge lists and adjacency lists keep every tie", {
  E <- rbind(c("b", "a"), c("c", "b")) # undirected ties listed in any order
  expect_equal(sum(edgelist_to_matrix(E, digraph = FALSE)), 4)
  W <- matrix(c(0, 0.5, 2, 0.5, 0, -1, 2, -1, 0), 3, dimnames = list(letters[1:3], letters[1:3]))
  expect_equal(matrix_to_edgelist(W, valued = TRUE)[, 3], c("0.5", "2", "-1"))
  expect_equal(nrow(matrix_to_edgelist(W, valued = TRUE, digraph = TRUE)), 6)
  single <- matrix(c(0, 1, 0, 1, 0, 0, 0, 0, 0), 3, dimnames = list(letters[1:3], letters[1:3]))
  expect_true(is.matrix(matrix_to_edgelist(single)))
  expect_equal(matrix_adjlist(W)$a, c("b", "c"))
  expect_equal(names(matrix_adjlist(unname(single))), c("1", "2", "3"))
  expect_equal(dim(edgelist_to_matrix(matrix(character(0), ncol = 2), label = c("a", "b"))), c(2, 2))
  # The labels give the order of the nodes, and the others follow in alphabetical order
  E <- rbind(c("z", "b"), c("b", "a"))
  expect_equal(rownames(edgelist_to_matrix(E)), c("a", "b", "z"))
  expect_equal(rownames(edgelist_to_matrix(E, label = c("z", "c"))), c("z", "c", "a", "b"))
  expect_equal(edgelist_to_matrix(E, label = c("z", "b", "a"))["z", "b"], 1)
  X <- edgelist_to_matrix(E, bipartite = TRUE, label = c("z", "b"), label2 = c("b", "a", "q"))
  expect_equal(dimnames(X), list(c("z", "b"), c("b", "a", "q")))
  expect_error(edgelist_to_matrix(matrix(character(0), ncol = 2)), "no ties")
  groups <- rbind(c("a", "b", "c"), c("a", "c", NA), c("b", "c", NA), c("c", NA, NA), c("c", "a", NA))
  expect_equal(unname(adj_to_matrix(groups, loops = TRUE)), rbind(c(1, 1, 1), c(0, 1, 1), c(1, 0, 1)))
  expect_equal(colnames(adj_to_matrix(rbind(c("a", "d", NA), c("b", "a", "d")))), c("a", "b", "d"))
})

test_that("adj_to_incidence handles networks without ties and names the edges", {
  expect_equal(dim(adj_to_incidence(U * 0)), c(5, 0))
  I <- adj_to_incidence(U, directed = FALSE)
  expect_equal(ncol(I), 6)
  expect_equal(colnames(I)[1], "a-b")
})

test_that("ego_net, redundancy and minmax_overlap work with a single alter or row", {
  A <- matrix(0, 3, 3, dimnames = list(letters[1:3], letters[1:3]))
  A["a", "b"] <- A["b", "a"] <- 1
  expect_equal(dimnames(ego_net(A, "a")), list("b", "b"))
  expect_equal(redundancy(A, ego = "a")$effective_size, 1)
  expect_equal(minmax_overlap(matrix(1:3, 1)), matrix(6, 1, 1))
})

test_that("extract_component returns every component of the same size", {
  A <- matrix(0, 7, 7, dimnames = list(letters[1:7], letters[1:7]))
  A["a", "b"] <- A["b", "a"] <- A["b", "c"] <- A["c", "b"] <- 1
  A["e", "f"] <- A["f", "e"] <- A["f", "g"] <- A["g", "f"] <- 1
  largest <- extract_component(A)
  expect_length(largest, 2)
  expect_equal(rownames(largest[[2]]), c("e", "f", "g"))
  expect_equal(rownames(extract_component(A, maximum = FALSE, position = 2)), "d")
})

test_that("meta_matrix places the ties between levels in both triangles", {
  A1 <- matrix(c(0, 1, 1, 0), 2)
  B1 <- matrix(c(1, 0, 1, 1, 0, 1), 2)
  B2 <- matrix(c(1, 0, 0, 1, 1, 0), 3)
  B3 <- matrix(c(1, 0, 0, 1), 2)
  M <- meta_matrix(A1, B1, B2 = B2, B3 = B3)
  expect_true(isSymmetric(M))
  expect_equal(M[3:5, 6:7], B2)
  expect_equal(M[6:7, 1:2], B3)
})

test_that("the censuses count every triple once", {
  set.seed(1)
  A <- matrix(rbinom(36, 1, 0.4), 6)
  diag(A) <- 0
  B1 <- matrix(rbinom(18, 1, 0.4), 6)
  B2 <- matrix(rbinom(24, 1, 0.4), 6)
  expect_equal(sum(suppressWarnings(mixed_census(A, B1, B2, quad = TRUE))), choose(6, 2) * 3 * 4)
  # Two empty networks: every triple is 003 in both
  expect_equal(unname(multiplex_census(matrix(0, 5, 5), matrix(0, 5, 5))["003_003"]), choose(5, 3))
  # Figure 12 of Espinosa-Rada (2021): the edge of the second network on the
  # arc of 012, and on one of the two equivalent arcs of 021U
  A3 <- matrix(0, 3, 3)
  A3[1, 2] <- 1
  B3 <- matrix(0, 3, 3)
  B3[1, 2] <- B3[2, 1] <- 1
  expect_equal(names(which(multiplex_census(A3, B3) == 1)), "012_102a")
  A3 <- matrix(0, 3, 3)
  A3[1, 3] <- A3[2, 3] <- 1
  B3 <- matrix(0, 3, 3)
  B3[2, 3] <- B3[3, 2] <- 1
  expect_equal(names(which(multiplex_census(A3, B3) == 1)), "021U_102ac")
  # With merge = "overlap", the groups of the mutual dyad in Figure 12
  merged <- names(multiplex_census(A3, B3, merge = "overlap"))
  expect_true(all(c("102_003-102a", "102_102bc-201ac", "102_201b-300") %in% merged))
  # The counts of each type of the first network are its triad census (sna 2.8),
  # and relabelling the nodes does not change the census
  A <- matrix(c(0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0), 5, byrow = TRUE)
  B <- matrix(c(0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0), 5, byrow = TRUE)
  census <- multiplex_census(A, B)
  first <- factor(sub("_.*", "", names(census)), levels = unique(sub("_.*", "", names(census))))
  expect_equal(unname(as.numeric(tapply(census, first, sum))), c(0, 1, 0, 1, 1, 5, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0))
  second <- factor(substr(sub("^[^_]*_", "", names(census)), 1, 3), levels = c("003", "102", "201", "300"))
  expect_equal(unname(as.numeric(tapply(census, second, sum))), c(1, 6, 3, 0))
  o <- c(3, 5, 1, 4, 2)
  expect_equal(multiplex_census(A[o, o], B[o, o]), census)
})

test_that("kp_reciprocity uses the number of arcs", {
  data(krackhardt_friends)
  G <- krackhardt_friends
  g <- nrow(G)
  M <- sum(G * t(G)) / 2
  L <- sum(G)
  L2 <- sum(rowSums(G)^2)
  expect_equal(kp_reciprocity(G), (2 * (g - 1)^2 * M - L^2 + L2) / (L * (g - 1)^2 - L^2 + L2))
})

test_that("signed measures agree with the definitions and with signnet", {
  # Names that would be confused when pasted: "1", "12" and "2"
  S <- matrix(c(
    0, 1, -1, 1,
    1, 0, 1, 0,
    -1, 1, 0, -1,
    1, 0, -1, 0
  ), byrow = TRUE, ncol = 4, dimnames = list(c("1", "12", "2", "21"), c("1", "12", "2", "21")))
  # Triangles 1-12-2 (+ - +: unbalanced) and 1-2-21 (- + -: balanced)
  expect_equal(struc_balance(S)$balance_score, 1 / 2)
  # signnet 1.0: pn_index of a directed network
  D <- matrix(c(0, 1, -1, 0, 0, 0, 0, 1, -1, 0, 1, 0, 0, 0, -1, 0, -1, 1, 0, 1, -1, 0, 0, 1, 0), 5, byrow = TRUE)
  expect_equal(unname(posneg_index(D, select = "in")), c(0.888412, 0.848545, 1.030092, 0.884119, 0.907732), tolerance = 1e-6)
  expect_equal(unname(posneg_index(D, select = "out")), c(0.848545, 0.907732, 0.884119, 1.030092, 0.888412), tolerance = 1e-6)
})

test_that("similarities of two-mode matrices agree with stats::dist", {
  set.seed(1)
  X <- matrix(rbinom(15, 1, 0.5), 5, 3, dimnames = list(letters[1:5], NULL))
  for (Z in list(X, t(X))) {
    expect_equal(unname(dist_sim_matrix(Z, method = "hamming", bipartite = TRUE)), unname(as.matrix(dist(Z, method = "manhattan"))))
    reference <- as.matrix(dist(Z, method = "binary"))
    reference[is.na(reference)] <- 0
    expect_equal(unname(dist_sim_matrix(Z, method = "jaccard", bipartite = TRUE)), unname(reference))
  }
  expect_equal(rownames(dist_sim_matrix(X, bipartite = TRUE)), letters[1:5])
  expect_equal(dim(co_occurrence(matrix(c(1, 1, 0, 0, 1, 1), 2, byrow = TRUE), occurrence = FALSE)), c(3, 3))
  expect_equal(jaccard(matrix(1, 3, 3), matrix(c(1, 0, 1, 1, 1, 0, 0, 1, 1), 3))$jaccard, 0.5)
})

test_that("the weighted k-core follows the definition", {
  P <- matrix(0, 3, 3, dimnames = list(letters[1:3], letters[1:3]))
  P["a", "b"] <- P["b", "a"] <- P["b", "c"] <- P["c", "b"] <- 1
  expect_equal(unname(k_core(P, weighted = TRUE)), c(1, 1, 1))
  expect_equal(unname(k_core(U, weighted = TRUE)), unname(k_core(U)))
  W <- U * 2
  expect_equal(unname(k_core(W, weighted = TRUE)), 2 * unname(k_core(U)))
})

test_that("ind_rand_matrix places exactly l ties", {
  set.seed(1)
  M <- ind_rand_matrix(10, type = "edges", l = 12, digraph = FALSE) # used to loop forever
  expect_equal(sum(M[upper.tri(M)]), 12)
  expect_true(isSymmetric(M))
  expect_equal(sum(ind_rand_matrix(10, type = "edges", l = 12)), 12)
  expect_error(ind_rand_matrix(4, type = "edges", l = 20), "possible ties")
})
