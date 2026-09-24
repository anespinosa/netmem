context("Test measures with overlapping categories")

# Everett and Borgatti (2026): advice network (Table 1) and hours spent on four
# tasks (Table 2)
nodes <- c("Barb", "Chuck", "Dave", "Ellen", "Frank", "Gavin", "Helen", "Ilse", "Joe", "Karen", "Larry")
A <- matrix(c(
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1,
  1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0,
  0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0,
  1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1,
  0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1,
  1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0,
  1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
  0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1,
  1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1,
  0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0
), byrow = TRUE, ncol = 11, dimnames = list(nodes, nodes))
hours <- matrix(c(
  0, 20, 20, 0,
  0, 0, 11, 33,
  30, 0, 0, 0,
  4, 16, 10, 10,
  0, 2, 2, 16,
  50, 0, 0, 0,
  0, 55, 0, 0,
  20, 10, 10, 0,
  4, 4, 4, 28,
  0, 0, 54, 6,
  8, 0, 32, 0
), byrow = TRUE, ncol = 4, dimnames = list(nodes, paste0("G", 1:4)))
groups <- c("G2", "G4", "G1", "G2", "G4", "G1", "G2", "G1", "G4", "G3", "G3")

# The E-I index, Yule's Q and the brokerage roles of the article are reproduced
# within rounding when Chuck spends 12 of 51 hours on G3 instead of 11 of 44; the
# other tables use the memberships as printed
hours_chuck <- hours
hours_chuck["Chuck", ] <- c(0, 0, 12, 39)

test_that("alter_composition reproduces Tables 1 and 3", {
  expect_equal(unname(alter_composition(A, groups)["Chuck", ]), c(3, 1, 0, 0))
  expect_equal(unname(alter_composition(A, hours)), rbind(
    c(1.2, 0, 0.8, 0), c(2.5, 0.75, 0.75, 0), c(0.5, 0.35, 0.6, 1.55), c(1.7, 0.75, 1.55, 0),
    c(2.8, 0.65, 1.3, 0.25), c(0.1, 2, 1.75, 1.15), c(1, 0.5, 0.5, 0), c(1.3, 0.2, 1.25, 2.25),
    c(0.8, 1.15, 1.8, 0.25), c(2, 1, 0, 0), c(0, 0, 0.25, 0.75)
  ))
  # The rows add up to the degree
  expect_equal(rowSums(alter_composition(A, hours)), rowSums(A))
  expect_equal(unname(rowSums(alter_composition(A, hours, proportion = TRUE))), rep(1, 11))
})

test_that("alter_heterogeneity reproduces Table 4", {
  expect_equal(
    unname(alter_heterogeneity(A, hours)),
    c(0.48, 0.54, 0.65, 0.63, 0.60, 0.66, 0.63, 0.67, 0.67, 0.44, 0.38),
    tolerance = 0.006
  )
  # Blau's index of a partition, and the IQV
  expect_equal(unname(alter_heterogeneity(A, groups)["Chuck"]), 1 - (3 / 4)^2 - (1 / 4)^2)
  expect_equal(
    unname(alter_heterogeneity(A, groups, normalized = TRUE)["Chuck"]),
    (1 - (3 / 4)^2 - (1 / 4)^2) / (1 - 1 / 4)
  )
})

test_that("alter_homophily reproduces Table 8", {
  # Trait versions, with the memberships as printed
  expect_equal(
    unname(alter_homophily(A, hours, similarity = "minimum")),
    c(0.500, 0.750, 0.667, 0.150, 0.700, 0.960, 0.500, 0.320, 0.375, 1.000, 0.500),
    tolerance = 0.001
  )
  expect_equal(
    unname(alter_homophily(A, hours, method = "yule", similarity = "minimum")),
    c(-0.235, -0.711, -0.111, 0.069, -0.645, -0.930, 0.474, -0.169, -0.120, -1.000, -0.151),
    tolerance = 0.001
  )
  # E-I and Yule's Q
  expect_equal(
    unname(alter_homophily(A, hours_chuck)),
    c(0.60, 0.91, 0.67, 0.57, 0.84, 0.96, 0.50, 0.60, 0.73, 1.00, 0.62),
    tolerance = 0.006
  )
  expect_equal(
    unname(alter_homophily(A, hours_chuck, method = "yule")),
    c(-0.09, -0.82, -0.11, -0.11, -0.69, -0.93, 0.47, -0.23, -0.39, -1.00, -0.17),
    tolerance = 0.006
  )
  # With the memberships as printed, Larry has I = 0.8 * 0.25 (Eq. 2)
  expect_equal(unname(alter_homophily(A, hours)["Larry"]), (1 - 2 * 0.2) / 1)
  # With a partition, (E - I) / degree
  expect_equal(unname(alter_homophily(A, groups)["Dave"]), (2 - 1) / 3)
  # Two nodes with the same memberships are fully similar with the cosine
  expect_equal(unname(alter_homophily(A, groups, similarity = "cosine")), unname(alter_homophily(A, groups)))
})

test_that("brokerage_roles reproduces Table 9", {
  roles <- brokerage_roles(A, hours_chuck)
  expect_equal(unname(roles[, "total"]), c(4, 8, 5, 4, 8, 20, 1, 14, 2, 1, 4))
  expect_equal(unname(roles[, 1:5]), rbind(
    c(0.09, 1.11, 0.62, 0.39, 1.78), c(0.17, 0.24, 0.70, 2.68, 4.21), c(0, 0.5, 0, 1.95, 2.55),
    c(0.11, 0.75, 0.60, 0.79, 1.75), c(0.02, 0.72, 0.08, 3.70, 3.48), c(0, 0.4, 0.3, 5.2, 14.1),
    c(0, 0.5, 0, 0.45, 0.05), c(0.38, 1.94, 2.04, 4.62, 5.01), c(0.05, 0.3, 0.15, 0.42, 1.08),
    c(0, 0, 0, 1, 0), c(0.18, 0.57, 0.62, 1.38, 1.24)
  ), tolerance = 0.006)
  # The parts of each path add up to one
  expect_equal(rowSums(roles[, 1:5]), roles[, "total"])
})

test_that("brokerage_roles agrees with sna::brokerage for a partition", {
  # Values computed with sna 2.8 and hard-coded
  D <- matrix(c(
    0, 1, 0, 0, 1,
    0, 0, 1, 1, 0,
    1, 0, 0, 1, 0,
    0, 0, 0, 0, 1,
    0, 1, 1, 0, 0
  ), byrow = TRUE, ncol = 5, dimnames = list(letters[1:5], letters[1:5]))
  roles <- brokerage_roles(D, c(1, 1, 2, 2, 3))
  # sna: w_I coordinator, w_O itinerant (consultant), b_IO representative,
  # b_OI gatekeeper, b_O liaison and t total
  expect_equal(unname(roles[, "coordinator"]), c(0, 0, 0, 0, 0))
  expect_equal(unname(roles[, "consultant"]), c(0, 0, 1, 0, 1))
  expect_equal(unname(roles[, "representative"]), c(0, 2, 0, 1, 0))
  expect_equal(unname(roles[, "gatekeeper"]), c(1, 0, 1, 0, 0))
  expect_equal(unname(roles[, "liaison"]), c(1, 1, 1, 1, 2))
  expect_equal(unname(roles[, "total"]), c(2, 3, 3, 2, 3))
})

test_that("structural_holes reproduces Table 10", {
  standard <- structural_holes(A)
  expect_equal(standard$alters, c(6, 5, 4, 6, 5, 6, 3, 6, 4, 3, 6))
  expect_equal(standard$effective_size,
    c(3.786, 3.571, 2.857, 3.571, 3.438, 5.045, 1.500, 3.750, 1.800, 2.250, 3.417),
    tolerance = 0.001
  )
  expect_equal(standard$constraint,
    c(0.543, 0.570, 0.637, 0.537, 0.544, 0.376, 1.042, 0.463, 0.687, 0.757, 0.557),
    tolerance = 0.001
  )
  categories <- structural_holes(A, hours)
  expect_equal(categories$effective_size,
    c(3.148, 2.612, 2.004, 2.935, 2.382, 3.877, 1.275, 2.784, 1.671, 1.500, 2.586),
    tolerance = 0.001
  )
  expect_equal(categories$constraint,
    c(0.561, 0.641, 0.739, 0.566, 0.638, 0.474, 1.022, 0.533, 0.747, 1.042, 0.577),
    tolerance = 0.001
  )
  # beta = 0 gives the original measures
  expect_equal(structural_holes(A, hours, beta = 0), standard)
})

test_that("structural_holes agrees with eb_constraint, redundancy and igraph", {
  U <- matrix(c(
    0, 1, 1, 0, 0, 1,
    1, 0, 1, 0, 0, 1,
    1, 1, 0, 0, 0, 1,
    0, 0, 0, 0, 1, 1,
    0, 0, 0, 1, 0, 1,
    1, 1, 1, 1, 1, 0
  ), byrow = TRUE, ncol = 6, dimnames = list(letters[1:6], letters[1:6]))
  holes <- structural_holes(U)
  expect_equal(round(holes["f", "constraint"], 3), eb_constraint(U, ego = "f")$results$constraint)
  expect_equal(holes["f", "effective_size"], redundancy(U, ego = "f")$effective_size)
  # igraph::constraint (igraph 2.2.1) uses the whole network
  expect_equal(
    structural_holes(A, ego_network = FALSE)$constraint,
    c(0.3912366, 0.3782021, 0.4480342, 0.4174461, 0.3762748, 0.2966457, 0.7174508, 0.3818536, 0.4613905, 0.5693440, 0.4111072),
    tolerance = 1e-6
  )
})

test_that("partition_centrality reproduces Tables 5 and 6 with Campnet", {
  data(campnet)
  gender <- partition_centrality(campnet$network, campnet$attributes$gender)
  expect_equal(unname(round(gender[, 1], 1)), c(24.3, 0, 1.3, 8.5, 25, 6.3, 7, 0.5, 9, 0, 0, 14, 0, 0, 10, 6, 6, 11))
  expect_equal(unname(round(gender[, 2], 1)), c(54, 0, 0, 24, 14.5, 0, 5.5, 0, 49.8, 0, 5, 2.3, 0, 2.3, 44.7, 10.8, 7.7, 36.3))
  expect_equal(rowSums(gender), betweenness_centrality(campnet$network))
  # Cliques of the symmetrized network: Holly receives from every clique
  U <- pmax(campnet$network, t(campnet$network))
  cliques <- clique_max(U, min = 3)
  expect_length(cliques, 10)
  K <- matrix(0, 18, length(cliques), dimnames = list(rownames(U), NULL))
  for (k in seq_along(cliques)) {
    K[cliques[[k]], k] <- 1
  }
  clique_partition <- partition_centrality(campnet$network, K)
  expect_equal(unname(sort(round(clique_partition["HOLLY", ], 1))), c(3.5, 3.5, 5, 5.5, 5.8, 7, 7, 9, 15, 17))
  expect_equal(max(colSums(clique_partition)), 138.2, tolerance = 0.001)
  # Degree: the alter composition
  expect_equal(partition_centrality(A, hours, measure = "degree"), alter_composition(A, hours))
})

test_that("the memberships are checked", {
  expect_error(alter_composition(A, hours[1:5, ]), "rows of B")
  empty <- hours
  empty[1, ] <- 0
  expect_error(alter_homophily(A, empty), "at least one category")
  expect_error(brokerage_roles(A, c(NA, groups[-1])), "category of every node")
})
