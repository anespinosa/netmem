# ============================================================
# 11_overlapping_categories.R
# Author: Alejandro Espinosa-Rada, Instituto de Sociología,
#         P. Universidad Católica de Chile
# Last update: 2026-09-18
#
# Compares the measures with overlapping categories with Everett
# and Borgatti (2026), Tables 3, 4, 8, 9 and 10 (example of the
# article) and Tables 5 and 6 (Campnet), and on random networks
# with sna::brokerage() for a partition, igraph::constraint() for
# the whole network, and eb_constraint() and redundancy() for the
# ego networks. Tables 8 and 9 are compared with Chuck spending 12
# of 51 hours on G3, as the values of the article suggest.
#
# Output: printed table of agreements
# ============================================================

rm(list = ls())

library(here)
library(igraph)
library(sna)

pkgload::load_all(here::here(), quiet = TRUE)

checks <- list()
# Missing in both (such as the constraint of an isolate, NA here and NaN in
# igraph) counts as an agreement
close_to <- function(a, b, tol) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  if (!identical(is.na(a), is.na(b))) {
    return(FALSE)
  }
  known <- !is.na(a)
  all(abs(a[known] - b[known]) <= tol)
}

#### Example of the article ####

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
  0, 20, 20, 0, 0, 0, 11, 33, 30, 0, 0, 0, 4, 16, 10, 10, 0, 2, 2, 16, 50, 0, 0, 0,
  0, 55, 0, 0, 20, 10, 10, 0, 4, 4, 4, 28, 0, 0, 54, 6, 8, 0, 32, 0
), byrow = TRUE, ncol = 4, dimnames = list(nodes, paste0("G", 1:4)))
hours_chuck <- hours
hours_chuck["Chuck", ] <- c(0, 0, 12, 39)

checks$table3 <- close_to(alter_composition(A, hours), rbind(
  c(1.2, 0, 0.8, 0), c(2.5, 0.75, 0.75, 0), c(0.5, 0.35, 0.6, 1.55), c(1.7, 0.75, 1.55, 0),
  c(2.8, 0.65, 1.3, 0.25), c(0.1, 2, 1.75, 1.15), c(1, 0.5, 0.5, 0), c(1.3, 0.2, 1.25, 2.25),
  c(0.8, 1.15, 1.8, 0.25), c(2, 1, 0, 0), c(0, 0, 0.25, 0.75)
), 1e-9)
checks$table4 <- close_to(alter_heterogeneity(A, hours), c(0.48, 0.54, 0.65, 0.63, 0.60, 0.66, 0.63, 0.67, 0.67, 0.44, 0.38), 0.005 + 1e-9)
checks$table8_trait <- close_to(
  cbind(alter_homophily(A, hours, similarity = "minimum"), alter_homophily(A, hours, "yule", "minimum")),
  cbind(
    c(0.500, 0.750, 0.667, 0.150, 0.700, 0.960, 0.500, 0.320, 0.375, 1, 0.500),
    c(-0.235, -0.711, -0.111, 0.069, -0.645, -0.930, 0.474, -0.169, -0.120, -1, -0.151)
  ), 0.0005 + 1e-9
)
checks$table8_ei_yule <- close_to(
  cbind(alter_homophily(A, hours_chuck), alter_homophily(A, hours_chuck, "yule")),
  cbind(
    c(0.60, 0.91, 0.67, 0.57, 0.84, 0.96, 0.50, 0.60, 0.73, 1, 0.62),
    c(-0.09, -0.82, -0.11, -0.11, -0.69, -0.93, 0.47, -0.23, -0.39, -1, -0.17)
  ), 0.005 + 1e-9
)
checks$table9 <- close_to(brokerage_roles(A, hours_chuck)[, 1:5], rbind(
  c(0.09, 1.11, 0.62, 0.39, 1.78), c(0.17, 0.24, 0.70, 2.68, 4.21), c(0, 0.5, 0, 1.95, 2.55),
  c(0.11, 0.75, 0.60, 0.79, 1.75), c(0.02, 0.72, 0.08, 3.70, 3.48), c(0, 0.4, 0.3, 5.2, 14.1),
  c(0, 0.5, 0, 0.45, 0.05), c(0.38, 1.94, 2.04, 4.62, 5.01), c(0.05, 0.3, 0.15, 0.42, 1.08),
  c(0, 0, 0, 1, 0), c(0.18, 0.57, 0.62, 1.38, 1.24)
), 0.005 + 1e-9)
standard <- structural_holes(A)
categories <- structural_holes(A, hours)
checks$table10 <- close_to(
  cbind(standard$effective_size, standard$constraint, categories$effective_size, categories$constraint),
  cbind(
    c(3.786, 3.571, 2.857, 3.571, 3.438, 5.045, 1.500, 3.750, 1.800, 2.250, 3.417),
    c(0.543, 0.570, 0.637, 0.537, 0.544, 0.376, 1.042, 0.463, 0.687, 0.757, 0.557),
    c(3.148, 2.612, 2.004, 2.935, 2.382, 3.877, 1.275, 2.784, 1.671, 1.500, 2.586),
    c(0.561, 0.641, 0.739, 0.566, 0.638, 0.474, 1.022, 0.533, 0.747, 1.042, 0.577)
  ), 0.0005 + 1e-9
)

#### Campnet ####

data(campnet)
gender <- partition_centrality(campnet$network, campnet$attributes$gender)
checks$table5 <- close_to(gender, cbind(
  c(24.3, 0, 1.3, 8.5, 25, 6.3, 7, 0.5, 9, 0, 0, 14, 0, 0, 10, 6, 6, 11),
  c(54, 0, 0, 24, 14.5, 0, 5.5, 0, 49.8, 0, 5, 2.3, 0, 2.3, 44.7, 10.8, 7.7, 36.3)
), 0.05 + 1e-9)
U <- pmax(campnet$network, t(campnet$network))
cliques <- clique_max(U, min = 3)
K <- matrix(0, 18, length(cliques), dimnames = list(rownames(U), NULL))
for (k in seq_along(cliques)) {
  K[cliques[[k]], k] <- 1
}
# The cliques are in another order than in the article, so the sums are sorted
checks$table6 <- length(cliques) == 10 && close_to(
  sort(colSums(partition_centrality(campnet$network, K))),
  sort(c(21.1, 138.2, 17.2, 14.2, 23.5, 16.1, 32.0, 43.2, 35.9, 44.9)), 0.15
)

#### Random networks ####

set.seed(3)
for (r in 1:20) {
  n <- sample(8:14, 1)
  D <- matrix(rbinom(n * n, 1, 0.3), n)
  diag(D) <- 0
  dimnames(D) <- list(paste0("v", 1:n), paste0("v", 1:n))
  att <- sample(1:3, n, TRUE)

  # sna: w_I coordinator, w_O itinerant, b_IO representative, b_OI gatekeeper, b_O liaison
  mine <- brokerage_roles(D, att)[, c("coordinator", "consultant", "representative", "gatekeeper", "liaison")]
  theirs <- sna::brokerage(D, att)$raw.nli[, c("w_I", "w_O", "b_IO", "b_OI", "b_O")]
  checks$brokerage_sna <- c(checks$brokerage_sna, close_to(mine, theirs, 1e-9))

  reference <- igraph::constraint(graph_from_adjacency_matrix(D, mode = "directed"))
  checks$constraint_igraph <- c(checks$constraint_igraph, close_to(structural_holes(D, ego_network = FALSE)$constraint, reference, 1e-9))

  S <- pmax(D, t(D))
  holes <- structural_holes(S)
  for (e in rownames(S)[rowSums(S) > 1]) {
    checks$constraint_eb <- c(checks$constraint_eb, close_to(round(holes[e, "constraint"], 3), eb_constraint(S, ego = e)$results$constraint, 1e-9))
    checks$effective_size <- c(checks$effective_size, close_to(holes[e, "effective_size"], redundancy(S, ego = e)$effective_size, 1e-9))
  }
}

data.frame(
  check = names(checks),
  agreements = sapply(checks, sum),
  comparisons = sapply(checks, length),
  row.names = NULL
)
