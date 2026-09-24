# ============================================================
# 10_q_analysis.R
# Author: Alejandro Espinosa-Rada, Instituto de Sociología,
#         P. Universidad Católica de Chile
# Last update: 2026-09-18
#
# Compares q_analysis() with the Python package q-analysis of
# Smirnov et al. (2025) on 20 random incidence matrices and on the
# clique complex of 20 random networks: the three structure vectors,
# the family eccentricity of Johnson, and the maximal cliques.
# Requires jsonlite and a Python with q-analysis, whose path is given
# in the environment variable QPYTHON (see 10_q_analysis.py).
#
# Output: printed table of agreements
# ============================================================

rm(list = ls())

library(here)
library(jsonlite)

pkgload::load_all(here::here(), quiet = TRUE)

folder <- tempfile("qanalysis")
dir.create(folder)

#### Random complexes ####

set.seed(42)
for (r in 1:20) {
  X <- matrix(rbinom(sample(8:25, 1) * 12, 1, runif(1, 0.1, 0.4)), ncol = 12)
  X <- X[rowSums(X) > 0, , drop = FALSE]
  dimnames(X) <- list(paste0("s", 1:nrow(X)), paste0("v", 1:12))
  write.csv(X, file.path(folder, sprintf("inc_%02d.csv", r)))

  n <- sample(8:14, 1)
  G <- matrix(rbinom(n * n, 1, runif(1, 0.2, 0.5)), n)
  G[lower.tri(G)] <- t(G)[lower.tri(G)]
  diag(G) <- 0
  dimnames(G) <- list(paste0("n", 1:n), paste0("n", 1:n))
  write.csv(G, file.path(folder, sprintf("graph_%02d.csv", r)))
}

system2(Sys.getenv("QPYTHON"), here::here("dev", "validation", "10_q_analysis.py"),
  env = paste0("QDIR=", folder)
)
python <- fromJSON(file.path(folder, "python.json"), simplifyVector = FALSE)

#### Comparison ####

checks <- list()
same <- function(a, b) isTRUE(all.equal(as.numeric(a), as.numeric(b)))
for (f in names(python)) {
  A <- as.matrix(read.csv(file.path(folder, f), row.names = 1))
  graph <- grepl("graph", f)
  mine <- q_analysis(A, simplicial_complex = !graph, eccentricity = "johnson")
  p <- python[[f]]

  # Python orders the vectors from q = 0 upwards
  upwards <- rev(seq_len(nrow(mine$q_table)))
  checks$first_vector <- c(checks$first_vector, same(mine$q_table$Q[upwards], unlist(p$FSV)))
  checks$second_vector <- c(checks$second_vector, same(mine$q_table$n[upwards], unlist(p$SSV)))
  checks$third_vector <- c(checks$third_vector, same(mine$q_table$Qbar[upwards], unlist(p$TSV)))

  # The simplices may be in another order, so they are matched by their vertices
  key_mine <- apply(mine$simplices, 1, function(x) paste(which(x > 0) - 1, collapse = "-"))
  key_python <- sapply(p$simplices, function(s) paste(sort(unlist(s)), collapse = "-"))
  checks$eccentricity <- c(
    checks$eccentricity,
    same(mine$eccentricity$eccentricity, unlist(p$ecc)[match(key_mine, key_python)])
  )
  if (graph) {
    checks$maximal_cliques <- c(checks$maximal_cliques, setequal(key_mine, key_python))
  }
}

data.frame(
  check = names(checks),
  agreements = sapply(checks, sum),
  comparisons = sapply(checks, length),
  row.names = NULL
)
