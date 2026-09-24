# ============================================================
# 06_dynamics.R
# Author: Alejandro Espinosa-Rada, Instituto de Sociología,
#         P. Universidad Católica de Chile
# Last update: 2026-09-18
#
# Compares social_influence() with the three rules of
# loneliness_students/simulation/h3_flache_cohesion.R, the
# Friedkin-Johnsen rule with its analytical fixed point, and the
# generators with the properties they should have.
#
# Output: printed table of agreements
# ============================================================

rm(list = ls())

library(here)

pkgload::load_all(here::here(), quiet = TRUE)

# The three rules, as they are written in h3_flache_cohesion.R
sim_assim <- function(o, W, mu = 0.3, T = 60) {
  traj <- matrix(NA, T + 1, length(o))
  traj[1, ] <- o
  for (t in 1:T) {
    o <- o + mu * as.numeric(W %*% o - rowSums(W) * o)
    traj[t + 1, ] <- o
  }
  traj
}

sim_bc <- function(o, W, A, mu = 0.3, eps = 0.15, T = 60) {
  traj <- matrix(NA, T + 1, length(o))
  traj[1, ] <- o
  n <- length(o)
  for (t in 1:T) {
    onew <- o
    for (i in 1:n) {
      nb <- which(A[i, ] > 0)
      if (!length(nb)) next
      conf <- nb[abs(o[nb] - o[i]) <= eps]
      if (!length(conf)) next
      w <- W[i, conf]
      w <- w / sum(w)
      onew[i] <- o[i] + mu * sum(w * (o[conf] - o[i]))
    }
    o <- onew
    traj[t + 1, ] <- o
  }
  traj
}

sim_rep <- function(o, W, mu = 0.3, T = 60) {
  traj <- matrix(NA, T + 1, length(o))
  traj[1, ] <- o
  n <- length(o)
  for (t in 1:T) {
    onew <- o
    for (i in 1:n) {
      j <- which(W[i, ] > 0)
      if (!length(j)) next
      fw <- mu * (1 - 2 * abs(o[j] - o[i]))
      onew[i] <- o[i] + sum(W[i, j] * fw * (o[j] - o[i]))
    }
    o <- pmin(pmax(onew, 0), 1)
    traj[t + 1, ] <- o
  }
  traj
}

set.seed(11)
checks <- list()

for (r in 1:10) {
  n <- sample(8:20, 1)
  A <- 1 * (matrix(runif(n * n), n) < 0.3)
  A <- pmax(A, t(A))
  diag(A) <- 0
  if (any(rowSums(A) == 0)) next
  W <- A / rowSums(A)
  opinion <- runif(n)

  checks$assimilation <- c(
    checks$assimilation,
    isTRUE(all.equal(unname(social_influence(W, opinion, "assimilation", mu = 0.3, steps = 60)$trajectory), sim_assim(opinion, W)))
  )
  checks$bounded_confidence <- c(
    checks$bounded_confidence,
    isTRUE(all.equal(
      unname(social_influence(W, opinion, "bounded", mu = 0.3, epsilon = 0.15, steps = 60)$trajectory),
      sim_bc(opinion, W, A)
    ))
  )
  checks$repulsion <- c(
    checks$repulsion,
    isTRUE(all.equal(unname(social_influence(W, opinion, "repulsion", mu = 0.3, steps = 60)$trajectory), sim_rep(opinion, W)))
  )

  # Friedkin and Johnsen converges to the solution of its own equation
  final <- social_influence(W, opinion, "friedkin", susceptibility = 0.6, steps = 500)$final
  fixed_point <- solve(diag(n) - 0.6 * W) %*% ((1 - 0.6) * opinion)
  checks$friedkin_johnsen <- c(checks$friedkin_johnsen, isTRUE(all.equal(unname(final), as.numeric(fixed_point))))

  # The opinions of a connected network converge to a single value
  checks$consensus <- c(checks$consensus, var(social_influence(W, opinion, "assimilation", steps = 500)$final) < 1e-8)
}

# Threshold diffusion on a network whose result can be followed by hand
chain <- matrix(0, 5, 5)
chain[1, 2] <- 1
chain[2, 3] <- 1
chain[3, 4] <- 1
chain[4, 5] <- 1
chain <- pmax(chain, t(chain))
rownames(chain) <- letters[1:5]
colnames(chain) <- rownames(chain)
checks$threshold_chain <- all(threshold_diffusion(chain, seeds = "a", threshold = 1, mode = "count")$time == 0:4)
checks$threshold_nobody <- threshold_diffusion(chain, seeds = "a", threshold = 1.1)$adopters == 1 / 5
checks$threshold_everybody <- threshold_diffusion(chain, seeds = "a", threshold = 0)$adopters == 1

# The ring lattice has the transitivity that the theory gives it
lattice <- small_world(30, neighbours = 2, p = 0)
checks$ring_lattice <- all(rowSums(lattice) == 4) &&
  isTRUE(all.equal(trans_coef(lattice, method = "global"), 3 * (4 - 2) / (4 * (4 - 1))))

set.seed(4)
rewired <- small_world(30, neighbours = 2, p = 1)
checks$rewiring_keeps_ties <- sum(rewired) == sum(lattice)
checks$rewiring_shortens <- geo_summary(rewired, digraph = FALSE)$average_distance <
  geo_summary(lattice, digraph = FALSE)$average_distance

scale_free <- pref_attachment(200, m = 2)
checks$preferential_ties <- sum(scale_free) / 2 == 2 * (200 - 2)
checks$preferential_order <- cor(1:200, rowSums(scale_free), method = "spearman") < 0

reference <- rep("simulation script", 3)
reference <- c(reference, rep("analytical or known result", length(checks) - 3))

data.frame(
  check = names(checks),
  agreements = sapply(checks, sum),
  comparisons = sapply(checks, length),
  reference = reference,
  row.names = NULL
)
