#' Modularity
#'
#' Modularity of a partition of the nodes (Newman and Girvan, 2004): the proportion of the ties
#' that are within groups, minus the proportion that would be expected if the ties were
#' distributed at random keeping the degree of every node.
#'
#' The modularity is positive when the nodes of a group are connected among themselves more
#' often than expected. It is used to compare partitions of the same network, as its maximum
#' depends on the network. For directed networks, the expected ties use the out-degree of the
#' sender and the in-degree of the receiver (Arenas et al., 2007).
#'
#' The \code{linkrank} method (Kim, Son and Jeong, 2010) replaces the ties by the flow of a
#' random walker: the value of a tie is the PageRank of the sender times the probability that
#' the walker uses that tie, and the expected value is the product of the PageRank of both
#' nodes. It takes the direction of the ties into account, which the modularity of Arenas et al.
#' does only through the degrees.
#'
#' Modularity has a resolution limit (Fortunato and Barthelemy, 2007): it does not detect groups
#' below a size that depends on the size of the network. The \code{resolution} parameter changes
#' the weight of the expected ties to look for smaller or larger groups.
#'
#' @param A   A square matrix
#' @param partition   A vector with the group of each node
#' @param method   Whether to use the modularity of \code{newman} (default) or of \code{linkrank}
#' @param digraph   Whether the matrix is directed or undirected
#' @param weighted   Whether the matrix is weighted
#' @param resolution   Weight given to the expected ties. Values above one give smaller groups
#' @param damping   Probability of following a tie in the random walk of the \code{linkrank} method
#'
#' @return This function returns the modularity of the partition.
#'
#' @references
#'
#' Arenas, A., Duch, J., Fernandez, A. and Gomez, S. (2007). Size reduction of complex networks preserving modularity. New Journal of Physics, 9(6), 176. \doi{10.1088/1367-2630/9/6/176}
#'
#' Fortunato, S. and Barthelemy, M. (2007). Resolution limit in community detection. Proceedings of the National Academy of Sciences, 104(1), 36–41. \doi{10.1073/pnas.0605965104}
#'
#' Kim, Y., Son, S.-W. and Jeong, H. (2010). Finding communities in directed networks. Physical Review E, 81(1), 016103. \doi{10.1103/PhysRevE.81.016103}
#'
#' Newman, M. E. J. and Girvan, M. (2004). Finding and evaluating community structure in networks. Physical Review E, 69(2), 026113. \doi{10.1103/PhysRevE.69.026113}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 0, 0, 0,
#'   1, 0, 1, 0, 0, 0,
#'   1, 1, 0, 1, 0, 0,
#'   0, 0, 1, 0, 1, 1,
#'   0, 0, 0, 1, 0, 1,
#'   0, 0, 0, 1, 1, 0
#' ), byrow = TRUE, ncol = 6)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- rownames(A)
#'
#' modularity_score(A, partition = c(1, 1, 1, 2, 2, 2))
#' modularity_score(A, partition = c(1, 1, 1, 2, 2, 2), method = "linkrank", digraph = TRUE)
#' @export

modularity_score <- function(A, partition, method = c("newman", "linkrank"),
                             digraph = FALSE, weighted = FALSE,
                             resolution = 1, damping = 0.85) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (length(partition) != nrow(A)) stop("The partition should have one group for each node")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  method <- match.arg(method)
  if (!weighted) {
    A[A > 0] <- 1
  }
  if (!digraph) {
    A <- pmax(A, t(A)) # Underlying graph
  }
  diag(A) <- 0

  same <- outer(partition, partition, "==")

  if (method == "linkrank") {
    if (sum(A) == 0) stop("The network has no ties")
    n <- nrow(A)
    # Google matrix: the walker follows a tie, or jumps to any node
    out <- rowSums(A)
    P <- A
    P[out > 0, ] <- A[out > 0, ] / out[out > 0]
    P[out == 0, ] <- 1 / n
    G <- damping * P + (1 - damping) / n
    pagerank <- page_rank_centrality(A, damping = damping, weighted = TRUE)
    linkrank <- pagerank * G
    expected <- outer(pagerank, pagerank)
    return(sum((linkrank - resolution * expected) * same))
  }

  if (digraph) {
    m <- sum(A)
    expected <- outer(rowSums(A), colSums(A)) / m
  } else {
    m <- sum(A) / 2
    expected <- outer(rowSums(A), rowSums(A)) / (2 * m)
  }
  if (m == 0) stop("The network has no ties")

  sum((A - resolution * expected) * same) / (if (digraph) m else 2 * m)
}


#' Communities with the leading eigenvector
#'
#' Groups of nodes that are connected among themselves more often than expected, found with the
#' leading eigenvector of the modularity matrix (Newman, 2006).
#'
#' The modularity matrix is \eqn{B = A - kk^T / 2m}, the observed ties minus the ties expected
#' from the degrees. The sign of its leading eigenvector splits the network into two groups, and
#' each group is split again while the modularity of the partition increases.
#'
#' @param A   A square matrix
#' @param digraph   Whether the matrix is directed or undirected
#' @param weighted   Whether the matrix is weighted
#' @param max_groups   Maximum number of groups
#'
#' @return This function returns the group of each node, the number of groups and the modularity of the partition.
#'
#' @references
#'
#' Newman, M. E. J. (2006). Finding community structure in networks using the eigenvectors of matrices. Physical Review E, 74(3), 036104. \doi{10.1103/PhysRevE.74.036104}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 0, 0, 0,
#'   1, 0, 1, 0, 0, 0,
#'   1, 1, 0, 1, 0, 0,
#'   0, 0, 1, 0, 1, 1,
#'   0, 0, 0, 1, 0, 1,
#'   0, 0, 0, 1, 1, 0
#' ), byrow = TRUE, ncol = 6)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- rownames(A)
#'
#' leading_eigen(A)
#' @export

leading_eigen <- function(A, digraph = FALSE, weighted = FALSE, max_groups = NULL) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  if (!weighted) {
    A[A > 0] <- 1
  }
  if (!digraph) {
    A <- pmax(A, t(A)) # Underlying graph
  }
  diag(A) <- 0
  n <- nrow(A)
  if (is.null(rownames(A))) {
    rownames(A) <- as.character(seq_len(n))
    colnames(A) <- rownames(A)
  }
  if (is.null(max_groups)) {
    max_groups <- n
  }

  degree <- rowSums(A)
  m <- sum(A) / 2
  if (m == 0) stop("The network has no ties")
  B <- A - outer(degree, degree) / (2 * m)

  partition <- rep(1, n)
  repeat {
    improved <- FALSE
    for (group in unique(partition)) {
      if (length(unique(partition)) >= max_groups) break
      members <- which(partition == group)
      if (length(members) < 2) next

      # The generalised modularity matrix of the subgraph keeps the degrees of
      # the whole network
      Bg <- B[members, members, drop = FALSE]
      diag(Bg) <- diag(Bg) - rowSums(Bg)
      vector <- Re(eigen(Bg, symmetric = TRUE)$vectors[, 1])
      side <- vector > 0
      if (all(side) | all(!side)) next

      candidate <- partition
      candidate[members[side]] <- paste0(group, "1")
      candidate[members[!side]] <- paste0(group, "2")
      if (modularity_score(A, candidate, digraph = digraph, weighted = TRUE) >
        modularity_score(A, partition, digraph = digraph, weighted = TRUE)) {
        partition <- candidate
        improved <- TRUE
      }
    }
    if (!improved) break
  }

  partition <- as.numeric(factor(partition))
  names(partition) <- rownames(A)
  return(list(
    partition = partition,
    groups = length(unique(partition)),
    modularity = modularity_score(A, partition, digraph = digraph, weighted = TRUE)
  ))
}


#' Leiden communities
#'
#' Communities found with the algorithm of Traag, Waltman and van Eck (2019), which improves
#' the algorithm of Louvain by guaranteeing that the groups it returns are internally connected.
#'
#' The algorithm has three phases that are repeated until the partition no longer changes.
#' First, every node is moved to the neighbouring group that improves the quality of the
#' partition the most, while any move improves it. Second, each group is refined: within the
#' group, the nodes start as singletons and are merged at random among the merges that improve
#' the quality, which is what keeps the groups connected. Third, the refined groups become the
#' nodes of an aggregated network, and the process starts again on it.
#'
#' Two quality functions are available. The \code{modularity} compares the ties within groups
#' with the ties expected from the degrees, and suffers from a resolution limit. The constant
#' Potts model (\code{cpm}) compares them with a constant density given by \code{resolution},
#' so the groups it finds do not depend on the size of the network.
#'
#' The refinement is random, so the result depends on the seed.
#'
#' @param A   A symmetric matrix object
#' @param resolution   Resolution of the quality function. Higher values give smaller groups
#' @param objective   Quality function: \code{modularity} (default) or \code{cpm}
#' @param theta   How random the refinement is. Small values only accept the best merges
#' @param iterations   Maximum number of times the three phases are repeated
#' @param weighted   Whether the matrix is weighted
#' @param refine   Whether the groups are refined before the aggregation. Without the refinement the algorithm is the one of Louvain (Blondel et al., 2008), which can return groups that are internally disconnected
#'
#' @return This function returns the group of each node, the number of groups, and the modularity and the quality of the partition.
#'
#' @references
#'
#' Blondel, V. D., Guillaume, J.-L., Lambiotte, R. and Lefebvre, E. (2008). Fast unfolding of communities in large networks. Journal of Statistical Mechanics, 2008(10), P10008. \doi{10.1088/1742-5468/2008/10/P10008}
#'
#' Traag, V. A., Waltman, L. and van Eck, N. J. (2019). From Louvain to Leiden: guaranteeing well-connected communities. Scientific Reports, 9, 5233. \doi{10.1038/s41598-019-41695-z}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(0, 12, 12)
#' A[1:4, 1:4] <- 1
#' A[5:8, 5:8] <- 1
#' A[9:12, 9:12] <- 1
#' diag(A) <- 0
#' A[4, 5] <- 1
#' A[5, 4] <- 1
#' A[8, 9] <- 1
#' A[9, 8] <- 1
#' rownames(A) <- letters[1:12]
#' colnames(A) <- rownames(A)
#'
#' set.seed(18051889)
#' leiden(A)
#' @export

leiden <- function(A, resolution = 1, objective = c("modularity", "cpm"),
                   theta = 0.01, iterations = 10, weighted = FALSE, refine = TRUE) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  objective <- match.arg(objective)
  if (!weighted) {
    A[A > 0] <- 1
  }
  if (!all(A[lower.tri(A)] == t(A)[lower.tri(A)])) warning("The network is directed. The underlying graph is used")
  A <- pmax(A, t(A)) # Symmetrize
  diag(A) <- 0
  n <- nrow(A)
  if (is.null(rownames(A))) {
    rownames(A) <- as.character(seq_len(n))
    colnames(A) <- rownames(A)
  }
  if (sum(A) == 0) stop("The network has no ties")

  total <- sum(A) # twice the total weight of the ties

  # The three phases are repeated, starting every time from the partition found
  # before, until the quality no longer improves
  membership <- seq_len(n)
  quality <- leiden_quality(A, membership, resolution, objective, total, rep(1, n))
  for (round in seq_len(iterations)) {
    # The aggregated network keeps the ties within a group in its diagonal, and
    # the size of a node is the number of original nodes it contains
    W <- A
    sizes <- rep(1, n)
    node_of <- seq_len(n) # node of the aggregated network of each original node
    partition <- membership

    repeat {
      partition <- leiden_move(W, sizes, partition, resolution, objective, total)
      if (length(unique(partition)) == nrow(W)) break # nothing left to aggregate

      if (refine) {
        refined <- leiden_refine(W, sizes, partition, resolution, objective, total, theta)
      } else {
        refined <- partition # Louvain aggregates the groups as they are
      }

      # Each refined group becomes a node, and starts in the group it belonged
      # to before the refinement
      partition <- as.numeric(tapply(partition, refined, function(x) x[1]))
      aggregated <- leiden_aggregate(W, sizes, refined)
      node_of <- refined[node_of]
      W <- aggregated$W
      sizes <- aggregated$sizes
    }

    candidate <- partition[node_of]
    candidate_quality <- leiden_quality(A, candidate, resolution, objective, total, rep(1, n))
    if (candidate_quality <= quality + 1e-12) break
    membership <- candidate
    quality <- candidate_quality
  }

  membership <- as.numeric(factor(membership))
  names(membership) <- rownames(A)
  return(list(
    partition = membership,
    groups = length(unique(membership)),
    modularity = modularity_score(A, membership, weighted = TRUE, resolution = resolution),
    quality = leiden_quality(A, membership, resolution, objective, total, rep(1, n))
  ))
}

# Quality of a partition: modularity or constant Potts model
leiden_quality <- function(W, membership, resolution, objective, total, sizes) {
  quality <- 0
  for (group in unique(membership)) {
    members <- membership == group
    internal <- sum(W[members, members, drop = FALSE])
    if (objective == "modularity") {
      quality <- quality + internal / total - resolution * (sum(rowSums(W)[members]) / total)^2
    } else {
      size <- sum(sizes[members])
      quality <- quality + internal / 2 - resolution * size * (size - 1) / 2
    }
  }
  quality
}

# Gain of moving a node to each group, with the node already removed from its own
leiden_gain <- function(W, sizes, membership, node, candidates, resolution, objective, total) {
  ties <- W[node, ]
  ties[node] <- 0
  gain <- rep(NA, length(candidates))
  for (k in seq_along(candidates)) {
    members <- membership == candidates[k] & seq_along(membership) != node
    if (objective == "modularity") {
      gain[k] <- 2 * sum(ties[members]) / total -
        2 * resolution * rowSums(W)[node] * sum(rowSums(W)[members]) / total^2
    } else {
      gain[k] <- sum(ties[members]) - resolution * sizes[node] * sum(sizes[members])
    }
  }
  gain
}

# Every node is moved to the group that improves the quality the most
leiden_move <- function(W, sizes, membership, resolution, objective, total) {
  repeat {
    improved <- FALSE
    for (node in seq_len(nrow(W))) {
      neighbours <- unique(membership[W[node, ] > 0])
      candidates <- unique(c(neighbours, max(membership) + 1)) # a group of its own
      gain <- leiden_gain(W, sizes, membership, node, candidates, resolution, objective, total)
      best <- candidates[which.max(gain)]
      current <- leiden_gain(W, sizes, membership, node, membership[node], resolution, objective, total)
      if (max(gain) > current + 1e-12) {
        membership[node] <- best
        improved <- TRUE
      }
    }
    if (!improved) break
  }
  as.numeric(factor(membership))
}

# Within each group, the nodes start alone and are merged at random among the
# merges that improve the quality, which keeps the groups connected
leiden_refine <- function(W, sizes, partition, resolution, objective, total, theta) {
  refined <- seq_along(partition)
  for (group in unique(partition)) {
    members <- which(partition == group)
    if (length(members) < 2) next
    for (node in members[sample.int(length(members))]) {
      if (sum(refined[members] == refined[node]) > 1) next # already merged
      candidates <- unique(refined[members])
      candidates <- candidates[candidates != refined[node]]
      if (length(candidates) == 0) next
      gain <- leiden_gain(W, sizes, refined, node, candidates, resolution, objective, total)
      gain[!is.finite(gain)] <- -Inf
      if (all(gain <= 0)) next
      # The merges that improve the quality are chosen with a probability that
      # grows with the gain
      probability <- exp(pmin(gain / theta, 700))
      probability[gain <= 0] <- 0
      refined[node] <- candidates[sample.int(length(candidates), 1, prob = probability)]
    }
  }
  as.numeric(factor(refined))
}

# The groups become the nodes of an aggregated network
leiden_aggregate <- function(W, sizes, refined) {
  groups <- sort(unique(refined))
  new_W <- matrix(0, length(groups), length(groups))
  for (i in seq_along(groups)) {
    for (j in seq_along(groups)) {
      new_W[i, j] <- sum(W[refined == groups[i], refined == groups[j], drop = FALSE])
    }
  }
  new_sizes <- tapply(sizes, refined, sum)
  list(W = new_W, sizes = as.numeric(new_sizes))
}


#' Communities with agglomeration, label propagation or edge betweenness
#'
#' Three classic ways of finding communities.
#'
#' \code{community_greedy} starts with every node alone and merges at each step the two groups
#' that increase the modularity the most, keeping the partition with the highest modularity
#' (Clauset, Newman and Moore, 2004).
#'
#' \code{community_label} gives every node a different label, and then each node takes the
#' label that most of its neighbours have, until no label changes. It is fast but the result
#' depends on the order in which the nodes are visited, so it varies between runs
#' (Raghavan, Albert and Kumara, 2007).
#'
#' \code{community_betweenness} removes, one at a time, the tie with the highest edge
#' betweenness, i.e. the tie through which most geodesics pass, as those ties connect groups
#' rather than being inside them. The components that remain at each step give a partition, and
#' the one with the highest modularity is returned (Girvan and Newman, 2002).
#'
#' @name communities
#'
#' @param A   A symmetric matrix object
#' @param weighted   Whether the matrix is weighted
#' @param max_iter   Maximum number of rounds of label propagation
#'
#' @return These functions return the group of each node, the number of groups and the modularity of the partition.
#'
#' @references
#'
#' Clauset, A., Newman, M. E. J. and Moore, C. (2004). Finding community structure in very large networks. Physical Review E, 70(6), 066111. \doi{10.1103/PhysRevE.70.066111}
#'
#' Girvan, M. and Newman, M. E. J. (2002). Community structure in social and biological networks. Proceedings of the National Academy of Sciences, 99(12), 7821–7826. \doi{10.1073/pnas.122653799}
#'
#' Raghavan, U. N., Albert, R. and Kumara, S. (2007). Near linear time algorithm to detect community structures in large-scale networks. Physical Review E, 76(3), 036106. \doi{10.1103/PhysRevE.76.036106}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 0, 0, 0,
#'   1, 0, 1, 0, 0, 0,
#'   1, 1, 0, 1, 0, 0,
#'   0, 0, 1, 0, 1, 1,
#'   0, 0, 0, 1, 0, 1,
#'   0, 0, 0, 1, 1, 0
#' ), byrow = TRUE, ncol = 6)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- rownames(A)
#'
#' community_greedy(A)
#' community_betweenness(A)
#' @export

community_greedy <- function(A, weighted = FALSE) {
  A <- community_matrix(A, weighted)
  n <- nrow(A)

  partition <- seq_len(n)
  best_partition <- partition
  best_modularity <- modularity_score(A, partition, weighted = TRUE)

  while (length(unique(partition)) > 1) {
    groups <- unique(partition)
    best_merge <- NULL
    best_gain <- -Inf
    for (i in seq_along(groups)[-length(groups)]) {
      for (j in (i + 1):length(groups)) {
        # Only the groups that are connected are worth merging
        if (sum(A[partition == groups[i], partition == groups[j], drop = FALSE]) == 0) next
        candidate <- partition
        candidate[partition == groups[j]] <- groups[i]
        gain <- modularity_score(A, candidate, weighted = TRUE)
        if (gain > best_gain) {
          best_gain <- gain
          best_merge <- candidate
        }
      }
    }
    if (is.null(best_merge)) break
    partition <- best_merge
    if (best_gain > best_modularity) {
      best_modularity <- best_gain
      best_partition <- partition
    }
  }

  community_output(A, best_partition)
}

#' @rdname communities
#' @examples
#' set.seed(18051889)
#' community_label(A)
#' @export

community_label <- function(A, weighted = FALSE, max_iter = 100) {
  A <- community_matrix(A, weighted)
  n <- nrow(A)

  partition <- seq_len(n)
  for (it in seq_len(max_iter)) {
    changed <- FALSE
    for (node in sample.int(n)) {
      neighbours <- which(A[node, ] > 0)
      if (length(neighbours) == 0) next
      # The label of the neighbours with the largest weight, with ties at random
      weight <- tapply(A[node, neighbours], partition[neighbours], sum)
      best <- names(weight)[weight == max(weight)]
      label <- as.numeric(best[sample.int(length(best), 1)])
      if (label != partition[node]) {
        partition[node] <- label
        changed <- TRUE
      }
    }
    if (!changed) break
  }

  community_output(A, partition)
}

#' @rdname communities
#' @export

community_betweenness <- function(A, weighted = FALSE) {
  A <- community_matrix(A, weighted)

  remaining <- A
  partition <- components_id(remaining)$components
  best_partition <- partition
  best_modularity <- modularity_score(A, partition, weighted = TRUE)

  while (sum(remaining) > 0) {
    betweenness <- edge_betweenness(remaining)
    cut <- which(betweenness == max(betweenness), arr.ind = TRUE)[1, ]
    remaining[cut[1], cut[2]] <- 0
    remaining[cut[2], cut[1]] <- 0

    partition <- components_id(remaining)$components
    modularity <- modularity_score(A, partition, weighted = TRUE)
    if (modularity > best_modularity) {
      best_modularity <- modularity
      best_partition <- partition
    }
  }

  community_output(A, best_partition)
}

# Edge betweenness: the geodesics that pass through each tie, accumulated with
# the algorithm of Brandes (2001)
edge_betweenness <- function(A) {
  n <- nrow(A)
  L <- tie_lengths(A, weighted = FALSE)
  betweenness <- matrix(0, n, n, dimnames = dimnames(A))

  for (s in 1:n) {
    dist <- rep(Inf, n)
    sigma <- rep(0, n)
    dist[s] <- 0
    sigma[s] <- 1
    visited <- rep(FALSE, n)
    preds <- vector("list", n)
    stack <- c()

    repeat {
      candidates <- which(!visited & is.finite(dist))
      if (length(candidates) == 0) break
      v <- candidates[which.min(dist[candidates])]
      visited[v] <- TRUE
      stack <- c(stack, v)
      for (w in which(is.finite(L[v, ]) & !visited)) {
        if (dist[v] + L[v, w] < dist[w] - 1e-10) {
          dist[w] <- dist[v] + L[v, w]
          sigma[w] <- sigma[v]
          preds[[w]] <- v
        } else if (abs(dist[v] + L[v, w] - dist[w]) <= 1e-10) {
          sigma[w] <- sigma[w] + sigma[v]
          preds[[w]] <- c(preds[[w]], v)
        }
      }
    }

    delta <- rep(0, n)
    for (w in rev(stack)) {
      for (v in preds[[w]]) {
        contribution <- (sigma[v] / sigma[w]) * (1 + delta[w])
        betweenness[v, w] <- betweenness[v, w] + contribution
        delta[v] <- delta[v] + contribution
      }
    }
  }

  betweenness <- betweenness + t(betweenness)
  betweenness / 2 # each pair is counted from both ends
}

# The matrix used by the community algorithms
community_matrix <- function(A, weighted) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  if (!weighted) {
    A[A > 0] <- 1
  }
  if (!all(A[lower.tri(A)] == t(A)[lower.tri(A)])) warning("The network is directed. The underlying graph is used")
  A <- pmax(A, t(A)) # Symmetrize
  diag(A) <- 0
  if (sum(A) == 0) stop("The network has no ties")
  if (is.null(rownames(A))) {
    rownames(A) <- as.character(seq_len(nrow(A)))
    colnames(A) <- rownames(A)
  }
  A
}

community_output <- function(A, partition) {
  partition <- as.numeric(factor(partition))
  names(partition) <- rownames(A)
  list(
    partition = partition,
    groups = length(unique(partition)),
    modularity = modularity_score(A, partition, weighted = TRUE)
  )
}
