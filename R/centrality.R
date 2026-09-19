#' Closeness centrality
#'
#' Closeness centrality of Freeman (1978) and its harmonic version (Marchiori and Latora, 2000; Rochat, 2009).
#'
#' Freeman's closeness is the inverse of the sum of the geodesic distances from a node to the others.
#' When the network is disconnected, the sum only considers the nodes that can be reached, and the
#' harmonic version is recommended, as it adds the inverse of each distance (an unreachable node adds zero).
#'
#' For valued matrices, the tie weights are treated as strengths and transformed into lengths
#' as \eqn{1 / w^{\alpha}} (Opsahl et al., 2010). If \code{alpha = 0} the binary network is used.
#'
#' @param A   A square matrix
#' @param digraph   Whether the matrix is directed or undirected
#' @param type   Whether to use the \code{out} (default), \code{in} or \code{all} distances. The \code{all} option uses the underlying graph
#' @param weighted   Whether the matrix is weighted
#' @param alpha   The tuning parameter of Opsahl et al. (2010) to transform weights into lengths
#' @param harmonic   Whether to return the harmonic closeness
#' @param normalized   If TRUE, Freeman's closeness is multiplied by the number of nodes reached, and the harmonic closeness is divided by (n-1)
#'
#' @return This function returns the closeness centrality of the nodes.
#'
#' @references
#'
#' Freeman, L. C. (1978). Centrality in social networks conceptual clarification. Social Networks, 1(3), 215–239. \doi{10.1016/0378-8733(78)90021-7}
#'
#' Marchiori, M. and Latora, V. (2000). Harmony in the small-world. Physica A, 285(3-4), 539–546. \doi{10.1016/S0378-4371(00)00311-3}
#'
#' Opsahl, T., Agneessens, F., and Skvoretz, J. (2010). Node centrality in weighted networks: Generalizing degree and shortest paths. Social Networks, 32(3), 245–251. \doi{10.1016/j.socnet.2010.03.006}
#'
#' Rochat, Y. (2009). Closeness centrality extended to unconnected graphs: The harmonic centrality index. ASNA.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 1, 0,
#'   1, 0, 0, 0, 0,
#'   1, 0, 0, 0, 1,
#'   1, 0, 0, 0, 0,
#'   0, 0, 1, 0, 0
#' ), byrow = TRUE, ncol = 5)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- rownames(A)
#'
#' closeness_centrality(A, digraph = FALSE)
#' closeness_centrality(A, digraph = FALSE, harmonic = TRUE)
#' @export

closeness_centrality <- function(A, digraph = TRUE, type = c("out", "in", "all"),
                                 weighted = FALSE, alpha = 1,
                                 harmonic = FALSE, normalized = FALSE) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  type <- match.arg(type)
  n <- nrow(A)

  if (!digraph | type == "all") {
    A <- pmax(A, t(A)) # Underlying graph
  }

  D <- geodesic_distances(A, weighted = weighted, alpha = alpha)
  if (type == "in") {
    D <- t(D)
  }
  diag(D) <- NA

  closeness <- rep(NA, n)
  if (harmonic) {
    for (i in 1:n) {
      closeness[i] <- sum(1 / D[i, ], na.rm = TRUE)
    }
    if (normalized) {
      closeness <- closeness / (n - 1)
    }
  } else {
    if (any(is.infinite(D))) {
      warning("The network is disconnected, only the reachable nodes are considered. The harmonic closeness might be preferred")
    }
    for (i in 1:n) {
      reached <- which(is.finite(D[i, ]))
      closeness[i] <- 1 / sum(D[i, reached])
      if (normalized) {
        closeness[i] <- closeness[i] * length(reached)
      }
    }
    closeness[is.infinite(closeness)] <- NaN # isolates
  }

  names(closeness) <- rownames(A)
  return(closeness)
}


#' Betweenness centrality
#'
#' Betweenness centrality of Freeman (1977), computed with the algorithm of Brandes (2001).
#'
#' The betweenness of a node is the sum, over all pairs of other nodes, of the proportion of
#' geodesics between the pair that pass through the node. For undirected networks each pair
#' is counted once.
#'
#' For valued matrices, the tie weights are treated as strengths and transformed into lengths
#' as \eqn{1 / w^{\alpha}} (Opsahl et al., 2010). If \code{alpha = 0} the binary network is used.
#'
#' @param A   A square matrix
#' @param digraph   Whether the matrix is directed or undirected
#' @param weighted   Whether the matrix is weighted
#' @param alpha   The tuning parameter of Opsahl et al. (2010) to transform weights into lengths
#' @param normalized   If TRUE, the result is divided by (n-1)(n-2) for directed networks and (n-1)(n-2)/2 for undirected networks
#'
#' @return This function returns the betweenness centrality of the nodes.
#'
#' @references
#'
#' Brandes, U. (2001). A faster algorithm for betweenness centrality. Journal of Mathematical Sociology, 25(2), 163–177. \doi{10.1080/0022250X.2001.9990249}
#'
#' Freeman, L. C. (1977). A set of measures of centrality based on betweenness. Sociometry, 40(1), 35–41. \doi{10.2307/3033543}
#'
#' Opsahl, T., Agneessens, F., and Skvoretz, J. (2010). Node centrality in weighted networks: Generalizing degree and shortest paths. Social Networks, 32(3), 245–251. \doi{10.1016/j.socnet.2010.03.006}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 1, 0, 0, 0, 0, 0,
#'   1, 0, 1, 1, 1, 0, 0, 0, 0,
#'   1, 1, 0, 1, 0, 1, 0, 0, 0,
#'   1, 1, 1, 0, 1, 1, 0, 0, 0,
#'   0, 1, 0, 1, 0, 1, 1, 0, 0,
#'   0, 0, 1, 1, 1, 0, 1, 0, 0,
#'   0, 0, 0, 0, 1, 1, 0, 1, 0,
#'   0, 0, 0, 0, 0, 0, 1, 0, 1,
#'   0, 0, 0, 0, 0, 0, 0, 1, 0
#' ), byrow = TRUE, ncol = 9)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- rownames(A)
#'
#' betweenness_centrality(A, digraph = FALSE)
#' @export

betweenness_centrality <- function(A, digraph = TRUE, weighted = FALSE, alpha = 1,
                                   normalized = FALSE) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  n <- nrow(A)

  # The betweenness of v adds up the dependency of every source on v
  D <- brandes_dependency(A, digraph = digraph, weighted = weighted, alpha = alpha)
  betweenness <- colSums(D)

  if (!digraph) {
    betweenness <- betweenness / 2 # each pair was counted from both ends
  }

  if (normalized) {
    if (digraph) {
      betweenness <- betweenness / ((n - 1) * (n - 2))
    } else {
      betweenness <- betweenness / ((n - 1) * (n - 2) / 2)
    }
  }

  names(betweenness) <- rownames(A)
  return(betweenness)
}

# Dependency matrix of Brandes (2001): D[s, v] is the extent to which the
# source s depends on v to reach the other nodes through shortest paths.
# The column sums are the betweenness of a directed network
brandes_dependency <- function(A, digraph = TRUE, weighted = FALSE, alpha = 1) {
  A <- as.matrix(A)
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  n <- nrow(A)

  if (!digraph) {
    A <- pmax(A, t(A)) # Underlying graph
  }
  L <- tie_lengths(A, weighted = weighted, alpha = alpha)

  # One shortest path search from each source, then the dependencies are
  # accumulated from the farthest nodes back to the source
  D <- matrix(0, n, n, dimnames = list(rownames(A), rownames(A)))
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
        new_dist <- dist[v] + L[v, w]
        # Tolerance, as the sum of transformed weights might differ at the last decimals
        if (new_dist < dist[w] - 1e-10) {
          dist[w] <- new_dist
          sigma[w] <- sigma[v]
          preds[[w]] <- v
        } else if (abs(new_dist - dist[w]) <= 1e-10) {
          sigma[w] <- sigma[w] + sigma[v]
          preds[[w]] <- c(preds[[w]], v)
        }
      }
    }

    delta <- rep(0, n)
    for (w in rev(stack)) {
      for (v in preds[[w]]) {
        delta[v] <- delta[v] + (sigma[v] / sigma[w]) * (1 + delta[w])
      }
      if (w != s) {
        D[s, w] <- delta[w]
      }
    }
  }
  return(D)
}


#' Eigenvector centrality
#'
#' Eigenvector centrality of Bonacich (1972), the leading eigenvector of the matrix.
#'
#' A node is central when it is connected to other central nodes. For directed networks,
#' the \code{in} option gives centrality to the nodes that receive ties from central nodes,
#' and \code{out} to the nodes that send ties to central nodes.
#'
#' @param A   A square matrix
#' @param digraph   Whether the matrix is directed or undirected
#' @param type   Whether to use the \code{in} (default) or \code{out} ties for directed networks
#' @param weighted   Whether the matrix is weighted
#' @param scale   Whether the vector is scaled with a maximum of one (\code{max}, default) or has unit length (\code{unit})
#' @param signed   Whether the matrix has negative ties (Bonacich and Lloyd, 2004). The scores can then be negative, and the eigenvector is the one of the eigenvalue with the largest absolute value
#'
#' @return This function returns the eigenvector centrality of the nodes and the leading eigenvalue.
#'
#' @references
#'
#' Bonacich, P. (1972). Factoring and weighting approaches to status scores and clique identification. Journal of Mathematical Sociology, 2(1), 113–120. \doi{10.1080/0022250X.1972.9989806}
#'
#' Bonacich, P. (1987). Power and centrality: A family of measures. American Journal of Sociology, 92(5), 1170–1182. \doi{10.1086/228631}
#'
#' Bonacich, P. and Lloyd, P. (2004). Calculating status with negative relations. Social Networks, 26(4), 331–338. \doi{10.1016/j.socnet.2004.08.007}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 1, 0,
#'   1, 0, 0, 0, 0,
#'   1, 0, 0, 0, 1,
#'   1, 0, 0, 0, 0,
#'   0, 0, 1, 0, 0
#' ), byrow = TRUE, ncol = 5)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- rownames(A)
#'
#' eigenvector_centrality(A, digraph = FALSE)
#' eigenvector_centrality(A, digraph = FALSE, scale = "unit")
#'
#' # With negative ties the status of a node can be negative
#' S <- matrix(c(
#'   0, 1, 1, -1,
#'   1, 0, 1, -1,
#'   1, 1, 0, -1,
#'   -1, -1, -1, 0
#' ), byrow = TRUE, ncol = 4)
#' rownames(S) <- letters[1:nrow(S)]
#' colnames(S) <- rownames(S)
#'
#' eigenvector_centrality(S, digraph = FALSE, signed = TRUE)
#' @export

eigenvector_centrality <- function(A, digraph = TRUE, type = c("in", "out"),
                                   weighted = FALSE, scale = c("max", "unit"),
                                   signed = FALSE) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  type <- match.arg(type)
  scale <- match.arg(scale)

  if (!weighted & !signed) {
    A[A > 0] <- 1
  }
  if (!digraph) {
    if (signed) {
      A <- underlying_signed(A)
    } else {
      A <- pmax(A, t(A)) # Underlying graph
    }
  }
  if (type == "in") {
    A <- t(A) # x_i = sum_j a_ji x_j
  }

  E <- eigen(A, symmetric = !digraph)
  values <- Re(E$values)
  if (signed) {
    # With negative ties the status follows the eigenvalue of the largest
    # absolute value, and the scores keep their sign (Bonacich and Lloyd, 2004)
    leading <- which.max(abs(values))
    vector <- Re(E$vectors[, leading])
    if (sum(vector) < 0) vector <- -vector # the sign of an eigenvector is arbitrary
    if (scale == "max") {
      vector <- vector / max(abs(vector))
    } else {
      vector <- vector / sqrt(sum(vector^2))
    }
    names(vector) <- rownames(A)
    return(list(vector = vector, value = values[leading]))
  }
  leading <- which.max(values)
  if (values[leading] <= 0) stop("The leading eigenvalue is not positive, e.g. an acyclic directed network")
  if (sum(abs(values - values[leading]) < 1e-8) > 1) {
    warning("The leading eigenvalue is not unique (e.g. a disconnected network), the eigenvector might not be meaningful")
  }

  # Perron-Frobenius: the leading vector can be chosen non-negative
  vector <- abs(Re(E$vectors[, leading]))
  if (scale == "max") {
    vector <- vector / max(vector)
  } else {
    vector <- vector / sqrt(sum(vector^2))
  }

  names(vector) <- rownames(A)
  return(list(vector = vector, value = values[leading]))
}


# Lengths of the ties. Weights are strengths, so lengths are 1/w^alpha
# (Opsahl et al., 2010). Absent ties have infinite length.
tie_lengths <- function(A, weighted = FALSE, alpha = 1) {
  L <- matrix(Inf, nrow(A), ncol(A))
  if (weighted) {
    L[A > 0] <- 1 / (A[A > 0]^alpha)
  } else {
    L[A > 0] <- 1
  }
  diag(L) <- Inf
  L
}

# Geodesic distances with the Floyd-Warshall algorithm in matrix form: for each
# intermediary node k, the path i -> k -> j replaces i -> j when it is shorter.
geodesic_distances <- function(A, weighted = FALSE, alpha = 1) {
  D <- tie_lengths(A, weighted = weighted, alpha = alpha)
  diag(D) <- 0
  for (k in 1:nrow(D)) {
    D <- pmin(D, outer(D[, k], D[k, ], "+"))
  }
  dimnames(D) <- dimnames(A)
  D
}


#' Katz centrality
#'
#' Katz centrality (1953), which counts the paths that arrive to a node, discounting the longer ones.
#'
#' The centrality is \eqn{x = \beta (I - \alpha A^T)^{-1} 1}, so a path of length \eqn{k} contributes
#' \eqn{\alpha^k}. The attenuation \code{alpha} should be smaller than the inverse of the leading
#' eigenvalue of the matrix, otherwise the sum does not converge.
#'
#' @param A   A square matrix
#' @param alpha   The attenuation of the longer paths
#' @param beta   The status that every node has independently of the network
#' @param type   Whether the paths considered are the ones that arrive (\code{in}, default) or leave (\code{out}) the node
#' @param digraph   Whether the matrix is directed or undirected
#' @param weighted   Whether the matrix is weighted
#'
#' @return This function returns the Katz centrality of the nodes.
#'
#' @references
#'
#' Katz, L. (1953). A new status index derived from sociometric analysis. Psychometrika, 18(1), 39–43. \doi{10.1007/BF02289026}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 1, 0,
#'   1, 0, 0, 0, 0,
#'   1, 0, 0, 0, 1,
#'   1, 0, 0, 0, 0,
#'   0, 0, 1, 0, 0
#' ), byrow = TRUE, ncol = 5)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- rownames(A)
#'
#' katz_centrality(A, alpha = 0.1, digraph = FALSE)
#' @export

katz_centrality <- function(A, alpha = 0.1, beta = 1, type = c("in", "out"),
                            digraph = TRUE, weighted = FALSE) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  type <- match.arg(type)
  if (!weighted) {
    A[A > 0] <- 1
  }
  if (!digraph) {
    A <- pmax(A, t(A)) # Underlying graph
  }
  if (type == "in") {
    A <- t(A)
  }

  lambda <- max(abs(Re(eigen(A, only.values = TRUE)$values)))
  if (alpha * lambda >= 1) warning("alpha should be smaller than the inverse of the leading eigenvalue, the series does not converge")

  katz <- beta * solve(diag(nrow(A)) - alpha * A) %*% rep(1, nrow(A))
  katz <- drop(katz)
  names(katz) <- rownames(A)
  return(katz)
}


#' Bonacich power centrality
#'
#' Power centrality of Bonacich (1987), where being connected to well-connected others can
#' increase or decrease the centrality of a node.
#'
#' The centrality is \eqn{x = (I - \beta A)^{-1} A 1}. When \code{beta} is positive, a node is
#' central when it is connected to central nodes, as in the eigenvector centrality. When
#' \code{beta} is negative, being connected to well-connected others reduces the centrality of a
#' node, which describes bargaining situations. With \code{beta = 0} the measure is the degree.
#'
#' @param A   A square matrix
#' @param beta   The weight given to the centrality of the neighbours. It should be smaller than the inverse of the leading eigenvalue
#' @param digraph   Whether the matrix is directed or undirected
#' @param weighted   Whether the matrix is weighted
#' @param scale   Whether the scores are returned as they are (\code{none}, default) or scaled so that the sum of their squares is the number of nodes (\code{ssq}), as in other packages
#'
#' @return This function returns the power centrality of the nodes.
#'
#' @references
#'
#' Bonacich, P. (1987). Power and centrality: A family of measures. American Journal of Sociology, 92(5), 1170–1182. \doi{10.1086/228631}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 1, 0,
#'   1, 0, 0, 0, 0,
#'   1, 0, 0, 0, 1,
#'   1, 0, 0, 0, 0,
#'   0, 0, 1, 0, 0
#' ), byrow = TRUE, ncol = 5)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- rownames(A)
#'
#' bonacich_power(A, beta = 0.1, digraph = FALSE)
#' bonacich_power(A, beta = -0.1, digraph = FALSE)
#' @export

bonacich_power <- function(A, beta = 0, digraph = TRUE, weighted = FALSE,
                           scale = c("none", "ssq")) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  scale <- match.arg(scale)
  if (!weighted) {
    A[A > 0] <- 1
  }
  if (!digraph) {
    A <- pmax(A, t(A)) # Underlying graph
  }

  if (beta != 0) {
    lambda <- max(abs(Re(eigen(A, only.values = TRUE)$values)))
    if (abs(beta) * lambda >= 1) warning("beta should be smaller than the inverse of the leading eigenvalue, the series does not converge")
  }

  power <- solve(diag(nrow(A)) - beta * A) %*% A %*% rep(1, nrow(A))
  power <- drop(power)
  if (scale == "ssq" & any(power != 0)) {
    power <- power * sqrt(nrow(A) / sum(power^2))
  }
  names(power) <- rownames(A)
  return(power)
}


#' PageRank centrality
#'
#' PageRank of Brin and Page (1998): the probability that a random walker, who follows the ties
#' and sometimes jumps to a random node, is found in each node.
#'
#' At each step, the walker follows one of the outgoing ties of the node with probability
#' \code{damping}, and jumps to a node chosen at random with probability \code{1 - damping}.
#' The nodes without outgoing ties are treated as if they were connected to every node.
#'
#' @param A   A square matrix
#' @param damping   Probability of following a tie instead of jumping to a random node
#' @param digraph   Whether the matrix is directed or undirected
#' @param weighted   Whether the matrix is weighted
#' @param tol   Tolerance of the power iteration
#' @param max_iter   Maximum number of iterations
#'
#' @return This function returns the PageRank of the nodes, which adds up to one.
#'
#' @references
#'
#' Brin, S. and Page, L. (1998). The anatomy of a large-scale hypertextual Web search engine. Computer Networks and ISDN Systems, 30(1-7), 107–117. \doi{10.1016/S0169-7552(98)00110-X}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 1, 0,
#'   1, 0, 0, 0, 0,
#'   1, 0, 0, 0, 1,
#'   1, 0, 0, 0, 0,
#'   0, 0, 1, 0, 0
#' ), byrow = TRUE, ncol = 5)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- rownames(A)
#'
#' page_rank_centrality(A, digraph = FALSE)
#' @export

page_rank_centrality <- function(A, damping = 0.85, digraph = TRUE, weighted = FALSE,
                                 tol = 1e-10, max_iter = 1000) {
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
  n <- nrow(A)

  # Transition matrix: the nodes without outgoing ties distribute their rank among all the nodes
  out <- rowSums(A)
  P <- A
  P[out > 0, ] <- A[out > 0, ] / out[out > 0]
  P[out == 0, ] <- 1 / n

  x <- rep(1 / n, n)
  for (i in 1:max_iter) {
    x_new <- (1 - damping) / n + damping * drop(t(P) %*% x)
    if (sum(abs(x_new - x)) < tol) {
      x <- x_new
      break
    }
    x <- x_new
  }
  if (i == max_iter) warning("The power iteration did not converge, increase max_iter")

  names(x) <- rownames(A)
  return(x)
}


#' Centralization
#'
#' Centralization of Freeman (1979): how much a network is dominated by its most central node.
#'
#' The centralization is the sum of the differences between the highest centrality and the
#' centrality of every node, divided by the largest sum that a network of the same order can
#' have. For degree, closeness and betweenness the maximum is given by the star network, and for
#' the eigenvector centrality by a network with a single tie, which gives \eqn{n - 2}.
#'
#' The function is named \code{centrality_centralization()} so that it does not mask \code{sna::centralization()}.
#'
#' @param A   A square matrix
#' @param measure   The centrality to be used: \code{degree} (default), \code{closeness}, \code{betweenness} or \code{eigenvector}
#' @param digraph   Whether the matrix is directed or undirected
#' @param type   Type of degree or distances for directed networks
#' @param loops   Whether to consider the loops of the matrix
#'
#' @return This function returns the centralization of the network, the centrality scores and the theoretical maximum.
#'
#' @references
#'
#' Freeman, L. C. (1979). Centrality in social networks conceptual clarification. Social Networks, 1(3), 215–239. \doi{10.1016/0378-8733(78)90021-7}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 1, 0,
#'   1, 0, 0, 0, 0,
#'   1, 0, 0, 0, 1,
#'   1, 0, 0, 0, 0,
#'   0, 0, 1, 0, 0
#' ), byrow = TRUE, ncol = 5)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- rownames(A)
#'
#' centrality_centralization(A, measure = "degree", digraph = FALSE)
#' centrality_centralization(A, measure = "betweenness", digraph = FALSE)
#' @export

centrality_centralization <- function(A, measure = c("degree", "closeness", "betweenness", "eigenvector"),
                           digraph = FALSE, type = c("out", "in", "all"), loops = FALSE) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  measure <- match.arg(measure)
  type <- match.arg(type)
  n <- nrow(A)
  if (n < 3) stop("The centralization is not defined for less than three nodes")

  # The star network is the most centralized network of the same order
  star <- matrix(0, n, n)
  star[1, ] <- 1
  star[, 1] <- 1
  diag(star) <- 0
  rownames(star) <- as.character(1:n)
  colnames(star) <- rownames(star)

  if (!digraph) {
    type <- "out" # for undirected networks the out-degree is the degree
  }
  scores <- centrality_scores(A, measure, digraph = digraph, type = type, loops = loops)
  reference <- centrality_scores(star, measure, digraph = FALSE, type = type, loops = loops)

  if (measure == "eigenvector") {
    # The largest deviations are given by a network with a single tie and the other nodes isolated
    maximum <- n - 2
  } else {
    maximum <- sum(max(reference) - reference)
  }
  centralization <- sum(max(scores) - scores) / maximum

  return(list(centralization = centralization, scores = scores, maximum = maximum))
}

# Scores used by the centralization, with the normalisation of each measure
centrality_scores <- function(A, measure, digraph, type, loops) {
  if (measure == "degree") {
    scores <- gen_degree(A, type = type, digraph = digraph, loops = loops)
  }
  if (measure == "closeness") {
    scores <- suppressWarnings(closeness_centrality(A, digraph = digraph, type = type, normalized = TRUE))
  }
  if (measure == "betweenness") {
    scores <- betweenness_centrality(A, digraph = digraph)
  }
  if (measure == "eigenvector") {
    scores <- eigenvector_centrality(A, digraph = digraph)$vector
  }
  scores
}
