# Membership of the nodes in categories, as a row-stochastic matrix (Everett and
# Borgatti, 2026). A vector of categories becomes an indicator matrix, and each
# row of a matrix is divided by its sum, so that it gives the proportion of the
# node that belongs to each category
category_matrix <- function(B) {
  if (is.null(dim(B))) {
    if (any(is.na(B))) stop("The category of every node should be known")
    groups <- sort(unique(B))
    M <- matrix(0, length(B), length(groups), dimnames = list(names(B), groups))
    M[cbind(seq_along(B), match(B, groups))] <- 1
  } else {
    M <- as.matrix(B)
    if (any(is.na(M))) stop("The memberships should not have missing values")
    if (any(M < 0)) stop("The memberships should not be negative")
  }
  if (any(rowSums(M) == 0)) stop("Every node should belong to at least one category")
  M <- M / rowSums(M)
  if (is.null(colnames(M))) {
    colnames(M) <- paste0("G", seq_len(ncol(M)))
  }
  return(M)
}


#' Alter composition
#'
#' Number of alters of each node in each category, for categories that can overlap (Everett and Borgatti, 2026).
#'
#' @details
#' The memberships \code{B} are made row-stochastic, so that each row gives the proportion of a node that belongs
#' to each category (for instance, the proportion of time spent on each project). The alter composition is the
#' product \eqn{AB}: for a partition, it counts the alters of each node in each category; with overlapping
#' categories, it gives the extent to which the alters of each node belong to each category. The rows of
#' \eqn{AB} add up to the degree of each node. \code{A} need not be square: its columns should be the rows of
#' \code{B}, as in a matrix of respondents and the alters they named.
#'
#' @param A   A matrix of the ties of the nodes (rows) with their alters (columns)
#' @param B   A matrix of the membership of the alters (rows) in the categories (columns), or a vector with the category of each alter
#' @param proportion   Whether to divide each row by its sum, giving the proportion of the alters in each category
#'
#' @return This function returns a matrix of the nodes (rows) and the categories (columns).
#'
#' @references
#'
#' Everett, M. G. and Borgatti, S. P. (2026). Alter composition with overlapping group memberships. Social Networks, 85, 80–88. \doi{10.1016/j.socnet.2025.12.001}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 0,
#'   1, 0, 1, 1,
#'   1, 1, 0, 0,
#'   0, 1, 0, 0
#' ), byrow = TRUE, ncol = 4)
#' rownames(A) <- colnames(A) <- c("a", "b", "c", "d")
#'
#' # A partition
#' alter_composition(A, c("x", "x", "y", "y"))
#'
#' # Overlapping categories: hours spent on two projects
#' B <- matrix(c(
#'   10, 0,
#'   5, 5,
#'   0, 8,
#'   2, 6
#' ), byrow = TRUE, ncol = 2)
#' alter_composition(A, B)
#' @export

alter_composition <- function(A, B, proportion = FALSE) {
  A <- as.matrix(A)
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  B <- category_matrix(B)
  if (ncol(A) != nrow(B)) stop("The columns of A should be the rows of B")

  AB <- A %*% B
  if (proportion) {
    degree <- rowSums(AB)
    degree[degree == 0] <- NA # nodes without alters have no composition
    AB <- AB / degree
  }
  rownames(AB) <- rownames(A)
  return(AB)
}


#' Alter heterogeneity
#'
#' Blau's heterogeneity of the alters of each node, for categories that can overlap (Everett and Borgatti, 2026).
#'
#' @details
#' The heterogeneity of a node is \eqn{1 - \sum_k p_k^2}, where \eqn{p_k} is the proportion of its alters in
#' category \eqn{k}, taken from the rows of \code{alter_composition()} (Blau, 1977). A node with a single alter
#' has some heterogeneity when that alter belongs to several categories. The IQV divides the index by its
#' maximum, \eqn{1 - 1/K}, where \eqn{K} is the number of categories. The nodes without alters are \code{NA}.
#'
#' @param A   A matrix of the ties of the nodes (rows) with their alters (columns)
#' @param B   A matrix of the membership of the alters (rows) in the categories (columns), or a vector with the category of each alter
#' @param normalized   Whether to return the IQV
#'
#' @return This function returns a vector with the heterogeneity of the alters of each node.
#'
#' @references
#'
#' Blau, P. M. (1977). Inequality and heterogeneity: A primitive theory of social structure. Free Press.
#'
#' Everett, M. G. and Borgatti, S. P. (2026). Alter composition with overlapping group memberships. Social Networks, 85, 80–88. \doi{10.1016/j.socnet.2025.12.001}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 0,
#'   1, 0, 1, 1,
#'   1, 1, 0, 0,
#'   0, 1, 0, 0
#' ), byrow = TRUE, ncol = 4)
#' rownames(A) <- colnames(A) <- c("a", "b", "c", "d")
#' B <- matrix(c(
#'   10, 0,
#'   5, 5,
#'   0, 8,
#'   2, 6
#' ), byrow = TRUE, ncol = 2)
#'
#' alter_heterogeneity(A, B)
#' @export

alter_heterogeneity <- function(A, B, normalized = FALSE) {
  P <- alter_composition(A, B, proportion = TRUE)
  heterogeneity <- 1 - rowSums(P^2)
  if (normalized) {
    heterogeneity <- heterogeneity / (1 - 1 / ncol(P))
  }
  names(heterogeneity) <- rownames(P)
  return(heterogeneity)
}


#' Alter homophily
#'
#' E-I index and Yule's Q of each node, for categories that can overlap (Everett and Borgatti, 2026).
#'
#' @details
#' The internal ties of a node \eqn{i} are \eqn{I = \sum_j A_{ij} S_{ij}}, where \eqn{S_{ij}} is the similarity
#' of the memberships of \eqn{i} and \eqn{j}, and the external ties are \eqn{E = D - I}, where \eqn{D} is the
#' degree. The E-I index is \eqn{(E - I) / (E + I)} (Krackhardt and Stern, 1988): -1 when all the alters are in
#' the categories of ego (homophily) and +1 when none is (heterophily).
#'
#' Yule's Q also uses the nodes that are not alters, to take into account how many nodes of each category are
#' available: \eqn{a = I} and \eqn{b = E} for the alters, and \eqn{c} and \eqn{d} are the same quantities for the
#' other nodes. Then \eqn{Q = (ad - bc) / (ad + bc)}, which is positive for homophily, and zero when the ties do
#' not depend on the categories.
#'
#' The similarity of two memberships can be defined in three ways:
#'
#' \code{similarity = "product"} (default), \eqn{S_{ij} = \sum_k B_{ik} B_{jk}}. It reduces to the usual indices
#' when the categories are a partition. If the categories are, for instance, the proportion of time spent in
#' each of several places, it is the probability that two nodes are in the same place.
#'
#' \code{similarity = "minimum"}, \eqn{S_{ij} = \sum_k \min(B_{ik}, B_{jk})}, the trait version (\eqn{E_s-I_s} and
#' \eqn{Q_s}). Two nodes with identical memberships are fully similar even when they split their time among
#' categories, which fits categories that are traits such as skills or interests.
#'
#' \code{similarity = "cosine"}, the cosine of the memberships, which favours two nodes that concentrate in the
#' same categories, such as two specialists with the same specialty.
#'
#' The ties are binary and the loops are ignored. For a directed network the alters are the out-neighbours.
#' Nodes without alters, and Yule's Q with \eqn{ad + bc = 0}, are \code{NA}.
#'
#' @param A   A square matrix
#' @param B   A matrix of the membership of the nodes (rows) in the categories (columns), or a vector with the category of each node
#' @param method   The index: \code{ei} (default) or \code{yule}
#' @param similarity   The similarity of the memberships: \code{product} (default), \code{minimum} or \code{cosine}
#'
#' @return This function returns a vector with the index of each node.
#'
#' @references
#'
#' Everett, M. G. and Borgatti, S. P. (2026). Alter composition with overlapping group memberships. Social Networks, 85, 80–88. \doi{10.1016/j.socnet.2025.12.001}
#'
#' Krackhardt, D. and Stern, R. N. (1988). Informal networks and organizational crises: An experimental simulation. Social Psychology Quarterly, 51(2), 123–140. \doi{10.2307/2786835}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 0,
#'   1, 0, 1, 1,
#'   1, 1, 0, 0,
#'   0, 1, 0, 0
#' ), byrow = TRUE, ncol = 4)
#' rownames(A) <- colnames(A) <- c("a", "b", "c", "d")
#' B <- matrix(c(
#'   10, 0,
#'   5, 5,
#'   0, 8,
#'   2, 6
#' ), byrow = TRUE, ncol = 2)
#'
#' alter_homophily(A, B)
#' alter_homophily(A, B, method = "yule", similarity = "minimum")
#' @export

alter_homophily <- function(A, B, method = c("ei", "yule"),
                            similarity = c("product", "minimum", "cosine")) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  method <- match.arg(method)
  similarity <- match.arg(similarity)
  B <- category_matrix(B)
  if (nrow(A) != nrow(B)) stop("There should be one row of B for each node")
  A[A > 0] <- 1
  diag(A) <- 0

  # Similarity of the memberships of every pair of nodes
  if (similarity == "product") {
    S <- B %*% t(B)
  }
  if (similarity == "minimum") {
    S <- matrix(0, nrow(B), nrow(B))
    for (i in seq_len(nrow(B))) {
      for (j in seq_len(nrow(B))) {
        S[i, j] <- sum(pmin(B[i, ], B[j, ]))
      }
    }
  }
  if (similarity == "cosine") {
    norm <- sqrt(rowSums(B^2))
    S <- (B %*% t(B)) / outer(norm, norm)
  }

  degree <- rowSums(A)
  internal <- rowSums(A * S)
  external <- degree - internal

  if (method == "ei") {
    index <- (external - internal) / degree
  } else {
    # The same quantities for the nodes that are not alters
    C <- 1 - A
    diag(C) <- 0
    internal_other <- rowSums(C * S)
    external_other <- rowSums(C) - internal_other
    index <- (internal * external_other - external * internal_other) /
      (internal * external_other + external * internal_other)
  }
  index[!is.finite(index)] <- NA
  names(index) <- rownames(A)
  return(index)
}


#' Brokerage roles
#'
#' Brokerage roles of Gould and Fernandez (1989), for categories that can overlap (Everett and Borgatti, 2026).
#'
#' @details
#' A node \eqn{b} brokers in a path \eqn{a \to b \to c} when \eqn{a} has no tie to \eqn{c}. The role depends on
#' the categories of the three nodes: \code{coordinator} (all in the same category), \code{gatekeeper} (\eqn{b}
#' and \eqn{c} in the same category, \eqn{a} in another), \code{representative} (\eqn{a} and \eqn{b} in the same
#' category, \eqn{c} in another), \code{consultant} (also called itinerant; \eqn{a} and \eqn{c} in the same
#' category, \eqn{b} in another) and \code{liaison} (the three in different categories).
#'
#' With overlapping categories, each path counts partly for each role (Everett and Borgatti, 2026: Eq. 5). The
#' coordinator part is \eqn{\sum_j B_{aj} B_{bj} B_{cj}}, the joint membership of the three nodes in each
#' category; the gatekeeper part is \eqn{\sum_j B_{bj} B_{cj} (1 - B_{aj})}; the representative part
#' \eqn{\sum_j B_{aj} B_{bj} (1 - B_{cj})}; the consultant part \eqn{\sum_j B_{aj} B_{cj} (1 - B_{bj})}; and the
#' liaison part is the rest. When the categories are a partition, each path counts for a single role, as in
#' Gould and Fernandez (1989). The parts of each path add up to one, so the total of each node is the number of
#' paths it brokers.
#'
#' A symmetric matrix is treated as a directed network with ties in both directions, so each path is counted
#' from both ends.
#'
#' @param A   A square matrix
#' @param B   A matrix of the membership of the nodes (rows) in the categories (columns), or a vector with the category of each node
#'
#' @return This function returns a matrix with the score of each node (rows) in each role, and the total.
#'
#' @references
#'
#' Everett, M. G. and Borgatti, S. P. (2026). Alter composition with overlapping group memberships. Social Networks, 85, 80–88. \doi{10.1016/j.socnet.2025.12.001}
#'
#' Gould, R. V. and Fernandez, R. M. (1989). Structures of mediation: A formal approach to brokerage in transaction networks. Sociological Methodology, 19, 89–126. \doi{10.2307/270949}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 0, 0,
#'   0, 0, 1, 1,
#'   0, 0, 0, 1,
#'   1, 0, 0, 0
#' ), byrow = TRUE, ncol = 4)
#' rownames(A) <- colnames(A) <- c("a", "b", "c", "d")
#'
#' brokerage_roles(A, c("x", "x", "y", "y"))
#'
#' B <- matrix(c(
#'   10, 0,
#'   5, 5,
#'   0, 8,
#'   2, 6
#' ), byrow = TRUE, ncol = 2)
#' brokerage_roles(A, B)
#' @export

brokerage_roles <- function(A, B) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  B <- category_matrix(B)
  if (nrow(A) != nrow(B)) stop("There should be one row of B for each node")
  A[A > 0] <- 1
  diag(A) <- 0

  n <- nrow(A)
  roles <- matrix(0, n, 6, dimnames = list(
    rownames(A),
    c("coordinator", "gatekeeper", "representative", "consultant", "liaison", "total")
  ))
  for (b in seq_len(n)) {
    sources <- which(A[, b] > 0)
    targets <- which(A[b, ] > 0)
    if (length(sources) == 0 || length(targets) == 0) next
    # The paths a -> b -> c in which a has no tie to c
    open <- A[sources, targets, drop = FALSE] == 0 & outer(sources, targets, "!=")
    if (!any(open)) next

    Ba <- B[sources, , drop = FALSE]
    Bc <- B[targets, , drop = FALSE]
    Bb <- matrix(B[b, ], nrow(Ba), ncol(B), byrow = TRUE)
    # Each matrix has the part of every path (sources in rows, targets in columns)
    coordinator <- (Ba * Bb) %*% t(Bc)
    gatekeeper <- ((1 - Ba) * Bb) %*% t(Bc)
    representative <- (Ba * Bb) %*% t(1 - Bc)
    consultant <- (Ba * (1 - Bb)) %*% t(Bc)
    liaison <- 1 - coordinator - gatekeeper - representative - consultant

    roles[b, "coordinator"] <- sum(coordinator[open])
    roles[b, "gatekeeper"] <- sum(gatekeeper[open])
    roles[b, "representative"] <- sum(representative[open])
    roles[b, "consultant"] <- sum(consultant[open])
    roles[b, "liaison"] <- sum(liaison[open])
    roles[b, "total"] <- sum(open)
  }
  return(roles)
}


#' Partition of centrality by category
#'
#' Contribution of the nodes of each category to the centrality of every node (Everett and Borgatti, 2012, 2026).
#'
#' @details
#' The betweenness of a node \eqn{v} adds up the dependency of every source \eqn{s} on \eqn{v}, \eqn{\delta_s(v)},
#' the extent to which \eqn{s} needs \eqn{v} to reach the other nodes through shortest paths (Brandes, 2001).
#' Grouping the sources by their category, \eqn{D^T B}, splits the betweenness of each node into the parts
#' contributed by each category, and with overlapping categories each source contributes in proportion to its
#' memberships. The rows add up to the betweenness of \code{betweenness_centrality()}.
#'
#' The degree is split with the alter composition, \eqn{AB} for the out-degree and \eqn{A^T B} for the
#' in-degree.
#'
#' @param A   A square matrix
#' @param B   A matrix of the membership of the nodes (rows) in the categories (columns), or a vector with the category of each node
#' @param measure   The centrality: \code{betweenness} (default) or \code{degree}
#' @param digraph   Whether the matrix is directed or undirected
#' @param type   For the degree of a directed network, \code{out} (default) or \code{in}
#' @param weighted   Whether the betweenness uses the values of the ties, as in \code{betweenness_centrality()}
#' @param alpha   The alpha parameter of Opsahl et al. (2010) for the weighted betweenness
#'
#' @return This function returns a matrix with the part of the centrality of each node (rows) contributed by each category (columns).
#'
#' @references
#'
#' Brandes, U. (2001). A faster algorithm for betweenness centrality. Journal of Mathematical Sociology, 25(2), 163–177. \doi{10.1080/0022250X.2001.9990249}
#'
#' Everett, M. G. and Borgatti, S. P. (2012). Categorical attribute based centrality: E–I and G–F centrality. Social Networks, 34(4), 562–569. \doi{10.1016/j.socnet.2012.06.002}
#'
#' Everett, M. G. and Borgatti, S. P. (2026). Alter composition with overlapping group memberships. Social Networks, 85, 80–88. \doi{10.1016/j.socnet.2025.12.001}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' data(campnet)
#' # Betweenness of the Camp 92 network split by the gender of the sources
#' partition_centrality(campnet$network, campnet$attributes$gender)
#' @export

partition_centrality <- function(A, B, measure = c("betweenness", "degree"), digraph = TRUE,
                                 type = c("out", "in"), weighted = FALSE, alpha = 1) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  measure <- match.arg(measure)
  type <- match.arg(type)
  B <- category_matrix(B)
  if (nrow(A) != nrow(B)) stop("There should be one row of B for each node")

  if (measure == "betweenness") {
    D <- brandes_dependency(A, digraph = digraph, weighted = weighted, alpha = alpha)
    partition <- t(D) %*% B
    if (!digraph) {
      partition <- partition / 2 # each pair was counted from both ends
    }
  } else {
    if (!digraph) {
      A <- pmax(A, t(A))
    }
    if (type == "in") {
      A <- t(A)
    }
    diag(A) <- 0
    partition <- A %*% B
  }
  rownames(partition) <- rownames(A)
  return(partition)
}
