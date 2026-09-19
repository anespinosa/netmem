#' Neighbourhood inclusion
#'
#' Neighbourhood-inclusion preorder of an undirected network (Brandes, 2016; Schoch and Brandes, 2016).
#'
#' A node \eqn{u} is dominated by a node \eqn{v} if the open neighbourhood of \eqn{u} is included in the
#' closed neighbourhood of \eqn{v}, \eqn{N(u) \subseteq N[v]}. With \code{closed = FALSE} both
#' neighbourhoods are open, \eqn{N(u) \subseteq N(v)}.
#'
#' In matrix form, \eqn{u} is dominated by \eqn{v} when the number of shared neighbours equals the degree of \eqn{u}.
#'
#' @param A   A symmetric matrix object
#' @param closed   Whether the neighbourhood of the dominating node is closed (i.e. includes the node itself)
#' @param direction   Whether \code{P[u, v] = 1} means that \code{u} is \code{dominated} by \code{v} (default, as in Schoch and Brandes, 2016) or that \code{u} \code{dominates} \code{v}
#'
#' @return This function returns a binary dominance matrix \code{P}.
#'
#' @references
#'
#' Brandes, U. (2016). Network positions. Methodological Innovations, 9, 1–19. \doi{10.1177/2059799116630650}
#'
#' Schoch, D. and Brandes, U. (2016). Re-conceptualizing centrality in social networks. European Journal of Applied Mathematics, 27(6), 971–985. \doi{10.1017/S0956792516000401}
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
#' neigh_inclusion(A)
#' @export

neigh_inclusion <- function(A, closed = TRUE, direction = c("dominated", "dominates")) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  direction <- match.arg(direction)
  if (!all(A[lower.tri(A)] == t(A)[lower.tri(A)])) warning("The network is directed. The underlying graph is used")
  A[A > 0] <- 1
  A <- pmax(A, t(A)) # Symmetrize
  diag(A) <- 0

  if (closed) {
    C <- A + diag(nrow(A))
  } else {
    C <- A
  }

  P <- set_inclusion(A, C)
  dimnames(P) <- dimnames(A)
  if (direction == "dominates") {
    P <- t(P)
  }
  return(P)
}


#' Set inclusion of neighbourhoods
#'
#' Inclusion of the neighbourhoods of the rows of an incidence matrix.
#'
#' The neighbourhoods of the rows can be defined on any set of columns (e.g. papers of authors, or
#' events of actors). \code{N} contains the neighbourhood that should be included, and \code{M} the
#' neighbourhood in which it should be included. \code{M} is the same as \code{N} for open
#' neighbourhoods, or can add other elements, such as the node itself, for closed neighbourhoods.
#'
#' With \code{proper = TRUE} the inclusion should be proper, \eqn{N(u) \subsetneq M(v)}, i.e.
#' \eqn{M(v)} has at least one element that is not in \eqn{N(u)}.
#'
#' @param N   An incidence matrix of the neighbourhoods to be included
#' @param M   An incidence matrix, with the same dimensions as \code{N}, of the including neighbourhoods
#' @param proper   Whether the inclusion should be proper
#'
#' @return This function returns a binary matrix \code{P} where \code{P[u, v] = 1} if \code{N(u)} is included in \code{M(v)}.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' X <- matrix(c(
#'   1, 1, 1, 0,
#'   1, 1, 0, 0,
#'   1, 0, 0, 0,
#'   0, 1, 0, 0,
#'   0, 0, 1, 1
#' ), byrow = TRUE, ncol = 4)
#' rownames(X) <- c("a1", "a2", "a3", "a4", "a5")
#' colnames(X) <- c("w1", "w2", "w3", "w4")
#'
#' set_inclusion(X)
#' @export

set_inclusion <- function(N, M = N, proper = FALSE) {
  N <- as.matrix(N)
  M <- as.matrix(M)
  if (!all(dim(N) == dim(M))) stop("Non-conformable arrays")
  N[N > 0] <- 1
  M[M > 0] <- 1

  # [u, v] counts the elements of N(u) that are also in M(v)
  shared <- N %*% t(M)
  included <- shared == rowSums(N)
  if (proper) {
    included <- included & outer(rowSums(N), rowSums(M), "<")
  }

  P <- 1 * included
  diag(P) <- 0
  rownames(P) <- rownames(N)
  colnames(P) <- rownames(N)
  return(P)
}


#' Pareto dominance
#'
#' Pareto-style dominance across several neighbourhood-inclusion relations (Espinosa-Rada, 2026).
#'
#' Given \eqn{K} inclusion matrices (e.g. from \code{set_inclusion()}), \eqn{u} is dominated by \eqn{v} if
#' (i) the neighbourhood of \eqn{u} is included in the neighbourhood of \eqn{v} in at least \code{tau}
#' relations (weak dominance), and (ii) the dominance is strict in at least one relation.
#'
#' By default, the dominance is strict when it is asymmetric: the neighbourhood of \eqn{u} is included
#' in the neighbourhood of \eqn{v} but not the other way around. Other criteria, such as proper inclusion,
#' can be given in \code{strict} (see \code{set_inclusion(proper = TRUE)}).
#'
#' With \code{tau = K} every relation should agree (unanimity), with a majority of relations the
#' dominance is less demanding, and with \code{tau = 1} one relation is enough.
#'
#' @param inclusions   A list of binary matrices where \code{[u, v] = 1} if \code{u} is included in \code{v}
#' @param tau   Minimum number of relations in which \code{u} should be weakly dominated by \code{v}
#' @param strict   An optional list of binary matrices, in the same order as \code{inclusions}, where \code{[u, v] = 1} if the inclusion of \code{u} in \code{v} is strict. If NULL, the asymmetric criterion is used
#' @param direction   Whether \code{D[u, v] = 1} means that \code{u} is \code{dominated} by \code{v} (default) or that \code{u} \code{dominates} \code{v}
#'
#' @return This function returns a binary dominance matrix \code{D}.
#'
#' @references
#'
#' Espinosa-Rada, A. (2026). Network positions within scholars and intellectual networks. Journal of Informetrics, 20, 101854. \doi{10.1016/j.joi.2026.101854}
#'
#' Schoch, D. and Brandes, U. (2016). Re-conceptualizing centrality in social networks. European Journal of Applied Mathematics, 27(6), 971–985. \doi{10.1017/S0956792516000401}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' X <- matrix(c(
#'   1, 1, 1, 0,
#'   1, 1, 0, 0,
#'   1, 0, 0, 0,
#'   0, 1, 0, 0,
#'   0, 0, 1, 1
#' ), byrow = TRUE, ncol = 4)
#' Y <- matrix(c(
#'   1, 1, 0,
#'   1, 0, 0,
#'   1, 0, 0,
#'   0, 1, 1,
#'   0, 0, 1
#' ), byrow = TRUE, ncol = 3)
#' rownames(X) <- c("a1", "a2", "a3", "a4", "a5")
#' rownames(Y) <- rownames(X)
#'
#' pareto_dominance(list(set_inclusion(X), set_inclusion(Y)), tau = 2)
#' @export

pareto_dominance <- function(inclusions, tau = length(inclusions), strict = NULL,
                             direction = c("dominated", "dominates")) {
  if (!is.list(inclusions)) stop("The inclusion matrices should be in a list")
  if (tau < 1 | tau > length(inclusions)) stop("tau should be between 1 and the number of relations")
  if (!is.null(strict) && length(strict) != length(inclusions)) stop("strict should have one matrix for each relation")
  direction <- match.arg(direction)

  n <- nrow(inclusions[[1]])
  weak <- matrix(0, n, n)
  strict_count <- matrix(0, n, n)
  for (k in seq_along(inclusions)) {
    P <- as.matrix(inclusions[[k]])
    if (!all(dim(P) == c(n, n))) stop("Non-conformable arrays")
    weak <- weak + P
    if (is.null(strict)) {
      strict_count <- strict_count + (P * (1 - t(P))) # u included in v, but not v in u
    } else {
      strict_count <- strict_count + as.matrix(strict[[k]])
    }
  }

  D <- 1 * (weak >= tau & strict_count >= 1)
  diag(D) <- 0
  dimnames(D) <- dimnames(inclusions[[1]])
  if (direction == "dominates") {
    D <- t(D)
  }
  return(D)
}


#' Hyper-event dominance
#'
#' Dominance among authors based on the hyper-event chain Author -> Citing paper -> Cited paper -> Author (Espinosa-Rada, 2026).
#'
#' Each author has three neighbourhoods built from the hyper-events in which the author wrote the citing paper:
#'
#' (\code{authored}) productive participation: the citing papers authored, \eqn{X},
#'
#' (\code{cited_papers}) citation reach: the cited papers, \eqn{X \circ W}. Its closed version adds the papers authored,
#'
#' (\code{cited_authors}) recognition: the cited authors, \eqn{X \circ W \circ X_b^T}. Its closed version adds the author itself.
#'
#' Author \eqn{a_i} dominates \eqn{a_j} when the neighbourhood of \eqn{a_j} is included in the (closed) neighbourhood of
#' \eqn{a_i} in at least \code{tau} dimensions, and the dominance is strict in at least one dimension
#' (see \code{pareto_dominance()}). Authors without hyper-events are excluded.
#'
#' The defaults reproduce the analysis of Espinosa-Rada (2026): the three dimensions, open neighbourhoods for
#' the authored papers and closed neighbourhoods for the cited papers and authors, and asymmetric strict dominance.
#' The alternatives are:
#'
#' \code{strict = "proper"} follows the formal definition of strict dominance as proper inclusion,
#' \eqn{N_k(a_j) \subsetneq N^*_k(a_i)}. As closed neighbourhoods add elements to \eqn{a_i}, proper inclusion
#' is less demanding than the asymmetric criterion, where \eqn{a_i} should not be included in \eqn{a_j}. With
#' proper inclusion, two authors might dominate each other, so the relation is not always a partial order.
#' The same might happen with the asymmetric criterion when \code{tau = 1}, if each author dominates the other in a different dimension.
#'
#' \code{closure_papers} sets which papers of \eqn{a_i} are added to close the neighbourhood of cited papers.
#' With \code{"citing"} (default, as in the analysis scripts of the article) they are the citing papers of
#' \eqn{a_i} with hyper-events. With \code{"authored"} they are every paper authored by \eqn{a_i}, in \code{X} or
#' in \code{Xb}, as in the text of Section 3.5, so that a paper of \eqn{a_i} cited by \eqn{a_j} is in the closed
#' neighbourhood of \eqn{a_i} even when it does not cite other papers of the corpus.
#'
#' \code{max_authors} excludes the citing papers of large teams before computing the neighbourhoods, as a
#' robustness check for consortium papers (in the article, papers with more than 20 authors). As the matrices
#' might only contain some of the authors of each paper (e.g. a bounded population), the number of authors can be
#' given in \code{team_size}.
#'
#' @param X   An incidence matrix of authors (rows) and the citing papers they authored (columns)
#' @param W   A square citation matrix where \code{W[p, q] = 1} if paper \code{p} cites paper \code{q}
#' @param Xb   An incidence matrix of authors and the cited papers they authored. By default, the same as \code{X}
#' @param tau   Minimum number of dimensions in which an author should be weakly dominated
#' @param dimensions   The dimensions to be considered: \code{authored}, \code{cited_papers} and/or \code{cited_authors}
#' @param closed   A logical vector with whether the neighbourhoods of \code{authored}, \code{cited_papers} and \code{cited_authors} are closed
#' @param strict   Whether the strict dominance is \code{asymmetric} (default) or a \code{proper} inclusion
#' @param closure_papers   Whether the closed neighbourhood of cited papers adds the \code{citing} papers of the author (default) or every paper \code{authored}
#' @param max_authors   If not NULL, the citing papers with more authors than this number are excluded
#' @param team_size   Number of authors of each paper, in the same order as the columns of \code{X}. By default, the column sums of \code{X}
#' @param direction   Whether \code{D[u, v] = 1} means that \code{u} is \code{dominated} by \code{v} (default) or that \code{u} \code{dominates} \code{v}, as in the figures of Espinosa-Rada (2026)
#'
#' @return This function returns a binary dominance matrix \code{D} of the authors.
#'
#' @references
#'
#' Espinosa-Rada, A. (2026). Network positions within scholars and intellectual networks. Journal of Informetrics, 20, 101854. \doi{10.1016/j.joi.2026.101854}
#'
#' Schoch, D. and Brandes, U. (2016). Re-conceptualizing centrality in social networks. European Journal of Applied Mathematics, 27(6), 971–985. \doi{10.1017/S0956792516000401}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' X <- matrix(c(
#'   1, 1, 1, 0,
#'   1, 1, 0, 0,
#'   1, 0, 0, 0,
#'   0, 1, 0, 0,
#'   0, 0, 1, 1
#' ), byrow = TRUE, ncol = 4)
#' rownames(X) <- c("a1", "a2", "a3", "a4", "a5")
#' colnames(X) <- c("w1", "w2", "w3", "w4")
#'
#' W <- matrix(c(
#'   0, 1, 1, 0,
#'   0, 0, 1, 0,
#'   0, 0, 0, 1,
#'   0, 0, 0, 0
#' ), byrow = TRUE, ncol = 4)
#' rownames(W) <- colnames(X)
#' colnames(W) <- colnames(X)
#'
#' hyperevent_dominance(X, W, tau = 2)
#' hyperevent_dominance(X, W, tau = 2, strict = "proper")
#' hyperevent_dominance(X, W, tau = 1, dimensions = c("authored", "cited_authors"))
#' @export

hyperevent_dominance <- function(X, W, Xb = X, tau = 2,
                                 dimensions = c("authored", "cited_papers", "cited_authors"),
                                 closed = c(FALSE, TRUE, TRUE),
                                 strict = c("asymmetric", "proper"),
                                 closure_papers = c("citing", "authored"),
                                 max_authors = NULL, team_size = NULL,
                                 direction = c("dominated", "dominates")) {
  X <- as.matrix(X)
  W <- as.matrix(W)
  Xb <- as.matrix(Xb)
  if (nrow(W) != ncol(W)) stop("Matrix W should be square")
  if (is.null(rownames(X))) stop("No label assigned to the rows of the matrix X")
  if (!identical(colnames(X), rownames(W))) stop("The columns of X should match the rows of W")
  if (!identical(colnames(Xb), colnames(W))) stop("The columns of Xb should match the columns of W")
  if (!identical(rownames(X), rownames(Xb))) stop("The rows of X and Xb should be the same authors")
  dimensions <- match.arg(dimensions, several.ok = TRUE)
  if (length(closed) != 3) stop("closed should have one value for each of the three dimensions")
  strict <- match.arg(strict)
  closure_papers <- match.arg(closure_papers)
  direction <- match.arg(direction)
  X[X > 0] <- 1
  W[W > 0] <- 1
  Xb[Xb > 0] <- 1
  # Every paper of each author, before any paper is excluded
  all_papers <- pmax(X, Xb)

  # Large teams are excluded as citing papers only
  if (!is.null(max_authors)) {
    if (is.null(team_size)) {
      team_size <- colSums(X)
    }
    if (length(team_size) != ncol(X)) stop("team_size should have one value for each column of X")
    X[, team_size > max_authors] <- 0
  }

  # A hyper-event requires a cited paper with at least one author
  W[, colSums(Xb) == 0] <- 0
  X[, rowSums(W) == 0] <- 0

  authored <- X
  cited_papers <- 1 * ((X %*% W) > 0)
  cited_authors <- 1 * ((cited_papers %*% t(Xb)) > 0)
  dimnames(cited_authors) <- list(rownames(X), rownames(X))

  # Cited authors without hyper-events remain in the neighbourhoods of the others
  keep <- rowSums(authored) > 0
  closed_authors <- pmax(cited_authors, diag(nrow(X)))
  authored <- authored[keep, , drop = FALSE]
  cited_papers <- cited_papers[keep, , drop = FALSE]
  cited_authors <- cited_authors[keep, , drop = FALSE]
  closed_authors <- closed_authors[keep, , drop = FALSE]
  if (closure_papers == "citing") {
    closed_papers <- pmax(cited_papers, authored)
  } else {
    closed_papers <- pmax(cited_papers, all_papers[keep, , drop = FALSE])
  }

  open <- list(authored = authored, cited_papers = cited_papers, cited_authors = cited_authors)
  closure <- list(authored = authored, cited_papers = closed_papers, cited_authors = closed_authors)
  names(closed) <- names(open)

  inclusions <- list()
  proper <- list()
  for (k in dimensions) {
    if (closed[[k]]) {
      M <- closure[[k]]
    } else {
      M <- open[[k]]
    }
    inclusions[[k]] <- set_inclusion(open[[k]], M)
    proper[[k]] <- set_inclusion(open[[k]], M, proper = TRUE)
  }

  if (tau > length(dimensions)) stop("tau should be between 1 and the number of dimensions")
  if (strict == "proper") {
    pareto_dominance(inclusions, tau = tau, strict = proper, direction = direction)
  } else {
    pareto_dominance(inclusions, tau = tau, direction = direction)
  }
}


#' Dominance pairs
#'
#' Comparable and incomparable pairs of a dominance relation.
#'
#' Two nodes are comparable if one is dominated by the other. Centrality indices resolve the
#' incomparable pairs with a total ranking, which is not required by the structure of the network.
#'
#' @param P   A binary dominance matrix (e.g. from \code{neigh_inclusion()})
#' @param direction   Whether \code{P[u, v] = 1} means that \code{u} is \code{dominated} by \code{v} (default) or that \code{u} \code{dominates} \code{v}
#'
#' @return This function returns the number and proportion of comparable pairs, and a table of the unordered pairs, where \code{u < v} means that \code{u} is dominated by \code{v}.
#'
#' @references
#'
#' Schoch, D. and Brandes, U. (2016). Re-conceptualizing centrality in social networks. European Journal of Applied Mathematics, 27(6), 971–985. \doi{10.1017/S0956792516000401}
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
#' dominance_pairs(neigh_inclusion(A))
#' @export

dominance_pairs <- function(P, direction = c("dominated", "dominates")) {
  P <- as.matrix(P)
  if (nrow(P) != ncol(P)) stop("Matrix should be square")
  direction <- match.arg(direction)
  if (direction == "dominates") {
    P <- t(P)
  }
  n <- nrow(P)
  if (is.null(rownames(P))) {
    rownames(P) <- as.character(1:n)
  }

  pairs <- t(combn(n, 2))
  table <- data.frame(
    u = rownames(P)[pairs[, 1]],
    v = rownames(P)[pairs[, 2]],
    comparable = FALSE,
    direction = NA,
    stringsAsFactors = FALSE
  )
  for (i in 1:nrow(pairs)) {
    uv <- P[pairs[i, 1], pairs[i, 2]] == 1
    vu <- P[pairs[i, 2], pairs[i, 1]] == 1
    table$comparable[i] <- uv | vu
    if (uv & vu) {
      table$direction[i] <- "equivalent"
    } else if (uv) {
      table$direction[i] <- paste(table$u[i], "<", table$v[i])
    } else if (vu) {
      table$direction[i] <- paste(table$v[i], "<", table$u[i])
    }
  }

  return(list(
    comparable = sum(table$comparable),
    incomparable = sum(!table$comparable),
    prop_comparable = mean(table$comparable),
    pairs = table
  ))
}


#' Preserved order
#'
#' Whether a centrality index preserves a dominance relation.
#'
#' A centrality index preserves the dominance if no dominated node has a higher score than the
#' node that dominates it (Schoch and Brandes, 2016). Differences smaller than \code{tol} are
#' considered ties, as eigenvectors and other iterative scores are only approximated.
#'
#' @param P   A binary dominance matrix
#' @param scores   A vector of centrality scores in the same order as the rows of \code{P}
#' @param tol   Numerical tolerance
#' @param direction   Whether \code{P[u, v] = 1} means that \code{u} is \code{dominated} by \code{v} (default) or that \code{u} \code{dominates} \code{v}
#'
#' @return This function returns whether the order is preserved and the pairs that violate it.
#'
#' @references
#'
#' Schoch, D. and Brandes, U. (2016). Re-conceptualizing centrality in social networks. European Journal of Applied Mathematics, 27(6), 971–985. \doi{10.1017/S0956792516000401}
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
#' P <- neigh_inclusion(A)
#' preserved_order(P, rowSums(A))
#' preserved_order(P, betweenness_centrality(A, digraph = FALSE))
#' @export

preserved_order <- function(P, scores, tol = sqrt(.Machine$double.eps),
                            direction = c("dominated", "dominates")) {
  P <- as.matrix(P)
  if (nrow(P) != ncol(P)) stop("Matrix should be square")
  if (length(scores) != nrow(P)) stop("The number of scores should be the same as the number of nodes")
  direction <- match.arg(direction)
  if (direction == "dominates") {
    P <- t(P)
  }
  scores <- as.numeric(scores)
  if (is.null(rownames(P))) {
    rownames(P) <- as.character(1:nrow(P))
  }

  # u dominated by v, but u has a higher score
  gap <- outer(scores, scores, "-")
  violation <- which(P == 1 & gap > tol, arr.ind = TRUE)

  violations <- data.frame(
    dominated = rownames(P)[violation[, 1]],
    dominating = rownames(P)[violation[, 2]],
    score_dominated = scores[violation[, 1]],
    score_dominating = scores[violation[, 2]],
    stringsAsFactors = FALSE
  )

  return(list(preserved = nrow(violations) == 0, violations = violations))
}


#' Dominance layers
#'
#' Layers, status and transitive reduction of a strict dominance relation.
#'
#' The first layer contains the nodes that are not dominated by any other node (maximal elements).
#' These nodes are removed, and the procedure is repeated until all nodes are assigned. Each layer
#' is an antichain, i.e. the nodes within a layer do not dominate each other.
#'
#' The maximal elements are of two kinds (Espinosa-Rada, 2026): \code{dominant} nodes dominate at least
#' one other node, while \code{independent} nodes neither dominate nor are dominated. The remaining nodes
#' are \code{dominated}. The net dominance is the number of nodes dominated minus the number of nodes dominating.
#'
#' The transitive reduction removes the tie \eqn{u \to v} when there is another node \eqn{k} such that
#' \eqn{u \to k \to v}, which is the usual representation of a hierarchy (Hasse diagram).
#'
#' @param D   A binary dominance matrix without cycles, such as the output of \code{pareto_dominance()}
#' @param reduction   Whether to return the transitive reduction of \code{D}
#' @param direction   Whether \code{D[u, v] = 1} means that \code{u} is \code{dominated} by \code{v} (default) or that \code{u} \code{dominates} \code{v}
#'
#' @return This function returns the layers, the layer, status and net dominance of each node and, if requested, the transitive reduction (with the same direction as \code{D}).
#'
#' @references
#'
#' Brandes, U. (2016). Network positions. Methodological Innovations, 9, 1–19. \doi{10.1177/2059799116630650}
#'
#' Espinosa-Rada, A. (2026). Network positions within scholars and intellectual networks. Journal of Informetrics, 20, 101854. \doi{10.1016/j.joi.2026.101854}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' X <- matrix(c(
#'   1, 1, 1, 0,
#'   1, 1, 0, 0,
#'   1, 0, 0, 0,
#'   0, 1, 0, 0,
#'   0, 0, 1, 1
#' ), byrow = TRUE, ncol = 4)
#' rownames(X) <- c("a1", "a2", "a3", "a4", "a5")
#' colnames(X) <- c("w1", "w2", "w3", "w4")
#'
#' D <- pareto_dominance(list(set_inclusion(X)))
#' dominance_layers(D, reduction = TRUE)
#' @export

dominance_layers <- function(D, reduction = FALSE, direction = c("dominated", "dominates")) {
  D <- as.matrix(D)
  if (nrow(D) != ncol(D)) stop("Matrix should be square")
  direction <- match.arg(direction)
  if (direction == "dominates") {
    D <- t(D)
  }
  if (is.null(rownames(D))) {
    rownames(D) <- as.character(seq_len(nrow(D)))
    colnames(D) <- rownames(D)
  }
  diag(D) <- 0

  layer_id <- rep(NA, nrow(D))
  names(layer_id) <- rownames(D)
  layers <- list()
  remaining <- rownames(D)
  k <- 1
  while (length(remaining) > 0) {
    D_work <- D[remaining, remaining, drop = FALSE]
    maximal <- remaining[rowSums(D_work) == 0]
    # With few dimensions required (e.g. tau = 1), two nodes might dominate each other in different dimensions
    if (length(maximal) == 0) {
      warning("The dominance relation has cycles, the nodes in the cycles and below them are not assigned to a layer")
      break
    }
    layers[[k]] <- maximal
    layer_id[maximal] <- k
    remaining <- setdiff(remaining, maximal)
    k <- k + 1
  }

  dominating <- colSums(D) # number of nodes dominated by each node
  dominated_by <- rowSums(D)
  status <- rep("independent", nrow(D))
  status[dominating > 0 & dominated_by == 0] <- "dominant"
  status[dominated_by > 0] <- "dominated"
  names(status) <- rownames(D)
  net <- dominating - dominated_by

  if (reduction) {
    # u -> v is redundant if there is a path u -> k -> v
    D_red <- D * (((D %*% D) > 0) == FALSE)
    if (direction == "dominates") {
      D_red <- t(D_red)
    }
    return(list(layers = layers, layer_id = layer_id, status = status, net_dominance = net, reduction = D_red))
  } else {
    return(list(layers = layers, layer_id = layer_id, status = status, net_dominance = net))
  }
}


#' Neighbourhood inclusion in directed networks
#'
#' Neighbourhood-inclusion preorders for directed networks (Marmulla and Brandes, 2026), which
#' extend the vicinal preorder of the undirected case to the criteria that different families of
#' centrality indices preserve.
#'
#' Let \eqn{N^+(i)} be the nodes that \eqn{i} sends ties to, \eqn{N^-(i)} the ones that send
#' ties to \eqn{i}, and \eqn{N[i]} the same set including \eqn{i}. The strong relations use the
#' open neighbourhoods and the weak ones the closed neighbourhoods:
#'
#' \code{radial_out}: \eqn{N^+(i) \subseteq N^+(j)}, preserved by the indices that measure how
#' far a node reaches, such as out-degree and closeness.
#'
#' \code{radial_in}: \eqn{N^-(i) \subseteq N^-(j)}, the same for the ties received.
#'
#' \code{hierarchical_down}: \eqn{N^+(i) \subseteq N^+(j)} and \eqn{N^-(i) \supseteq N^-(j)}, so
#' \eqn{j} sends more and receives less than \eqn{i}.
#'
#' \code{hierarchical_up}: \eqn{N^-(i) \subseteq N^-(j)} and \eqn{N^+(i) \supseteq N^+(j)}, so
#' \eqn{j} receives more and sends less, which is the criterion of the indices of status.
#'
#' \code{medial}: \eqn{N^+(i) \subseteq N^+[j]} and \eqn{N^-(i) \subseteq N^-[j]}, with two extra
#' conditions when \eqn{i} and \eqn{j} are adjacent, so that the advantage that the dominated
#' node has from that tie is compensated. It is the criterion preserved by betweenness.
#'
#' @param A   A square matrix
#' @param type   The criterion: \code{radial_out} (default), \code{radial_in}, \code{hierarchical_down}, \code{hierarchical_up} or \code{medial}
#' @param strength   Whether the neighbourhoods are open (\code{strong}, default) or closed (\code{weak}). It is ignored for the medial criterion
#' @param direction   Whether \code{P[u, v] = 1} means that \code{u} is \code{dominated} by \code{v} (default) or that \code{u} \code{dominates} \code{v}
#'
#' @return This function returns a binary dominance matrix \code{P}.
#'
#' @references
#'
#' Marmulla, G. and Brandes, U. (2026). Centrality in directed networks. Social Networks, 86, 23–34. \doi{10.1016/j.socnet.2026.01.001}
#'
#' Schoch, D. and Brandes, U. (2016). Re-conceptualizing centrality in social networks. European Journal of Applied Mathematics, 27(6), 971–985. \doi{10.1017/S0956792516000401}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 0, 0,
#'   0, 0, 1, 0, 0,
#'   0, 0, 0, 1, 1,
#'   0, 0, 0, 0, 1,
#'   0, 0, 0, 0, 0
#' ), byrow = TRUE, ncol = 5)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- rownames(A)
#'
#' dir_inclusion(A, type = "radial_out")
#' dir_inclusion(A, type = "medial")
#' @export

dir_inclusion <- function(A, type = c(
                            "radial_out", "radial_in", "hierarchical_down",
                            "hierarchical_up", "medial"
                          ),
                          strength = c("strong", "weak"),
                          direction = c("dominated", "dominates")) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  type <- match.arg(type)
  strength <- match.arg(strength)
  direction <- match.arg(direction)
  A[A > 0] <- 1
  diag(A) <- 0

  out_open <- A # N+(i) is the row of i
  in_open <- t(A) # N-(i) is the column of i
  out_closed <- pmax(A, diag(nrow(A)))
  in_closed <- pmax(t(A), diag(nrow(A)))

  if (type == "medial") {
    # The first two conditions hold for every pair, and the other two only when
    # the nodes are adjacent
    P <- set_inclusion(out_open, out_closed) &
      set_inclusion(in_open, in_closed) &
      (A == 0 | set_inclusion(in_open, out_closed)) &
      (t(A) == 0 | set_inclusion(out_open, in_closed))
    P <- 1 * P
  } else {
    if (strength == "strong") {
      out <- out_open
      into <- in_open
    } else {
      out <- out_closed
      into <- in_closed
    }
    if (type == "radial_out") {
      P <- set_inclusion(out, out)
    }
    if (type == "radial_in") {
      P <- set_inclusion(into, into)
    }
    if (type == "hierarchical_down") {
      # j sends at least as much as i, and receives at most as much
      P <- set_inclusion(out, out) & t(set_inclusion(into, into))
      P <- 1 * P
    }
    if (type == "hierarchical_up") {
      P <- set_inclusion(into, into) & t(set_inclusion(out, out))
      P <- 1 * P
    }
  }

  diag(P) <- 0
  dimnames(P) <- dimnames(A)
  if (direction == "dominates") {
    P <- t(P)
  }
  return(P)
}


#' Positional dominance on indirect relations
#'
#' Dominance between the rows of a matrix of relations, which do not need to be the ties
#' themselves (Brandes, 2016; Schoch and Brandes, 2016).
#'
#' Neighbourhood inclusion compares the ties of the nodes. The same comparison can be made on
#' any relation derived from the network, such as the distances between the nodes or the number
#' of walks that join them, which is what makes different centrality indices comparable.
#'
#' Under total heterogeneity (\code{map = FALSE}) the values are compared one by one: \eqn{i} is
#' dominated by \eqn{j} when its relation with every other node is at most as large. Under total
#' homogeneity (\code{map = TRUE}) the values are sorted before being compared, so it does not
#' matter with whom the relation is held, only how large the values are.
#'
#' With \code{benefit = FALSE} a smaller value is better, which is the case of distances.
#'
#' @param R   A square matrix of relations, such as the output of \code{indirect_rel()}
#' @param map   Whether the values are sorted before being compared (total homogeneity)
#' @param benefit   Whether a larger value is better
#' @param direction   Whether \code{P[u, v] = 1} means that \code{u} is \code{dominated} by \code{v} (default) or that \code{u} \code{dominates} \code{v}
#'
#' @return This function returns a binary dominance matrix \code{P}.
#'
#' @references
#'
#' Brandes, U. (2016). Network positions. Methodological Innovations, 9, 1–19. \doi{10.1177/2059799116630650}
#'
#' Schoch, D. and Brandes, U. (2016). Re-conceptualizing centrality in social networks. European Journal of Applied Mathematics, 27(6), 971–985. \doi{10.1017/S0956792516000401}
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
#' # Distances: the closer to the others, the better
#' D <- indirect_rel(A, type = "distance", digraph = FALSE)
#' pos_dominance(D, benefit = FALSE)
#' pos_dominance(D, benefit = FALSE, map = TRUE)
#' @export

pos_dominance <- function(R, map = FALSE, benefit = TRUE,
                          direction = c("dominated", "dominates")) {
  R <- as.matrix(R)
  if (nrow(R) != ncol(R)) stop("Matrix should be square")
  direction <- match.arg(direction)
  n <- nrow(R)

  P <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) next
      # The relations with the two nodes being compared are left out
      others <- setdiff(seq_len(n), c(i, j))
      x <- R[i, others]
      y <- R[j, others]
      if (map) {
        x <- sort(x)
        y <- sort(y)
      }
      if (benefit) {
        P[i, j] <- 1 * all(x <= y)
      } else {
        P[i, j] <- 1 * all(x >= y)
      }
    }
  }

  dimnames(P) <- dimnames(R)
  if (direction == "dominates") {
    P <- t(P)
  }
  return(P)
}


#' Indirect relations
#'
#' Relations between the nodes that are derived from the ties of the network, to be compared
#' with \code{pos_dominance()}.
#'
#' \code{adjacency}: the ties themselves.
#'
#' \code{distance}: the length of the shortest path between the nodes.
#'
#' \code{walks}: the number of walks of any length that join two nodes, where a walk of length
#' \eqn{k} is discounted by \eqn{\alpha^k}, as in the Katz centrality.
#'
#' \code{shared}: the number of neighbours that two nodes have in common.
#'
#' @param A   A square matrix
#' @param type   The relation: \code{adjacency}, \code{distance} (default), \code{walks} or \code{shared}
#' @param digraph   Whether the matrix is directed or undirected
#' @param alpha   The discount of the longer walks, for the \code{walks} relation
#'
#' @return This function returns a square matrix of relations.
#'
#' @references
#'
#' Brandes, U. (2016). Network positions. Methodological Innovations, 9, 1–19. \doi{10.1177/2059799116630650}
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
#' indirect_rel(A, type = "distance", digraph = FALSE)
#' indirect_rel(A, type = "walks", digraph = FALSE)
#' @export

indirect_rel <- function(A, type = c("distance", "adjacency", "walks", "shared"),
                         digraph = TRUE, alpha = 0.1) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  type <- match.arg(type)
  if (!digraph) {
    A <- pmax(A, t(A)) # Underlying graph
  }

  if (type == "adjacency") {
    R <- A
  }
  if (type == "distance") {
    R <- geodesic_distances(A)
  }
  if (type == "walks") {
    lambda <- max(abs(Re(eigen(A, only.values = TRUE)$values)))
    if (alpha * lambda >= 1) warning("alpha should be smaller than the inverse of the leading eigenvalue, the series does not converge")
    R <- solve(diag(nrow(A)) - alpha * A) - diag(nrow(A))
  }
  if (type == "shared") {
    R <- A %*% t(A)
    diag(R) <- 0
  }

  dimnames(R) <- dimnames(A)
  return(R)
}


#' Rank intervals
#'
#' The ranks that a node can take in the rankings that are consistent with a dominance relation
#' (Schoch and Brandes, 2016).
#'
#' A dominance relation only orders some pairs of nodes. Any centrality index that preserves it
#' gives a complete ranking, but different indices give different rankings. The interval of a
#' node contains every rank it can take in such a ranking: it cannot be ranked below the nodes
#' it dominates, nor above the nodes that dominate it. A node with a wide interval is one whose
#' position depends on the index that is chosen, and a node with an interval of a single value
#' has the same rank under every index that preserves the relation.
#'
#' The rank one is the lowest.
#'
#' @param P   A binary dominance matrix, such as the output of \code{neigh_inclusion()}
#' @param direction   Whether \code{P[u, v] = 1} means that \code{u} is \code{dominated} by \code{v} (default) or that \code{u} \code{dominates} \code{v}
#'
#' @return This function returns the minimum and the maximum rank of every node, and the width of the interval.
#'
#' @references
#'
#' Schoch, D. and Brandes, U. (2016). Re-conceptualizing centrality in social networks. European Journal of Applied Mathematics, 27(6), 971–985. \doi{10.1017/S0956792516000401}
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
#' dominance_ranks(neigh_inclusion(A))
#' @export

dominance_ranks <- function(P, direction = c("dominated", "dominates")) {
  P <- as.matrix(P)
  if (nrow(P) != ncol(P)) stop("Matrix should be square")
  direction <- match.arg(direction)
  if (direction == "dominates") {
    P <- t(P)
  }
  n <- nrow(P)
  if (is.null(rownames(P))) {
    rownames(P) <- as.character(seq_len(n))
  }

  # A node cannot be ranked below the nodes it dominates, nor above the ones
  # that dominate it. The nodes with the same position are counted as well
  strictly_below <- colSums((P - t(P)) == 1)
  strictly_above <- rowSums((P - t(P)) == 1)
  equivalent <- rowSums(P == 1 & t(P) == 1)

  intervals <- data.frame(
    node = rownames(P),
    min_rank = strictly_below + 1,
    max_rank = n - strictly_above - equivalent,
    stringsAsFactors = FALSE
  )
  intervals$width <- intervals$max_rank - intervals$min_rank
  rownames(intervals) <- NULL
  return(intervals)
}
