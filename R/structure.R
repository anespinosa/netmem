#' Reciprocity
#'
#' This measure calculated the reciprocity of an asymmetric matrix (directed graph).
#'
#' @param A   A matrix
#' @param diag   Whether to consider the diagonal of the matrix
#' @param method   Whether to use \code{total_ratio}, \code{ratio_nonnull} or \code{global} reciprocity
#'
#' @return Return a reciprocity coefficient
#'
#' @references
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(0, 1, 1, 0,
#'               1, 0, 1, 0,
#'               0, 0, 0, 0,
#'               1, 0, 0, 0), byrow = TRUE, ncol = 4)
#' recip_coef(A)
#' @export

recip_coef <- function(A, diag = NULL, method = c("total_ratio", "ratio_nonnull", "global")) {
  if (all(A[lower.tri(A)] == t(A)[lower.tri(A)], na.rm = TRUE)) {
    message("Matrix is symmetric (network is undirected)")
  }

  if (is.null(diag)) {
    diag(A) <- 0
  }

  method <- switch(method_option(method),
    "total_ratio" = 1,
    "ratio_nonnull" = 2,
    "global" = 3
  )
  dyad <- dyadic_census(A)
  if (method == 1) {
    # proportion of dyads that are symmetric
    return((dyad[1] + dyad[3]) / sum(unlist(dyad)))
  }
  if (method == 2) {
    # reciprocity, ignoring the null dyads
    return((dyad[1]) / (dyad[1] + dyad[2]))
  }
  if (method == 3) {
    # global reciprocity
    return(sum(A * t(A)) / sum(A))
  }
}

#' Transitivity
#'
#' This measure is sometimes called clustering coefficient.
#'
#' @param A   A matrix
#' @param method   Whether to calculate the \code{weakcensus}, \code{global} transitivity ratio, the \code{mean} transitivity, the \code{local} transitivity or the weighted transitivity of \code{barrat}.
#' @param select    Whether to consider \code{all}, \code{in} or \code{out} ties for the local transitivity.
#'
#' @return Return a transitivity measure
#'
#' @references
#'
#' Barrat, A., Barthelemy, M., Pastor-Satorras, R. and Vespignani, A. (2004). The architecture of complex weighted networks. Proceedings of the National Academy of Sciences, 101(11), 3747–3752. \doi{10.1073/pnas.0400087101}
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' A <- matrix(c(
#'   0, 1, 0, 1, 0,
#'   1, 0, 1, 1, 0,
#'   0, 1, 0, 0, 0,
#'   1, 1, 0, 0, 1,
#'   0, 0, 0, 1, 0
#' ), byrow = TRUE, ncol = 5)
#' rownames(A) <- letters[1:ncol(A)]
#' colnames(A) <- rownames(A)
#'
#' trans_coef(A, method = "local")
#'
#' # The weighted transitivity of Barrat et al. (2004) weighs each triangle
#' # by the strength of the two ties of the node
#' W <- matrix(c(
#'   0, 4, 0, 2, 0,
#'   4, 0, 1, 3, 0,
#'   0, 1, 0, 0, 0,
#'   2, 3, 0, 0, 5,
#'   0, 0, 0, 5, 0
#' ), byrow = TRUE, ncol = 5)
#' trans_coef(W, method = "barrat")
#' @export

# TODO: Improve documentation, add the rank condition, the correlation option of Dekker, the strong census, and expand if necessary.

trans_coef <- function(A, method = c("weakcensus", "global", "mean", "local", "barrat"),
                       select = c("all", "in", "out")) {
  A <- as.matrix(A)
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }

  method <- switch(method_option(method),
    "weakcensus" = 1,
    "global" = 2,
    "mean" = 3,
    "local" = 4,
    "barrat" = 5
  )

  if (method == 5) {
    if (!all(A[lower.tri(A)] == t(A)[lower.tri(A)], na.rm = TRUE)) {
      message("Matrix is asymmetric (network is directed), the underlying graph is used")
      A <- pmax(A, t(A))
    }
    diag(A) <- 0
    ties <- A
    ties[ties > 0] <- 1
    strength <- rowSums(A)
    degree <- rowSums(ties)

    # For every pair of neighbours that are tied to each other, the strength of
    # the two ties of the node is added (Barrat et al., 2004)
    barrat <- rep(NA, nrow(A))
    for (i in 1:nrow(A)) {
      neighbours <- which(ties[i, ] > 0)
      if (length(neighbours) < 2) next
      triangles <- 0
      for (j in neighbours) {
        for (h in neighbours) {
          if (j >= h) next
          if (ties[j, h] > 0) triangles <- triangles + (A[i, j] + A[i, h]) / 2
        }
      }
      barrat[i] <- triangles / (strength[i] * (degree[i] - 1) / 2)
    }
    names(barrat) <- rownames(A)
    return(barrat)
  }

  if (method == 1) {
    path2_A <- (A %*% A)
    diag(path2_A) <- 0
    return(sum(A * path2_A, na.rm = TRUE) / sum(path2_A, na.rm = TRUE))
  }

  if (method == 2) {
    if (!all(A[lower.tri(A)] == t(A)[lower.tri(A)], na.rm = TRUE)) {
      message("Matrix is asymmetric (network is directed), the underlying graph is used")
    }
    A <- A + t(A) # Symmetrize
    A[A > 0] <- 1

    B <- A %*% A
    diag(B) <- 0
    return(sum(diag(A %*% A %*% A)) / sum(B))
  }

  if (method == 3 | method == 4) {
    if (!all(A[lower.tri(A)] == t(A)[lower.tri(A)], na.rm = TRUE)) {
      message("Matrix is asymmetric (network is directed), the underlying graph is used")
    }
    A <- A + t(A) # Symmetrize
    A[A > 0] <- 1

    select <- switch(node_direction(select),
      "out" = 1,
      "in" = 2,
      "all" = 3
    )
    local_trans <- list()
    for (i in 1:ncol(A)) {
      subA <- ego_net(A, ego = rownames(A)[i], select = "all")

      if (all(dim(subA) == 1)) {
        total_pairs <- 0
        pairs <- 0
      } else {
        pairs <- sum(subA) / 2
        total_pairs <- 1 / 2 * ncol(subA) * (ncol(subA) - 1)
      }
      local_trans[[i]] <- pairs / total_pairs
      names(local_trans)[i] <- rownames(A)[i]
    }
  }

  if (method == 3) {
    return(mean = mean(unlist(local_trans), na.rm = TRUE))
  }

  if (method == 4) {
    return(local_trans)
  }
}


method_option <- function(arg, choices, several.ok = FALSE) {
  if (missing(choices)) {
    formal.args <- formals(sys.function(sys.parent()))
    choices <- eval(formal.args[[deparse(substitute(arg))]])
  }

  arg <- tolower(arg)
  choices <- tolower(choices)

  match.arg(arg = arg, choices = choices, several.ok = several.ok)
}

#' Transitivity matrix
#'
#' This function assigns a one in the elements of the matrix if a group of actors are part of a transitivity structure (030T label considering the MAN triad census)
#'
#' @param A   A matrix
#' @param loops  Whether to expect nonzero elements in the diagonal of the matrix
#'
#' @return A vector assigning an id the components that each of the nodes of the matrix belongs
#'
#' @references
#'
#' Davis, J.A. and Leinhardt, S. (1972). “The Structure of Positive Interpersonal Relations in Small Groups.” In J. Berger (Ed.), Sociological Theories in Progress, Vol. 2, 218-251. Boston: Houghton Mifflin.
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' A <- matrix(
#'   c(
#'     0, 1, 1, 0, 0, 0,
#'     0, 0, 1, 0, 0, 0,
#'     0, 0, 0, 1, 0, 0,
#'     0, 0, 0, 0, 0, 0,
#'     0, 0, 1, 1, 0, 0,
#'     0, 0, 0, 0, 0, 0
#'   ),
#'   byrow = TRUE, ncol = 6
#' )
#' rownames(A) <- letters[1:NROW(A)]
#' colnames(A) <- rownames(A)
#' trans_matrix(A, loops = TRUE)
#'
#' @export

trans_matrix <- function(A, loops = FALSE) {
  A <- as.matrix(A)
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }

  if (is.null(rownames(A))) stop("No label assigned to the columns of the matrix")
  if (is.null(colnames(A))) stop("No label assigned to the columns of the matrix")

  B <- matrix(0, ncol = NCOL(A), NROW(A))
  rownames(B) <- rownames(A)
  colnames(B) <- colnames(A)

  # A transitive triple is i -> j, j -> k and i -> k, and the three nodes are
  # marked in the matrix
  for (i in 1:NROW(A)) {
    for (j in 1:NROW(A)) {
      if (A[i, j] == 0) next
      for (k in 1:NROW(A)) {
        if (A[j, k] == 0 | A[i, k] == 0) next
        triple <- c(i, j, k)
        B[triple, triple] <- 1
      }
    }
  }

  if (!loops) {
    diag(B) <- 0
  }
  return(B)
}


#' Components
#'
#' Components of a network: the groups of nodes that can reach each other.
#'
#' In a \code{weak} component the nodes are connected when the direction of the ties is
#' ignored. In a \code{strong} component every node can reach every other node following the
#' direction of the ties, so a strong component is contained in a weak one. For undirected
#' networks both are the same.
#'
#' For two-mode networks, the nodes of both sets are placed in the same network before looking
#' for the components, so a component contains the nodes of the first mode and the ones of the
#' second mode that they share.
#'
#' @param A   A square matrix, or an incidence matrix if \code{bipartite = TRUE}
#' @param mode   Whether the components are \code{weak} (default) or \code{strong}
#' @param bipartite   Whether the matrix is an incidence matrix of a two-mode network
#'
#' @return A vector with the component of each node, and the size of the components.
#'
#' @references
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' A <- matrix(c(
#'   0, 1, 1, 0, 0,
#'   1, 0, 1, 0, 0,
#'   1, 1, 0, 0, 0,
#'   0, 0, 0, 0, 1,
#'   0, 0, 0, 1, 0
#' ), byrow = TRUE, ncol = 5)
#' rownames(A) <- letters[1:ncol(A)]
#' colnames(A) <- rownames(A)
#' components_id(A)
#'
#' # In a chain of citations every node is in the same weak component,
#' # but each of them is its own strong component
#' B <- matrix(c(
#'   0, 1, 0, 0,
#'   0, 0, 1, 0,
#'   0, 0, 0, 1,
#'   0, 0, 0, 0
#' ), byrow = TRUE, ncol = 4)
#' components_id(B)
#' components_id(B, mode = "strong")
#' @export

components_id <- function(A, mode = c("weak", "strong"), bipartite = FALSE) {
  A <- as.matrix(A)
  mode <- match.arg(mode)
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  A[A > 0] <- 1

  if (bipartite) {
    # The two sets of nodes are placed in the same network
    rows <- nrow(A)
    columns <- ncol(A)
    labels <- c(rownames(A), colnames(A))
    B <- matrix(0, rows + columns, rows + columns)
    B[1:rows, (rows + 1):(rows + columns)] <- A
    B[(rows + 1):(rows + columns), 1:rows] <- t(A)
    A <- B
    rownames(A) <- labels
  } else {
    if (nrow(A) != ncol(A)) stop("Matrix should be square")
  }

  if (mode == "weak") {
    A <- pmax(A, t(A)) # the direction of the ties is ignored
  }
  reachable <- is.finite(geodesic_distances(A))
  if (mode == "strong") {
    reachable <- reachable & t(reachable) # both nodes reach each other
  }

  # The nodes that reach the same nodes belong to the same component, and the
  # components are numbered in the order in which their nodes appear
  membership <- apply(reachable, 1, paste, collapse = "")
  components <- as.numeric(factor(membership, levels = unique(membership)))
  names(components) <- rownames(A)
  return(list(components = components, size = table(components)))
}


#' Krackhardt's dimensions of informal organisations
#'
#' The four dimensions that Krackhardt (1994) uses to compare a network with a perfect
#' hierarchy (an out-tree): connectedness, hierarchy, efficiency and least upper boundedness.
#'
#' The measures are computed on the reachability matrix \eqn{R}, where \eqn{R[i,j] = 1} when
#' \eqn{j} can be reached from \eqn{i}:
#'
#' \code{connectedness} is the proportion of pairs of nodes that are connected in the
#' underlying graph, i.e. one minus the proportion of pairs in different weak components.
#'
#' \code{hierarchy} is one minus the proportion of the reachable ordered pairs that are also
#' reachable in the opposite direction. It is one when no pair of nodes can reach each other.
#'
#' \code{efficiency} is one minus the proportion of the ties that are not needed to keep the
#' same weak components. A network is efficient when it has no more ties than a spanning tree.
#'
#' \code{lubness} (upper boundedness) is the proportion of the pairs of nodes that have an
#' upper bound, i.e. a node that reaches both of them. Everett and Krackhardt (2012) recommend
#' this version, as the original condition asks for a \emph{least} upper bound, an upper bound
#' that is on a directed path from every other upper bound to both nodes, which need not be
#' unique and can be a very distant node. The original condition is used with
#' \code{lubness = "least"}. In both cases a node reaches itself, and the violations are counted
#' within the weak components of more than two nodes.
#'
#' All the measures are one for a perfect out-tree.
#'
#' @param A   A square matrix
#' @param lubness   Whether every pair of nodes should have an \code{upper} bound (default, Everett and Krackhardt, 2012) or a \code{least} upper bound (Krackhardt, 1994)
#'
#' @return This function returns the connectedness, hierarchy, efficiency and least upper boundedness of the network.
#'
#' @references
#'
#' Everett, M. G. and Krackhardt, D. (2012). A second look at Krackhardt's graph theoretical dimensions of informal organizations. Social Networks, 34(2), 159–163. \doi{10.1016/j.socnet.2011.10.006}
#'
#' Krackhardt, D. (1994). Graph theoretical dimensions of informal organizations. In K. M. Carley and M. J. Prietula (Eds.), Computational Organization Theory (pp. 89–111). Hillsdale, NJ: Lawrence Erlbaum.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' # A perfect out-tree
#' A <- matrix(c(
#'   0, 1, 1, 0, 0, 0, 0,
#'   0, 0, 0, 1, 1, 0, 0,
#'   0, 0, 0, 0, 0, 1, 1,
#'   0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0
#' ), byrow = TRUE, ncol = 7)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- rownames(A)
#'
#' krackhardt_index(A)
#' krackhardt_index(A, lubness = "least")
#' @export

krackhardt_index <- function(A, lubness = c("upper", "least")) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  lubness <- match.arg(lubness)
  A[A > 0] <- 1
  diag(A) <- 0
  n <- nrow(A)

  R <- is.finite(geodesic_distances(A)) # reachability
  diag(R) <- FALSE
  weak <- is.finite(geodesic_distances(pmax(A, t(A))))
  components <- components_id(pmax(A, t(A)))$components

  # Connectedness: pairs in the same weak component
  connectedness <- (sum(weak) - n) / (n * (n - 1))

  # Hierarchy: dyads of the reachability that are mutual instead of asymmetric
  mutual <- sum(R & t(R)) / 2
  asymmetric <- sum(R | t(R)) / 2 - mutual
  hierarchy <- 1 - (mutual / (mutual + asymmetric))

  # Efficiency: ties beyond the ones needed to connect each component
  sizes <- table(components)
  needed <- n - length(sizes)
  possible <- sum(sizes * (sizes - 1)) - needed
  if (possible == 0) {
    efficiency <- 1
  } else {
    efficiency <- 1 - ((sum(A) - needed) / possible)
  }

  # Lubness: pairs of nodes with an upper bound, within each weak component of
  # more than two nodes. A node reaches itself, so it is an upper bound of a
  # pair that includes it
  diag(R) <- TRUE
  violations <- 0
  max_violations <- 0
  for (k in unique(components)) {
    members <- which(components == k)
    if (length(members) <= 2) next
    max_violations <- max_violations + (length(members) - 1) * (length(members) - 2) / 2
    for (i in members) {
      for (j in members) {
        if (j <= i) next
        upper <- members[R[members, i] & R[members, j]]
        if (length(upper) == 0) {
          violations <- violations + 1
          next
        }
        if (lubness == "least") {
          # A least upper bound is on a directed path from every other upper
          # bound to both nodes, i.e. every upper bound reaches it
          least <- upper[colSums(R[upper, upper, drop = FALSE]) == length(upper)]
          if (length(least) == 0) violations <- violations + 1
        }
      }
    }
  }
  if (max_violations == 0) {
    lubness <- 1
  } else {
    lubness <- 1 - (violations / max_violations)
  }

  return(list(
    connectedness = connectedness,
    hierarchy = hierarchy,
    efficiency = efficiency,
    lubness = lubness
  ))
}


#' Core-periphery structure
#'
#' Core-periphery model of Borgatti and Everett (2000): a group of nodes that are connected
#' among themselves and with the rest, and a periphery of nodes that are connected with the
#' core but not with each other.
#'
#' The \code{discrete} model looks for the partition of the nodes into a core and a periphery
#' that maximises the correlation between the observed matrix and the ideal pattern, where a
#' tie is expected when at least one of the two nodes belongs to the core. The search starts
#' from the nodes sorted by degree, and from \code{rep} random partitions, and then moves one
#' node at a time while the correlation improves. As the search can end in a local optimum, the
#' result of the random starts depends on the seed.
#'
#' The \code{continuous} model gives each node a coreness score instead of a class. The scores
#' maximise the correlation between the observed matrix and the products of the scores of each
#' pair, and are given by the leading eigenvector of the matrix.
#'
#' @param A   A square matrix
#' @param method   Whether to return a \code{discrete} partition (default) or \code{continuous} coreness scores
#' @param digraph   Whether the matrix is directed or undirected
#' @param rep   Number of random partitions used to start the search, besides the one given by the degree of the nodes
#'
#' @return This function returns the fit of the model, and the partition or the coreness scores.
#'
#' @references
#'
#' Borgatti, S. P. and Everett, M. G. (2000). Models of core/periphery structures. Social Networks, 21(4), 375–395. \doi{10.1016/S0378-8733(99)00019-2}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 1, 1, 0,
#'   1, 0, 1, 1, 0, 1,
#'   1, 1, 0, 1, 0, 0,
#'   1, 1, 1, 0, 0, 0,
#'   1, 0, 0, 0, 0, 0,
#'   0, 1, 0, 0, 0, 0
#' ), byrow = TRUE, ncol = 6)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- rownames(A)
#'
#' core_periphery(A)
#' core_periphery(A, method = "continuous")
#' @export

core_periphery <- function(A, method = c("discrete", "continuous"), digraph = FALSE, rep = 50) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  method <- match.arg(method)
  if (!digraph) {
    A <- pmax(A, t(A)) # Underlying graph
  }
  n <- nrow(A)
  if (is.null(rownames(A))) {
    rownames(A) <- as.character(seq_len(n))
    colnames(A) <- rownames(A)
  }

  if (method == "continuous") {
    coreness <- eigenvector_centrality(A, digraph = digraph, scale = "unit")$vector
    ideal <- outer(coreness, coreness)
    return(list(fit = pattern_fit(A, ideal), coreness = coreness))
  }

  # The nodes with the highest degree are the first candidates for the core,
  # and the other starting partitions are random
  degree <- rowSums(A) + colSums(A)
  order_degree <- order(degree, decreasing = TRUE)
  starts <- list()
  for (k in 1:(n - 1)) {
    core <- rep(FALSE, n)
    core[order_degree[1:k]] <- TRUE
    starts[[length(starts) + 1]] <- core
  }
  for (k in seq_len(rep)) {
    core <- sample(c(TRUE, FALSE), n, replace = TRUE)
    if (all(core) | all(!core)) next
    starts[[length(starts) + 1]] <- core
  }

  best_core <- rep(FALSE, n)
  best_fit <- -Inf
  for (start in starts) {
    core <- start
    fit <- pattern_fit(A, ideal_core(core))
    if (!is.finite(fit)) fit <- -Inf

    # Move one node at a time while the fit improves
    repeat {
      improved <- FALSE
      for (i in 1:n) {
        candidate <- core
        candidate[i] <- !candidate[i]
        if (all(candidate) | all(!candidate)) next
        new_fit <- pattern_fit(A, ideal_core(candidate))
        if (is.finite(new_fit) && new_fit > fit + 1e-12) {
          fit <- new_fit
          core <- candidate
          improved <- TRUE
        }
      }
      if (!improved) break
    }

    if (fit > best_fit) {
      best_fit <- fit
      best_core <- core
    }
  }

  return(list(
    fit = best_fit,
    core = rownames(A)[best_core],
    periphery = rownames(A)[!best_core],
    class = ifelse(best_core, "core", "periphery")
  ))
}

# Ideal core-periphery pattern: a tie is expected when at least one of the nodes is in the core
ideal_core <- function(core) {
  1 * outer(core, core, "|")
}

# Correlation between the observed ties and the ideal pattern, without the diagonal
pattern_fit <- function(A, ideal) {
  observed <- A[row(A) != col(A)]
  expected <- ideal[row(ideal) != col(ideal)]
  if (stats::sd(observed) == 0 | stats::sd(expected) == 0) {
    return(NA)
  }
  stats::cor(observed, expected)
}
