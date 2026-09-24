#' Redundancy measures
#'
#' Redundancy measures of the structural holes theory for binary matrixes
#'
#' @param A   A symmetric matrix object
#' @param ego   Name of ego in the matrix
#' @param digraph   Whether the  matrix is directed or undirected
#' @param weighted  Whether the matrix is weighted or not
#'
#' @return This function returns redundancy, effective size and efficiency measures (Burt, 1992).
#'
#' @references
#'
#' Burt, R.S., 1992. Structural Holes: the Social Structure of Competition. Harvard University Press, Cambridge.
#'
#' Borgatti, S., 1997. Unpacking Burt's redundancy measure. Connections, 20(1): 35-38.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' A <- matrix(c(
#'   0, 1, 0, 0, 1, 1, 1,
#'   1, 0, 0, 1, 0, 0, 1,
#'   0, 0, 0, 0, 0, 0, 1,
#'   0, 1, 0, 0, 0, 0, 1,
#'   1, 0, 0, 0, 0, 0, 1,
#'   1, 0, 0, 0, 0, 0, 1,
#'   1, 1, 1, 1, 1, 1, 0
#' ), ncol = 7, byrow = TRUE)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- letters[1:ncol(A)]
#' redundancy(A, ego = "g")
#' @export

# TODO: DIRECTED AND WEIGHTED CASES

redundancy <- function(A, ego = NULL, digraph = FALSE, weighted = FALSE) {
  A <- suppressMessages(ego_net(A, ego = ego))
  A <- as.matrix(A)
  if (nrow(A) == 0) stop("Ego is an isolate, the redundancy is not defined")
  A <- ifelse(A > 0, 1, 0) # Binarize
  A <- pmax(A, t(A)) # Underlying graph
  diag(A) <- 0

  if (digraph == TRUE) stop("Measure only implemented for binary and undirected networks,
                        for directed networks it would use the underlying graph")
  if (weighted == TRUE) stop("Measure only implemented for binary and undirected networks,
                        for directed networks it would use the underlying graph")
  if (digraph == TRUE & weighted == TRUE) stop("Measure only implemented for binary and undirected networks,
                        for directed networks it would use the underlying graph")

  if (digraph == FALSE) {
    redundancy <- mean(rowSums(A))
    effective_size <- ncol(A) - redundancy
    efficiency <- effective_size / ncol(A)
    return(list(
      redundancy = redundancy,
      effective_size = effective_size,
      efficiency = efficiency
    ))
  }
}

#' Constraint
#'
#' Everett and Borgatti specification of the constraint measure for binary, directed and valued matrices
#'
#' @param A   A matrix object
#' @param ego   Name of ego in the matrix
#' @param digraph   Whether the  matrix is directed or undirected
#' @param weighted  Whether the matrix is weighted or not
#'
#' @return This function returns term 1, 2 and 3, the normalization and the maximum value of the specification of Everett and Borgatti (2020),
#' and the constraint of Burt (1992).
#'
#' @details
#' The constraint of Burt (1992) is computed in the ego network, from the proportion \eqn{p_{ij}} of the ties of
#' each node \eqn{i} that go to \eqn{j}, and is split in the three terms of Everett and Borgatti (2020: Eq. 2):
#' \deqn{\sum_j p_{ij}^2 + 2 \sum_j p_{ij} \sum_q p_{iq} p_{qj} + \sum_j \left(\sum_q p_{iq} p_{qj}\right)^2}
#' For binary undirected networks, the first term is one over the number of alters \eqn{N}.
#'
#' Burt (1992) uses the ties in both directions, \eqn{p_{ij} \propto a_{ij} + a_{ji}}, so the constraint of a directed
#' network is that of the undirected valued network \eqn{A + A^T}, in which a reciprocated tie counts twice and an
#' unreciprocated tie once (Everett and Borgatti, 2020: 53).
#'
#' The normalization is \eqn{(c - 1/N) / (c_{max} - 1/N)}, where \eqn{1/N} is the minimum and \eqn{c_{max}}
#' the maximum constraint of an ego with \eqn{N} alters. The maximum is reached in a complete ego network or in a
#' shadow ego network, in which one alter is tied to all the others and there are no other ties among alters
#' (Everett and Borgatti, 2020: Eq. 4, 6, 7, 8 and 9). For valued networks the maximum depends on the smallest
#' (\eqn{m}) and the largest (\eqn{M}) value of the ties in the ego network, and binary networks are the case
#' \eqn{m = M = 1}. The maximum is a conjecture of Everett and Borgatti, checked by enumerating ego networks.
#' An ego with a single alter has a normalized constraint of one.
#'
#' @references
#'
#' Burt, R.S., 1992. Structural Holes: the Social Structure of Competition. Harvard University Press, Cambridge.
#'
#' Everett, M.G. and Borgatti, S., 2020. Unpacking Burt's constraint measure. Social Networks 62, pp. 50-57. \doi{10.1016/j.socnet.2020.02.001}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' A <- matrix(c(
#'   0, 1, 1, 0, 0, 1,
#'   1, 0, 1, 0, 0, 1,
#'   1, 1, 0, 0, 0, 1,
#'   0, 0, 0, 0, 1, 1,
#'   0, 0, 0, 1, 0, 1,
#'   1, 1, 1, 1, 1, 0
#' ), ncol = 6, byrow = TRUE)
#'
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- letters[1:ncol(A)]
#' eb_constraint(A, ego = "f")
#'
#' # Directed network: f -> a is not reciprocated
#' D <- A
#' D["a", "f"] <- 0
#' eb_constraint(D, ego = "f", digraph = TRUE)
#'
#' # Valued network
#' W <- A
#' W["f", "a"] <- W["a", "f"] <- 3
#' eb_constraint(W, ego = "f", weighted = TRUE)
#' @export

eb_constraint <- function(A, ego = NULL, digraph = FALSE, weighted = FALSE) {
  A <- ego_net(A, ego = ego, addEgo = TRUE)
  A <- as.matrix(A)
  if (nrow(A) < 2) stop("Ego is an isolate, the constraint is not defined")
  if (!weighted) {
    A <- ifelse(A > 0, 1, 0) # Binarize
  }
  if (!digraph) {
    A <- pmax(A, t(A)) # Underlying graph
  }
  diag(A) <- 0

  if (dim(A)[1] != dim(A)[2]) stop("Matrix should be square")

  if (is.numeric(ego)) stop("Label of the name of ego should be in character format")
  if (is.null(rownames(A))) stop("No label assigned to the rows of the matrix")
  if (is.null(colnames(A))) stop("No label assigned to the columns of the matrix")
  if (any(A < 0)) stop("The constraint is defined for non-negative ties")

  # Burt (1992) uses the ties in both directions, so a directed network is the
  # undirected valued network A + t(A), in which a reciprocated tie counts twice
  if (digraph) {
    W <- A + t(A)
  } else {
    W <- A
  }
  # Proportion of the ties of each node that go to each contact. Every alter is
  # tied to ego, so no row sums to zero
  P <- W / rowSums(W)

  alters <- rownames(P) != ego
  N <- sum(alters)
  p_ego <- P[ego, alters]
  # Investment of ego in each alter j through the other alters q. The diagonal
  # of P is zero, so q is never j
  indirect <- as.vector(p_ego %*% P[alters, alters, drop = FALSE])

  term1 <- sum(p_ego^2)
  term2 <- 2 * sum(p_ego * indirect)
  term3 <- sum(indirect^2)
  constraint <- term1 + term2 + term3

  # Maximum constraint of an ego with N alters, in a complete or in a shadow ego
  # network, for the smallest (m) and the largest (M) value of the ties
  m <- min(A[A > 0])
  M <- max(A[A > 0])
  if (digraph) {
    complete <- (1 / N) * ((m + 4 * M * (N - 1)) / (m + 2 * M * (N - 1)))^2
    shadow <- (N - 1) * ((N * m + 2 * M) / (N * (N * m - m + 2 * M)))^2 +
      ((2 * M * (2 * M + N * m)) / ((m + 2 * M) * (N * m - m + 2 * M)))^2
  } else {
    complete <- (1 / N) * ((m + 2 * M * (N - 1)) / (m + M * (N - 1)))^2
    shadow <- (N - 1) * ((N * m + M) / (N * (N * m - m + M)))^2 +
      ((M * (M + N * m)) / ((m + M) * (N * m - m + M)))^2
  }
  maximum <- max(complete, shadow)

  # The minimum is 1/N, reached when the alters are not tied to each other
  if (N == 1) {
    normalization <- 1
  } else {
    normalization <- (constraint - 1 / N) / (maximum - 1 / N)
  }
  output <- cbind(term1, term2, term3, constraint, normalization)
  label <- c("term1", "term2", "term3", "constraint", "normalization")
  output <- as.data.frame(output)
  output <- round(output, 3)
  colnames(output) <- label
  # The row and the maximum are named after ego, as in version 1.0-3
  rownames(output) <- ego
  maximum <- round(maximum, 3)
  names(maximum) <- ego
  newlist <- list(results = output, maximum = maximum)
  return(newlist)
}

#' Structural holes
#'
#' Effective size, efficiency and constraint of Burt (1992) for every node of a valued or directed network, with
#' the option of treating the alters of the same category as redundant (Everett and Borgatti, 2026).
#'
#' @details
#' Burt (1992) measures the ties of a node \eqn{i} with its alters \eqn{j} through the proportion of the ties of
#' \eqn{i} that go to \eqn{j}, \eqn{p_{ij} = (a_{ij} + a_{ji}) / \sum_k (a_{ik} + a_{ki})}, so a directed tie counts in
#' both directions. The effective size is \eqn{\sum_j (1 - \sum_q p_{iq} m_{jq})}, where \eqn{m_{jq}} is the tie of
#' \eqn{j} with \eqn{q} divided by the strongest tie of \eqn{j}; the efficiency is the effective size divided by the
#' number of alters; and the constraint is \eqn{\sum_j (p_{ij} + \sum_q p_{iq} p_{qj})^2}.
#'
#' With \code{ego_network = TRUE} (default, as in UCINET) the proportions are computed within the ego network of
#' each node, and with \code{ego_network = FALSE} within the whole network, as in \code{igraph::constraint()}. For
#' binary undirected networks and ego networks, the effective size is the one of \code{redundancy()} and the
#' constraint the one of \code{eb_constraint()}.
#'
#' When \code{B} is given, two alters of the same category are treated as partly redundant even when they are not
#' tied: each missing tie between two alters is given the value \eqn{\beta \sum_k B_{xk} B_{yk}}, the product of
#' their memberships, before the measures are computed (Everett and Borgatti, 2026: Eq. 6). With a partition and
#' \code{beta = 1} two alters of the same category count as tied; \code{beta = 0} gives the original measures, and
#' the values in between set how much the category matters.
#'
#' @param A   A square matrix, binary or valued
#' @param B   An optional matrix of the membership of the nodes (rows) in the categories (columns), or a vector with the category of each node
#' @param beta   The strength of the tie added between two alters of the same category
#' @param ego_network   Whether the measures are computed within the ego network of each node (TRUE) or within the whole network
#'
#' @return This function returns a data frame with the number of alters, the effective size, the efficiency and the constraint of each node.
#'
#' @references
#'
#' Burt, R.S., 1992. Structural Holes: the Social Structure of Competition. Harvard University Press, Cambridge.
#'
#' Everett, M. G. and Borgatti, S. P. (2026). Alter composition with overlapping group memberships. Social Networks, 85, 80–88. \doi{10.1016/j.socnet.2025.12.001}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' data(campnet)
#' structural_holes(campnet$network)
#'
#' # Alters of the same gender are partly redundant
#' structural_holes(campnet$network, B = campnet$attributes$gender, beta = 0.5)
#' @export

structural_holes <- function(A, B = NULL, beta = 1, ego_network = TRUE) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  if (any(A < 0)) stop("The structural holes are defined for non-negative ties")
  diag(A) <- 0
  if (!is.null(B)) {
    B <- category_matrix(B)
    if (nrow(A) != nrow(B)) stop("There should be one row of B for each node")
    # Strength of the tie added between two alters for their shared categories
    shared <- beta * B %*% t(B)
  }

  n <- nrow(A)
  holes <- data.frame(
    alters = rep(0, n), effective_size = NA, efficiency = NA, constraint = NA,
    row.names = rownames(A)
  )
  for (i in seq_len(n)) {
    alters <- which(A[i, ] + A[, i] > 0)
    if (length(alters) == 0) next
    M <- A
    if (!is.null(B)) {
      for (x in alters) {
        for (y in alters) {
          if (x != y && A[x, y] == 0) {
            M[x, y] <- shared[x, y]
          }
        }
      }
    }
    if (ego_network) {
      keep <- c(i, alters)
      M <- M[keep, keep, drop = FALSE]
      ego <- 1
      alters <- 2:length(keep)
    } else {
      ego <- i
    }

    # Ties in both directions, as proportions of the ties of each node (P) and
    # of its strongest tie (S)
    Z <- M + t(M)
    P <- Z / rowSums(Z)
    S <- Z / apply(Z, 1, max)
    P[!is.finite(P)] <- 0
    S[!is.finite(S)] <- 0

    effective_size <- 0
    constraint <- 0
    for (j in alters) {
      others <- setdiff(alters, j)
      effective_size <- effective_size + 1 - sum(P[ego, others] * S[j, others])
      constraint <- constraint + (P[ego, j] + sum(P[ego, others] * P[others, j]))^2
    }
    holes$alters[i] <- length(alters)
    holes$effective_size[i] <- effective_size
    holes$efficiency[i] <- effective_size / length(alters)
    holes$constraint[i] <- constraint
  }
  return(holes)
}
