#' Mixing matrix
#'
#' Create a mixing matrix from node attributes. The mixing matrix is a two-dimensional
#' matrix that cross-classifies the edges depending on the values of their attributes.
#' This matrix allowed identifying segregation and homophily at the network level.
#'
#' Values in the diagonal are the number of ties within groups, and off-diagonal are the number of relations between groups.
#' For directed networks the entries are the arcs between the groups. For undirected networks every edge is counted
#' once, so the entries outside the diagonal are half of the edges between the two groups.
#'
#' @param A   A symmetric matrix object
#' @param att   Categorical attribute of the nodes
#' @param digraph   Whether the matrix is directed. A symmetric matrix is always treated as undirected
#'
#' @return This function returns a mixing matrix
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' n <- 100
#' A <- matrix(c(rbinom(n, 1, 0.5)),
#'   ncol = sqrt(n), nrow = sqrt(n), byrow = TRUE
#' )
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- letters[1:ncol(A)]
#' att <- rbinom(sqrt(n), 3, 0.5)
#' mix_matrix(A, att = att)
#' @export
#'

# TODO: select a better example!

mix_matrix <- function(A, att = NULL, digraph = TRUE) {
  A <- as.matrix(A)
  if (is.null(att)) stop("No attribute has been specified")
  if (nrow(A) != length(att)) stop("There should be one value of the attribute for each node")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  if (all(A[lower.tri(A)] == t(A)[lower.tri(A)])) {
    digraph <- FALSE # the matrix is symmetric
  }
  diag(A) <- 0

  groups <- sort(unique(att))
  mix_matrix <- matrix(0, length(groups), length(groups),
    dimnames = list(From = groups, To = groups)
  )
  for (g in seq_along(groups)) {
    for (h in seq_along(groups)) {
      mix_matrix[g, h] <- sum(A[att == groups[g], att == groups[h], drop = FALSE])
    }
  }

  if (!digraph) {
    # Every edge is counted once: the diagonal has the ties within a group, and
    # the ties between two groups are split between the two cells
    mix_matrix <- mix_matrix / 2
  }

  return(mix_matrix)
}

#' Krackhardt and Stern's E-I index
#'
#' This index was proposed by Krackhardt and Stern (1988) to distinguish between the relative prevalence
#' of between and within-group ties. This measure can be interpreted as homophily at the network level.
#'
#' @param A  A symmetric matrix object, or a mixing matrix if no attribute is given
#' @param mixed  Whether the matrix provided is already a mixing matrix. It is only used when no attribute is given
#' @param att  Categorical attribute of the nodes. When it is given, the mixing matrix is computed from \code{A}
#'
#' @return Numerical value of the E-I index.
#'
#' @examples
#'
#' set.seed(18051889)
#' n <- 100
#' A <- matrix(c(rbinom(n, 1, 0.5)),
#'   ncol = sqrt(n), nrow = sqrt(n), byrow = TRUE
#' )
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- letters[1:ncol(A)]
#'
#' att <- rbinom(sqrt(n), 3, 0.5)
#' ei_index(A, att = att)
#'
#' # All the ties are within the groups, so the index is -1
#' B <- matrix(0, 6, 6)
#' B[1:3, 1:3] <- 1
#' B[4:6, 4:6] <- 1
#' diag(B) <- 0
#' rownames(B) <- letters[1:6]
#' colnames(B) <- rownames(B)
#' ei_index(B, att = c(1, 1, 1, 2, 2, 2))
#' @export

# TODO: select a better example!

ei_index <- function(A, mixed = TRUE, att = NULL) {
  A <- as.matrix(A)
  # With an attribute the mixing matrix is computed, as the index cannot be
  # read from the ties themselves
  if (!is.null(att)) {
    matrix <- netmem::mix_matrix(A, att)
  } else {
    if (!mixed) stop("An attribute of the nodes is needed to compute the mixing matrix")
    matrix <- A
  }
  if (length(dim(matrix)) == 3) {
    m <- matrix[, , 2]
  } else {
    m <- matrix
  }
  pI <- m / sum(m)
  I <- sum(diag(pI))
  diag(pI) <- 0
  E <- sum(pI)
  EIindex <- E - I
  return(EIindex)
}

#' Blau's and IQV index
#'
#' This index was used by Blau (1977) to distinguish between the relative prevalence
#' of between and within-group ties. This measure can be interpreted as heterogeneity at the network level.
#'
#' @param att  Categorical attribute of the nodes
#' @param normalized  Whether to return IQV index
#'
#' @return Numerical value of the Blau index.
#'
#' If \code{normalized = TRUE}, then the function also return IQV index.
#'
#' @references
#'
#' Agresti, A. and Agresti, B. (1978). Statistical Analysis of Qualitative Variation. Sociological Methodology, 9, 204-237. \doi{10.2307/270810}
#'
#' Blau, P. M. (1977). Inequality and heterogeneity. New York: Free Press.
#'
#' @examples
#'
#' a <- rep(1:10, 10)
#' heterogeneity(a, normalized = TRUE)
#'
#' a <- rep(1:2, 10)
#' heterogeneity(a, normalized = TRUE)
#' @export

heterogeneity <- function(att, normalized = FALSE) {
  att <- as.character(att)
  p <- (table(att) / sum(table(att)))^2
  r <- length(p)
  blau <- 1
  for (i in 1:r) {
    blau <- blau - p[[i]]
  }
  if (normalized) {
    iqv <- blau / (1 - 1 / r)
    return(list(blau = blau, iqv = iqv))
  } else {
    (return(blau))
  }
}


#' Segregation measures
#'
#' Measures of how much the ties of a network stay within the groups given by an attribute of
#' the nodes, reviewed by Bojanowski and Corten (2014).
#'
#' All the measures are computed from the mixing matrix, which counts the ties within and
#' between groups:
#'
#' \code{assortativity}: the proportion of ties that are within groups, compared with the
#' proportion expected if the ties were distributed at random keeping how active each group is
#' (Newman, 2003). It is one when every tie is within a group, and zero under random mixing.
#'
#' \code{gam}: the index of Gupta, Anderson and May (1989), the trace of the matrix of the
#' proportion of the ties of each group that go to every other group, rescaled to go from
#' \eqn{-1/(K-1)} to one. It is defined for undirected networks, and every group should have
#' at least one tie.
#'
#' \code{orwg}: the odds of a tie within a group divided by the odds of a tie between groups
#' (Moody, 2001). Unlike the other measures, it takes into account the pairs of nodes that are
#' not tied, so it is not affected by the density of the network.
#'
#' \code{coleman}: the homophily index of Coleman (1958), computed for each group: how many
#' ties the group sends to itself compared with the ties it would send if it chose the other
#' nodes at random. It is one when the group only relates to itself.
#'
#' \code{freeman}: the segregation index of Freeman (1978) for two groups: how many fewer ties
#' between the groups there are than the ones expected in a random network with the same
#' density and group sizes. It is zero when there are as many as expected, or more.
#'
#' @param A   A square matrix
#' @param att   A vector with the group of each node
#' @param method   The measure: \code{assortativity} (default), \code{gam}, \code{orwg}, \code{coleman} or \code{freeman}
#' @param digraph   Whether the matrix is directed or undirected. The \code{coleman} index is defined for directed networks
#' @param loops   Whether to consider the loops of the matrix
#'
#' @return This function returns the value of the measure, which is a value per group for the \code{coleman} index.
#'
#' @references
#'
#' Bojanowski, M. and Corten, R. (2014). Measuring segregation in social networks. Social Networks, 39, 14–32. \doi{10.1016/j.socnet.2014.04.001}
#'
#' Coleman, J. (1958). Relational analysis: The study of social organizations with survey methods. Human Organization, 17(4), 28–36. \doi{10.17730/humo.17.4.q5604m676260q8n7}
#'
#' Freeman, L. C. (1978). Segregation in social networks. Sociological Methods and Research, 6(4), 411–429. \doi{10.1177/004912417800600401}
#'
#' Gupta, S., Anderson, R. M. and May, R. M. (1989). Networks of sexual contacts: implications for the pattern of spread of HIV. AIDS, 3(12), 807–817. \doi{10.1097/00002030-198912000-00005}
#'
#' Moody, J. (2001). Race, school integration, and friendship segregation in America. American Journal of Sociology, 107(3), 679–716. \doi{10.1086/338954}
#'
#' Newman, M. E. J. (2003). Mixing patterns in networks. Physical Review E, 67(2), 026126. \doi{10.1103/PhysRevE.67.026126}
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
#' att <- c("a", "a", "a", "b", "b", "b")
#'
#' segregation(A, att)
#' segregation(A, att, method = "orwg")
#' segregation(A, att, method = "coleman", digraph = FALSE)
#' @export

segregation <- function(A, att, method = c("assortativity", "gam", "orwg", "coleman", "freeman"),
                        digraph = FALSE, loops = FALSE) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (length(att) != nrow(A)) stop("There should be one group for each node")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  method <- match.arg(method)
  A[A > 0] <- 1
  if (!digraph) {
    A <- pmax(A, t(A)) # Underlying graph
  }
  if (!loops) {
    diag(A) <- 0
  }

  groups <- sort(unique(att))
  sizes <- as.numeric(table(factor(att, levels = groups)))
  names(sizes) <- groups
  n <- nrow(A)

  # Mixing matrix: the ties within and between the groups
  M <- matrix(0, length(groups), length(groups), dimnames = list(groups, groups))
  for (g in seq_along(groups)) {
    for (h in seq_along(groups)) {
      M[g, h] <- sum(A[att == groups[g], att == groups[h], drop = FALSE])
    }
  }
  if (!digraph) {
    M <- M / 2 # every tie is counted from both ends
  }

  if (method == "coleman") {
    # The ties that each group sends to itself, against the ones expected by chance
    sent <- rowSums(M)
    expected <- sent * (sizes - 1) / (n - 1)
    coleman <- (diag(M) - expected) / (sent - expected)
    below <- diag(M) <= expected
    coleman[below] <- ((diag(M) - expected) / expected)[below]
    names(coleman) <- groups
    return(coleman)
  }

  if (method == "assortativity") {
    p <- M / sum(M)
    expected <- sum(colSums(p) * rowSums(p))
    return((sum(diag(p)) - expected) / (1 - expected))
  }

  if (method == "gam") {
    if (length(groups) < 2) stop("There should be at least two groups")
    # Here the ties between two groups are counted in full in both cells, so
    # the rows give the ties of each group to every other group
    ties <- M
    ties[row(ties) != col(ties)] <- 2 * ties[row(ties) != col(ties)]
    if (any(rowSums(ties) == 0)) stop("Every group should have at least one tie")
    p <- ties / rowSums(ties)
    return((sum(diag(p)) - 1) / (length(groups) - 1))
  }

  if (method == "freeman") {
    if (length(groups) != 2) stop("The segregation index of Freeman is defined for two groups")
    between <- sum(M) - sum(diag(M))
    expected <- (sum(M) * (n^2 - sum(sizes^2))) / (n * (n - 1))
    return((expected - between) / expected)
  }

  if (method == "orwg") {
    within_ties <- sum(diag(M))
    between_ties <- sum(M) - within_ties
    if (digraph) {
      within_pairs <- sum(sizes * (sizes - 1))
      total_pairs <- n * (n - 1)
    } else {
      within_pairs <- sum(sizes * (sizes - 1)) / 2
      total_pairs <- n * (n - 1) / 2
    }
    between_pairs <- total_pairs - within_pairs
    return((within_ties * (between_pairs - between_ties)) /
      ((within_pairs - within_ties) * between_ties))
  }
}
