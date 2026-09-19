#' Forbidden triad table
#'
#' This function explores dyads and triads (Simmel, 1950), building from the
#' 'forbidden triad' (Granovetter, 1973).
#' First, the minimum structure is an isolated node, then dyads.
#' Afterwards, different combinations of 'forbidden triads' are explored.
#'
#' @details
#' For each node, every pair of its neighbours forms a triad with the node at its centre. The triad is a
#' forbidden triad (type \code{201}) when the two neighbours are not tied, which Granovetter (1973) argued is
#' unlikely when both ties are strong, and it is closed (type \code{300}) when they are. A node with a single
#' neighbour is listed with its dyad (type \code{102}) and an isolated node alone (type \code{003}). The
#' underlying graph of the network is used.
#'
#' The same triad receives the same number in \code{triad} for every node that lists it: a closed triad is
#' listed by its three nodes, and a forbidden triad only by its centre.
#'
#' @param A   A symmetric matrix object.
#' @param adjacency_list   Whether to return the adjacency list of the triads per node.
#' @param min   Numeric constant, lower limit on the number of forbidden triads (201) of which a node is the centre. NULL means no limit.
#' @param max   Numeric constant, upper limit on the number of forbidden triads (201) of which a node is the centre. NULL means no limit.
#'
#' @return This function returns a data frame with the triads of each node: the \code{node}, the number of the
#' \code{triad}, its \code{members} and its \code{type}.
#'
#' If \code{adjacency_list = TRUE} it also  return the adjacency list of
#' the triads per node.
#'
#' @references
#'
#' Granovetter, M.S. (1973). The Strength of Weak Ties. American Journal of Sociology. 78 (6): 1360–80. \doi{10.1086/225469}.
#'
#' Simmel, G. (1950). Individual and Society. In K. H. Wolff (Ed.), The Sociology of George Simmel. New York: Free Press.
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 1, 0,
#'   1, 0, 1, 0, 0,
#'   1, 1, 0, 0, 0,
#'   1, 0, 0, 0, 1,
#'   0, 0, 0, 1, 0
#' ), byrow = TRUE, ncol = 5)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- letters[1:ncol(A)]
#'
#' dyad_triad_table(A)
#'
#' # Nodes at the centre of at least two forbidden triads
#' dyad_triad_table(A, adjacency_list = TRUE, min = 2)
#' @export
#'

dyad_triad_table <- function(A, adjacency_list = FALSE, min = NULL, max = NULL) {
  A <- as.matrix(A)
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  if (is.null(rownames(A))) stop("No label assigned to the rows of the matrix")
  if (is.null(colnames(A))) stop("No label assigned to the columns of the matrix")
  if (all(rownames(A) != colnames(A))) stop("The names of rows and columns does not match")
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(abs(A > 1), na.rm = TRUE)) warning("The matrix should be binary")
  if (!all(A[lower.tri(A)] == t(A)[lower.tri(A)], na.rm = TRUE)) warning("The network is directed. The underlying graph is used")
  A <- pmax(A, t(A)) # Underlying graph
  A[A > 0] <- 1
  diag(A) <- 0
  nodes <- rownames(A)

  # The structures of each node: every pair of its neighbours, its dyad when it
  # has a single neighbour, or the node alone when it is isolated
  table <- NULL
  for (i in seq_along(nodes)) {
    neighbours <- which(A[i, ] > 0)
    if (length(neighbours) == 0) {
      table <- rbind(table, data.frame(node = nodes[i], members = nodes[i], type = "003"))
    }
    if (length(neighbours) == 1) {
      table <- rbind(table, data.frame(
        node = nodes[i],
        members = paste(sort(nodes[c(i, neighbours)]), collapse = "|"), type = "102"
      ))
    }
    if (length(neighbours) > 1) {
      pairs <- t(utils::combn(neighbours, 2))
      for (k in seq_len(nrow(pairs))) {
        closed <- A[pairs[k, 1], pairs[k, 2]] > 0
        table <- rbind(table, data.frame(
          node = nodes[i],
          members = paste(sort(nodes[c(i, pairs[k, ])]), collapse = "|"),
          type = if (closed) "300" else "201"
        ))
      }
    }
  }
  table$triad <- as.numeric(factor(table$members, levels = unique(table$members)))
  table <- table[, c("node", "triad", "members", "type")]

  # Number of forbidden triads of which each node is the centre
  if (!is.null(min) || !is.null(max)) {
    forbidden <- table(factor(table$node[table$type == "201"], levels = nodes))
    keep <- rep(TRUE, length(nodes))
    if (!is.null(min)) keep <- keep & forbidden >= min
    if (!is.null(max)) keep <- keep & forbidden <= max
    if (!any(keep)) stop("No node is the centre of a number of forbidden triads within the limits")
    table <- table[table$node %in% nodes[keep], ]
  }
  rownames(table) <- NULL

  if (adjacency_list) {
    return(list(nodes = table, adjacency_list = split(table$members, factor(table$node, levels = unique(table$node)))))
  }
  return(table)
}

#' Clique table
#'
#' Exploration of a 3-cliques, as the maximum number of three or more actors who
#' have all possible ties present among themselves
#'
#' @param A   A symmetric matrix object.
#' @param list_cliques   Whether to return the list of cliques.
#' @param number   Number of triangles
#'
#' @return This function return an edge list of actors participating in 3-cliques.
#'
#' If \code{list_cliques = TRUE} it also  return the list of cliques per nodes.
#' If \code{number = TRUE} the output returns the number of 3-cliques in the matrix.
#'
#' @references
#'
#' Luce, R.D. and Perry, A.D. (1949). A method of matrix analysis of group structure. Psychometrika, 14: 95-116.
#'
#' Roethlisberger, F.J. and Dickson, W.J. (1939). Management and the Worker. Harvard University Press, Cambridge, MA.
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 0, 0, 0, 0, 1, 0,
#'   1, 0, 1, 0, 0, 0, 0, 0, 0,
#'   1, 1, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 1, 1, 0, 0, 0,
#'   0, 0, 0, 1, 0, 0, 0, 0, 0,
#'   0, 0, 0, 1, 0, 0, 1, 1, 0,
#'   0, 0, 0, 0, 0, 1, 0, 1, 0,
#'   1, 0, 0, 0, 0, 1, 1, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0
#' ), byrow = TRUE, ncol = 9)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- rownames(A)
#' clique_table(A, list_cliques = TRUE, number = TRUE)
#' @export

clique_table <- function(A, list_cliques = FALSE, number = FALSE) {
  A <- as.matrix(A)
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  if (is.null(rownames(A))) stop("No label assigned to the rows of the matrix")
  if (is.null(colnames(A))) stop("No label assigned to the columns of the matrix")
  if (all(rownames(A) != colnames(A))) stop("The names of rows and columns does not match")
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(abs(A > 1), na.rm = TRUE)) warning("The matrix should be binary")
  if (!all(A[lower.tri(A)] == t(A)[lower.tri(A)], na.rm = TRUE)) warning("The network is directed. The underlying graph is used")
  A <- pmax(A, t(A)) # Underlying graph
  diag(A) <- 0

  adj_list <- list()
  cliques <- list()
  neighbours <- list()

  for (i in 1:ncol(A)) {
    adj_list[[i]] <- names(A[i, ][A[i, ] >= 1])
    if (length(adj_list[[i]]) > 1) {
      adj_list[[i]] <- t(combn(adj_list[[i]], 2))
      adj_list[[i]] <- cbind(colnames(A)[i], adj_list[[i]])
      adj_list[[i]] <- t(apply(adj_list[[i]], 1, sort))

      neighbours[[i]] <- adj_list[[i]]
      names(neighbours)[i] <- rownames(A)[i]

      # The nodes of each triad, separated so that names such as "1" and "12"
      # cannot be confused
      cliques[[i]] <- apply(adj_list[[i]], 1, paste, collapse = "|")
    }
  }
  t <- table(unlist(cliques))[which(table(unlist(cliques)) >= 3)]

  if (all(table(unlist(cliques)) < 2)) stop("There are no cliques in the matrix")

  clique_table <- list()
  for (i in 1:ncol(A)) {
    if (length(adj_list[[i]]) > 1) {
      clique_table[[i]] <- which(cliques[[i]] %in% names(t))
      clique_table[[i]] <- (cliques[[i]])[clique_table[[i]]]
      names(clique_table)[i] <- rownames(A)[i]
    }
  }
  new_list <- list()
  size <- list()
  for (i in 1:length(clique_table)) {
    if (length(clique_table[[i]]) > 0) {
      new_list[[i]] <- clique_table[[i]]
      names(new_list)[i] <- names(clique_table[i])
      size[[i]] <- lengths(new_list[[i]])
    }
  }

  size <- sapply(size, sum)
  triad300 <- as.numeric(factor(unlist(new_list)))
  node <- rep(names(new_list), times = size)
  nodes <- cbind(node, triad300)
  neighbours <- neighbours[names(neighbours) %in% nodes[, 1]]
  if (any(sapply(new_list, is.null))) {
    new_list <- new_list[-which(sapply(new_list, is.null))]
  }
  for (i in 1:length(names(neighbours))) {
    neighbours[[i]] <- neighbours[[i]][which(apply(neighbours[[i]],
      1, paste,
      collapse = "|"
    ) %in% new_list[[i]]), , drop = FALSE]
  }

  if (list_cliques & number) {
    return(list(
      table = nodes,
      n_triangles = sum(diag(A %*% A %*% A)) / 6,
      neighbours = neighbours
    ))
  } else {
    if (list_cliques) {
      return(list(table = nodes, neighbours = neighbours))
    }
    if (number) {
      return(list(table = nodes, n_triangles = sum(diag(A %*% A %*% A)) / 6))
    } else {
      return(list(table = nodes))
    }
  }
}

#' Shared partners
#'
#' @param A   A binary matrix
#' @param loops   Whether to consider the loops
#' @param directed   Whether the matrix is directed
#' @param type   Whether to return the \code{dyad-wise (dsp)} (default), \code{edge-wise (esp)} or \code{non-edgewise (nsp)} shared partners (Hunter and Handcock, 2006)
#'
#' @return This function return the distribution of shared partners.
#'
#' @references
#'
#' Hunter, D. R. and M. S. Handcock (2006), Inference in curved exponential family models for networks, Journal of Computational and Graphical Statistics, 15: 565– 583.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 0, 0, 0, 0,
#'   1, 0, 1, 1, 0, 1,
#'   0, 1, 0, 1, 0, 0,
#'   0, 1, 1, 0, 1, 1,
#'   0, 0, 0, 1, 0, 1,
#'   0, 1, 0, 1, 1, 0
#' ), byrow = TRUE, ncol = 6)
#' shared_partners(A, type = "dsp")
#' shared_partners(A, type = "esp")
#' shared_partners(A, type = "nsp")
#' @export

shared_partners <- function(A, loops = FALSE, directed = TRUE,
                            type = c("dsp", "esp", "nsp")) {
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(abs(A > 1), na.rm = TRUE)) warning("The matrix should be binary")

  twoA <- A %*% A

  type <- switch(edge_dyad(type),
    "dsp" = 1,
    "esp" = 2,
    "nsp" = 3
  )

  if (type == 1) {
    dsp <- twoA
    if (directed) {
      if (!loops) {
        diag(dsp) <- NA
      }
      return(table(dsp))
    } else {
      return(table(dsp[upper.tri(dsp, diag = loops)]))
    }
  }

  if (type == 2) {
    if (!loops) {
      diag(A) <- 0
    }
    m2 <- ifelse(A & twoA != 0, twoA, NA) # only considering those who are connected!
    m3 <- ifelse(A & twoA == 0, 0, NA)
    vector <- c(m2, m3)
    if (directed) {
      return(table(vector[!is.na(vector)]))
    } else {
      return(table(vector[!is.na(vector)]) / 2)
    }
  }

  if (type == 3) {
    nsp <- ifelse(twoA >= 0 & A == 1, NA, twoA) # dyads that do not have an edge
    if (directed) {
      if (!loops) {
        diag(nsp) <- NA
      }
      return(table(nsp))
    } else {
      return(table(nsp[upper.tri(nsp, diag = loops)]))
    }
  }
}

edge_dyad <- function(arg, choices, several.ok = FALSE) {
  if (missing(choices)) {
    formal.args <- formals(sys.function(sys.parent()))
    choices <- eval(formal.args[[deparse(substitute(arg))]])
  }

  arg <- tolower(arg)
  choices <- tolower(choices)

  match.arg(arg = arg, choices = choices, several.ok = several.ok)
}

#' Clique percolation
#'
#' Clique Percolation Method (CPM) is an algorithm for finding overlapping communities within networks, introduced by Palla et al. (2005). This function firstly identify cliques of size k, then creates a incidence matrix as an affiliation network.
#'
#' @param A   A matrix
#'
#' @return A matrix that assign each node to a clique
#'
#' @references
#'
#' Palla, G., Derényi, I., Farkas, I., & Vicsek, T. (2005). Uncovering the overlapping community structure of complex networks in nature and society. Nature, 435(7043), 814-818.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' A <- matrix(
#'   c(
#'     0, 1, 1, 1, 0, 0, 0, 0, 0,
#'     1, 0, 1, 0, 0, 0, 0, 0, 0,
#'     1, 1, 0, 1, 0, 0, 0, 0, 0,
#'     1, 0, 1, 0, 1, 1, 0, 0, 0,
#'     0, 0, 0, 1, 0, 1, 1, 1, 0,
#'     0, 0, 0, 1, 1, 0, 1, 1, 0,
#'     0, 0, 0, 0, 1, 1, 0, 1, 1,
#'     0, 0, 0, 0, 1, 1, 1, 0, 0,
#'     0, 0, 0, 0, 0, 0, 1, 0, 0
#'   ),
#'   byrow = TRUE, ncol = 9
#' )
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- letters[1:ncol(A)]
#' percolation_clique(A)
#' @export

percolation_clique <- function(A) {
  C <- clique_table(A)
  clique_matrix <- edgelist_to_matrix(C$table, digraph = FALSE, bipartite = TRUE)
  proj <- matrix_projection(clique_matrix)$matrix1
  proj[proj < 2] <- 0 # 2 = k-1 (in this case a clique is k = 3)
  proj[proj >= 2] <- 1
  block <- components_id(proj)$components
  colnames(clique_matrix) <- block
  no_clique <- NULL # all nodes might belong to a clique
  if (!length(rownames(A)) == length(rownames(clique_matrix))) {
    temp <- rownames(A)[!rownames(A) %in% rownames(clique_matrix)]
    no_clique <- list()
    for (i in 1:length(temp)) {
      no_clique[[i]] <- rep(0, ncol(clique_matrix))
      names(no_clique)[i] <- temp[[i]]
    }
    no_clique <- do.call(rbind, no_clique)
  }
  clique_matrix <- rbind(clique_matrix, no_clique)
  return(clique_matrix)
}

#' Q-analysis
#'
#' Q-analysis of a simplicial complex (Atkin, 1974): the q-connected components at every dimension,
#' the structure vectors, the obstruction vector and the eccentricity of each simplex.
#'
#' @details
#' A simplex is a set of vertices, and its dimension \eqn{q} is the number of its vertices minus one.
#' Two simplices are q-near when they share at least \eqn{q + 1} vertices, that is, a face of dimension
#' \eqn{q}, and q-connected when a chain of q-near simplices joins them. For every \eqn{q} from the largest
#' dimension down to 0, the simplices of dimension \eqn{q} or more are grouped in q-connected components
#' (Atkin, 1974; Freeman, 1980). The table of the results has, for each \eqn{q}:
#'
#' \code{Q}, the first structure vector: the number of q-connected components.
#'
#' \code{n}, the second structure vector: the number of simplices of dimension \eqn{q} or more.
#'
#' \code{Qbar}, the third structure vector: \eqn{1 - Q/n}, which is zero when no simplex is q-connected to
#' another and approaches one when they all form a single component (Raj et al., 2024).
#'
#' \code{obstruction}, the obstruction vector: \eqn{Q - 1}, the number of gaps that separate the components
#' (Atkin, 1974).
#'
#' The eccentricity measures how much a simplex stands apart from the others, and there are two definitions.
#' With \code{eccentricity = "atkin"} (default) it is \eqn{(\hat{q} - \check{q}) / (\check{q} + 1)}, where
#' \eqn{\hat{q}} is the dimension of the simplex and \eqn{\check{q}} the dimension of the largest face it shares
#' with another simplex (Atkin, 1974). It is zero for a simplex that is a face of another, and infinite for a
#' simplex that shares no vertex with the others. With \code{eccentricity = "johnson"} it is the family
#' eccentricity of Johnson, the smallest proportion of the vertices of the simplex that are not in another
#' simplex, \eqn{\min_{\sigma'} |\sigma \setminus \sigma'| / |\sigma|}, as implemented by Smirnov et al. (2025).
#' It runs from zero to one, which makes simplices of different dimension comparable, and it is \code{NA}
#' when the complex has a single simplex.
#'
#' With \code{simplicial_complex = TRUE}, the rows of \code{A} are the simplices and the columns their
#' vertices, as in the example of Freeman (1980), where the researchers are simplices of the events that
#' linked them. The conjugate complex, in which the columns are the simplices, is the analysis of \code{t(A)}.
#'
#' With \code{simplicial_complex = FALSE}, \code{A} is a network and the complex is built from it (Raj et al., 2024):
#' with \code{complex = "clique"}, the simplices are the maximal cliques of the underlying undirected
#' network, including the isolated nodes as simplices of dimension 0; with \code{complex = "neighbourhood"},
#' each node is the simplex of its neighbours, the rows of \code{A} (the out-neighbours of a directed
#' network). With \code{closed = TRUE} the node is also a vertex of its own simplex (closed neighbourhood),
#' so that two adjacent nodes share at least the two of them. The rows without vertices are not simplices.
#' The complex is the one returned by \code{simplicial_complexes()}.
#'
#' @param A   An incidence matrix of simplices (rows) and vertices (columns), or a square matrix of a network
#' @param simplicial_complex   Whether \code{A} is an incidence matrix of simplices (TRUE) or a network (FALSE)
#' @param complex   The complex built from a network: the maximal cliques (\code{clique}, default) or the neighbourhoods (\code{neighbourhood})
#' @param closed   Whether the neighbourhoods include the node itself, for \code{complex = "neighbourhood"}
#' @param eccentricity   The definition of the eccentricity: \code{atkin} (default) or \code{johnson}
#' @param dimensions  Kept for compatibility with version 1.0-3. The table of the dimensions is always returned
#'
#' @return This function returns a list with the incidence matrix of the \code{simplices} analysed, the
#' \code{q_table} with the structure and obstruction vectors, the \code{components} at each \eqn{q} (named
#' \code{q3}, \code{q2}, ...), and the \code{eccentricity} of each simplex.
#'
#' @references
#'
#' Atkin, R. H. (1974). Mathematical structure in human affairs. New York: Crane, Rusak.
#'
#' Freeman, L. C. (1980). Q-analysis and the structure of friendship networks. International Journal of Man-Machine Studies, 12(4), 367–378. \doi{10.1016/S0020-7373(80)80021-6}
#'
#' Raj, U., Banerjee, A., Ray, S. and Bhattacharya, S. (2024). Structure of higher-order interactions in social-ecological networks through Q-analysis of their neighbourhood and clique complex. PLOS ONE, 19(8), e0306409. \doi{10.1371/journal.pone.0306409}
#'
#' Smirnov, N., Kurkin, S. and Hramov, A. E. (2025). A Q-analysis package for higher-order interactions analysis in Python and its application in network physiology. Frontiers in Network Physiology, 5. \doi{10.3389/fnetp.2025.1691159}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' # Freeman (1980): 29 researchers (simplices) and the 19 events that linked them (vertices)
#' A <- matrix(c(
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0,
#'   0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
#'   0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
#'   0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
#'   0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0
#' ), byrow = TRUE, ncol = 19)
#' colnames(A) <- letters[1:ncol(A)]
#' rownames(A) <- 1:nrow(A)
#'
#' Q <- q_analysis(A, simplicial_complex = TRUE)
#' Q$q_table
#' Q$components$q3
#'
#' # A network: a clique of four nodes and a pendant node
#' B <- matrix(c(
#'   0, 1, 1, 1, 0,
#'   1, 0, 1, 1, 0,
#'   1, 1, 0, 1, 0,
#'   1, 1, 1, 0, 1,
#'   0, 0, 0, 1, 0
#' ), byrow = TRUE, ncol = 5)
#' rownames(B) <- letters[1:nrow(B)]
#' colnames(B) <- rownames(B)
#'
#' q_analysis(B, complex = "clique")$q_table
#' q_analysis(B, complex = "neighbourhood")$q_table
#' @export

q_analysis <- function(A, simplicial_complex = FALSE, complex = c("clique", "neighbourhood"),
                       closed = FALSE, eccentricity = c("atkin", "johnson"), dimensions = FALSE) {
  A <- as.matrix(A)
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  complex <- match.arg(complex)
  eccentricity <- match.arg(eccentricity)

  if (is.null(rownames(A))) stop("No label assigned to the rows of the matrix")
  if (is.null(colnames(A))) stop("No label assigned to the columns of the matrix")
  A[A > 0] <- 1

  if (simplicial_complex) {
    X <- A
  } else {
    if (ncol(A) != nrow(A)) stop("Matrix should be square. Use simplicial_complex = TRUE for an incidence matrix")
    # The simplices are the columns of simplicial_complexes()
    X <- t(simplicial_complexes(A, zero_simplex = TRUE, complex = complex, closed = closed))
  }
  # The empty set is not a simplex
  X <- X[rowSums(X) > 0, , drop = FALSE]
  if (nrow(X) == 0) stop("There are no simplices")

  # Vertices shared by each pair of simplices: two simplices sharing q + 1
  # vertices share a face of dimension q
  shared <- X %*% t(X)
  dimension <- rowSums(X) - 1
  levels <- max(dimension):0

  q_table <- data.frame(q = levels, Q = NA, n = NA, Qbar = NA, obstruction = NA)
  components <- list()
  for (k in seq_along(levels)) {
    q <- levels[k]
    ids <- which(dimension >= q)
    near <- 1 * (shared[ids, ids, drop = FALSE] >= q + 1)
    # Each component is the set of simplices reached through q-near simplices
    # from the first simplex not yet assigned
    membership <- rep(0, length(ids))
    for (s in seq_along(ids)) {
      if (membership[s] > 0) next
      membership[s] <- max(membership) + 1
      frontier <- s
      while (length(frontier) > 0) {
        frontier <- which(colSums(near[frontier, , drop = FALSE]) > 0 & membership == 0)
        membership[frontier] <- membership[s]
      }
    }
    components[[paste0("q", q)]] <- data.frame(
      component = membership,
      simplex = rownames(X)[ids],
      row.names = NULL
    )[order(membership), ]
    q_table$Q[k] <- max(membership)
    q_table$n[k] <- length(ids)
  }
  q_table$Qbar <- 1 - q_table$Q / q_table$n
  q_table$obstruction <- q_table$Q - 1

  # Largest face shared with another simplex; -1 when no vertex is shared
  others <- shared
  diag(others) <- 0
  bottom <- apply(others, 1, max) - 1
  if (eccentricity == "atkin") {
    # Infinite when no vertex is shared
    value <- (dimension - bottom) / (bottom + 1)
  } else {
    # Proportion of the vertices that are not in the most similar simplex
    value <- 1 - (bottom + 1) / (dimension + 1)
    if (nrow(X) == 1) value <- NA
  }
  eccentricities <- data.frame(
    simplex = rownames(X),
    dimension = dimension,
    bottom = bottom,
    eccentricity = value,
    row.names = NULL
  )

  return(list(simplices = X, q_table = q_table, components = components, eccentricity = eccentricities))
}


#' Maximal cliques
#'
#' Maximal complete subgraphs of an undirected network, found with the algorithm of
#' Bron and Kerbosch (1973).
#'
#' A clique is a set of nodes that are all adjacent to each other, and it is maximal when no
#' other node can be added to it. Unlike \code{clique_table()}, which returns the triangles of
#' the network, this function returns cliques of any size.
#'
#' @param A   A symmetric matrix object
#' @param min   Minimum size of the cliques returned
#' @param max   Maximum size of the cliques returned. If NULL, there is no limit
#'
#' @return This function returns a list with the names of the nodes of each maximal clique.
#'
#' @references
#'
#' Bron, C. and Kerbosch, J. (1973). Algorithm 457: Finding all cliques of an undirected graph. Communications of the ACM, 16(9), 575–577. \doi{10.1145/362342.362367}
#'
#' Luce, R. D. and Perry, A. D. (1949). A method of matrix analysis of group structure. Psychometrika, 14(2), 95–116. \doi{10.1007/BF02289146}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 0, 0, 0,
#'   1, 0, 1, 1, 0, 0,
#'   1, 1, 0, 1, 0, 0,
#'   0, 1, 1, 0, 1, 1,
#'   0, 0, 0, 1, 0, 1,
#'   0, 0, 0, 1, 1, 0
#' ), byrow = TRUE, ncol = 6)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- rownames(A)
#'
#' clique_max(A)
#' @export

clique_max <- function(A, min = 2, max = NULL) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  if (!all(A[lower.tri(A)] == t(A)[lower.tri(A)])) warning("The network is directed. The underlying graph is used")
  A[A > 0] <- 1
  A <- pmax(A, t(A)) # Symmetrize
  diag(A) <- 0
  if (is.null(rownames(A))) {
    rownames(A) <- as.character(seq_len(nrow(A)))
    colnames(A) <- rownames(A)
  }

  cliques <- list()
  bron_kerbosch <- function(R, P, X) {
    if (length(P) == 0 & length(X) == 0) {
      cliques[[length(cliques) + 1]] <<- R
      return(invisible(NULL))
    }
    # The pivot is the node of P and X with more neighbours in P, so that
    # only the nodes that are not adjacent to it are explored
    pivot <- c(P, X)[which.max(colSums(A[P, c(P, X), drop = FALSE]))]
    candidates <- P[A[pivot, P] == 0]
    for (v in candidates) {
      neighbours <- which(A[v, ] > 0)
      bron_kerbosch(c(R, v), intersect(P, neighbours), intersect(X, neighbours))
      P <- setdiff(P, v)
      X <- c(X, v)
    }
  }
  bron_kerbosch(integer(0), seq_len(nrow(A)), integer(0))

  size <- lengths(cliques)
  keep <- size >= min
  if (!is.null(max)) {
    keep <- keep & size <= max
  }
  cliques <- cliques[keep]
  cliques <- cliques[order(lengths(cliques), decreasing = TRUE)]
  lapply(cliques, function(x) rownames(A)[sort(x)])
}
