#' Forbidden triad table
#'
#' This function explores dyads and triads (Simmel, 1950), building from the
#' 'forbidden triad' (Granovetter, 1973).
#' First, the minimum structure is an isolated node, then dyads.
#' Afterwards, different combinations of 'forbidden triads' are explored.
#'
#' @param A   A symmetric matrix object.
#' @param adjacency_list   Whether to return the adjacency list of triads 201 per node.
#' @param min   Numeric constant, lower limit on the size of the triads 201 to find. NULL means no limit, ie. it is the same as 0.
#' @param max   Numeric constant, upper limit on the size of the triads 201 to find. NULL means no limit.
#'
#' @return This function return the list of triads that each node belong.
#'
#' If \code{adjacency_list = TRUE} it also  return the adjacency list of
#' the 'forbidden triads' per node.
#'
#' @references
#'
#' Granovetter, M.S. (1973). The Strength of Weak Ties. American Journal of Sociology. 78 (6): 1360–80. https://doi.org/10.1086/225469.
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
#' dyad_triad_table(A, adjacency_list = TRUE, min = 3)
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
  A[lower.tri(A)] <- t(A)[lower.tri(A)] # Symmetrize
  diag(A) <- 0

  adj_list <- list()
  size <- list()
  temp <- list()
  for (i in 1:ncol(A)) {
    adj_list[[i]] <- names(A[i, ][A[i, ] >= 1])

    if (length(adj_list[[i]]) > 1) {
      adj_list[[i]] <- t(combn(adj_list[[i]], 2))
      adj_list[[i]] <- cbind(colnames(A)[i], adj_list[[i]])
      adj_list[[i]] <- t(apply(adj_list[[i]], 1, sort))
      temp[[i]] <- apply(adj_list[[i]], 1, paste, collapse = "")
      size[[i]] <- lengths(temp[[i]])
    } else {
      adj_list[[i]] <- sort(c(colnames(A)[i], adj_list[[i]])) # ok
      temp[[i]] <- paste(adj_list[[i]], collapse = "")
      size[[i]] <- lengths(temp[[i]])
    }
    names(adj_list)[[i]] <- rownames(A)[i]
    names(temp)[[i]] <- rownames(A)[i]
  }
  size <- sapply(size, sum)
  Triad201 <- as.numeric(factor(unlist(temp)))
  node <- rep(rownames(A), times = size)
  nodes <- cbind(node, Triad201)

  if (!is.null(min) & !is.null(max)) {
    if (min == max) stop("Min and max should be different")
  }

  if (!is.null(min)) {
    nodes <- subset(
      nodes,
      Triad201 %in% as.vector(which((table(nodes[, 2]) >= min) == TRUE))
    )
    if (all(!table(nodes[, 2]) >= min)) stop(paste("There is no triad 201 mayor or equal to", min))
  }
  if (!is.null(max)) {
    t <- table(nodes[, 2]) <= max
    t <- as.vector(which((t) == TRUE))
    if (!any(nodes[, 2] %in% t)) {
      nodes <- subset(
        nodes,
        Triad201 %in% t
      )
    }
    if (all(!table(nodes[, 2]) <= max)) stop(paste("There is no triad 201 minor or equal to", max))
  }

  a <- as.data.frame(cbind(
    order = rep(1:length(unique(nodes[, 2]))),
    Triad201 = unique(as.numeric(nodes[, 2]))
  ))
  b <- as.data.frame(nodes)
  nodes <- merge(b, a, by = "Triad201")
  nodes <- nodes[order(nodes$order, nodes$node), ]
  nodes <- nodes[, -1]
  colnames(nodes) <- c("node", "Triad201")

  if (adjacency_list) {
    if (!is.null(min)) {
      temp <- temp[nodes[, 1]]
    }
    if (!is.null(max)) {
      temp <- temp[nodes[, 1]]
    }

    newlist <- list(nodes = nodes, adjacency_list = temp)
    return(newlist)
  } else {
    return(nodes)
  }
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
  A[lower.tri(A)] <- t(A)[lower.tri(A)]
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

      cliques[[i]] <- apply(adj_list[[i]], 1, paste, collapse = "")
    }
  }
  t <- table(unlist(cliques))[which(table(unlist(cliques)) >= 3)]

  if (all(table(unlist(cliques)) < 2)) stop(message("No cliques"))

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
      collapse = ""
    ) %in% new_list[[i]]), ]
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
#' Q-structure of a simplicial complex.
#'
#' @param A   An incidence matrix
#' @param simplicial_complex   Whether the incidence matrix is a simplices or simplicial complexes representation
#' @param dimensions  Return the successively chains from high to low dimensions ($q$) and the number of components ($Q_{p}$)
#'
#' @return This function return a q-analysis of a simplicial complex matrix
#'
#' @references
#'
#' Atkin, R. H. (1974). Mathematical structure in human affairs. New York: Crane, Rusak.
#'
#' Freeman, L. C. (1980). Q-analysis and the structure of friendship networks. International Journal of Man-Machine Studies, 12(4), 367–378. https://doi.org/10.1016/S0020-7373(80)80021-6
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
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
#' q_analysis(A, simplicial_complex = TRUE)
#' @export

q_analysis <- function(A, simplicial_complex = FALSE, dimensions = FALSE) {
  A <- as.matrix(A)
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }

  if (is.null(rownames(A))) stop("No label assigned to the rows of the matrix")
  if (is.null(colnames(A))) stop("No label assigned to the columns of the matrix")

  if (!simplicial_complex) {
    if (ncol(A) != nrow(A)) stop("Matrix should be square")
    A <- simplicial_complexes(A, zero_simplex = FALSE)
  } else {
    if (ncol(A) == nrow(A)) stop("Matrix should be rectangular")
  }

  # Q ANALYSIS
  q_analysis <- list()
  vector <- sort(unique(rowSums(A)), decreasing = TRUE)
  for (i in 1:length(vector)) {
    q_analysis[[i]] <- unique(which(rowSums(A) == vector[i]))
    names(q_analysis)[i] <- vector[i]
  }

  Q_table <- list()
  comp <- list()
  ac <- NULL
  for (i in 1:length(q_analysis[names(q_analysis) != 0])) {
    temp <- minmax_overlap(A, row = TRUE, min = TRUE)
    diag(temp) <- 0
    temp <- ifelse(temp >= as.numeric(names(q_analysis)[i]), 1, 0)
    ac <- unique(c(ac, q_analysis[[i]]))
    temp <- temp[ac, ac]
    comp_temp <- components_id(temp)
    comp[[i]] <- comp_temp$components
    comp[[i]] <- as.data.frame(cbind(component = comp[[i]], node = rownames(temp)))
    comp[[i]] <- comp[[i]][order(as.numeric(comp[[i]]$component)), ]
    # rownames(comp[[i]]) <- 1:nrow(comp[[i]])
    names(comp)[i] <- length(comp_temp$size)
    Q_table[[i]] <- cbind(q = vector[i] - 1, Qp = length(comp_temp$size))
  }

  if (dimensions) {
    return(list(components = comp, q_table = do.call(rbind, Q_table)))
  } else {
    return(components = comp)
  }
}
