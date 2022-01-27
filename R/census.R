# TODO: Add NA for all censuses

#' Dyadic census
#'
#' @param G   A symmetric matrix object.
#' @param directed   Whether the matrix is directed or not
#'
#' @return This function return the counts of the dyadic census.
#'
#' @references
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' data(krackhardt_friends)
#' dyadic_census(krackhardt_friends)
#'
#' data(FIFAin)
#' dyadic_census(FIFAin[[1]], directed = FALSE)
#' @export

dyadic_census <- function(G, directed = TRUE) {
  G <- as.matrix(G)
  A <- G
  if (any(abs(G > 1), na.rm = TRUE)) stop("The matrix should be binary")
  g <- nrow(G)
  na <- sum(is.na(G)) - sum(diag(is.na(G)))

  if (any(is.na(G) == TRUE)) {
    G <- ifelse(is.na(G), 0, G)
  }

  m <- (1 / 2) * sum(diag(G %*% G))
  a <- sum(diag(G %*% t(G))) - sum(diag(G %*% G))
  n <- ((g * (g - 1)) / 2) - (sum(diag(G %*% t(G))) - sum(diag(G %*% G))) - ((1 / 2) * sum(diag(G %*% G)))

  if (directed) {
    if (any(is.na(A) == TRUE)) {
      c(
        "Mutual" = m,
        "Asymmetrics" = a,
        "Nulls" = n - na,
        "NA" = na
      )
    } else {
      c(
        "Mutual" = m,
        "Asymmetrics" = a,
        "Nulls" = n
      )
    }
  } else {
    if (any(is.na(A) == TRUE)) {
      c(
        "Mutual" = m,
        "Nulls" = n - na,
        "NA" = na
      )
    } else {
      c(
        "Mutual" = m,
        "Nulls" = n
      )
    }
  }
}

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
#' A <- matrix(c(0,1,1,0,0,0,0,1,0,
#'               1,0,1,0,0,0,0,0,0,
#'               1,1,0,0,0,0,0,0,0,
#'               0,0,0,0,1,1,0,0,0,
#'               0,0,0,1,0,0,0,0,0,
#'               0,0,0,1,0,0,1,1,0,
#'               0,0,0,0,0,1,0,1,0,
#'               1,0,0,0,0,1,1,0,0,
#'               0,0,0,0,0,0,0,0,0), byrow = TRUE, ncol = 9)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- rownames(A)
#' clique_table(A, list_cliques = TRUE, number = TRUE)
#'
#' @export
#'
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

  if (all(table(unlist(cliques)) < 2)) stop(print("No cliques"))

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

#' Multiplex triad census
#'
#' This function counts the different subgraphs of three nodes in a multiplex directed and undirected network.
#'
#' @param A   A directed matrix object.
#' @param B   An undirected matrix object.
#'
#' @return This function gives the counts of the mixed multiplex triad census for a directed and an undirected network.
#'
#' @references
#'
#' Espinosa-Rada, A. (2021). A Network Approach for the Sociological Study of Science: Modelling Dynamic Multilevel Networks. [PhD]. The University of Manchester.
#'
#' @author Alejandro Espinosa-Rada
#'
#'
#' @examples
#'
#' # SOAR
#' A <- matrix(c(
#'   0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1,
#'   0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0,
#'   0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1,
#'   0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
#'   0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
#' ),
#' byrow = TRUE, ncol = 12
#' )
#'
#' B <- matrix(c(
#'   0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
#'   1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0,
#'   0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
#'   0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
#' ),
#' byrow = TRUE, ncol = 12
#' )
#'
#' multiplex_census(A, B)
#' @export
#'

multiplex_census <- function(A, B) {
  # TODO: Experimental version, please use with caution

  if (!all(A <= 1)) warning(paste("Measure only implemented for binary networks,", "the `first` network", "would be binarized for the triad census"))
  A <- as.matrix(A)
  if (!dim(A)[1] == dim(A)[2]) stop("Matrix should be square")
  A <- ifelse(A > 0, 1, 0)

  if (!all(B <= 1)) warning(paste("Measure only implemented for binary networks,", "the `second` network", "would be binarized for the triad census")) # deparse(quote(B))
  B <- as.matrix(B)
  if (!dim(B)[1] == dim(B)[2]) stop("Matrix should be square")
  B <- ifelse(B > 0, 1, 0)

  if (!all(B[lower.tri(B)] == t(B)[lower.tri(B)])) warning(paste("Measure only implemented for undirected networks", "in the `second` network,", "the measure would use the underlying graph"))
  B[lower.tri(B)] <- t(B)[lower.tri(B)]

  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  if (any(is.na(B) == TRUE)) {
    B <- ifelse(is.na(B), 0, B)
  }

  E <- ifelse((A + t(A)) > 0, 1, 0)
  Eb <- ifelse(E == 0, 1, 0)
  diag(Eb) <- 0
  M <- ifelse((A + t(A)) > 1, 1, 0)
  C <- A - M
  t201 <- sum(M %*% M * Eb)
  t021D <- sum(t(C) %*% C * Eb)
  t021U <- sum(C %*% t(C) * Eb)

  E2 <- ifelse((B + t(B)) > 0, 1, 0)
  Eb2 <- ifelse(E2 == 0, 1, 0)
  diag(Eb2) <- 0
  M2 <- ifelse((B + t(B)) > 1, 1, 0)
  C2 <- B - M
  t201b <- sum(M %*% M * Eb)
  t021Db <- sum(t(C2) %*% C2 * Eb2)
  t021Ub <- sum(C2 %*% t(C2) * Eb2)

  D <- (A + B)
  D <- ifelse(D >= 1, 1, 0)
  E3 <- ifelse((D + t(D)) > 0, 1, 0)
  Eb3 <- ifelse(E3 == 0, 1, 0)
  diag(Eb3) <- 0
  M3 <- ifelse((D + t(D)) > 1, 1, 0)
  C3 <- D - M3
  t201d <- sum(M3 %*% M3 * Eb3)
  t021Dd <- sum(t(C3) %*% C3 * Eb3)
  t021Ud <- sum(C3 %*% t(C3) * Eb3)

  res <- c(
    "003_003" = sum(diag(Eb3 %*% Eb3 %*% Eb3)) / 6,
    "003_102" = sum(diag(Eb %*% Eb %*% Eb)) / 6 + sum((Eb2 %*% Eb2 * M2)) / 2,
    "003_201" = sum(diag(Eb %*% Eb %*% Eb)) / 6 + sum(M2 %*% M2 * Eb2) / 2,
    "003_300" = sum(diag(Eb %*% Eb %*% Eb)) / 6 + sum(diag(M2 %*% M2 %*% M2)) / 6,
    "012_003" = sum((Eb %*% Eb) * (C + t(C))) / 2 + sum(diag(Eb2 %*% Eb2 %*% Eb2)) / 6,
    "012_102a" = sum((Eb %*% Eb) * (C + t(C))) / 2 + sum((Eb3 %*% Eb3 * M3)) / 2,
    "012_102b" = sum((Eb %*% Eb) * (C + t(C))) / 2 + (sum(t(D) %*% D * Eb3) - t201d - t021Dd) / 2,
    "012_102c" = sum((Eb %*% Eb) * (C + t(C))) / 2 + (sum(D %*% t(D) * Eb3) - t201d - t021Ud) / 2,
    "012_201b" = sum((Eb %*% Eb) * (C + t(C))) / 2 + sum(M3 %*% M3 * (C3 + t(C3))) / 2,
    "012_201ac" = sum((Eb %*% Eb) * (C + t(C))) / 2 + sum(M3 %*% M3 * Eb3) / 2,
    "012_300" = sum((Eb %*% Eb) * (C + t(C))) / 2 + sum(diag(M3 %*% M3 %*% M3)) / 6,
    "021u_003" = sum(C %*% t(C) * Eb) / 2 + sum(diag(Eb2 %*% Eb2 %*% Eb2)) / 6,
    "021u_102ac" = sum(C %*% t(C) * Eb) / 2 + (sum(D %*% t(D) * Eb3) - t201d - t021Ud) / 2,
    "021u_102b" = sum(C %*% t(C) * Eb) / 2 + sum(C3 %*% t(C3) * M3) / 2,
    "021u_201ab" = sum(C %*% t(C) * Eb) / 2 + sum(M3 %*% M3 * (C3 + t(C3))) / 2,
    "021u_201c" = sum(C %*% t(C) * Eb) / 2 + sum(M3 %*% M3 * Eb3) / 2,
    "021u_300" = sum(C %*% t(C) * Eb) / 2 + sum(diag(M3 %*% M3 %*% M3)) / 6,
    "021d_003" = sum(t(C) %*% C * Eb) / 2 + sum(diag(Eb2 %*% Eb2 %*% Eb2)) / 6,
    "021d_102ac" = sum(t(C) %*% C * Eb) / 2 + (sum(t(D) %*% D * Eb3) - t201d - t021Dd) / 2,
    "021d_120b" = sum(t(C) %*% C * Eb) / 2 + sum(t(C3) %*% C3 * M3) / 2,
    "021d_201ab" = sum(t(C) %*% C * Eb) / 2 + sum(M3 %*% M3 * (C3 + t(C3))) / 2,
    "021d_201c" = sum(t(C) %*% C * Eb) / 2 + sum(M3 %*% M3 * Eb3) / 2,
    "021d_300" = sum(t(C) %*% C * Eb) / 2 + sum(diag(M2 %*% M2 %*% M2)) / 6,
    "102_003_102a" = sum((Eb %*% Eb * M)) / 2 + sum((Eb3 %*% Eb3 * M3)) / 2,
    "102_102bc_201ac" = sum((Eb %*% Eb * M)) / 2 + sum(M3 %*% M3 * Eb3) / 2,
    "102_300" = sum((Eb %*% Eb * M)) / 2 + sum(diag(M2 %*% M2 %*% M2)) / 6,
    "021c_003" = sum(C %*% C * Eb) + sum(diag(Eb2 %*% Eb2 %*% Eb2)) / 6,
    "021c_102a" = sum(C %*% C * Eb) + (sum(t(D) %*% D * Eb3) - t201d - t021Dd) / 2,
    "021c_103b" = sum(C %*% C * Eb) + sum(C3 %*% C3 * M3),
    "021c_102c" = sum(C %*% C * Eb) + (sum(D %*% t(D) * Eb3) - t201d - t021Ud) / 2,
    "021c_210ab" = sum(C %*% C * Eb) + sum(M3 %*% M3 * (C3 + t(C3))) / 2,
    "021c_201c" = sum(C %*% C * Eb) + sum(M3 %*% M3 * Eb3) / 2,
    "021c_300" = sum(C %*% C * Eb) + sum(diag(M3 %*% M3 %*% M3)) / 6,
    "030t_003" = sum((C %*% C) * C) + sum(diag(Eb2 %*% Eb2 %*% Eb2)) / 6,
    "030t_102ab" = sum((C %*% C) * C) + sum(C3 %*% C3 * M3),
    "030t_102b" = sum((C %*% C) * C) + sum(C3 %*% t(C3) * M3) / 2,
    "030t_102c" = sum((C %*% C) * C) + sum(t(C3) %*% C3 * M3) / 2,
    "030t_210ab" = sum((C %*% C) * C) + sum(M3 %*% M3 * (C3 + t(C3))) / 2,
    "030t_201c_300" = sum((C %*% C) * C) + sum(diag(M3 %*% M3 %*% M3)) / 6,
    "030c_003" = sum(diag(C %*% C %*% C)) / 3 + sum(diag(Eb2 %*% Eb2 %*% Eb2)) / 6,
    "030c_102abc" = sum(diag(C %*% C %*% C)) / 3 + sum(C3 %*% C3 * M3),
    "030c_201abc" = sum(diag(C %*% C %*% C)) / 3 + sum(M3 %*% M3 * (C3 + t(C3))) / 2,
    "030c_300" = sum(diag(C %*% C %*% C)) / 3 + sum(diag(M3 %*% M3 %*% M3)) / 6,
    "111d_003" = (sum(A %*% t(A) * Eb) - t201 - t021U) / 2 + sum(diag(Eb2 %*% Eb2 %*% Eb2)) / 6,
    "111d_102a_201a" = (sum(A %*% t(A) * Eb) - t201 - t021U) / 2 + sum(M3 %*% M3 * (C3 + t(C3))) / 2,
    "111d_102b" = (sum(D %*% t(D) * Eb3) - t201d - t021Ud) / 2,
    "111d_102c_201b" = (sum(A %*% t(A) * Eb) - t201 - t021U) / 2 + sum(M3 %*% M3 * Eb3) / 2,
    "111d_201c_300" = (sum(A %*% t(A) * Eb) - t201 - t021U) / 2 + sum(diag(M3 %*% M3 %*% M3)) / 6,
    "111u_003" = (sum(t(A) %*% A * Eb) - t201 - t021D) / 2 + sum(diag(Eb2 %*% Eb2 %*% Eb2)) / 6,
    "111u_102a_201a" = (sum(t(A) %*% A * Eb) - t201 - t021D) / 2 + sum(M3 %*% M3 * (C3 + t(C3))) / 2,
    "111u_102bc_201b" = (sum(t(A) %*% A * Eb) - t201 - t021D) / 2 + sum(M3 %*% M3 * Eb3) / 2,
    "111u_201c_300" = (sum(t(A) %*% A * Eb) - t201 - t021D) / 2 + sum(diag(M3 %*% M3 %*% M3)) / 6,
    "120u_003_102b" = sum(C %*% t(C) * M) / 2 + sum(C3 %*% t(C3) * M3) / 2,
    "120u_102ab_201ab" = sum(C %*% t(C) * M) / 2 + sum(M3 %*% M3 * (C3 + t(C3))) / 2,
    "120u_201c_300" = sum(C %*% t(C) * M) / 2 + sum(diag(M3 %*% M3 %*% M3)) / 6,
    "120d_003_120b" = sum(t(C) %*% C * M) / 2 + sum(t(C3) %*% C3 * M3) / 2,
    "120d_102ab_201ab" = sum(t(C) %*% C * M) / 2 + sum(M3 %*% M3 * (C3 + t(C3))) / 2,
    "120d_201c_300" = sum(t(C) %*% C * M) / 2 + sum(diag(M3 %*% M3 %*% M3)) / 6,
    "201_003" = sum(M %*% M * Eb) / 2 + sum(diag(Eb2 %*% Eb2 %*% Eb2)) / 6,
    "201_102ac_201ab" = sum(M %*% M * Eb) / 2 + sum(M3 %*% M3 * Eb3) / 2,
    "201_102c_201bc_300" = sum(M %*% M * Eb) / 2 + sum(diag(M3 %*% M3 %*% M3)) / 6,
    "120c_003" = sum(C %*% C * M) + sum(diag(Eb2 %*% Eb2 %*% Eb2)) / 6,
    "120c_120c" = sum(C %*% C * M) + sum(C3 %*% C3 * M3),
    "120c_210" = sum(C %*% C * M) + sum(M3 %*% M3 * (C3 + t(C3))) / 2,
    "120c_300" = sum(C %*% C * M) + sum(diag(M3 %*% M3 %*% M3)) / 6,
    "210_003_210" = sum(M %*% M * (C + t(C))) / 2 + sum(M3 %*% M3 * (C3 + t(C3))) / 2,
    "210_300" = sum(M %*% M * (C + t(C))) / 2 + sum(diag(M3 %*% M3 %*% M3)) / 6,
    "300_003_300" = sum(diag(M %*% M %*% M)) / 6 + sum(diag(M3 %*% M3 %*% M3)) / 6
  )
  return(floor(res / 2))
}

#' Multilevel triad and quadrilateral census
#'
#' @param A1   An adjacent matrix object.
#' @param B1   An incident matrix object.
#' @param B2   An incident matrix object.
#' @param quad  Whether the matrix is a quadrilateral census or not.
#'
#' @return This function return the counts of a multilevel census.
#'
#' If \code{quad = TRUE}, then the function return the multilevel quadrilateral census.
#'
#' @references
#'
#' Espinosa-Rada, A. (2021). A Network Approach for the Sociological Study of Science: Modelling Dynamic Multilevel Networks. [PhD]. The University of Manchester.
#'
#' Hollway, J., Lomi, A., Pallotti, F., & Stadtfeld, C. (2017). Multilevel social spaces: The network dynamics of organizational fields. Network Science, 5(2), 187–212. https://doi.org/10.1017/nws.2017.8
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' B1 <- matrix(c(
#'   1, 1, 0,
#'   0, 0, 1,
#'   0, 0, 1,
#'   1, 0, 0
#' ), byrow = TRUE, ncol = 3)
#' A1 <- matrix(c(
#'   0, 1, 0, 1,
#'   1, 0, 0, 1,
#'   0, 1, 0, 1,
#'   1, 0, 1, 0
#' ), byrow = TRUE, ncol = 4)
#' B2 <- matrix(c(
#'   1, 0, 0, 0, 0,
#'   0, 1, 0, 1, 0,
#'   0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0
#' ), byrow = TRUE, ncol = 5)
#'
#' mixed_census(A1, B1, B2, quad = TRUE)
#' @export
#'

mixed_census <- function(A1, B1, B2 = NULL, quad = FALSE) {
  if (!is.null(B2) & quad == FALSE) {
    stop("For the quadrilateral census you should specify `quad=TRUE` in the function")
  }
  if (dim(A1)[1] != dim(A1)[2]) stop("Matrix is not square")
  if (dim(B1)[1] == dim(B1)[2]) warning("Matrix should be rectangular")
  if (!dim(A1)[1] == dim(B1)[1]) stop("Non-conformable arrays")

  if (any(is.na(A1) == TRUE)) {
    A1 <- ifelse(is.na(A1), 0, A1)
  }
  if (any(is.na(B1) == TRUE)) {
    B1 <- ifelse(is.na(B1), 0, B1)
  }
  if (any(is.na(B2) == TRUE)) {
    B2 <- ifelse(is.na(B2), 0, B2)
  }

  if (any(abs(A1 > 1), na.rm = TRUE)) stop("The matrix should be binary")
  if (any(abs(B1 > 1), na.rm = TRUE)) stop("The matrix should be binary")
  if (any(abs(B2 > 1), na.rm = TRUE)) stop("The matrix should be binary")

  m1 <- as.matrix(A1)
  m2 <- as.matrix(B1)
  cp <- function(m) (-m + 1)

  onemode.reciprocal <- m1 * t(m1)
  onemode.forward <- m1 * cp(t(m1))
  onemode.backward <- cp(m1) * t(m1)
  onemode.null <- cp(m1) * cp(t(m1))
  diag(onemode.forward) <- 0
  diag(onemode.backward) <- 0
  diag(onemode.null) <- 0

  bipartite.twopath <- m2 %*% t(m2)
  bipartite.null <- cp(m2) %*% cp(t(m2))
  bipartite.onestep1 <- m2 %*% cp(t(m2))
  bipartite.onestep2 <- cp(m2) %*% t(m2)
  diag(bipartite.twopath) <- 0
  diag(bipartite.null) <- 0
  diag(bipartite.onestep1) <- 0
  diag(bipartite.onestep2) <- 0

  res <- c(
    "22" = sum(onemode.reciprocal * bipartite.twopath) / 2,
    "21" = sum(onemode.forward * bipartite.twopath) / 2 + sum(onemode.backward * bipartite.twopath) / 2,
    "20" = sum(onemode.null * bipartite.twopath) / 2,
    "12" = sum(onemode.reciprocal * bipartite.onestep1) / 2 + sum(onemode.reciprocal * bipartite.onestep2) / 2,
    "11D" = sum(onemode.forward * bipartite.onestep1) / 2 + sum(onemode.backward * bipartite.onestep2) / 2,
    "11U" = sum(onemode.forward * bipartite.onestep2) / 2 + sum(onemode.backward * bipartite.onestep1) / 2,
    "10" = sum(onemode.null * bipartite.onestep2) / 2 + sum(onemode.null * bipartite.onestep1) / 2,
    "02" = sum(onemode.reciprocal * bipartite.null) / 2,
    "01" = sum(onemode.forward * bipartite.null) / 2 + sum(onemode.backward * bipartite.null) / 2,
    "00" = sum(onemode.null * bipartite.null) / 2
  )

  if (quad) {
    if (!dim(B2)[1] == dim(A1)[1]) stop("Non-conformable arrays")
    if (dim(B2)[1] == dim(B2)[2]) warning("Matrix should be rectangular")
    m3 <- as.matrix(B2)
    bipartite.twopath2 <- m3 %*% t(m3)
    bipartite.null2 <- cp(m3) %*% cp(t(m3))
    bipartite.onestep12 <- m3 %*% cp(t(m3))
    bipartite.onestep22 <- cp(m3) %*% t(m3)
    diag(bipartite.twopath2) <- 0
    diag(bipartite.null2) <- 0
    diag(bipartite.onestep12) <- 0
    diag(bipartite.onestep22) <- 0

    res <- c(
      "000" = sum(onemode.null * bipartite.null * bipartite.null2) / 2,
      "100" = sum(onemode.null * bipartite.onestep1 * bipartite.null2) / 2 + sum(onemode.null * bipartite.onestep2 * bipartite.null2) / 2,
      "001" = sum(onemode.null * bipartite.null * bipartite.onestep12) / 2 + sum(onemode.null * bipartite.null * bipartite.onestep22) / 2,
      "010" = sum(onemode.forward * bipartite.null * bipartite.null2) / 2 + sum(onemode.backward * bipartite.null * bipartite.null2) / 2,
      "020" = sum(onemode.reciprocal * bipartite.null * bipartite.null2) / 2,
      "200" = sum(onemode.null * bipartite.twopath * bipartite.null2) / 2,
      "11D0" = sum(onemode.forward * bipartite.onestep1 * bipartite.null2) / 2 + sum(onemode.backward * bipartite.onestep2 * bipartite.null2) / 2,
      "11U0" = sum(onemode.forward * bipartite.onestep2 * bipartite.null2) / 2 + sum(onemode.backward * bipartite.onestep1 * bipartite.null2) / 2,
      "120" = sum(onemode.reciprocal * bipartite.onestep1 * bipartite.null2) / 2 + sum(onemode.reciprocal * bipartite.onestep2 * bipartite.null2) / 2,
      "210" = sum(onemode.forward * bipartite.twopath * bipartite.null2) / 2 + sum(onemode.backward * bipartite.twopath * bipartite.null2) / 2,
      "220" = sum(onemode.reciprocal * bipartite.twopath * bipartite.null2) / 2,
      "002" = sum(onemode.null * bipartite.null * bipartite.twopath2) / 2,
      "01D1" = sum(onemode.forward * bipartite.null * bipartite.onestep22) / 2 + sum(onemode.backward * bipartite.null * bipartite.onestep12) / 2,
      "01U1" = sum(onemode.forward * bipartite.null * bipartite.onestep12) / 2 + sum(onemode.backward * bipartite.null * bipartite.onestep22) / 2,
      "012" = sum(onemode.forward * bipartite.null * bipartite.twopath2) / 2 + sum(onemode.backward * bipartite.null * bipartite.twopath2) / 2,
      "021" = sum(onemode.reciprocal * bipartite.null * bipartite.onestep12) / 2 + sum(onemode.reciprocal * bipartite.null * bipartite.onestep22) / 2,
      "022" = sum(onemode.reciprocal * bipartite.null * bipartite.twopath2) / 2,
      "101N" = sum(onemode.null * bipartite.onestep1 * bipartite.onestep22) / 2 + sum(onemode.null * bipartite.onestep2 * bipartite.onestep12) / 2,
      "101P" = sum(onemode.null * bipartite.onestep1 * bipartite.onestep12) / 2 + sum(onemode.null * bipartite.onestep2 * bipartite.onestep22) / 2,
      "201" = sum(onemode.null * bipartite.twopath * bipartite.onestep12) * sum(onemode.null * bipartite.twopath * bipartite.onestep22),
      "102" = sum(onemode.null * bipartite.onestep1 * bipartite.twopath2) / 2 + sum(onemode.null * bipartite.onestep2 * bipartite.twopath2) / 2,
      "202" = sum(onemode.null * bipartite.twopath * bipartite.twopath2) / 2,
      "11D1W" = sum(onemode.forward * bipartite.onestep1 * bipartite.onestep12) / 2 + sum(onemode.backward * bipartite.onestep2 * bipartite.onestep22) / 2,
      "11U1P" = sum(onemode.forward * bipartite.onestep2 * bipartite.onestep22) / 2 + sum(onemode.backward * bipartite.onestep1 * bipartite.onestep12) / 2,
      "11D1P" = sum(onemode.forward * bipartite.onestep1 * bipartite.onestep22) / 2 + sum(onemode.backward * bipartite.onestep2 * bipartite.onestep12) / 2,
      "11U1W" = sum(onemode.forward * bipartite.onestep2 * bipartite.onestep12) / 2 + sum(onemode.backward * bipartite.onestep1 * bipartite.onestep22) / 2,
      "121W" = sum(onemode.reciprocal * bipartite.onestep1 * bipartite.onestep12) / 2 + sum(onemode.reciprocal * bipartite.onestep2 * bipartite.onestep22) / 2,
      "121P" = sum(onemode.reciprocal * bipartite.onestep1 * bipartite.onestep22) / 2 + sum(onemode.reciprocal * bipartite.onestep2 * bipartite.onestep12) / 2,
      "21D1" = sum(onemode.forward * bipartite.twopath * bipartite.onestep12) / 2 + sum(onemode.backward * bipartite.twopath * bipartite.onestep22) / 2,
      "21U1" = sum(onemode.forward * bipartite.twopath * bipartite.onestep22) / 2 + sum(onemode.backward * bipartite.twopath * bipartite.onestep12) / 2,
      "11D2" = sum(onemode.forward * bipartite.onestep1 * bipartite.twopath2) / 2 + sum(onemode.backward * bipartite.onestep2 * bipartite.twopath2) / 2,
      "11U2" = sum(onemode.forward * bipartite.onestep2 * bipartite.twopath2) / 2 + sum(onemode.backward * bipartite.onestep1 * bipartite.twopath2) / 2,
      "221" = sum(onemode.reciprocal * bipartite.twopath * bipartite.onestep12) / 2 + sum(onemode.reciprocal * bipartite.twopath * bipartite.onestep22) / 2,
      "122" = sum(onemode.reciprocal * bipartite.onestep1 * bipartite.twopath2) / 2 + sum(onemode.reciprocal * bipartite.onestep2 * bipartite.twopath2) / 2,
      "212" = sum(onemode.forward * bipartite.twopath * bipartite.twopath2) / 2 + sum(onemode.backward * bipartite.twopath * bipartite.twopath2) / 2,
      "222" = sum(onemode.reciprocal * bipartite.twopath * bipartite.twopath2) / 2
    )
  }
  return(res)
}
