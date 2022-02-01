#' Matrix report
#'
#' The primary matrix used in social network analysis are the
#' adjacency matrix or sociomatrix, and the incidence matrix.
#'
#' @param A   A matrix
#'
#' @return This function return a report of some of the characteristics of the matrix.
#'
#' @references
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   1, 1, 0, 0, -1,
#'   1, 0, 0, 1, 1,
#'   0, 0, NA, 1, 1,
#'   0, 1, 1, 0, 1,
#'   1, 1, 1, 1, 0
#' ), byrow = TRUE, ncol = 5)
#'
#' B <- matrix(c(
#'   1, 0, 0,
#'   1, 1, 0,
#'   0, NA, 0,
#'   0, 1, 0,
#'   0, 1, 1
#' ), byrow = TRUE, ncol = 3)
#' matrix_report(A)
#' matrix_report(B)
#' @export

matrix_report <- function(A) {
  if (!is.matrix(A)) stop("The object is not a matrix")
  if (length(A) == 1) {
    stop("A 1 x 1 matrix")
  }

  nodes <- ncol(A)

  message(paste("The matrix", as.character(bquote(A)), "might have the following characteristics:"))

  # if(is.double(A))message('--> The vectors of the matrix are `double`')
  if (is.numeric(A)) message("--> The vectors of the matrix are `numeric`")
  if (is.integer(A)) message("--> The vectors of the matrix are `integer`")
  if (is.character(A)) message("--> The vectors of the matrix are `character`")
  if (is.logical(A)) message("--> The vectors of the matrix are `logical`")

  if (is.null(rownames(A))) message("--> No names assigned to the rows of the matrix")
  if (is.null(colnames(A))) message("--> No names assigned to the columns of the matrix")

  if (any(abs(A) > 1, na.rm = TRUE)) message("--> Valued matrix")
  if (any(A < 0, na.rm = TRUE)) message("--> The matrix has negative elements (network is signed)")
  if (any(is.na(A))) message("--> The matrix has NA elements")

  if (ncol(A) == nrow(A)) {
    if (!all(A[lower.tri(A)] == t(A)[lower.tri(A)], na.rm = TRUE)) {
      message("--> Matrix is asymmetric (network is directed)")
      if (any(diag(A) != 0)) message("--> The main diagonal is nonzero (the network has loops)")
      edges <- sum(A, na.rm = TRUE)
    } else {
      message("--> Matrix is symmetric (network is undirected)")
      if (any(diag(A) != 0)) {
        message("--> The main diagonal is nonzero (the network has loops)")
        I <- diag(1, ncol(A))
        if (all(A == I)) message("--> An identity matrix")
      }
      edges <- sum(A, na.rm = TRUE) / 2
    }
  }

  if (ncol(A) == nrow(A)) {
    message(paste("--> The matrix is square,", ncol(A), "by", nrow(A)))

    if (!all(A[lower.tri(A)] == t(A)[lower.tri(A)], na.rm = TRUE)) {
      return(cbind(nodes = nodes, arcs = edges))
    } else {
      return(cbind(nodes = nodes, edges = edges))
    }
  } else {
    message(paste("--> The matrix is rectangular,", ncol(A), "by", nrow(A)))
    mode_level1 <- ncol(A)
    mode_level2 <- nrow(A)
    return(cbind(
      nodes_rows = mode_level1,
      nodes_columns = mode_level2,
      incident_lines = edges
    ))
  }
}


#' Transform a square matrix to an edge-list
#'
#' @param A   A square matrix
#' @param digraph   Whether the matrix is directed or not
#' @param valued  Add a third columns with the valued of the relationship
#' @param loops   Whether the loops are retained or not
#'
#' @return This function transform the matrix into an edgelist
#'
#' @importFrom stats aggregate
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 2, 1,
#'   1, 0, 0,
#'   1, 0, 1
#' ), byrow = TRUE, ncol = 3)
#' matrix_to_edgelist(A, digraph = TRUE, valued = TRUE, loops = TRUE)
#' @export

matrix_to_edgelist <- function(A, digraph = FALSE, valued = FALSE, loops = FALSE) {
  M <- A

  if (digraph) {
    if (all(A[lower.tri(A)] == t(A)[lower.tri(A)], na.rm = TRUE)) message("The network might be undirected")
  } else {
    if (!all(A[lower.tri(M)] == t(M)[lower.tri(M)], na.rm = TRUE)) message("The network might be directed")
    M[lower.tri(M)] <- 0
  }

  if (is.null(colnames(M))) colnames(M) <- 1:ncol(M)
  if (is.null(rownames(M))) rownames(M) <- 1:nrow(M)

  if (any(is.na(M) == TRUE)) {
    M <- ifelse(is.na(M), 0, M)
  }

  if (!digraph) {
    M[lower.tri(M)] <- 0
  }

  edge <- NULL
  for (i in 1:nrow(M)) {
    for (j in 1:ncol(M)) {
      if (M[i, j] != 0) {
        edge <- c(
          edge,
          rep(c(
            dimnames(M)[[1]][i],
            dimnames(M)[[2]][j]
          ))
        )
      }
    }
  }
  edge <- matrix(edge, byrow = TRUE, ncol = 2)

  if (valued) {
    edge <- NULL
    for (i in 1:nrow(M)) {
      for (j in 1:ncol(M)) {
        if (M[i, j] != 0) {
          edge <- c(
            edge,
            rep(
              c(
                dimnames(M)[[1]][i],
                dimnames(M)[[2]][j]
              ),
              M[i, j]
            )
          )
        }
      }
    }
    edge <- matrix(edge, byrow = TRUE, ncol = 2)
    df <- as.data.frame(edge)
    edge <- as.matrix(stats::aggregate(list(valued = rep(1, nrow(df))), df, length))
    colnames(edge) <- NULL
  } else {
    if (any(abs(A) > 1)) message("The networks is valued")
  }

  if (loops == FALSE) {
    if (any(diag(M > 0))) message("There are loops in the network")
    edge <- edge[edge[, 1] != edge[, 2], ]
  }

  return(edge)
}

#' Transform an edgelist to a matrix
#'
#' @param E   An edge list
#' @param digraph   Whether the matrix is directed or not
#' @param label  A vector with the names of the nodes
#' @param label2   A vector with the names of a different set of nodes
#' @param bipartite  Whether the matrix is bipartite
#'
#' @return This function transform the edgelist into a matrix
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
#' E <- matrix_to_edgelist(A)
#' edgelist_to_matrix(E, label = c("i"))
#' @export

# TODO: add valued matrix
edgelist_to_matrix <- function(E, digraph = TRUE, label = NULL,
                               label2 = NULL, bipartite = FALSE) {
  if (bipartite) {
    if (!is.null(label)) {
      nodes1 <- unique(c(E[, 1], label))
    } else {
      nodes1 <- unique(c(E[, 1]))
    }
    if (!is.null(label2)) {
      nodes2 <- unique(c(E[, 2], label2))
    } else {
      nodes2 <- unique(c(E[, 2]))
    }
    empty <- matrix(0,
      nrow = length(nodes1), ncol = length(nodes2),
      dimnames = list(nodes1, nodes2)
    )

    for (i in 1:nrow(E)) {
      temp1 <- which(rownames(empty) %in% E[i, ])
      temp2 <- which(colnames(empty) %in% E[i, ])
      empty[temp1, temp2] <- 1 # valued
    }
  } else {
    if (!is.null(label)) {
      nodes <- unique(c(unlist(E)))
      nodes <- unique(c(nodes, label))
    } else {
      nodes <- unique(c(unlist(E)))
    }
    empty <- matrix(0,
      nrow = length(nodes), ncol = length(nodes),
      dimnames = list(nodes, nodes)
    )
    for (i in 1:nrow(E)) {
      temp <- which(rownames(empty) %in% E[i, ])
      empty[temp[1], temp[2]] <- 1 # valued
    }
  }

  A <- empty[
    order(rownames(empty)),
    order(colnames(empty))
  ]

  if (!digraph) {
    A[lower.tri(A)] <- t(A)[lower.tri(A)]
  }
  return(A)
}

#' Transform a matrix to an adjacency list
#'
#' @param A   A matrix
#'
#' @return This function transform a matrix to an adjacency list
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
#' matrix_adjlist(A)
#' @export

matrix_adjlist <- function(A) {
  adj_list <- list()
  for (i in 1:ncol(A)) {
    adj_list[[i]] <- names(A[i, ][A[i, ] >= 1])
    names(adj_list)[i] <- rownames(A)[i]
  }
  return(adj_list)
}


#' Unipartite projections
#'
#' Two-mode networks can be represented (or 'projected') as one-mode networks.
#'
#' @param A  A matrix object
#'
#' @return This function return a list of matrices of the two projections of the original matrix.
#'
#' @references
#'
#' Davis, Allison; Gardner, Burleigh B. and Mary. R. Gardner (1941). Deep South: A Social Anthropological Study of Caste and Class. The University of Chicago Press, Chicago.
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   2, 0, 2,
#'   1, 1, 0,
#'   0, 3, 3,
#'   0, 2, 2,
#'   0, 0, 1
#' ), byrow = TRUE, ncol = 3)
#' matrix_projection(A)
#' @export
matrix_projection <- function(A) {
  A <- as.matrix(A)
  projection1 <- t(A) %*% A
  projection2 <- A %*% t(A)
  return(list(matrix1 = projection1, matrix2 = projection2))
}

#' Minimum/maximum overlap
#'
#' Two-mode networks can be represented (or 'projected') as one-mode networks.
#'
#' @param A  A matrix object
#' @param row  Whether to consider the actors in the rows of the matrix (default) or the column.
#' @param min  Whether to extract the minimum (default) or the maximum overlap.
#'
#' @return This function return the overlap between the modes (a.k.a. actors, nodes, vertices).
#'
#' @references
#'
#' Morris, S.A. (2005). Unified Mathematical Treatment of Complex Cascaded Bipartite Networks: The Case of Collections of Journal Papers. Unpub- lished PhD Thesis, Oklahoma State University. Retrieved from \url{http://digital.library.okstate.edu/etd/umi-okstate-1334.pdf}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' A <- matrix(c(
#'   2, 0, 2,
#'   1, 1, 0,
#'   0, 3, 3,
#'   0, 2, 2,
#'   0, 0, 1
#' ), byrow = TRUE, ncol = 3)
#' minmax_overlap(A)
#' @export
minmax_overlap <- function(A, row = TRUE, min = TRUE) {
  A <- as.matrix(A)
  if (!row) {
    A <- t(A)
  }
  sim.jac <- matrix(0, nrow = nrow(A), ncol = nrow(A))
  rownames(sim.jac) <- rownames(A)
  colnames(sim.jac) <- rownames(A)
  pairs <- t(combn(1:nrow(A), 2))
  for (i in 1:nrow(pairs)) {
    if (min) {
      num <- sum(sapply(1:ncol(A), function(x) (min(A[pairs[i, 1], x], A[pairs[i, 2], x]))))
    } else {
      num <- sum(sapply(1:ncol(A), function(x) (max(A[pairs[i, 1], x], A[pairs[i, 2], x]))))
    }
    sim.jac[pairs[i, 1], pairs[i, 2]] <- num
    sim.jac[pairs[i, 2], pairs[i, 1]] <- num
  }
  sim.jac[which(is.na(sim.jac))] <- 0
  diag(sim.jac) <- rowSums(A)
  return(sim.jac)
}

#' Ego network
#'
#' Submatrix of ego's neighbourhoods
#'
#' @param A   A symmetric matrix object
#' @param ego   Name of ego in the matrix
#' @param bipartite  Whether the matrix is a two-mode network
#' @param addEgo  Whether to retain ego in the submatrix or not
#' @param select   Whether to consider all sender and receiver ties of ego (\code{all}), only incoming ties (\code{in}), or outgoing ties (\code{out}). By default, \code{all}.
#'
#' @return This function returns redundancy, effective size and efficincy measures (Burt, 1992).
#'
#' @references
#'
#' Burt, R.S., 1992. Structural Holes: the Social Structure of Competition. Harvard University Press, Cambridge.
#'
#' Borgatti, S., 1997. Unpacking Burt's redundancy measure. Connections, 20(1): 35-38. doi: \url{http://www.analytictech.com/connections/v20(1)/holes.htm}
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
#' ego_net(A, ego = "g")
#' @export

ego_net <- function(A, ego = NULL, bipartite = FALSE, addEgo = FALSE,
                    select = c("all", "in", "out")) {
  if (class(ego) == "numeric") stop("Label of the name of ego should be in character format")
  if (is.null(rownames(A))) stop("No label assigned to the rows of the matrix")
  if (is.null(colnames(A))) stop("No label assigned to the columns of the matrix")
  if (!(ego %in% sort(unique(c(rownames(A), colnames(A)))))) stop("Ego name does not match with the names of the enlisted nodes")

  A <- as.matrix(A)
  ego <- as.character(ego)
  select <- switch(node_direction(select),
    "out" = 1,
    "in" = 2,
    "all" = 3
  )

  if (bipartite == TRUE) {
    if (ncol(A) == nrow(A)) warning("Matrix should be rectangular")
    E <- matrix(0, nrow = nrow(A), ncol = nrow(A), byrow = TRUE)
    rownames(E) <- rownames(A)
    colnames(E) <- rownames(A)
    S <- matrix(0, nrow = ncol(A), ncol = ncol(A), byrow = TRUE)
    rownames(S) <- colnames(A)
    colnames(S) <- colnames(A)
    SE <- t(A)
    UP <- cbind(A, E)
    DOWN <- cbind(S, SE)
    A <- rbind(UP, DOWN)
  }

  if (is.null(rownames(A))) {
    rownames(A) <- as.character(1:nrow(A))
  }
  if (is.null(rownames(A))) {
    colnames(A) <- as.character(1:nrow(A))
  }

  # In, out or all
  if (select == 1) { # out
    name <- names(which(A[ego, ] != 0))
    if (length(name) == 0) {
      name <- NULL
    }
  }
  if (select == 2) { # in
    name <- names(which(A[, ego] != 0))
    if (length(name) == 0) {
      name <- NULL
    }
  }
  if (select == 3) { # all
    nameOut <- names(which(A[ego, ] != 0))
    nameIn <- names(which(A[, ego] != 0))
    name <- unique(c(nameOut, nameIn))
  }

  if (addEgo) {
    subA <- A[name, name]
    # A[ego,][A[ego,]!=0]
    addEGO <- rbind(subA, A[ego, ][c(name)])
    # c(A[,ego][A[,ego]!=0],0)
    addEGO <- cbind(addEGO, A[, ego][c(name, ego)])
    name <- c(name, ego)
    rownames(addEGO) <- name
    colnames(addEGO) <- name

    return(addEGO)
  } else {
    if (length(name) == 1) {
      return(matrix(c(name), ncol = 1, nrow = 1))
    }
    if (length(name) == 0) {
      message(paste("actor", ego, "has no neighbour"))
    } else {
      (A[name, name])
    }
  }
}

node_direction <- function(arg, choices, several.ok = FALSE) {
  if (missing(choices)) {
    formal.args <- formals(sys.function(sys.parent()))
    choices <- eval(formal.args[[deparse(substitute(arg))]])
  }

  arg <- tolower(arg)
  choices <- tolower(choices)

  match.arg(arg = arg, choices = choices, several.ok = several.ok)
}

#' Meta matrix for multilevel networks
#'
#' @param A1  The square matrix of the lowest level
#' @param B1  The incident matrix of the ties between the nodes of first level and the nodes of the second level
#' @param A2  The square matrix of the second level
#' @param B2  The incident matrix of the ties between the nodes of the second level and the nodes of the third level
#' @param A3  The square matrix of the third level
#' @param B3  The incident matrix of the ties between the nodes of the third level and the nodes of the first level
#'
#' @return Return a meta matrix for multilevel networks
#'
#' @references
#'
#' Carley, K. M. (2002). Smart agents and organizations of the future. In: Leah Lievrouw & Sonia Livingstone (Eds.), The Handbook of New Media (pp. 206-220). Thousand Oaks, CA, Sage.
#'
#' Krackhardt, D., & Carley, K. M. (1998). PCANS model of structure in organizations (pp. 113- 119). Pittsburgh, Pa, USA: Carnegie Mellon University, Institute for Complex Engineered Systems.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' A1 <- matrix(c(
#'   0, 1, 0, 0, 0,
#'   1, 0, 0, 1, 0,
#'   0, 0, 0, 1, 0,
#'   0, 1, 1, 0, 1,
#'   0, 0, 0, 1, 0
#' ), byrow = TRUE, ncol = 5)
#'
#' B1 <- matrix(c(
#'   1, 0, 0,
#'   1, 1, 0,
#'   0, 1, 0,
#'   0, 1, 0,
#'   0, 1, 1
#' ), byrow = TRUE, ncol = 3)
#'
#' A2 <- matrix(c(
#'   0, 1, 1,
#'   1, 0, 0,
#'   1, 0, 0
#' ), byrow = TRUE, nrow = 3)
#'
#' B2 <- matrix(c(
#'   1, 1, 0, 0,
#'   0, 0, 1, 0,
#'   0, 0, 1, 1
#' ), byrow = TRUE, ncol = 4)
#'
#' A3 <- matrix(c(
#'   0, 1, 1, 1,
#'   1, 0, 0, 0,
#'   1, 0, 0, 1,
#'   1, 0, 1, 0
#' ), byrow = TRUE, ncol = 4)
#'
#' B3 <- matrix(c(
#'   1, 0, 0, 0, 0,
#'   0, 1, 0, 1, 0,
#'   0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0
#' ), byrow = TRUE, ncol = 5)
#'
#' rownames(A1) <- letters[1:nrow(A1)]
#' colnames(A1) <- rownames(A1)
#' rownames(A2) <- letters[nrow(A1) + 1:nrow(A2)]
#' colnames(A2) <- rownames(A2)
#' rownames(B1) <- rownames(A1)
#' colnames(B1) <- colnames(A2)
#' rownames(A3) <- letters[nrow(A1) + nrow(A2) + 1:nrow(A3)]
#' colnames(A3) <- rownames(A3)
#' rownames(B2) <- rownames(A2)
#' colnames(B2) <- colnames(A3)
#' rownames(B3) <- rownames(A3)
#' colnames(B3) <- rownames(A1)
#' meta_matrix(A1, B1, A2, B2, A3, B3)
#' @export
#'

meta_matrix <- function(A1, B1,
                        A2 = NULL, B2 = NULL,
                        A3 = NULL, B3 = NULL) {
  A1 <- as.matrix(A1)

  if (!is.null(A3)) {
    if (is.null(B1)) stop("Is not available the bipartite network between levels")
  }
  if (!is.null(B1)) {
    if (!nrow(A1) == nrow(B1)) stop("Non-conformable arrays")
    if (nrow(B1) == ncol(B1)) warning("Matrix should be rectangular")

    M1 <- cbind(A1, B1)
    M1b <- cbind(
      t(B1),
      matrix(0,
        nrow = (ncol(M1) - nrow(B1)), byrow = T,
        ncol = (ncol(M1) - nrow(A1))
      )
    )

    colnames(M1b) <- c(rownames(M1), rownames(M1b))

    meta_matrix <- rbind(M1, M1b)
  }

  if (!is.null(A2)) {
    if (is.null(B1)) stop("Multilevel networks require at least one bipartite network between levels")
    if (!nrow(A2) == ncol(A2)) stop("Matrix should be square")

    if (!ncol(B1) == nrow(A2)) stop("Non-conformable arrays")
    M2 <- cbind(A1, B1)
    M2b <- cbind(
      t(B1), A2
    )
    meta_matrix <- rbind(M2, M2b)
  } else {
    A2 <- matrix(0, byrow = TRUE, ncol = ncol(B1), nrow = ncol(B1))

    rownames(A2) <- colnames(B1)
    colnames(A2) <- rownames(A2)
  }

  if (!is.null(B2)) {
    if (is.null(B1)) stop("Is not available the first bipartite network between lower and medium levels")
    if (nrow(B2) == ncol(B2)) warning("Matrix should be rectangular")
    if (!nrow(A2) == nrow(B2)) stop("Non-conformable arrays")


    M3 <- cbind(
      meta_matrix,
      rbind(matrix(0,
        nrow = nrow(A1), byrow = T,
        ncol = ncol(B2)
      ), B2)
    )
    M3b <- matrix(0,
      nrow = ncol(B2), byrow = T,
      ncol = ncol(M3)
    )
    rownames(M3b) <- colnames(B2)
    meta_matrix <- rbind(M3, M3b)
  }

  if (!is.null(A3)) {
    if (!nrow(A3) == ncol(A3)) stop("Matrix should be square")
    if (!ncol(B2) == nrow(A3)) stop("Non-conformable arrays")

    meta_matrix <- rbind(
      M3,
      cbind(matrix(0,
        nrow = ncol(A3), byrow = T,
        ncol = ncol(M3) - ncol(A3)
      ), A3)
    )
  }
  if (!is.null(B3)) {
    if (is.null(B2)) {
      B2 <- matrix(0, byrow = TRUE, ncol = ncol(B1), nrow = nrow(B3))
    }

    if (!nrow(B3) == ncol(A3)) stop("Non-conformable arrays")
    if (!ncol(B3) == nrow(A1)) stop("Non-conformable arrays")

    M4 <- cbind(
      rbind(M2, M2b),
      rbind(
        t(B3),
        matrix(0, nrow = ncol(A2), ncol = nrow(B3), byrow = TRUE)
      )
    )

    M4a <- cbind(matrix(0,
      nrow = ncol(A3), byrow = T,
      ncol = ncol(M3) - ncol(A3)
    ), A3)

    meta_matrix <- rbind(M4, M4a)
  }

  return(meta_matrix)
}

#' Structural missing data
#'
#' Assign NA to missing data in the matrices
#'
#' @param A   A symmetric or incident matrix object
#' @param label   String vector with the names of the theoretical complete matrix
#' @param bipartite   Whether the matrix is bipartite or not.
#' @param column   Whether the assignation of NA is for columns in the biparite network, row by default.
#'
#' @return This function returns NA to missing data.
#'
#' @author Alejandro Espinosa-Rada

#' @examples
#'
#' A <- matrix(c(
#'   0, 1, 1,
#'   1, 0, 1,
#'   0, 0, 0
#' ), byrow = TRUE, ncol = 3)
#' colnames(A) <- c("A", "C", "D")
#' rownames(A) <- c("A", "C", "D")
#' label <- c("A", "B", "C", "D", "E")
#' structural_na(A, label)
#' @export

structural_na <- function(A, label = NULL, bipartite = FALSE, column = FALSE) {
  if (bipartite) {
    if (dim(A)[1] == dim(A)[2]) warning("Incident matrix should be rectangular")
    if (column) {
      for (i in 1:dim(A)[2]) {
        A[, i] <- ifelse((A[, i] | sum(A[, i])) == 0,
          NA, A[, i]
        )
      }
      x <- A
    } else {
      for (i in 1:dim(A)[1]) {
        A[i, ] <- ifelse((A[i, ] | sum(A[i, ])) == 0,
          NA, A[i, ]
        )
      }
      x <- A
    }
    return(x)
  } else {
    if (!dim(A)[1] == dim(A)[2]) stop("Matrix should be square")
    if (is.null(colnames(A))) stop("Assign column names to the matrix.")
    if (is.null(rownames(A))) stop("Assign column names to the matrix.")
    if (!is.character(label)) stop("Assign a string vector with the names of the complete matrix.")
    x <- array(NA, dim = list(length(label), length(label)))
    colnames(x) <- label
    rownames(x) <- label
    rowmatch <- match(rownames(A), rownames(x))
    colmatch <- match(colnames(A), colnames(x))
    x[rowmatch, colmatch] <- A
  }
  return(x)
}

#' Zone-2 sampling from second-mode
#'
#' Second-zone multilevel sampling considering a second-mode focal actor
#'
#' @param A   A symmetric matrix object.
#' @param X   X an incident matrix object.
#' @param ego   Whether to add or not ego into the subgraph.
#' @param core  Whether to add actors at distance one from ego
#'
#' @return This function return a list of second-zone subgraphs using as a focal actor the second-mode of the multilevel network.
#'
#' @references
#'
#' Espinosa-Rada, A. (2021). A Network Approach for the Sociological Study of Science: Modelling Dynamic Multilevel Networks. [PhD]. The University of Manchester.
#'
#' @import igraph
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' A <- matrix(c(
#'   0, 1, 0, 0, 0, 0, 0, 0,
#'   0, 0, 1, 0, 0, 0, 0, 0,
#'   0, 1, 0, 1, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 1, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0
#' ), byrow = TRUE, ncol = 8)
#' colnames(A) <- c("1", "2", "3", "4", "5", "6", "7", "8")
#' rownames(A) <- c("1", "2", "3", "4", "5", "6", "7", "8")
#'
#' X <- matrix(c(
#'   1, 0, 0, 0,
#'   1, 0, 0, 0,
#'   1, 0, 1, 0,
#'   0, 1, 1, 0,
#'   0, 1, 1, 1,
#'   0, 1, 0, 0,
#'   0, 0, 0, 0,
#'   0, 0, 0, 1
#' ), byrow = TRUE, ncol = 4)
#' colnames(X) <- c("a", "b", "c", "d")
#' rownames(X) <- c("1", "2", "3", "4", "5", "6", "7", "8")
#'
#' set.seed(18051889)
#' zone_sample(A, X, core = TRUE)
#' @export

zone_sample <- function(A, X, ego = TRUE, core = FALSE) {
  A <- as.matrix(A)
  X <- as.matrix(X)
  if (is.null(rownames(A))) stop("Assign `rownames` to the adjacent matrix")
  if (is.null(colnames(A))) stop("Assign `colnames` to the adjacent matrix")
  if (is.null(rownames(X))) stop("Assign `rownames` to the incident matrix")
  if (is.null(colnames(X))) stop("Assign `colnames` to the incident matrix")
  if (dim(A)[1] != dim(X)[1]) {
    X <- t(X)
  }
  if (!all(colnames(A) == rownames(X))) warning("The names for the combination of the matrices should be the same")
  zero <- matrix(0, ncol = ncol(X), nrow = ncol(X))
  M1 <- cbind(A, X)
  M2 <- cbind(t(X), zero)
  gM <- rbind(M1, M2)
  A1 <- gM
  gM <- igraph::graph.adjacency(gM)
  label <- colnames(X)
  subgraphs <- list()
  zone1 <- list()
  external <- list()
  for (i in label) {
    zone1[[i]] <- which(A1[, i] != 0)
    zone2 <- A1 %*% A1
    zone2 <- zone2 + A1 %*% t(A1)
    nei <- which(zone2[i, ] != 0)
    nei <- rownames(as.data.frame(nei))
    nei <- nei[which(nei != i)]
    not <- names(zone1[[i]]) %in% label
    members_zone1 <- names(zone1[[i]][!not])
    out <- nei[!nei %in% members_zone1]
    out <- out[!out %in% colnames(X)]
    external[[i]] <- length(out)
    outInst <- names(which(X[out, ] != 0))
    if (ego == TRUE) {
      nei <- c(nei, members_zone1, outInst, i)
    }
    if (ego == FALSE) {
      nei <- c(nei, members_zone1, outInst)
    }
    nei <- unique(nei)
    subgraphs[[i]] <- igraph::delete.vertices(gM, !(V(gM)$name %in% nei))
    subgraphs[[i]] <- igraph::simplify(subgraphs[[i]])
    if (core == TRUE) {
      V(subgraphs[[i]])$core <- ifelse(V(subgraphs[[i]])$name %in% names(zone1[[i]]), 1, 0)
    }
  }

  return(subgraphs)
}

#' Geodesic
#'
#' Shortest path between two nodes using Dijkstra's algorithm
#'
#' @name geodesic
#'
#' @param A   A symmetric matrix object
#' @param select   Whether to consider all sender and receiver ties of ego (\code{all}), only incoming ties (\code{in}), or outgoing ties (\code{out}). By default, \code{all}.
#' @param from  Node in which the path start
#' @param to  Node in which the path end
#' @param path  Path of the nodes
#'
#' @return This function returns the geodesic o shortest path distance between two nodes
#'
#' @references
#'
#' Dijkstra, E. W. (1959). A note on two problems in connexion with graphs. Numerische Mathematik. 1: 269–271.
#'
#' @author Alejandro Espinosa-Rada

NULL

#' @rdname geodesic
#' @examples
#' A <- matrix(c(
#'   0, 3, 3, 10, 15, 0, 0, 0,
#'   1, 0, 5, 2, 7, 0, 0, 0,
#'   3, 5, 0, 0, 0, 0, 0, 0,
#'   10, 2, 0, 0, 2, 7, 12, 0,
#'   11, 3, 0, 3, 0, 11, 2, 0,
#'   0, 0, 0, 7, 11, 0, 3, 2,
#'   0, 0, 0, 12, 2, 3, 0, 2,
#'   0, 0, 0, 0, 0, 2, 2, 0
#' ),
#' byrow = TRUE, ncol = 8, nrow = 8
#' )
#' rownames(A) <- c("a", "b", "s", "c", "d", "e", "f", "z")
#' colnames(A) <- rownames(A)
#' local_geodesic(A, from = "a", to = "d")
#' @export

local_geodesic <- function(A, select = c("all", "in", "out"),
                           from, to, path = c()) {
  adjlist <- matrix_adjlist(A)
  edgelist <- as.data.frame(matrix_to_edgelist(A, valued = TRUE, digraph = TRUE))
  edgelist$V3 <- as.numeric(edgelist$V3)
  test <- list()
  test <- internal_geodesic(adjlist, init = from, fin = to, walk = path)
  return(list(path = test))
}

internal_geodesic <- function(A, init, fin, walk = c()) {
  if (is.null(A[[init]])) {
    return(NULL)
  }
  walk <- c(walk, init)

  if (init == fin) {
    return(walk)
  }

  short_path <- NULL
  for (node in A[[init]]) {
    if (!(node %in% walk)) {
      newwalk <- internal_geodesic(A, node, fin, walk)
      if (walk_length(newwalk) < walk_length(short_path)) {
        short_path <- newwalk
      }
    }
  }

  short_path
}

# TODO: nest the function and check parameter A inside walk_length
walk_length <- function(walk) {
  edgelist <- as.data.frame(matrix_to_edgelist(A, valued = TRUE, digraph = TRUE))
  edgelist$V3 <- as.numeric(edgelist$V3)
  if (is.null(walk)) {
    return(Inf)
  }

  pairs <- cbind(V1 = walk[-length(walk)], V2 = walk[-1])
  sum(merge(pairs, edgelist)[, "V3"])
}

#' @rdname geodesic
#' @examples
#' A <- matrix(c(
#'   0, 3, 3, 10, 15, 0, 0, 0,
#'   1, 0, 5, 2, 7, 0, 0, 0,
#'   3, 5, 0, 0, 0, 0, 0, 0,
#'   10, 2, 0, 0, 2, 7, 12, 0,
#'   11, 3, 0, 3, 0, 11, 2, 0,
#'   0, 0, 0, 7, 11, 0, 3, 2,
#'   0, 0, 0, 12, 2, 3, 0, 2,
#'   0, 0, 0, 0, 0, 2, 2, 0
#' ),
#' byrow = TRUE, ncol = 8, nrow = 8
#' )
#' rownames(A) <- c("a", "b", "s", "c", "d", "e", "f", "z")
#' colnames(A) <- rownames(A)
#' all_geodesic(A, select = "in")
#' @export

all_geodesic <- function(A, select = c("all", "in", "out")) {
  adjlist <- matrix_adjlist(A)
  edgelist <- as.data.frame(matrix_to_edgelist(A, valued = TRUE, digraph = TRUE))
  edgelist$V3 <- as.numeric(edgelist$V3)

  select <- switch(node_direction(select),
    "out" = 1,
    "in" = 2,
    "all" = 3
  )

  if (select == 1) {
    temp3 <- list()
    temp4 <- list()
    for (i in 1:ncol(A)) {
      for (j in i:ncol(A)) {
        temp4[[j]] <- internal_geodesic(adjlist, init = rownames(A)[j], fin = rownames(A)[i])
      }
      temp3[[i]] <- temp4[[j]]
      names(temp3)[i] <- rownames(A)[i]
    }
    return(list(toFrom = temp3))
  }

  if (select == 2) {
    temp1 <- list()
    temp2 <- list()
    for (i in 1:ncol(A)) {
      for (j in i:ncol(A)) {
        temp2[[j]] <- internal_geodesic(adjlist, init = rownames(A)[i], fin = rownames(A)[j])
      }
      temp1[[i]] <- temp2[[j]]
      names(temp1)[i] <- rownames(A)[i]
    }
    return(list(fromTo = temp1))
  }

  if (select == 3) {
    # init -> fin
    temp1 <- list()
    temp2 <- list()
    for (i in 1:ncol(A)) {
      for (j in i:ncol(A)) {
        temp2[[j]] <- internal_geodesic(adjlist, init = rownames(A)[i], fin = rownames(A)[j])
      }
      temp1[[i]] <- temp2[[j]]
      names(temp1)[i] <- rownames(A)[i]
    }

    # fin -> init
    temp3 <- list()
    temp4 <- list()
    for (i in 1:ncol(A)) {
      for (j in i:ncol(A)) {
        temp4[[j]] <- internal_geodesic(adjlist, init = rownames(A)[j], fin = rownames(A)[i])
      }
      temp3[[i]] <- temp4[[j]]
      names(temp3)[i] <- rownames(A)[i]
    }
    return(list(fromTo = temp1, toFrom = temp3))
  }
}

node_direction <- function(arg, choices, several.ok = FALSE) {
  if (missing(choices)) {
    formal.args <- formals(sys.function(sys.parent()))
    choices <- eval(formal.args[[deparse(substitute(arg))]])
  }

  arg <- tolower(arg)
  choices <- tolower(choices)

  match.arg(arg = arg, choices = choices, several.ok = several.ok)
}
