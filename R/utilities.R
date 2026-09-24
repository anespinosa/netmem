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

  cat(paste("The matrix", as.character(bquote(A)), "might have the following characteristics:\n"))

  if (is.numeric(A)) cat("--> The vectors of the matrix are `numeric`\n")
  if (is.integer(A)) cat("--> The vectors of the matrix are `integer`\n")
  if (is.character(A)) cat("--> The vectors of the matrix are `character`\n")
  if (is.logical(A)) cat("--> The vectors of the matrix are `logical`\n")

  if (is.null(rownames(A))) cat("--> No names assigned to the rows of the matrix\n")
  if (is.null(colnames(A))) cat("--> No names assigned to the columns of the matrix\n")

  if (any(abs(A) > 1, na.rm = TRUE)) cat("--> Valued matrix\n")
  if (any(A < 0, na.rm = TRUE)) cat("--> The matrix has negative elements (network is signed)\n")
  if (any(is.na(A))) cat("--> The matrix has NA elements\n")

  if (ncol(A) == nrow(A)) {
    if (!all(A[lower.tri(A)] == t(A)[lower.tri(A)], na.rm = TRUE)) {
      cat("--> Matrix is asymmetric (network is directed)\n")
      if (any(diag(A) != 0)) cat("--> The main diagonal is nonzero (the network has loops)\n")
      edges <- sum(A, na.rm = TRUE)
    } else {
      cat("--> Matrix is symmetric (network is undirected)\n")
      if (any(diag(A) != 0)) {
        cat("--> The main diagonal is nonzero (the network has loops)\n")
        I <- diag(1, ncol(A))
        if (all(A == I)) cat("--> An identity matrix\n")
      }
      edges <- sum(A, na.rm = TRUE) / 2
    }
  }

  if (ncol(A) == nrow(A)) {
    cat(paste("--> The matrix is square,", ncol(A), "by", nrow(A), "\n"))

    if (!all(A[lower.tri(A)] == t(A)[lower.tri(A)], na.rm = TRUE)) {
      return(cbind(nodes = nodes, arcs = edges))
    } else {
      return(cbind(nodes = nodes, edges = edges))
    }
  } else {
    cat(paste("--> The matrix is rectangular,", ncol(A), "by", nrow(A), "\n"))
    mode_level1 <- ncol(A)
    mode_level2 <- nrow(A)
    return(cbind(
      nodes_rows = mode_level1,
      nodes_columns = mode_level2,
      incidence_lines = sum(A, na.rm = TRUE)
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
  M <- as.matrix(A)
  if (any(is.na(M) == TRUE)) {
    M <- ifelse(is.na(M), 0, M)
  }
  if (is.null(colnames(M))) {
    colnames(M) <- 1:ncol(M)
  }
  if (is.null(rownames(M))) {
    rownames(M) <- 1:nrow(M)
  }
  # An undirected tie is present when either of the two cells is, and it is
  # listed once, from the upper triangle
  undirected <- !digraph && nrow(M) == ncol(M)
  if (undirected) {
    M <- ifelse(abs(M) >= abs(t(M)), M, t(M))
  }

  edge <- NULL
  for (i in 1:nrow(M)) {
    for (j in 1:ncol(M)) {
      if (M[i, j] == 0) next
      if (undirected && j < i) next
      if (!loops && rownames(M)[i] == colnames(M)[j]) next
      if (valued) {
        edge <- rbind(edge, c(rownames(M)[i], colnames(M)[j], M[i, j]))
      } else {
        edge <- rbind(edge, c(rownames(M)[i], colnames(M)[j]))
      }
    }
  }
  if (is.null(edge)) {
    return(matrix(character(0), ncol = if (valued) 3 else 2))
  }
  colnames(edge) <- NULL
  return(edge)
}

#' Transform an edgelist to a matrix
#'
#' @param E   An edge list
#' @param digraph   Whether the matrix is directed or not
#' @param label  A vector with the names of the nodes, which gives their order in the matrix and adds the nodes without ties
#' @param label2   A vector with the names of the nodes of the second mode, with the same role as \code{label}, when \code{bipartite = TRUE}
#' @param bipartite  Whether the matrix is bipartite
#' @param valued  Whether the third column of the edgelist has the value of the tie
#' @param loops  Whether to keep the ties of a node with itself
#' @param rule  For \code{digraph = FALSE}, whether an undirected tie is kept when it is listed in either order (\code{weak}, default) or only when it is listed in both orders (\code{strong}), as in \code{sna::symmetrize}
#'
#' @details
#' With \code{digraph = FALSE} each undirected tie is placed in both cells of the matrix, so the number of
#' ties is the number of cells of one triangle (or half the sum of a binary matrix). A tie listed in both
#' orders is a single tie.
#'
#' The rows and the columns follow the order of \code{label}. The nodes that are not in \code{label}, or all the
#' nodes when it is not given, are added in alphabetical order.
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
#' edgelist_to_matrix(E, label = c("i"), digraph = FALSE)
#'
#' # With a third column, the ties keep their value
#' V <- rbind(
#'   c("a", "b", 3),
#'   c("b", "c", 1),
#'   c("c", "a", 7)
#' )
#' edgelist_to_matrix(V, valued = TRUE)
#' @export

edgelist_to_matrix <- function(E, digraph = TRUE, label = NULL,
                               label2 = NULL, bipartite = FALSE,
                               valued = FALSE, loops = FALSE,
                               rule = c("weak", "strong")) {
  rule <- match.arg(rule)
  E <- as.matrix(E)
  if (nrow(E) == 0 && is.null(label)) stop("The edgelist has no ties; give the names of the nodes in `label`")
  if (valued & ncol(E) < 3) stop("The edgelist should have a third column with the value of the ties")
  if (valued) {
    values <- as.numeric(E[, 3])
  } else {
    values <- rep(1, nrow(E))
  }
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

    for (i in seq_len(nrow(E))) {
      empty[match(E[i, 1], rownames(empty)), match(E[i, 2], colnames(empty))] <- values[i]
    }
  } else {
    if (!is.null(label)) {
      nodes <- unique(c(E[, 1], E[, 2], label))
    } else {
      nodes <- unique(c(E[, 1], E[, 2]))
    }
    empty <- matrix(0,
      nrow = length(nodes), ncol = length(nodes),
      dimnames = list(nodes, nodes)
    )
    for (i in seq_len(nrow(E))) {
      # The names are matched one by one, so that the direction of the tie is
      # the one of the edgelist
      empty[match(E[i, 1], rownames(empty)), match(E[i, 2], colnames(empty))] <- values[i]
    }
    if (!loops) {
      diag(empty) <- 0
    }
  }

  # The nodes follow the order of the labels, and the nodes that are not in the
  # labels are added after them in alphabetical order
  if (bipartite) {
    rows <- c(intersect(label, rownames(empty)), sort(setdiff(rownames(empty), label)))
    cols <- c(intersect(label2, colnames(empty)), sort(setdiff(colnames(empty), label2)))
  } else {
    rows <- c(intersect(label, rownames(empty)), sort(setdiff(rownames(empty), label)))
    cols <- rows
  }
  A <- empty[rows, cols, drop = FALSE]

  # An undirected tie is placed in both cells. With the weak rule it is kept
  # when it is listed in either order (the largest value if in both); with the
  # strong rule only when it is listed in both orders (the smallest value)
  if (!digraph && !bipartite) {
    if (rule == "weak") {
      A <- ifelse(abs(A) >= abs(t(A)), A, t(A))
    } else {
      A <- ifelse(A != 0 & t(A) != 0, ifelse(abs(A) <= abs(t(A)), A, t(A)), 0)
    }
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
  A <- as.matrix(A)
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  if (is.null(rownames(A))) {
    rownames(A) <- as.character(1:nrow(A))
  }
  if (is.null(colnames(A))) {
    colnames(A) <- as.character(1:ncol(A))
  }
  # Every tie with a value other than zero, including weak and negative ties
  adj_list <- list()
  for (i in 1:nrow(A)) {
    adj_list[[i]] <- colnames(A)[A[i, ] != 0]
    names(adj_list)[i] <- rownames(A)[i]
  }
  return(adj_list)
}

#' Transform an adjacency list into a matrix
#'
#' @param A   An adjacent list
#' @param type   Transform the adjacent list into an \code{adjacency} matrix, an \code{incidence} matrix or a \code{weighted} matrix
#' @param loops   Whether to include loops into the matrix
#'
#' @return This function transforms an adjacency list into a matrix
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' adj_groups <- rbind(
#'   c("a", "b", "c"), c("a", "c", NA),
#'   c("b", "c", NA), c("c", NA, NA),
#'   c("c", "a", NA)
#' )
#' M <- adj_to_matrix(adj_groups, type = "adjacency", loops = TRUE)
#' M
#' @export

adj_to_matrix <- function(A, type = c("adjacency", "incidence", "weighted"),
                          loops = FALSE) {
  A <- as.matrix(A)

  type <- switch(type_matrix(type),
    "adjacency" = 1,
    "incidence" = 2,
    "weighted" = 3
  )

  # The adjacency matrix is the weighted matrix made binary: ego is tied to
  # every node named in any of its lines
  if (type == 1) {
    A <- adj_to_matrix(A, type = "weighted", loops = loops)
    A <- 1 * (A > 0)
  }

  if (type == 2) {
    Ab <- A[, -1]
    B <- unique(c(Ab))
    B <- sort(B) # sort is removing NA cases
    EMPTY <- matrix(NA, nrow = NROW(A), ncol = length(B), byrow = TRUE)
    rownames(EMPTY) <- A[, 1]
    colnames(EMPTY) <- B

    # SAME A
    for (i in 1:NROW(A)) {
      EMPTY[i, ] <- names(EMPTY[i, ]) %in% A[i, ]
    }
    A <- abs(EMPTY)

    if (!loops) {
      for (i in 1:NROW(A)) {
        for (j in 1:NCOL(A)) {
          A[i, j] <- ifelse(rownames(A)[i] == colnames(A)[j], 0, A[i, j])
        }
      }
    }
  }

  if (type == 3) {
    B <- unique(c(A))
    B <- sort(B)
    EMPTY <- matrix(NA, nrow = length(B), ncol = length(B), byrow = TRUE)
    rownames(EMPTY) <- B
    colnames(EMPTY) <- B

    for (i in 1:NROW(EMPTY)) {
      weight <- A[A[, 1] %in% B[i]]
      weight <- sort(weight)
      t <- table(weight)
      temp <- as.data.frame(t)
      if (any(table(c(weight)) > 1)) {
        EMPTY[i, ][which(names(EMPTY[i, ]) %in% temp$weight)] <- temp$Freq
      } else {
        EMPTY[i, ] <- names(EMPTY[i, ]) %in% weight
      }
      EMPTY <- ifelse(is.na(EMPTY), 0, EMPTY)
    }
    A <- EMPTY
    if (!loops) {
      diag(A) <- 0
    }
  }
  return(A)
}
type_matrix <- function(arg, choices, several.ok = FALSE) {
  if (missing(choices)) {
    formal.args <- formals(sys.function(sys.parent()))
    choices <- eval(formal.args[[deparse(substitute(arg))]])
  }

  arg <- tolower(arg)
  choices <- tolower(choices)

  match.arg(arg = arg, choices = choices, several.ok = several.ok)
}


#' Unipartite projections
#'
#' Two-mode networks can be represented (or 'projected') as one-mode networks.
#'
#' @param A  A first matrix object
#' @param B  A second matrix object
#' @param digraph  Whether the matrix is directed or not
#'
#' @return This function return a list of matrices of the two projections of the original matrix.
#'
#' @references
#'
#' Davis, Allison; Gardner, Burleigh B. and Mary. R. Gardner (1941). Deep South: A Social Anthropological Study of Caste and Class. The University of Chicago Press, Chicago.
#'
#' Breiger, Ronald L. (1976). The Duality of Persons and Groups, 53(2), 181-190 \doi{10.2307/2576011}
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
#'
#' A <- matrix(c(
#'   0, 0, 0, 0, 1,
#'   1, 0, 0, 0, 0,
#'   1, 1, 0, 0, 0,
#'   0, 1, 1, 1, 1,
#'   0, 0, 1, 0, 0,
#'   0, 0, 1, 1, 0
#' ), byrow = TRUE, ncol = 5)
#'
#' B <- matrix(c(
#'   0, 0, 0, 0, 1,
#'   1, 0, 0, 0, 0,
#'   1, 0, 0, 0, 0,
#'   0, 1, 0, 0, 0,
#'   0, 0, 1, 0, 0,
#'   0, 0, 1, 0, 0
#' ), byrow = TRUE, ncol = 5)
#' matrix_projection(A, B, digraph = TRUE)
#' @export
matrix_projection <- function(A, B = NULL, digraph = FALSE) {
  A <- as.matrix(A)
  if (!digraph) {
    projection1 <- t(A) %*% A
    projection2 <- A %*% t(A)
  } else {
    if (is.null(B)) stop("A `B` matrix has to be provided")
    B <- as.matrix(B)
    if (!all(rowSums(B) == 1)) stop("Specify only one incident tie between nodes of different modes")
    projection1 <- t(B) %*% A
    projection2 <- B %*% t(A)
  }
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
#' Morris, S.A. (2005). Unified Mathematical Treatment of Complex Cascaded Bipartite Networks: The Case of Collections of Journal Papers. Unpublished PhD Thesis, Oklahoma State University.
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
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  # Sum over the columns of the smallest (or largest) value of each pair of rows
  sim.jac <- matrix(0, nrow = nrow(A), ncol = nrow(A))
  rownames(sim.jac) <- rownames(A)
  colnames(sim.jac) <- rownames(A)
  for (i in seq_len(nrow(A))) {
    for (j in seq_len(nrow(A))) {
      if (min) {
        sim.jac[i, j] <- sum(pmin(A[i, ], A[j, ]))
      } else {
        sim.jac[i, j] <- sum(pmax(A[i, ], A[j, ]))
      }
    }
  }
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
#' @return This function returns the submatrix of the alters of ego, with ego in the last row and column when \code{addEgo = TRUE}.
#' An isolate gives an empty matrix (or a 1 x 1 matrix with ego).
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
#' ego_net(A, ego = "g")
#' @export

ego_net <- function(A, ego = NULL, bipartite = FALSE, addEgo = FALSE,
                    select = c("all", "in", "out")) {
  if (is.null(ego)) stop("Provide the name of ego")
  if (is.numeric(ego)) stop("Label of the name of ego should be in character format")
  if (is.null(rownames(A))) stop("No label assigned to the rows of the matrix")
  if (is.null(colnames(A))) stop("No label assigned to the columns of the matrix")
  if (!(ego %in% sort(unique(c(rownames(A), colnames(A)))))) stop("Ego name does not match with the names of the enlisted nodes")

  A <- as.matrix(A)
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
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

  # A loop does not make ego its own alter
  name <- setdiff(name, ego)
  if (length(name) == 0) {
    message(paste("actor", ego, "has no neighbour"))
  }
  # The submatrix of the alters, with ego in the last row and column
  if (addEgo) {
    keep <- c(name, ego)
  } else {
    keep <- name
  }
  return(A[keep, keep, drop = FALSE])
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

#' Expand Matrix
#'
#' @param A  A square matrix
#' @param label  Duplicated labels to expand the matrix
#' @param loops  Whether the loops are retained or not
#' @param normalize  Whether to normalize the matrix considering the fractional counting per group
#'
#' @return Return an expanded matrix
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1,
#'   0, 0, 1,
#'   1, 0, 0
#' ), byrow = TRUE, ncol = 3, nrow = 3)
#' rownames(A) <- letters[1:NROW(A)]
#' colnames(A) <- rownames(A)
#' label <- sort(rep(rownames(A), 2))
#' expand_matrix(A, label, loops = FALSE, normalize = TRUE)
#' @export

expand_matrix <- function(A, label = NULL, loops = FALSE, normalize = FALSE) {
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

  # Expand matrix
  for (i in 1:NROW(x)) {
    for (j in 1:NCOL(x)) {
      if (rownames(x)[i] == rownames(x)[j]) {
        x[j, ] <- x[i, ]
        x[, j] <- x[, i]
      } else {
        next
      }
    }
  }

  if (loops) {
    x[abs(outer(rownames(x), rownames(x), "==")) == 1] <- 1
  }

  if (normalize) {
    v <- rep(as.numeric(table(label)), as.numeric(table(label)))
    D <- diag(1 / v)
    x <- x %*% D
  }

  return(x)
}

#' Meta matrix for multilevel networks
#'
#' @param A1  The square matrix of the lowest level
#' @param B1  The incidence matrix of the ties between the nodes of first level and the nodes of the second level
#' @param A2  The square matrix of the second level
#' @param B2  The incidence matrix of the ties between the nodes of the second level and the nodes of the third level
#' @param A3  The square matrix of the third level
#' @param B3  The incidence matrix of the ties between the nodes of the third level and the nodes of the first level
#'
#' @return Return a meta matrix for multilevel networks
#'
#' @details
#' The meta matrix places the network of each level on the diagonal and the ties between two levels in both
#' triangles, as an incidence matrix and its transpose:
#' \deqn{\begin{pmatrix} A_1 & B_1 & B_3^T \\ B_1^T & A_2 & B_2 \\ B_3 & B_2^T & A_3 \end{pmatrix}}
#' The matrices that are not given are zero. The names of the nodes are kept when every level has them.
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
  B1 <- as.matrix(B1)
  if (nrow(A1) != ncol(A1)) stop("Matrix should be square")
  if (nrow(A1) != nrow(B1)) stop("Non-conformable arrays")
  n1 <- nrow(A1)
  n2 <- ncol(B1)
  # A level without ties among its nodes is a matrix of zeros
  if (is.null(A2)) {
    A2 <- matrix(0, n2, n2)
  }
  A2 <- as.matrix(A2)
  if (nrow(A2) != ncol(A2)) stop("Matrix should be square")
  if (nrow(A2) != n2) stop("Non-conformable arrays")

  # The ties between two levels are placed in both triangles, as an incidence
  # matrix and its transpose (Krackhardt and Carley, 1998)
  if (is.null(B2) && is.null(A3) && is.null(B3)) {
    meta_matrix <- rbind(cbind(A1, B1), cbind(t(B1), A2))
    level_names <- list(rownames(A1), colnames(B1))
  } else {
    if (!is.null(B2)) {
      n3 <- ncol(as.matrix(B2))
    } else if (!is.null(A3)) {
      n3 <- nrow(as.matrix(A3))
    } else {
      n3 <- nrow(as.matrix(B3))
    }
    if (is.null(B2)) B2 <- matrix(0, n2, n3)
    if (is.null(A3)) A3 <- matrix(0, n3, n3)
    if (is.null(B3)) B3 <- matrix(0, n3, n1)
    B2 <- as.matrix(B2)
    A3 <- as.matrix(A3)
    B3 <- as.matrix(B3)
    if (nrow(B2) != n2 || ncol(B2) != n3) stop("Non-conformable arrays")
    if (nrow(A3) != n3 || ncol(A3) != n3) stop("Non-conformable arrays")
    if (nrow(B3) != n3 || ncol(B3) != n1) stop("Non-conformable arrays")
    meta_matrix <- rbind(
      cbind(A1, B1, t(B3)),
      cbind(t(B1), A2, B2),
      cbind(B3, t(B2), A3)
    )
    level_names <- list(rownames(A1), colnames(B1), colnames(B2))
  }

  # The names of the nodes, when every level has them
  if (!any(sapply(level_names, is.null))) {
    labels <- unlist(level_names)
    dimnames(meta_matrix) <- list(labels, labels)
  } else {
    dimnames(meta_matrix) <- NULL
  }
  return(meta_matrix)
}

#' Structural Missing Data
#'
#' Assign NA to missing data in matrices.
#'
#' @param A An incident or symmetric matrix object.
#' @param label A string vector with the names of the theoretical complete matrix (used for one-mode networks only).
#' @param row_labels A string vector with the names of the rows (used for two-mode networks).
#' @param col_labels A string vector with the names of the columns (used for two-mode networks).
#' @param two_mode Boolean indicating whether the matrix is two-mode. Default is FALSE.
#'
#' @return This function returns a matrix with NA assigned to missing data.
#'
#' @examples
#' # Example for one-mode network
#' A <- matrix(c(
#'   0, 1, 1,
#'   1, 0, 1,
#'   0, 0, 0
#' ), byrow = TRUE, ncol = 3)
#' colnames(A) <- c("A", "C", "D")
#' rownames(A) <- c("A", "C", "D")
#' label <- c("A", "B", "C", "D", "E")
#' structural_na(A, label = label)
#'
#' # Example for two-mode network
#' B <- matrix(c(
#'   0, 1, 0,
#'   1, 0, 1,
#'   0, 1, 0,
#'   1, 0, 1
#' ), byrow = TRUE, ncol = 3)
#' rownames(B) <- c("X1", "X2", "X3", "X4")
#' colnames(B) <- c("Y1", "Y2", "Y3")
#' rlabels <- c("X1", "X2", "X3", "X4", "X5")
#' clabels <- c("Y1", "Y2", "Y3", "Y4")
#' structural_na(B, row_labels = rlabels, col_labels = clabels, two_mode = TRUE)
#' @export

structural_na <- function(A, label = NULL, row_labels = NULL, col_labels = NULL, two_mode = FALSE) {
  if (two_mode) {
    if (dim(A)[1] == dim(A)[2]) warning("Incident matrix should be rectangular")

    # Create a new matrix with the desired dimensions and fill with NA
    x <- matrix(NA, nrow = length(row_labels), ncol = length(col_labels))
    rownames(x) <- row_labels
    colnames(x) <- col_labels

    # Find the matching rows and columns
    rowmatch <- match(rownames(A), row_labels)
    colmatch <- match(colnames(A), col_labels)

    # Ensure no NAs in match results
    valid_rows <- !is.na(rowmatch)
    valid_cols <- !is.na(colmatch)

    # Fill in the values from the original matrix
    x[rowmatch[valid_rows], colmatch[valid_cols]] <- A[valid_rows, valid_cols]

    return(x)
  } else {
    if (dim(A)[1] != dim(A)[2]) stop("Matrix should be symmetric")
    if (is.null(colnames(A))) stop("Assign column names to the matrix.")
    if (is.null(rownames(A))) stop("Assign row names to the matrix.")

    if (is.null(label)) stop("Label must be provided for symmetric matrices.")

    # The labels add the unobserved nodes; a node of A that is not in the labels
    # would be dropped
    if (!all(rownames(A) %in% label)) {
      warning("Some nodes of the matrix are not in the labels and are dropped.")
    }

    # Create a new matrix with the desired dimensions and fill with NA
    x <- matrix(NA, nrow = length(label), ncol = length(label))
    rownames(x) <- label
    colnames(x) <- label

    # Find the matching rows and columns
    rowmatch <- match(rownames(A), label)
    colmatch <- match(colnames(A), label)

    # Ensure no NAs in match results
    valid_rows <- !is.na(rowmatch)
    valid_cols <- !is.na(colmatch)

    # Fill in the values from the original matrix
    x[rowmatch[valid_rows], colmatch[valid_cols]] <- A[valid_rows, valid_cols]

    return(x)
  }
}

#' Zone-2 sampling from second-mode
#'
#' Second-zone multilevel sampling considering a second-mode focal actor
#'
#' @param A   A symmetric matrix object.
#' @param X   X an incidence matrix object.
#' @param ego   Whether to add or not ego into the subgraph.
#' @param core  Whether to add actors at distance one from ego
#'
#' @return This function return a list of second-zone subgraphs using as a focal actor the second-mode of the multilevel network.
#' Each subgraph is the binary adjacency matrix of the meta-matrix of \code{A} and \code{X} restricted to the
#' nodes of the zone, without loops. When \code{core = TRUE}, each matrix has an attribute \code{core}, a named
#' vector that is one for the actors at distance one from the focal node and zero otherwise.
#'
#' @references
#'
#' Espinosa-Rada, A. (2021). A Network Approach for the Sociological Study of Science: Modelling Dynamic Multilevel Networks. [PhD](https://research.manchester.ac.uk/en/studentTheses/a-network-approach-for-the-sociological-study-of-science-and-know). The University of Manchester.
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
  if (is.null(rownames(X))) stop("Assign `rownames` to the incidence matrix")
  if (is.null(colnames(X))) stop("Assign `colnames` to the incidence matrix")
  if (dim(A)[1] != dim(X)[1]) {
    X <- t(X)
  }
  if (!all(colnames(A) == rownames(X))) warning("The names for the combination of the matrices should be the same")
  zero <- matrix(0, ncol = ncol(X), nrow = ncol(X))
  M1 <- cbind(A, X)
  M2 <- cbind(t(X), zero)
  gM <- rbind(M1, M2)
  A1 <- gM
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
    # The nodes keep the order of the meta-matrix, and the valued ties and the
    # loops are reduced to a binary matrix without loops
    keep <- rownames(gM) %in% nei
    S <- ifelse(gM[keep, keep, drop = FALSE] != 0, 1, 0)
    diag(S) <- 0
    if (core == TRUE) {
      attr(S, "core") <- ifelse(rownames(S) %in% names(zone1[[i]]), 1, 0)
      names(attr(S, "core")) <- rownames(S)
    }
    subgraphs[[i]] <- S
  }

  return(subgraphs)
}

#' Hypergraphs
#'
#' Hypergraph consist of a set of objects and a collection of subsets of objects, in which each object belongs to at least one subset, and no subset is empty (Berge, 1989)
#'
#' @param A   An incidence matrix.
#' @param dual   Whether to return the dual hypergraph (which rever the role of the pointes and the edges)
#' @param both   Whether to return the hypergraph and the dual hypergraph
#'
#' @return This function returns an adjacent list of the subsets of entities in the hypergraph.
#'
#' @references
#'
#' Berge, C. (1973). Graphs and hypergraphs.Amsterdam: North-Holland.
#'
#' Berge, C. (1989). Hypergraphs: Combinatorics of finite sets. Amsterdam: North-Holland.
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   1, 0, 1,
#'   0, 1, 0,
#'   0, 1, 1,
#'   0, 0, 1,
#'   1, 1, 1,
#'   1, 1, 0
#' ), byrow = TRUE, ncol = 3)
#' colnames(A) <- letters[1:ncol(A)]
#' rownames(A) <- letters[(ncol(A) + 1):(nrow(A) + ncol(A))]
#' hypergraph(A, both = TRUE)
#' @export

hypergraph <- function(A, dual = TRUE, both = TRUE) {
  A <- as.matrix(A)
  if (ncol(A) == nrow(A)) warning("For hypergraphs, an incidence matrix should be used")

  if (!both) {
    if (!dual) {
      A <- t(A)
    }
    A <- matrix_adjlist(A)
    return(A)
  } else {
    A1 <- A
    A1 <- matrix_adjlist(A1)

    A2 <- t(A)
    A2 <- matrix_adjlist(A2)
    return(list(hypergraph = A1, dual_hypergraph = A2))
  }
}

#' Simplicial complexes
#'
#' Incidence matrix of the nodes of a network and the simplices of its clique complex or of its
#' neighbourhood complex.
#'
#' @details
#' A simplex is a set of nodes, and a simplicial complex a collection of simplices that contains every
#' face of its simplices (Atkin, 1974). The complex is represented by its maximal simplices, as the faces
#' are implied by the simplices that contain them.
#'
#' With \code{complex = "clique"} (default), the simplices are the maximal cliques of the underlying
#' undirected network, so that a clique of four nodes is a single simplex of dimension 3. The isolated
#' nodes are cliques of a single node, and they are included as simplices of dimension 0 when
#' \code{zero_simplex = TRUE}.
#'
#' With \code{complex = "neighbourhood"}, each node is the simplex of its neighbours, the rows of
#' \code{A} (the out-neighbours of a directed network), and with \code{closed = TRUE} the node is also a
#' vertex of its own simplex (Raj et al., 2024). The nodes without neighbours do not form a simplex
#' unless the neighbourhoods are closed.
#'
#' The rows of the result are the nodes and the columns the simplices, named after their nodes
#' (\code{a-b-c}) for the clique complex, and after the node whose neighbourhood they are (\code{N(a)}
#' or \code{N[a]}) for the neighbourhood complex. \code{q_analysis(t(S), simplicial_complex = TRUE)} is
#' the Q-analysis of the simplices, and \code{q_analysis(S, simplicial_complex = TRUE)} that of the
#' conjugate complex, in which the nodes are connected through the simplices they share.
#'
#' @param A   A square matrix of a network, with names.
#' @param zero_simplex   Whether to include the isolated nodes as simplices of dimension 0, for \code{complex = "clique"}.
#' @param projection  Whether to return the links between the simplices through their shared nodes, and between the nodes through their shared simplices.
#' @param complex   The complex: the maximal cliques (\code{clique}, default) or the neighbourhoods (\code{neighbourhood}).
#' @param closed   Whether the neighbourhoods include the node itself, for \code{complex = "neighbourhood"}.
#' @param valued   Whether the projections count the shared nodes or simplices instead of indicating whether there are any.
#'
#' @return This function returns the incidence matrix of the nodes (rows) and the simplices (columns). With
#' \code{projection = TRUE}, a list with the incidence matrix (\code{simplex}), the projection of the simplices
#' (\code{projection1}) and the projection of the nodes (\code{projection2}).
#'
#' @references
#'
#' Atkin, R. H. (1974). Mathematical structure in human affairs. New York: Crane, Rusak.
#'
#' Freeman, L. C. (1980). Q-analysis and the structure of friendship networks. International Journal of Man-Machine Studies, 12(4), 367–378. \doi{10.1016/S0020-7373(80)80021-6}
#'
#' Raj, U., Banerjee, A., Ray, S. and Bhattacharya, S. (2024). Structure of higher-order interactions in social-ecological networks through Q-analysis of their neighbourhood and clique complex. PLOS ONE, 19(8), e0306409. \doi{10.1371/journal.pone.0306409}
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
#'
#' simplicial_complexes(A)
#' simplicial_complexes(A, zero_simplex = FALSE)
#' simplicial_complexes(A, complex = "neighbourhood", closed = TRUE)
#' simplicial_complexes(A, projection = TRUE, valued = TRUE)$projection2
#' @export

simplicial_complexes <- function(A, zero_simplex = TRUE, projection = FALSE,
                                 complex = c("clique", "neighbourhood"), closed = FALSE,
                                 valued = FALSE) {
  A <- as.matrix(A)
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  complex <- match.arg(complex)
  if (is.null(rownames(A))) stop("No label assigned to the rows of the matrix")
  if (is.null(colnames(A))) stop("No label assigned to the columns of the matrix")
  if (ncol(A) != nrow(A)) stop("Matrix should be square")
  A[A > 0] <- 1

  if (complex == "clique") {
    cliques <- clique_max(A, min = 1)
    # The isolated nodes are the only maximal cliques of a single node
    if (!zero_simplex) {
      cliques <- cliques[lengths(cliques) > 1]
    }
    S <- matrix(0, nrow(A), length(cliques), dimnames = list(rownames(A), NULL))
    for (i in seq_along(cliques)) {
      S[cliques[[i]], i] <- 1
    }
    colnames(S) <- sapply(cliques, paste, collapse = "-")
  } else {
    # Column v holds the neighbours of v
    S <- t(A)
    diag(S) <- 1 * closed
    if (closed) {
      colnames(S) <- paste0("N[", rownames(A), "]")
    } else {
      colnames(S) <- paste0("N(", rownames(A), ")")
    }
    S <- S[, colSums(S) > 0, drop = FALSE]
  }

  if (projection) {
    # Simplices linked by their shared nodes, and nodes by their shared simplices
    proj1 <- t(S) %*% S
    proj2 <- S %*% t(S)
    if (!valued) {
      proj1 <- ifelse(proj1 >= 1, 1, 0)
      proj2 <- ifelse(proj2 >= 1, 1, 0)
    }
    diag(proj1) <- 0
    diag(proj2) <- 0
    return(list(simplex = S, projection1 = proj1, projection2 = proj2))
  } else {
    return(S)
  }
}

#' Extract components
#'
#' This function extract the matrix of different components
#'
#' @param A   A matrix
#' @param maximum   Whether to extract the maximum component
#' @param position   The position of the size of the component, from the largest (1). Used when \code{maximum = FALSE}
#'
#' @return The matrix of the component, or a list with the matrices of the components when several have the same size
#'
#' @references
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' A <- FIFAex$Matrix
#' rownames(A) <- FIFAex$label
#' colnames(A) <- rownames(A)
#' extract_component(A, maximum = TRUE)
#' extract_component(A, maximum = FALSE, position = 2)
#' @export

extract_component <- function(A, maximum = TRUE, position = NULL) {
  A <- as.matrix(A)
  temp <- components_id(A)
  if (maximum) {
    position <- 1
  }
  if (is.null(position)) stop("Give the position of the component, or maximum = TRUE")
  if (!is.numeric(position)) stop("The position should be a number")
  if (position < 1) stop("Please specify a number greater than 0")

  # The distinct sizes of the components, from the largest; position i is the
  # i-th largest size, and all the components of that size are returned
  sizes <- sort(unique(as.numeric(temp$size)), decreasing = TRUE)
  if (position > length(sizes)) stop(paste("The components have only", length(sizes), "different sizes"))
  ids <- as.numeric(names(temp$size))[as.numeric(temp$size) == sizes[position]]

  components <- list()
  for (k in seq_along(ids)) {
    members <- which(temp$components == ids[k])
    components[[k]] <- A[members, members, drop = FALSE]
  }
  if (length(components) == 1) {
    return(components[[1]])
  }
  names(components) <- paste("component", ids)
  return(components)
}

#' Power matrix
#'
#' Power of a matrix computed by successive matrix multiplication.
#'
#' @param A   A matrix
#' @param n   Positive integer
#'
#' @return This function return the power of a matrix by repeating matrix multiplication.
#'
#' @references
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   1, 0, 0, 0,
#'   1, 1, 0, 0,
#'   1, 0, 1, 0,
#'   0, 1, 1, 1
#' ), byrow = TRUE, ncol = 4, nrow = 4)
#' power_function(A, 1000)
#'
#' @export

power_function <- function(A, n) {
  if (n < 1) stop("n should be a positive integer")
  # A loop instead of recursion, which reached the limit of nested expressions for large n
  P <- A
  for (i in seq_len(n - 1)) {
    P <- P %*% A
  }
  return(P)
}

#' Permutation matrix
#'
#' This function create permutation matrices.
#'
#' @param n   The size of the square matrix
#' @param m   Number of permutations
#' @param unique   Whether to return unique cases
#'
#' @return This function returns a list of permutation matrices
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' W <- matrix(c(
#'   0, 1, 0, 0, 0,
#'   0, 0, 1, 0, 0,
#'   1, 0, 0, 0, 0,
#'   0, 0, 0, 0, 1,
#'   0, 0, 0, 1, 0
#' ), byrow = TRUE, ncol = 5)
#' rownames(W) <- c("P", "Q", "R", "S", "T")
#' colnames(W) <- rownames(W)
#' perm_matrix(5, m = 1000, unique = TRUE)
#'
#' @export

perm_matrix <- function(n, m = 1, unique = FALSE) {
  matrices <- list()
  for (m in seq_len(m)) {
    A <- matrix(0, ncol = n, nrow = n, byrow = TRUE)
    vector <- sample(NROW(A), NROW(A))
    for (i in seq_len(length(vector))) {
      A[vector[i], i] <- 1
    }
    matrices[[m]] <- A
  }

  if (unique) {
    matrices <- unique(matrices)
    # factorial(n) == length(matrices) # should reach this limit!
  }

  return(matrices)
}

#' Permute labels of a matrix
#'
#' This function permutes the labels of a matrix.
#'
#' @param A   A matrix
#' @param m   Number of permutations
#' @param unique   Whether to return unique cases
#'
#' @return This function returns the permutation of labels.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' W <- matrix(c(
#'   0, 1, 0, 0, 0,
#'   0, 0, 1, 0, 0,
#'   1, 0, 0, 0, 0,
#'   0, 0, 0, 0, 1,
#'   0, 0, 0, 1, 0
#' ), byrow = TRUE, ncol = 5)
#' rownames(W) <- c("P", "Q", "R", "S", "T")
#' colnames(W) <- rownames(W)
#' perm_label(W, m = 1000, unique = TRUE)
#'
#' @export

perm_label <- function(A, m = 1, unique = FALSE) {
  if (is.null(rownames(A))) {
    rownames(A) <- 1:NROW(A)
    colnames(A) <- rownames(A)
  }
  label <- rownames(A)

  perm <- list()
  for (i in seq_len(m)) {
    perm[[i]] <- label[seq_len(NROW(A)) %*% perm_matrix(NCOL(A))[[1]]]
  }

  if (unique) {
    perm <- unique(perm)
  }

  return(do.call(rbind, perm))
}

#' Cumulative sum of matrices
#'
#' @param matrixList   A list of matrices
#'
#' @return This function returns the cumulative sum of matrices
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' A <- matrix(c(
#'   0, 1, 1,
#'   0, 0, 0,
#'   0, 1, 0
#' ), byrow = TRUE, ncol = 3)
#' B <- matrix(c(
#'   0, 0, 1,
#'   0, 0, 0,
#'   0, 0, 0
#' ), byrow = TRUE, ncol = 3)
#' C <- matrix(c(
#'   0, 0, 0,
#'   1, 0, 0,
#'   0, 0, 0
#' ), byrow = TRUE, ncol = 3)
#' matrixList <- list(A, B, C)
#' cumulativeSumMatrices(matrixList)
#'
#' @export

cumulativeSumMatrices <- function(matrixList) {
  cumulativeSumList <- list() # To store cumulative sum matrices

  cumSumMatrix <- NULL # Initialize cumulative sum matrix

  for (i in 1:length(matrixList)) {
    if (is.null(cumSumMatrix)) {
      cumSumMatrix <- matrixList[[i]]
    } else {
      cumSumMatrix <- cumSumMatrix + matrixList[[i]]
    }

    cumulativeSumList[[i]] <- cumSumMatrix
  }

  return(cumulativeSumList)
}

#' Convert an Adjacency Matrix to an Incidence Matrix
#'
#' This function transforms an adjacency matrix into an incidence matrix.
#'
#' @param A A square numeric matrix representing the adjacency matrix
#'   of a graph. The matrix should have non-negative values, where `A[i, j]`
#'   represents the weight of the edge from node `i` to node `j`.
#' @param loops Logical. If `TRUE`, self-loops (edges from a node to itself)
#'   are included in the incidence matrix. If `FALSE`, they are removed. Default is `TRUE`.
#' @param directed Logical. If `TRUE`, the graph is treated as directed, meaning
#'   each edge has a specific source and target. If `FALSE`, the graph is treated
#'   as undirected, and edges are symmetrically represented. Default is `TRUE`.
#' @param weighted Logical. If `TRUE`, edge weights from `A` are included
#'   in the incidence matrix. If `FALSE`, all edges are treated as having weight `1`.
#'   Default is `TRUE`.
#'
#' @return A numeric matrix where rows represent nodes and columns represent edges.
#'   - In a **directed** network, a source node has a negative value (-weight),
#'     and a target node has a positive value (+weight).
#'   - In an **undirected** network, both nodes involved in an edge share the weight
#'     (positive values).
#'   - If `weighted = FALSE`, all edges have a weight of `1`.
#'
#' @examples
#' # Define an adjacency matrix (directed and weighted)
#' A <- matrix(c(
#'   1, 3, 0, 0, 2,
#'   0, 0, 2, 0, 0,
#'   5, 0, 0, 0, 0,
#'   0, 0, 0, 0, 1,
#'   0, 4, 0, 0, 0
#' ), byrow = TRUE, nrow = 5)
#'
#' # Convert to an incidence matrix (directed, weighted)
#' (inc_matrix <- adj_to_incidence(A))
#'
#' # Undirected, weighted graph
#' (inc_matrix_undirected <- adj_to_incidence(A, directed = FALSE))
#'
#' # Directed, unweighted graph
#' (inc_matrix_unweighted <- adj_to_incidence(A, weighted = FALSE))
#'
#' # Ignore loops
#' (inc_matrix_no_loops <- adj_to_incidence(A, loops = FALSE))
#'
#' @export

adj_to_incidence <- function(A, loops = TRUE, directed = TRUE, weighted = TRUE) {
  A <- as.matrix(A)
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  # Get the number of nodes
  n <- nrow(A)
  if (is.null(rownames(A))) {
    rownames(A) <- as.character(1:n)
  }

  # An undirected tie is present when either of the two cells is
  if (!directed) {
    A <- pmax(A, t(A))
  }

  # Identify edges (i -> j) from adjacency matrix
  edges <- which(A != 0, arr.ind = TRUE)

  # Remove self-loops if loops is FALSE
  if (!loops) {
    edges <- edges[edges[, 1] != edges[, 2], , drop = FALSE]
  }

  # If undirected, only keep unique edges (i, j) where i < j
  if (!directed) {
    edges <- edges[edges[, 1] <= edges[, 2], , drop = FALSE]
  }

  # Number of edges
  num_edges <- nrow(edges)

  # Initialize incidence matrix (n x num_edges), with the edges named after
  # their nodes
  separator <- if (directed) "->" else "-"
  incidence_matrix <- matrix(0, n, num_edges, dimnames = list(
    rownames(A),
    paste(rownames(A)[edges[, 1]], rownames(A)[edges[, 2]], sep = separator)
  ))

  # Populate incidence matrix
  for (e in seq_len(num_edges)) {
    i <- edges[e, 1] # Source node
    j <- edges[e, 2] # Target node
    weight <- ifelse(weighted, A[i, j], 1) # Use weights or binary

    # Directed case: i -> j means row i gets -weight, row j gets +weight
    if (directed) {
      incidence_matrix[i, e] <- -weight
      incidence_matrix[j, e] <- weight
    } else {
      # Undirected case: Symmetric treatment of edges
      incidence_matrix[i, e] <- weight
      incidence_matrix[j, e] <- weight
    }
  }

  # Return the incidence matrix
  return(incidence_matrix)
}
