#' Relational composition
#'
#' This function returns the relational composition of the given matrices. The compound relations define the paths and the social process flows of the given matrices (Pattison, 1993). However, those whom they link may or may not be aware of them. The compound relations allow us to identify "the possibly very long and devious chains of effects propagating withing concrete social systems through links of various kinds" (Lorrain & White, 1971: 50).
#'
#' @param l   A list of matrices.
#' @param comp  A number with the length of paths to form the compound relation.
#' @param matrices   Whether to return the resulting matrices of the compound relations.
#' @param equate   Whether to return the semigroup equations.
#'
#' @return This function provides the composition or concatenation of compound relations and the primitives of the matrices.
#'
#' @references
#'
#' Boorman, Scott A. and White, Harrison C. (1976) Social Structure from Multiple Networks. II. Role Structures. American Journal of Sociology. 81(6): 1384-1446.
#'
#' Lorrain, Francois and White, Harrison C. (1971) Structural Equivalence of Individuals in Social Networks. Journal of Mathematical Sociology. 1: 49-80
#'
#' Pattison, Philippa (1993) Algebraic Models for Social Networks. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' A <- matrix(c(
#'   0, 1, 0, 0,
#'   1, 0, 0, 0,
#'   1, 1, 0, 1,
#'   0, 0, 1, 0
#' ), byrow = TRUE, ncol = 4)
#' rownames(A) <- letters[1:NCOL(A)]
#' colnames(A) <- rownames(A)
#'
#' B <- matrix(c(
#'   0, 1, 0, 0,
#'   1, 0, 0, 0,
#'   0, 0, 0, 1,
#'   0, 0, 1, 0
#' ), byrow = TRUE, ncol = 4)
#' rownames(B) <- letters[1:NCOL(B)]
#' colnames(B) <- rownames(B)
#'
#' cmp <- compound_relation(list(A, B), comp = 2, matrices = TRUE, equate = TRUE)
#' cmp$compound_relations
#' cmp$compound_matrices
#' cmp$equated
#'
#' @export

compound_relation <- function(l = list(), comp = 3, matrices = FALSE, equate = FALSE) {
  # Assign names to the matrices
  names(l) <- letters[1:length(l)]
  networks <- names(l)
  elements <- rep(networks, comp)

  # Create a new list with all the elements
  new_l <- list()
  for (i in 1:comp) {
    new_l[[i]] <- unique(t(combn(elements, i)))
  }

  # Change format of the new list. Output: result
  max_cols <- max(sapply(new_l, ncol))
  result <- matrix(NA, nrow = sum(sapply(new_l, nrow)), ncol = max_cols)
  row_index <- 1
  for (i in seq_along(new_l)) {
    num_rows <- nrow(new_l[[i]])
    result[row_index:(row_index + num_rows - 1), 1:ncol(new_l[[i]])] <- new_l[[i]]
    row_index <- row_index + num_rows
  }

  # Now we are extracting the matrices!
  if (matrices) {
    # Create a list of list
    comp_relations2 <- list()
    for (m in 1:comp) {
      data <- as.data.frame(new_l[[m]])
      comp_relations <- list()
      for (j in 1:NROW(data)) {
        comp_list <- list()
        for (h in 1:NCOL(data)) {
          comp_list[[h]] <- l[[c(data[j, ][h])[[1]]]]
        }
        comp_relations[[j]] <- Reduce("%*%", comp_list)

        # Binarize! We are working with boolean...
        comp_relations[[j]] <- ifelse(comp_relations[[j]] > 1, 1, comp_relations[[j]])
      }
      comp_relations2[[m]] <- comp_relations
    }

    # Change the format from list of list, to one list
    comp_relations2 <- do.call(c, comp_relations2)

    # Provide names to the matrices
    temp <- apply(result, 1, function(x) paste(x, collapse = ""))
    temp <- gsub("NA", "", temp)
    names(comp_relations2) <- temp

    if (equate) {
      # Check if there are repeated equations
      repeated <- names(comp_relations2[duplicated(comp_relations2)])
      if (length(repeated) == 0) {
        return(list(
          compound_relations = names(comp_relations2),
          compound_matrices = comp_relations2, equated = "No reduced equation"
        ))
      } else {
        equated_repeated <- list()

        # Reduce the number of equations
        for (i in 1:length(repeated)) {
          temp <- comp_relations2[names(comp_relations2) != repeated[i]]

          for (j in 1:length(temp)) {
            if (all(temp[j][[1]] == comp_relations2[repeated[i]][[1]])) {
              temp2 <- c(names(temp[j]), names(comp_relations2[repeated[i]]))
            }
            equated_repeated[[i]] <- temp2
          }
        }
        matrix <- do.call(rbind, equated_repeated)

        # Transform equated equations into a two-mode representation:
        edgelist <- cbind(names(comp_relations2), names(comp_relations2))
        matrix <- netmem::edgelist_to_matrix(rbind(matrix, edgelist),
          bipartite = TRUE
        )

        # Matrices
        comp_relations2 <- comp_relations2[!duplicated(comp_relations2)]

        return(list(
          compound_relations = names(comp_relations2),
          compound_matrices = comp_relations2, equated = matrix
        ))
      }
    } else {
      return(list(compound_relations = result, compound_matrices = comp_relations2))
    }
  } else {
    return(result)
  }
}

#' Path distances
#'
#' Distances between nodes using breadth-first search (BFS) or Dijkstra's algorithm to find shortest path distances.
#'
#' @name distances
#'
#' @param A   A symmetric matrix object
#' @param select   Whether to consider all sender and receiver ties of ego (\code{all}), only incoming ties (\code{in}), or outgoing ties (\code{out}). By default, \code{all}.
#' @param from  Node in which the path start
#' @param to  Node in which the path end
#' @param path  Path of the nodes
#'
#' @return This function returns the distances o shortest path distance between two nodes for unweighted graph (\code{bfs_ugraph}, \code{count_geodesics} and \code{short_path} respectively) and weighted graphs (\code{wlocal_distances} or \code{wall_distances})
#'
#' @references
#'
#' Dijkstra, E. W. (1959). A note on two problems in connexion with graphs. Numerische Mathematik. 1: 269–271.
#'
#' @author Alejandro Espinosa-Rada

NULL

#' @rdname distances
#' @examples
#' \donttest{
#' A <- matrix(c(
#'   0, 1, 1, 0, 0, 0,
#'   0, 0, 0, 1, 1, 0,
#'   0, 0, 0, 0, 1, 0,
#'   0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 1,
#'   0, 0, 0, 0, 0, 0
#' ), byrow = TRUE, nrow = 6)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- letters[1:ncol(A)]
#'
#' bfs_ugraph(A, from = "a")
#' }
#' @export

bfs_ugraph <- function(A, from = NULL) {
  A <- as.matrix(A)
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  if (any(abs(A > 1))) stop("Not an unweighted matrix")
  if (is.null(from)) {
    m <- list()
    for (j in 1:nrow(A)) {
      first_buffer <- j
      visited <- rep(FALSE, nrow(A))
      visited[j] <- TRUE
      distances <- rep(Inf, nrow(A))
      distances[j] <- 0

      while (length(first_buffer) > 0) {
        node <- first_buffer[1]
        first_buffer <- first_buffer[-1]
        for (i in seq_along(A[node, ])) {
          if (A[node, i] && !visited[i]) {
            visited[i] <- TRUE
            distances[i] <- distances[node] + 1
            first_buffer <- c(first_buffer, i)
          }
        }
      }
      m[[j]] <- distances
    }
    m <- as.matrix(do.call(rbind, m))
    dimnames(m) <- list(rownames(A), rownames(A))
    return(distances = m)
  } else {
    from <- which(rownames(A) %in% from)
    first_buffer <- from
    visited <- rep(FALSE, nrow(A))
    visited[from] <- TRUE
    distances <- rep(Inf, nrow(A))
    distances[from] <- 0
    pointers <- rep(NULL, nrow(A))

    while (length(first_buffer) > 0) {
      node <- first_buffer[1]
      first_buffer <- first_buffer[-1]
      for (i in seq_along(A[node, ])) {
        if (A[node, i] && !visited[i]) {
          visited[i] <- TRUE
          distances[i] <- distances[node] + 1
          pointers[i] <- node
          first_buffer <- c(first_buffer, i)
        }
      }
    }
    return(list(pointers = pointers, distances = distances))
  }
}

#' @rdname distances
#' @examples
#' \donttest{
#' A <- matrix(c(
#'   0, 1, 1, 0, 0, 0,
#'   0, 0, 0, 1, 1, 0,
#'   0, 0, 0, 0, 1, 0,
#'   0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 1,
#'   0, 0, 0, 0, 0, 0
#' ), byrow = TRUE, nrow = 6)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- letters[1:ncol(A)]
#'
#' count_geodesics(A)
#' }
#' @export

count_geodesics <- function(A) {
  A <- as.matrix(A)
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  if (any(abs(A > 1))) stop("Not an unweighted matrix")
  n <- nrow(A)
  distances <- matrix(Inf, n, n)
  counts <- matrix(0, n, n)

  for (j in 1:n) {
    first_buffer <- j
    visited <- rep(FALSE, n)
    visited[j] <- TRUE
    distances[j, j] <- 0
    counts[j, j] <- 1

    while (length(first_buffer) > 0) {
      node <- first_buffer[1]
      first_buffer <- first_buffer[-1]

      neighbors <- which(A[node, ] != 0 & !visited)
      visited[neighbors] <- TRUE
      distances[j, neighbors] <- distances[j, node] + 1
      first_buffer <- c(first_buffer, neighbors)

      # Every neighbour at the next distance receives the geodesics of node,
      # including the neighbours that were already discovered by another node
      next_level <- which(A[node, ] != 0 & distances[j, ] == distances[j, node] + 1)
      counts[j, next_level] <- counts[j, next_level] + counts[j, node]
    }
  }

  return(list(counts = counts, distances = distances))
}

#' @rdname distances
#' @examples
#' \donttest{
#' A <- matrix(c(
#'   0, 1, 1, 0, 0, 0,
#'   0, 0, 0, 1, 1, 0,
#'   0, 0, 0, 0, 1, 0,
#'   0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 1,
#'   0, 0, 0, 0, 0, 0
#' ), byrow = TRUE, nrow = 6)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- letters[1:ncol(A)]
#'
#' short_path(A, from = "a", to = "d")
#' }
#' @export

short_path <- function(A, from = NULL, to = NULL) {
  A <- as.matrix(A)
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  if (is.null(rownames(A))) stop("No label assigned to the rows of the matrix")
  if (!all(c(from, to) %in% rownames(A))) stop("`from` and `to` should be names of the nodes")
  search <- bfs_ugraph(A, from = from)
  pointers <- search$pointers

  from <- which(rownames(A) %in% from)
  to <- which(rownames(A) %in% to)
  if (is.infinite(search$distances[to])) {
    warning("There is no path between the nodes")
    return(NULL)
  }

  path <- c()
  while (to != from) {
    path <- c(path, to)
    to <- pointers[to]
  }
  path <- rev(path)
  return(c(rownames(A)[from], rownames(A)[path]))
}

#' @rdname distances
#' @examples
#' \donttest{
#' A <- matrix(
#'   c(
#'     0, 3, 3, 10, 15, 0, 0, 0,
#'     1, 0, 5, 2, 7, 0, 0, 0,
#'     3, 5, 0, 0, 0, 0, 0, 0,
#'     10, 2, 0, 0, 2, 7, 12, 0,
#'     11, 3, 0, 3, 0, 11, 2, 0,
#'     0, 0, 0, 7, 11, 0, 3, 2,
#'     0, 0, 0, 12, 2, 3, 0, 2,
#'     0, 0, 0, 0, 0, 2, 2, 0
#'   ),
#'   byrow = TRUE, ncol = 8, nrow = 8
#' )
#' rownames(A) <- c("a", "b", "s", "c", "d", "e", "f", "z")
#' colnames(A) <- rownames(A)
#' wlocal_distances(A, from = "a", to = "d")
#' }
#' @export

wlocal_distances <- function(A, select = c("all", "in", "out"),
                             from, to, path = c()) {
  A <- as.matrix(A)
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  # A binary matrix is a valued matrix in which every tie has length one
  if (is.null(rownames(A))) stop("No label assigned to the rows of the matrix")
  if (!all(c(from, to) %in% rownames(A))) stop("`from` and `to` should be names of the nodes")

  test <- dijkstra_path(A, from = from, to = to)
  return(list(path = test))
}

# Dijkstra (1959): the unvisited node with the shortest distance is fixed at
# each step, and its ties update the distances of its neighbours. The weights
# are the lengths of the ties. Returns the names of the nodes in the path, or
# NULL if there is no path.
dijkstra_path <- function(A, from, to) {
  n <- nrow(A)
  from <- which(rownames(A) == from)
  to <- which(rownames(A) == to)

  dist <- rep(Inf, n)
  dist[from] <- 0
  previous <- rep(NA, n)
  visited <- rep(FALSE, n)

  repeat {
    candidates <- which(!visited & is.finite(dist))
    if (length(candidates) == 0) break
    v <- candidates[which.min(dist[candidates])]
    if (v == to) break
    visited[v] <- TRUE
    for (w in which(A[v, ] > 0 & !visited)) {
      if (dist[v] + A[v, w] < dist[w]) {
        dist[w] <- dist[v] + A[v, w]
        previous[w] <- v
      }
    }
  }

  if (is.infinite(dist[to])) {
    return(NULL)
  }
  path <- to
  while (path[1] != from) {
    path <- c(previous[path[1]], path)
  }
  return(rownames(A)[path])
}


#' @rdname distances
#' @examples
#' \donttest{
#' A <- matrix(
#'   c(
#'     0, 3, 3, 10, 15, 0, 0, 0,
#'     1, 0, 5, 2, 7, 0, 0, 0,
#'     3, 5, 0, 0, 0, 0, 0, 0,
#'     10, 2, 0, 0, 2, 7, 12, 0,
#'     11, 3, 0, 3, 0, 11, 2, 0,
#'     0, 0, 0, 7, 11, 0, 3, 2,
#'     0, 0, 0, 12, 2, 3, 0, 2,
#'     0, 0, 0, 0, 0, 2, 2, 0
#'   ),
#'   byrow = TRUE, ncol = 8, nrow = 8
#' )
#' rownames(A) <- c("a", "b", "s", "c", "d", "e", "f", "z")
#' colnames(A) <- rownames(A)
#' wall_distances(A, select = "in")
#' }
#' @export

wall_distances <- function(A, select = c("all", "in", "out")) {
  A <- as.matrix(A)
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  # A binary matrix is a valued matrix in which every tie has length one
  if (is.null(rownames(A))) {
    rownames(A) <- as.character(1:nrow(A))
    colnames(A) <- rownames(A)
  }

  select <- switch(node_direction(select),
    "out" = 1,
    "in" = 2,
    "all" = 3
  )

  # init -> fin, for every pair of nodes
  fromTo <- list()
  toFrom <- list()
  for (i in 1:ncol(A)) {
    paths_from <- list()
    paths_to <- list()
    for (j in 1:ncol(A)) {
      if (select != 1) {
        paths_from[j] <- list(dijkstra_path(A, from = rownames(A)[i], to = rownames(A)[j]))
      }
      if (select != 2) {
        paths_to[j] <- list(dijkstra_path(A, from = rownames(A)[j], to = rownames(A)[i]))
      }
    }
    if (select != 1) {
      names(paths_from) <- rownames(A)
      fromTo[[i]] <- paths_from
    }
    if (select != 2) {
      names(paths_to) <- rownames(A)
      toFrom[[i]] <- paths_to
    }
  }
  names(fromTo) <- rownames(A)[seq_along(fromTo)]
  names(toFrom) <- rownames(A)[seq_along(toFrom)]

  if (select == 1) {
    return(list(toFrom = toFrom))
  }
  if (select == 2) {
    return(list(fromTo = fromTo))
  }
  if (select == 3) {
    return(list(fromTo = fromTo, toFrom = toFrom))
  }
}

#' Geodesic distances
#'
#' Matrix of geodesic distances and a summary of the distances of the network.
#'
#' \code{geo_distances} returns the length of the shortest path between every pair of nodes,
#' computed with the Floyd-Warshall algorithm in matrix form. The distance is infinite when
#' there is no path. For valued matrices, the weights are treated as strengths and transformed
#' into lengths as \eqn{1 / w^{\alpha}} (Opsahl et al., 2010).
#'
#' \code{geo_summary} returns the diameter (the longest geodesic distance), the average distance
#' and the proportion of ordered pairs that can reach each other. When the network is
#' disconnected, both the diameter and the average distance only consider the pairs that are
#' connected by a path.
#'
#' @name geodesics
#'
#' @param A   A square matrix
#' @param digraph   Whether the matrix is directed or undirected
#' @param type   Whether to use the \code{out} (default), \code{in} or \code{all} distances
#' @param weighted   Whether the matrix is weighted
#' @param alpha   The tuning parameter of Opsahl et al. (2010) to transform weights into lengths
#'
#' @return \code{geo_distances} returns a matrix of distances, and \code{geo_summary} the diameter, the average distance and the proportion of reachable pairs.
#'
#' @references
#'
#' Opsahl, T., Agneessens, F., and Skvoretz, J. (2010). Node centrality in weighted networks: Generalizing degree and shortest paths. Social Networks, 32(3), 245–251. \doi{10.1016/j.socnet.2010.03.006}
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 0, 0, 0,
#'   0, 0, 0, 1, 1, 0,
#'   0, 0, 0, 0, 1, 0,
#'   0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 1,
#'   0, 0, 0, 0, 0, 0
#' ), byrow = TRUE, nrow = 6)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- letters[1:ncol(A)]
#'
#' geo_distances(A)
#' geo_summary(A)
#' @export

geo_distances <- function(A, digraph = TRUE, type = c("out", "in", "all"),
                          weighted = FALSE, alpha = 1) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  type <- match.arg(type)

  if (!digraph | type == "all") {
    A <- pmax(A, t(A)) # Underlying graph
  }
  D <- geodesic_distances(A, weighted = weighted, alpha = alpha)
  if (type == "in") {
    D <- t(D)
  }
  return(D)
}

#' @rdname geodesics
#' @export

geo_summary <- function(A, digraph = TRUE, weighted = FALSE, alpha = 1) {
  D <- geo_distances(A, digraph = digraph, weighted = weighted, alpha = alpha)
  diag(D) <- NA

  reachable <- is.finite(D) & !is.na(D)
  if (!any(reachable)) stop("No node can reach another node")

  return(list(
    diameter = max(D[reachable]),
    average_distance = mean(D[reachable]),
    prop_reachable = sum(reachable) / (nrow(D) * (nrow(D) - 1))
  ))
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
