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
#' @export

bfs_ugraph <- function(A, from = NULL) {
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
#' @export

count_geodesics <- function(A) {
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
      counts[j, neighbors] <- counts[j, neighbors] + counts[j, node]

      first_buffer <- c(first_buffer, neighbors)
    }
  }

  return(list(distances = distances, counts = counts))
}

#' @rdname distances
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
#' short_path(A, from = "a", to = "d")
#' @export

short_path <- function(A, from = NULL, to = NULL) {
  pointers <- bfs_ugraph(A, from = from)$pointers

  from <- which(rownames(A) %in% from)
  to <- which(rownames(A) %in% to)

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
#' @export

wlocal_distances <- function(A, select = c("all", "in", "out"),
                             from, to, path = c()) {
  if (!any(abs(A > 1))) stop("Not a valued matrix")

  adjlist <- matrix_adjlist(A)
  edgelist <- as.data.frame(matrix_to_edgelist(A, valued = TRUE, digraph = TRUE))
  edgelist$V3 <- as.numeric(edgelist$V3)
  test <- list()
  test <- internal_distances(adjlist, init = from, fin = to, walk = path)
  return(list(path = test))
}

internal_distances <- function(A, init, fin, walk = c()) {
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
      newwalk <- internal_distances(A, node, fin, walk)
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

#' @rdname distances
#' @examples
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
#' @export

wall_distances <- function(A, select = c("all", "in", "out")) {
  if (!any(abs(A > 1))) stop("Not a valued matrix")
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
        temp4[[j]] <- internal_distances(adjlist, init = rownames(A)[j], fin = rownames(A)[i])
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
        temp2[[j]] <- internal_distances(adjlist, init = rownames(A)[i], fin = rownames(A)[j])
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
        temp2[[j]] <- internal_distances(adjlist, init = rownames(A)[i], fin = rownames(A)[j])
      }
      temp1[[i]] <- temp2[[j]]
      names(temp1)[i] <- rownames(A)[i]
    }

    # fin -> init
    temp3 <- list()
    temp4 <- list()
    for (i in 1:ncol(A)) {
      for (j in i:ncol(A)) {
        temp4[[j]] <- internal_distances(adjlist, init = rownames(A)[j], fin = rownames(A)[i])
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
