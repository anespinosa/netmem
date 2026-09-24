#' Independent random matrix
#'
#' The function creates random matrices following a uniform probability over the space of networks having exactly m fixed number of edges (Moreno and Jennings, 1938; Rapoport, 1948; Solomonoff and Rapoport, 1951; Erdos and Renyi, 1959) or following a probability of the formation of the ties (Gilbert, 1959) assuming ties independency.
#'
#' The fixed model is often called the G(n,m) graph with 'n' nodes and 'm' edges, and the 'm' edges are chosen uniformly randomly from the set of all possible ties.
#'
#' The probability model is known as the G(n,p) graph, in which the matrix has 'n' nodes, and for each tie, the probability that it is present in the matrix is 'p'.
#'
#' These are the simplest models that follow a conditional uniform distribution that place nonnull probability on a subset of networks with distinctive characteristics corresponding to the observed networks - for example, simulating a matrix based on the number of ties observed in the network.
#'
#' @param n   The number of nodes of the first set
#' @param m   The  number of nodes of a second set
#' @param type  The model assumes a fixed number of \code{edges} model (a.k.a. G(n,m)) (default) or a \code{probability} model (a.k.a. G(n,p))
#' @param digraph  Whether the matrix is symmetric or not
#' @param loops  Whether to expect nonzero elements in the diagonal of the matrix
#' @param l  The number of ties expected for the \code{edges} (a.k.a. G(n,m)) model
#' @param p  The probability of the ties expected for the \code{probability} (a.k.a. G(n,p)) model. If no parameter `p` is specified, a uniform distribution is considered (p=0.5).
#' @param trials  Whether to add counting numbers to the \code{probability} (a.k.a. G(n,p)) model
#' @param multilevel Whether to return a meta-matrix to represent a multilevel network
#' @param sparse Whether to return a sparse matrix of the \code{Matrix} package, which is built without the dense matrix
#'
#' @details
#' With \code{sparse = TRUE} the ties are drawn among the cells that the model allows, and only they are stored, so the
#' memory grows with the number of ties and not with the square of the number of nodes. The two models are the same
#' as with a dense matrix: the number of ties of \code{G(n,p)} is binomial, which is what drawing every tie
#' independently gives. It is available for one-mode and two-mode networks with \code{trials = 1}.
#'
#' @return This function returns a random matrix, or a list of matrices for a multilevel network.
#'
#' @references
#'
#' Erdos, P. and Renyi, A. (1959). On random graphs. Publicationes Mathematicae 6, 290–297.
#'
#' Gilbert, N. (1959). Random Graphs. The Annals of Mathematical Statistics, 30(4): 1141-1144.
#'
#' Moreno, J. and Jennings, H. (1938). Statistics of social configurations. Sociometry, 1(3/4):342–374.
#'
#' Rapoport, A. (1948). Cycle distributions in random nets. Bulletin of Mathematical Biology, 10(3):145–157.
#'
#' Solomonoff, R. and Rapoport, A. (1951). Connectivity of random nets. Bulletin of Mathematical Biology, 13:107–117.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' set.seed(18051889)
#' ind_rand_matrix(5, type = "edges", l = 3, digraph = TRUE, loops = TRUE)
#' ind_rand_matrix(5, type = "probability")
#' ind_rand_matrix(n = 5, m = 2, p = 0.20, type = "probability", multilevel = TRUE)
#'
#' # Large networks are cheaper as sparse matrices
#' dim(ind_rand_matrix(10000, type = "edges", l = 5000, digraph = FALSE, sparse = TRUE))
#' @importFrom stats rbinom
#' @importFrom Matrix sparseMatrix
#'
#' @export

ind_rand_matrix <- function(n, m = NULL,
                            type = c("edges", "probability"),
                            digraph = TRUE, loops = FALSE,
                            l = NULL, p = NULL, trials = 1, multilevel = FALSE,
                            sparse = FALSE) {
  type <- switch(graph_type(type),
    "edges" = 1,
    "probability" = 2
  )

  if (trials > 1) {
    trials <- as.integer(trials)
  }

  if (type == 1) {
    if (is.null(l)) {
      stop("The number of fixed ties is not specified")
    }
  }

  if (type == 2) {
    if (is.null(p)) {
      p <- 0.5
    }
  }

  if (sparse) {
    if (multilevel) stop("The sparse matrix is not available for multilevel networks")
    if (trials > 1) stop("The sparse matrix is available for binary ties, with trials = 1")
    if (type == 1 && is.null(l)) stop("The number of fixed ties is not specified")
    return(sparse_rand_matrix(
      n = n, m = m, type = type, digraph = digraph,
      loops = loops, l = l, p = p
    ))
  }

  if (!is.null(m)) {
    # TWO-MODE
    if (type == 1) {
      A <- fixed_ties(matrix(TRUE, nrow = m, ncol = n), l)
    }

    if (type == 2) {
      ties <- n * m
      A <- matrix(c(rbinom(ties, trials, p)),
        ncol = n, nrow = m, byrow = TRUE
      )
    }

    if (multilevel) {
      # MULTILEVEL
      if (digraph) {
        if (!loops) {
          # DIRECTED AND NO LOOPS
          if (type == 1) {
            B <- fixed_ties(row(diag(n)) != col(diag(n)), l)
          }

          if (type == 2) {
            ties <- n * (n - 1)
            B <- matrix(0, ncol = n, nrow = n, byrow = TRUE)
            edges <- c(rbinom(ties, trials, p))
            B[upper.tri(B, diag = FALSE)] <- edges[1:(ties / 2)]
            B[lower.tri(B, diag = FALSE)] <- edges[((ties / 2) + 1):ties]
          }
        } else {
          # DIRECTED AND LOOPS
          if (type == 1) {
            B <- fixed_ties(matrix(TRUE, n, n), l)
          }

          if (type == 2) {
            ties <- n * n
            B <- matrix(c(rbinom(ties, trials, p)),
              ncol = n, nrow = n, byrow = TRUE
            )
          }
        }
      } else {
        if (!loops) {
          # UNDIRECTED AND NO LOOPS
          if (type == 1) {
            B <- fixed_ties(upper.tri(diag(n)), l)
            B[lower.tri(B)] <- t(B)[lower.tri(B)]
          }

          if (type == 2) {
            ties <- (n * (n - 1)) / 2
            B <- matrix(0, ncol = n, nrow = n, byrow = TRUE)
            edges <- c(rbinom(ties, trials, p))
            B[upper.tri(B, diag = FALSE)] <- edges[1:ties]
            B[lower.tri(B)] <- t(B)[lower.tri(B)]
          }
        } else {
          # UNDIRECTED AND LOOPS

          if (type == 1) {
            B <- fixed_ties(upper.tri(diag(n), diag = TRUE), l)
            B[lower.tri(B)] <- t(B)[lower.tri(B)]
          }

          if (type == 2) {
            ties <- (n * (n - 1)) / 2 + n
            B <- matrix(0, ncol = n, nrow = n, byrow = TRUE)
            edges <- c(rbinom(ties, trials, p))
            B[upper.tri(B, diag = TRUE)] <- edges[1:ties]
            B[lower.tri(B)] <- t(B)[lower.tri(B)]
          }
        }
      }
      M <- meta_matrix(B, t(A))
      rownames(M) <- c(paste("n", 1:ncol(B), sep = ""), paste("m", 1:nrow(A), sep = ""))
      colnames(M) <- rownames(M)
      return(M)
    } else {
      # ONLY TWO-MODE
      return(A)
    }
  } else {
    # ONE-MODE NETWORK
    if (digraph) {
      if (!loops) {
        # DIRECTED AND NO LOOPS
        if (type == 1) {
          A <- fixed_ties(row(diag(n)) != col(diag(n)), l)
        }

        if (type == 2) {
          ties <- n * (n - 1)
          A <- matrix(0, ncol = n, nrow = n, byrow = TRUE)
          edges <- c(rbinom(ties, trials, p))
          A[upper.tri(A, diag = FALSE)] <- edges[1:(ties / 2)]
          A[lower.tri(A, diag = FALSE)] <- edges[((ties / 2) + 1):ties]
        }
      } else {
        # DIRECTED AND LOOPS
        if (type == 1) {
          A <- fixed_ties(matrix(TRUE, n, n), l)
        }

        if (type == 2) {
          ties <- n * n
          A <- matrix(c(rbinom(ties, trials, p)),
            ncol = n, nrow = n, byrow = TRUE
          )
        }
      }
    } else {
      if (!loops) {
        # UNDIRECTED AND NO LOOPS
        if (type == 1) {
          A <- fixed_ties(upper.tri(diag(n)), l)
          A[lower.tri(A)] <- t(A)[lower.tri(A)]
        }

        if (type == 2) {
          ties <- (n * (n - 1)) / 2
          A <- matrix(0, ncol = n, nrow = n, byrow = TRUE)
          edges <- c(rbinom(ties, trials, p))
          A[upper.tri(A, diag = FALSE)] <- edges[1:ties]
          A[lower.tri(A)] <- t(A)[lower.tri(A)]
        }
      } else {
        # UNDIRECTED AND LOOPS

        if (type == 1) {
          A <- fixed_ties(upper.tri(diag(n), diag = TRUE), l)
          A[lower.tri(A)] <- t(A)[lower.tri(A)]
        }

        if (type == 2) {
          ties <- (n * (n - 1)) / 2 + n
          A <- matrix(0, ncol = n, nrow = n, byrow = TRUE)
          edges <- c(rbinom(ties, trials, p))
          A[upper.tri(A, diag = TRUE)] <- edges[1:ties]
          A[lower.tri(A)] <- t(A)[lower.tri(A)]
        }
      }
    }
  }
  return(A)
}

graph_type <- function(arg, choices, several.ok = FALSE) {
  if (missing(choices)) {
    formal.args <- formals(sys.function(sys.parent()))
    choices <- eval(formal.args[[deparse(substitute(arg))]])
  }

  arg <- tolower(arg)
  choices <- tolower(choices)

  match.arg(arg = arg, choices = choices, several.ok = several.ok)
}


#' Small world network
#'
#' Random network of Watts and Strogatz (1998), which has the short distances of a random
#' network and the high transitivity of a regular one.
#'
#' The network starts as a ring in which every node is tied to its \code{neighbours} closest
#' nodes on each side. Then each tie is rewired with probability \code{p}: one of its ends is
#' replaced by a node chosen at random, avoiding loops and repeated ties. With \code{p = 0} the
#' ring is left as it is, and with \code{p = 1} every tie is rewired.
#'
#' @param n   The number of nodes
#' @param neighbours   The number of closest nodes on each side that every node is tied to in the ring
#' @param p   Probability of rewiring each tie
#'
#' @return This function returns a symmetric matrix.
#'
#' @references
#'
#' Watts, D. J. and Strogatz, S. H. (1998). Collective dynamics of 'small-world' networks. Nature, 393(6684), 440–442. \doi{10.1038/30918}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' set.seed(18051889)
#' A <- small_world(20, neighbours = 2, p = 0.05)
#' gen_degree(A, digraph = FALSE)
#' @export

small_world <- function(n, neighbours = 2, p = 0.05) {
  if (n < 2 * neighbours + 1) stop("The number of nodes should be larger than twice the neighbours")
  if (p < 0 | p > 1) stop("p should be a probability")

  A <- matrix(0, n, n, dimnames = list(as.character(1:n), as.character(1:n)))
  for (i in 1:n) {
    for (k in 1:neighbours) {
      j <- ((i + k - 1) %% n) + 1 # the ring closes on itself
      A[i, j] <- 1
      A[j, i] <- 1
    }
  }

  ties <- which(upper.tri(A) & A > 0, arr.ind = TRUE)
  for (e in seq_len(nrow(ties))) {
    if (stats::runif(1) >= p) next
    i <- ties[e, 1]
    # The new partner is any node that is not already tied to i
    candidates <- setdiff(which(A[i, ] == 0), i)
    if (length(candidates) == 0) next
    new <- candidates[sample.int(length(candidates), 1)]
    A[ties[e, 1], ties[e, 2]] <- 0
    A[ties[e, 2], ties[e, 1]] <- 0
    A[i, new] <- 1
    A[new, i] <- 1
  }

  return(A)
}


#' Preferential attachment network
#'
#' Random network of Barabasi and Albert (1999), in which the nodes that already have more ties
#' are more likely to receive the ties of the nodes that arrive.
#'
#' The network starts with \code{m} nodes without ties. Every new node creates \code{m} ties
#' with the nodes that are already there, choosing each of them with a probability proportional
#' to their degree raised to \code{power}. With \code{power = 0} the nodes are chosen at random,
#' and the higher the power, the more the ties concentrate in a few nodes.
#'
#' @param n   The number of nodes
#' @param m   The number of ties that every new node creates
#' @param power   The power of the degree in the probability of being chosen
#' @param digraph   Whether the ties of the new nodes are directed towards the nodes that are already there
#'
#' @return This function returns a matrix.
#'
#' @references
#'
#' Barabasi, A. L. and Albert, R. (1999). Emergence of scaling in random networks. Science, 286(5439), 509–512. \doi{10.1126/science.286.5439.509}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' set.seed(18051889)
#' A <- pref_attachment(20, m = 2)
#' gen_degree(A, digraph = FALSE)
#' @export

pref_attachment <- function(n, m = 1, power = 1, digraph = FALSE) {
  if (n <= m) stop("The number of nodes should be larger than the ties of each new node")
  if (m < 1) stop("Every new node should create at least one tie")

  A <- matrix(0, n, n, dimnames = list(as.character(1:n), as.character(1:n)))
  for (new in (m + 1):n) {
    present <- 1:(new - 1)
    degree <- rowSums(A[present, , drop = FALSE]) + colSums(A[, present, drop = FALSE])
    # The first node arrives when nobody has ties, so the choice is at random
    weight <- degree^power
    if (all(weight == 0)) {
      weight <- rep(1, length(present))
    }
    chosen <- sample(present, min(m, length(present)), replace = FALSE, prob = weight)
    A[new, chosen] <- 1
    if (!digraph) {
      A[chosen, new] <- 1
    }
  }

  return(A)
}

# A matrix with exactly l ties, placed at random among the eligible cells (a
# logical matrix): the cells of the upper triangle for an undirected network,
# every cell but the diagonal for a directed network without loops
fixed_ties <- function(eligible, l) {
  if (l > sum(eligible)) stop(paste("There are only", sum(eligible), "possible ties, fewer than l =", l))
  M <- matrix(0, nrow(eligible), ncol(eligible))
  M[sample(which(eligible), l)] <- 1
  return(M)
}

# Cells drawn uniformly among the ones that the model allows, without building
# the matrix. With a fixed number of ties they are a sample of that size, and
# with a probability the number of ties is binomial, which is the same as
# drawing every cell independently
sampled_cells <- function(total, type, l, p) {
  if (type == 1) {
    if (l > total) {
      stop("The number of ties is larger than the number of cells of the matrix")
    }
    k <- l
  } else {
    k <- stats::rbinom(1, total, p)
  }
  sample.int(total, k)
}

# Row and column of the k-th cell of a matrix, counting the cells by columns,
# as `which()` does. Every family of cells has its own arithmetic, so that the
# logical matrix of the dense version is never created
cells_full <- function(k, nrow) {
  list(i = ((k - 1) %% nrow) + 1, j = ((k - 1) %/% nrow) + 1)
}

cells_nodiag <- function(k, n) {
  j <- ceiling(k / (n - 1))
  r <- k - (j - 1) * (n - 1)
  list(i = r + (r >= j), j = j)
}

cells_upper <- function(k, loops) {
  if (loops) {
    # Column j holds the j cells with i <= j
    j <- ceiling((sqrt(8 * k + 1) - 1) / 2)
    j <- j + (k > j * (j + 1) / 2) - (k <= j * (j - 1) / 2) # The square root rounds
    i <- k - j * (j - 1) / 2
  } else {
    # Column j holds the j - 1 cells with i < j
    j <- ceiling((sqrt(8 * k + 1) + 1) / 2)
    j <- j + (k > j * (j - 1) / 2) - (k <= (j - 1) * (j - 2) / 2)
    i <- k - (j - 1) * (j - 2) / 2
  }
  list(i = i, j = j)
}

sparse_rand_matrix <- function(n, m, type, digraph, loops, l, p) {
  if (!is.null(m)) {
    # TWO-MODE: every cell can hold a tie
    cells <- sampled_cells(n * m, type, l, p)
    ij <- cells_full(cells, m)
    return(Matrix::sparseMatrix(i = ij$i, j = ij$j, x = 1, dims = c(m, n)))
  }

  if (digraph) {
    if (loops) {
      ij <- cells_full(sampled_cells(n * n, type, l, p), n)
    } else {
      ij <- cells_nodiag(sampled_cells(n * (n - 1), type, l, p), n)
    }
    return(Matrix::sparseMatrix(i = ij$i, j = ij$j, x = 1, dims = c(n, n)))
  }

  # UNDIRECTED: the ties are drawn in one triangle and placed in both
  if (loops) {
    ij <- cells_upper(sampled_cells(n * (n + 1) / 2, type, l, p), loops = TRUE)
  } else {
    ij <- cells_upper(sampled_cells(n * (n - 1) / 2, type, l, p), loops = FALSE)
  }
  mirror <- ij$i != ij$j # A loop is a single cell
  Matrix::sparseMatrix(
    i = c(ij$i, ij$j[mirror]), j = c(ij$j, ij$i[mirror]), x = 1,
    dims = c(n, n)
  )
}
