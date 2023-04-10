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
#'
#' @return This function return the counts of the dyad census.
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
#' @importFrom stats rbinom
#'
#' @export

ind_rand_matrix <- function(n, m = NULL,
                            type = c("edges", "probability"),
                            digraph = TRUE, loops = FALSE,
                            l = NULL, p = NULL, trials = 1, multilevel = FALSE) {
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

  if (!is.null(m)) {
    # TWO-MODE
    if (type == 1) {
      A <- matrix(0, ncol = n, nrow = m, byrow = TRUE)
      while (l) {
        i <- sample(m, 1)
        j <- sample(n, 1)
        A[i, j] <- 1
        if (sum(A) == l) break
      }
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
            B <- matrix(0, ncol = n, nrow = n, byrow = TRUE)
            while (l) {
              i <- sample(n, 1)
              j <- sample(n, 1)
              if (i == j) next
              B[i, j] <- 1
              if (sum(B) == l) break
            }
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
            B <- matrix(0, ncol = n, nrow = n, byrow = TRUE)
            while (l) {
              i <- sample(n, 1)
              j <- sample(n, 1)
              B[i, j] <- 1
              if (sum(B) == l) break
            }
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
            B <- matrix(0, ncol = n, nrow = n, byrow = TRUE)
            while (l) {
              i <- sample(n, 1)
              B[upper.tri(B, diag = FALSE)][i] <- 1
              print(sum(B))
              if (sum(B) == l) break
            }
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
            B <- matrix(0, ncol = n, nrow = n, byrow = TRUE)
            while (l) {
              i <- sample(n, 1)
              B[upper.tri(B, diag = TRUE)][i] <- 1
              print(sum(B))
              if (sum(B) == l) break
            }
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
          A <- matrix(0, ncol = n, nrow = n, byrow = TRUE)
          while (l) {
            i <- sample(n, 1)
            j <- sample(n, 1)
            if (i == j) next
            A[i, j] <- 1
            if (sum(A) == l) break
          }
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
          A <- matrix(0, ncol = n, nrow = n, byrow = TRUE)
          while (l) {
            i <- sample(n, 1)
            j <- sample(n, 1)
            A[i, j] <- 1
            if (sum(A) == l) break
          }
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
          A <- matrix(0, ncol = n, nrow = n, byrow = TRUE)
          while (l) {
            i <- sample(n, 1)
            A[upper.tri(A, diag = FALSE)][i] <- 1
            print(sum(A))
            if (sum(A) == l) break
          }
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
          A <- matrix(0, ncol = n, nrow = n, byrow = TRUE)
          while (l) {
            i <- sample(n, 1)
            A[upper.tri(A, diag = TRUE)][i] <- 1
            print(sum(A))
            if (sum(A) == l) break
          }
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
