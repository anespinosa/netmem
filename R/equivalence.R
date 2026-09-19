#' Block densities and image matrix
#'
#' Densities of the blocks given by a partition of the nodes, and the image matrix that
#' summarises them (Lorrain and White, 1971; Wasserman and Faust, 1994).
#'
#' A partition of the nodes divides the matrix into blocks. The density of a block is the
#' proportion of the possible ties that are present, and the image matrix assigns a one to the
#' blocks whose density is at least the \code{cutoff}. The usual criterion is the density of
#' the whole network (\code{cutoff = "density"}), so a block is one when it is denser than the
#' network as a whole.
#'
#' The diagonal blocks contain the ties within a position, where the possible ties exclude the
#' loops unless \code{loops = TRUE}.
#'
#' @param A   A square matrix
#' @param partition   A vector with the position of each node
#' @param cutoff   Density above which a block is a one in the image matrix: a number, or \code{density} (default) for the density of the whole network
#' @param loops   Whether the loops are counted as possible ties in the diagonal blocks
#'
#' @return This function returns the density of each block, the image matrix and the density of the network.
#'
#' @references
#'
#' Lorrain, F. and White, H. C. (1971). Structural equivalence of individuals in social networks. Journal of Mathematical Sociology, 1(1), 49–80. \doi{10.1080/0022250X.1971.9989788}
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 0, 0, 0,
#'   1, 0, 1, 0, 0, 0,
#'   1, 1, 0, 1, 1, 1,
#'   0, 0, 1, 0, 0, 0,
#'   0, 0, 1, 0, 0, 0,
#'   0, 0, 1, 0, 0, 0
#' ), byrow = TRUE, ncol = 6)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- rownames(A)
#'
#' block_density(A, partition = c(1, 1, 1, 2, 2, 2))
#' @export

block_density <- function(A, partition, cutoff = "density", loops = FALSE) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (length(partition) != nrow(A)) stop("The partition should have one position for each node")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  if (!loops) {
    diag(A) <- NA
  }

  positions <- sort(unique(partition))
  densities <- matrix(NA, length(positions), length(positions),
    dimnames = list(positions, positions)
  )
  for (i in seq_along(positions)) {
    for (j in seq_along(positions)) {
      block <- A[partition == positions[i], partition == positions[j], drop = FALSE]
      densities[i, j] <- mean(block, na.rm = TRUE)
    }
  }

  density <- mean(A, na.rm = TRUE)
  if (identical(cutoff, "density")) {
    cutoff <- density
  }
  image <- 1 * (densities >= cutoff)

  return(list(densities = densities, image = image, density = density))
}


#' CONCOR
#'
#' Convergence of iterated correlations (Breiger, Boorman and Arabie, 1975), a partition of the
#' nodes into positions of structurally equivalent actors.
#'
#' The rows and the columns of the matrix describe how each node relates to the others, so they
#' are stacked into a profile. The correlations between the profiles of every pair of nodes are
#' computed, and the correlations of those correlations are computed again and again. The
#' matrix converges to a matrix of ones and minus ones, which splits the nodes into two
#' positions. The procedure is repeated within each position, so \code{splits} divisions give
#' at most \eqn{2^{splits}} positions.
#'
#' @param A   A square matrix, or a list of matrices of the same order for multiple relations
#' @param splits   Number of successive divisions
#' @param max_iter   Maximum number of iterated correlations
#' @param tol   Tolerance to decide that the correlations have converged
#'
#' @return This function returns the position of each node and the number of positions.
#'
#' @references
#'
#' Breiger, R. L., Boorman, S. A. and Arabie, P. (1975). An algorithm for clustering relational data with applications to social network analysis and comparison with multidimensional scaling. Journal of Mathematical Psychology, 12(3), 328–383. \doi{10.1016/0022-2496(75)90028-0}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 0, 0, 0,
#'   1, 0, 1, 0, 0, 0,
#'   1, 1, 0, 0, 0, 0,
#'   0, 0, 0, 0, 1, 1,
#'   0, 0, 0, 1, 0, 1,
#'   0, 0, 0, 1, 1, 0
#' ), byrow = TRUE, ncol = 6)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- rownames(A)
#'
#' concor(A, splits = 1)
#' @export

concor <- function(A, splits = 1, max_iter = 50, tol = 1e-8) {
  if (!is.list(A)) {
    A <- list(A)
  }
  A <- lapply(A, as.matrix)
  n <- nrow(A[[1]])
  for (k in seq_along(A)) {
    if (nrow(A[[k]]) != ncol(A[[k]])) stop("Matrix should be square")
    if (nrow(A[[k]]) != n) stop("The matrices should have the same order")
  }
  labels <- rownames(A[[1]])
  if (is.null(labels)) {
    labels <- as.character(seq_len(n))
  }

  # The profile of a node stacks its rows and its columns in every relation
  profile <- do.call(rbind, c(A, lapply(A, t)))

  partition <- rep(1, n)
  for (s in seq_len(splits)) {
    new_partition <- partition
    for (position in unique(partition)) {
      members <- which(partition == position)
      if (length(members) < 2) next
      side <- concor_split(profile[, members, drop = FALSE], max_iter = max_iter, tol = tol)
      if (all(side) | all(!side)) next
      new_partition[members[side]] <- paste0(position, "1")
      new_partition[members[!side]] <- paste0(position, "2")
    }
    partition <- new_partition
  }

  partition <- as.numeric(factor(partition))
  names(partition) <- labels
  return(list(partition = partition, positions = length(unique(partition))))
}

# One division: the correlations are iterated until they converge to 1 and -1,
# and the sign of the first column splits the nodes into two groups
concor_split <- function(profile, max_iter, tol) {
  M <- stats::cor(profile)
  M[is.na(M)] <- 0
  for (i in seq_len(max_iter)) {
    M_new <- stats::cor(M)
    M_new[is.na(M_new)] <- 0
    if (max(abs(abs(M_new) - 1)) < tol) {
      M <- M_new
      break
    }
    M <- M_new
  }
  M[1, ] >= 0
}


#' Regular equivalence
#'
#' REGE algorithm (White and Reitz, 1983; Borgatti and Everett, 1993): two nodes are regularly
#' equivalent when they are connected to nodes that are themselves equivalent, even if they are
#' not connected to the same nodes.
#'
#' Structural equivalence asks for the same neighbours, while regular equivalence only asks for
#' neighbours that play the same role. Each alter of a node is matched with the alter of the
#' other node that resembles it the most, and the similarities are computed again with the
#' matches of the previous iteration.
#'
#' @param A   A square matrix, which can be valued
#' @param iter   Number of iterations
#'
#' @return This function returns a matrix with the regular equivalence of every pair of nodes, between zero and one.
#'
#' @references
#'
#' Borgatti, S. P. and Everett, M. G. (1993). Two algorithms for computing regular equivalence. Social Networks, 15(4), 361–376. \doi{10.1016/0378-8733(93)90012-A}
#'
#' White, D. R. and Reitz, K. P. (1983). Graph and semigroup homomorphisms on networks of relations. Social Networks, 5(2), 193–234. \doi{10.1016/0378-8733(83)90025-4}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' # Two managers with different subordinates play the same role
#' A <- matrix(c(
#'   0, 1, 1, 0, 0, 0, 0,
#'   0, 0, 0, 1, 1, 0, 0,
#'   0, 0, 0, 0, 0, 1, 1,
#'   0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0
#' ), byrow = TRUE, ncol = 7)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- rownames(A)
#'
#' round(rege(A), 3)
#' @export

rege <- function(A, iter = 3) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  n <- nrow(A)
  diag(A) <- 0

  similarity <- matrix(1, n, n)
  for (it in seq_len(iter)) {
    previous <- similarity
    similarity <- matrix(0, n, n)
    for (i in 1:n) {
      for (j in 1:n) {
        matched <- 0
        total <- 0
        # Every alter of i is matched with the alter of j that resembles it the
        # most, in both directions of the tie, and the other way around
        for (k in 1:n) {
          if (A[i, k] > 0 | A[k, i] > 0) {
            total <- total + A[i, k] + A[k, i]
            best <- 0
            for (m in 1:n) {
              if (A[j, m] > 0 | A[m, j] > 0) {
                fit <- (min(A[i, k], A[j, m]) + min(A[k, i], A[m, j])) * previous[k, m]
                if (fit > best) best <- fit
              }
            }
            matched <- matched + best
          }
        }
        for (m in 1:n) {
          if (A[j, m] > 0 | A[m, j] > 0) {
            total <- total + A[j, m] + A[m, j]
            best <- 0
            for (k in 1:n) {
              if (A[i, k] > 0 | A[k, i] > 0) {
                fit <- (min(A[i, k], A[j, m]) + min(A[k, i], A[m, j])) * previous[k, m]
                if (fit > best) best <- fit
              }
            }
            matched <- matched + best
          }
        }
        if (total == 0) {
          similarity[i, j] <- 1 # two isolates play the same role
        } else {
          similarity[i, j] <- matched / total
        }
      }
    }
  }

  dimnames(similarity) <- dimnames(A)
  return(similarity)
}
